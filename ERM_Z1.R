#Ермошина Анастасия ПАЭ124
#Для региона 28 рассчитать урожайность пшеницы в 2011 году, взяв для рассчета средние суммы 
#активных температур за предыдущие 5 лет, с метеостанций в радиусе не более 300 км
#Проверка рабочей директории
getwd()
#устанавливаем пакеты
install.packages("tidyverse")
install.packages("rnoaa")
install.packages("lubridate")
#подключаем пакеты
library(tidyverse)
library(rnoaa)
library(lubridate)
#скачиваем список метеостанций
station_data = ghcnd_stations()
#сохраняем результат в отдельный файл
write.csv(station_data, file="station_data.csv")
station_data = read.csv("station_data.csv")
#После получения всписка всех станций, выбираем из него список станций ближайших 
#к столице региона,создав таблицу с именем региона и координатами его столицы
blagoveshchensk = data.frame(id = "BLAGOVESHCHENSK", latitude = 50.287077,  longitude = 127.540895)
#выберем станции, для этого введем необходимые переменные и даты, также уберем лимит,тк в задании он не указан
blagoveshchensk_around = meteo_nearby_stations(lat_lon_df = blagoveshchensk, station_data = station_data,
                                var = c("PRCP", "TAVG"),
                                year_min = 2006, year_max = 2011)
#blagoveshchensk_around это список единственным элементом которого является таблица, содержащая идентификаторы метеостанций отсортированных по их 
#удаленности от Благовещенска, очевидно что первым элементом таблицы будет идентификатор метеостанции Благовещенска, его то мы и попытаемся получить
blagoveshchensk_id = blagoveshchensk_around[["BLAGOVESHCHENSK"]][["id"]][1]
#чтобы получить таблицу всех метеостанций вокруг Благовещенска нужно выбрать первый объект из списка
blagoveshchensk_table = blagoveshchensk_around[[1]]
blagoveshchensk_table
blagoveshchensk_table$id
#отберем все станции, на расстоянии до 300 км при помощи функции filter
blagoveshchensk_stations=filter(blagoveshchensk_table, distance<=300)
str(blagoveshchensk_stations)
blagoveshchensk_stations$id
#их оказалось 16
#Для получения всех данных с метеостанции, зная ее идентификатор, используйте след. команду
all_blagoveshchensk_data=meteo_tidy_ghcnd(stationid=blagoveshchensk_id)
#Нужно создать цикл, в котором бы скачивались  нужные данные для всех метеостанций из созданного списка
#Создадим промежуточный объект, куда будем скачивать данные с конкретной метеостанции
all_i = data.frame()
#создаем объект, куда будут скачиваться данные всех метеостанций
all_blagoveshchensk_meteodata = data.frame()
#Цикл для 16 метеостанций
for(i in 1:16) 
{
  all_i=meteo_tidy_ghcnd(stationid=blagoveshchensk_stations$id[i])
  all_i=all_i[,c("id","date","tavg")]
  all_blagoveshchensk_meteodata=rbind(all_blagoveshchensk_meteodata,all_i)
}
#сохраняем результат в отдельный файл
write.csv(all_blagoveshchensk_meteodata,file="all_blagoveshchensk_meteodata.csv")
#добавим год,месяц и день
all_blagoveshchensk_meteodata = mutate(all_blagoveshchensk_meteodata, year=year(date),month=month(date),day=day(date))
#отфильтруем данные за 2006-2011
years_blagoveshchensk_meteodata = filter(all_blagoveshchensk_meteodata, year>2006&year<2011)
# Разделим температуру на 10, для дальнейших рассчетов
years_blagoveshchensk_meteodata[,"tavg"]= years_blagoveshchensk_meteodata$tavg / 10
#Для рассчета нам нужны суммы активных температур,т.е температур больше 5 градусов
#Превратим в нули все NA и где tavg<5
years_blagoveshchensk_meteodata [is.na(years_blagoveshchensk_meteodata$tavg), "tavg"] = 0
years_blagoveshchensk_meteodata [years_blagoveshchensk_meteodata$tavg<5, "tavg"] = 0
# Расчитаем суммарную температуру за месяц за 5 лет для всех станций 
# группируем по метеостанциям, годам и месяцам
alldays = group_by(years_blagoveshchensk_meteodata,id,year,month)
#функция summarize применяет некоторые действия к отдельным группам, полученным с помощью функции group_by
#просуммирую температуру по этим группам с помощью sum
sumT_alldays_blagoveshchensk = summarize(alldays, tsum = sum(tavg))
#рассчет среднедневной температуры 723/31=23,3 => показатель в норме
summary(sumT_alldays_blagoveshchensk)
#сгруппируем данные по месяцам
groups_blagoveshchensk_months = group_by(sumT_alldays_blagoveshchensk,month)
# найдем для всех метеостанций среднее по месяцам
sumT_months = summarize(groups_blagoveshchensk_months, St = mean(tsum))
#рассчитаем урожайность по формуле
#для этого вводим константы
afi = c(0.000,0.000,0.000,32.110,26.310,25.640,23.200,18.730,16.300,13.830,0.000,0.000)
bfi = c(0.000,0.000,0.000,11.300,9.260,9.030,8.160,6.590,5.730,4.870,0.000,0.000)
di = c(0.000,0.000,0.000,0.330,1.000,1.000,1.000,0.320,0.000,0.000,0.000,0.000) 
y = 1.0
Kf = 300
Qj = 1600
Lj = 2.2 
Ej = 25
# Рассчитаем Fi по месяца
sumT_months = mutate(sumT_months, Fi = afi+bfi*y*St)
#Рассчитаем Yi
sumT_months = mutate(sumT_months, Yi = ((Fi*di)*Kf)/(Qj*Lj*(100 - Ej)))
#Расчитываем урожай как сумму по месяцам
Yield = sum(sumT_months$Yi)
Yield
#урожайность пшеницы в Благовещенске за 2011г составила 17,94 ц/га
