#Ермошина Анастасия
#создайте модель множественной линейной регрессии дневных потоков паров воды за 
#весенний период 2013 года по данным измерений методом турбулентной пульсации
#Проверка рабочей директории
setwd("C:/Group_124/Errmoshina/MathMod")
getwd()
#устанавливаем пакеты
library("tidyverse") 
library("stringr")    
library("dplyr")      
library("ggplot2")
#перед тем, как прочитывать данные пропустим 1 строчку,пропуски данных заменяем на NA, данные нечислового типа тоже. 
#Воспользуемся comment параметром,который заставляет readr игнорировать все строчки, содержащие символ “[”
eddypro = read_csv("eddypro.csv", skip = 1, na=c("","NA","-9999","-9999.0"), comment=c("["))
#удаляем 1 строку
eddypro = eddypro[-1,]
#удаляем пустой столбец
eddypro = select(eddypro, -(roll))
#преобразуем данные типа char в factor
eddypro = eddypro %>% mutate_if(is.character, factor)
#Заменяем символы в названии столбцов на допустимые имена переменных
names(eddypro) = names(eddypro) %>% 
  str_replace_all("[!]", "_exclam_") %>% 
  str_replace_all("[?]", "_quest_") %>% 
  str_replace_all("[*]", "_star_") %>% 
  str_replace_all("[+]", "_plus_") %>%
  str_replace_all("[-]", "_minus_") %>%
  str_replace_all("[@]", "_at_") %>%
  str_replace_all("[$]", "_dollar_") %>%
  str_replace_all("[#]", "_hash_") %>%
  str_replace_all("[/]", "_slash_") %>%
  str_replace_all("[%]", "__pecent_") %>%
  str_replace_all("[&]", "_amp_") %>%
  str_replace_all("[\\^]", "_power_") %>%
  str_replace_all("[()]", "_")
glimpse(eddypro)
#Удaляем NA
eddypro = drop_na(eddypro)
#транспонируем колонки таблицы в векторы, чтобы осуществить проверку
sapply(eddypro,is.numeric)
#возьмем данные за весенний период
eddypro = filter(eddypro, DOY >= 60 & DOY < 152)
# Отфильтруем данные за дневное время
eddypro = filter(eddypro, daytime ==TRUE)
#подставляем вектор в таблицу, получая таблицу только из нужных колонок
eddypro_numeric = eddypro[,sapply(eddypro,is.numeric) ]
#таблица содержащая все колонки
eddypro_non_numeric = eddypro[,!sapply(eddypro,is.numeric) ]
#Сделаем выборки непересекающимися
row_numbers = 1:length(eddypro_numeric$h2o_flux)
teach = sample(row_numbers, floor(length(eddypro_numeric$h2o_flux)*.7))
test = row_numbers[-teach]
#Обучающая выборка
teaching_tbl = eddypro_numeric[teach,]
#Тестирующая выборка
testing_tbl = eddypro_numeric[test,]

#1 модель по обучающей выборке
mod1 = lm(h2o_flux~ (.) , data = teaching_tbl)
#выведем информацию о модели
summary(mod1)
#коэф-ты модели
coef(mod1)
#дисперсионный анализ модели
anova(mod1)
#графическое представление модели:
plot(mod1)
# 2 модель
mod2 = lm (h2o_flux~ DOY + used_records + Tau+qc_Tau + rand_err_Tau + H +qc_H + rand_err_H + LE + qc_LE + rand_err_LE 
           + co2_flux + qc_h2o_flux + rand_err_co2_flux + rand_err_h2o_flux + H_strg + co2_v_minus_adv + h2o_v_minus_adv 
           + co2_molar_density + co2_mole_fraction + co2_mixing_ratio + h2o_molar_density + h2o_mole_fraction + h2o_mixing_ratio 
           + h2o_time_lag + sonic_temperature + air_temperature + air_pressure + air_density + air_heat_capacity + air_molar_volume 
           + water_vapor_density + es + specific_humidity + RH + Tdew + u_unrot + v_unrot + w_unrot + u_rot + v_rot + w_rot + max_speed 
           + wind_dir + yaw + pitch + u_star_ + TKE + L + `_z_minus_d__slash_L` + bowen_ratio + T_star_ + x_peak + x_offset + x_10__pecent_ x_30__percent_ 
           + x_50__percent_ + x_70__pecent_ +x_90__percent_ + un_Tau + Tau_scf + un_H + H_scf + un_LE + LE_scf + un_co2_flux + un_h2o_flux 
           + w_slash_ts_cov + w_slash_co2_cov + w_slash_h2o_cov + co2_1 + h2o_1 + co2_signal_strength_7200 , data = teaching_tbl)
summary(mod2)
coef(mod2)
resid(mod2)
confint(mod2)
anova(mod2)
anova(mod2, mod1)
plot(mod2)
#3 модель
mod3 = lm (h2o_flux~ DOY + used_records + Tau+qc_Tau + rand_err_Tau + H +qc_H + rand_err_H + LE + qc_LE + rand_err_LE
           + co2_flux + qc_h2o_flux + rand_err_co2_flux + rand_err_h2o_flux + H_strg + co2_v_minus_adv + h2o_v_minus_adv 
           + co2_molar_density + co2_mole_fraction + co2_mixing_ratio + h2o_molar_density + h2o_mole_fraction + h2o_mixing_ratio 
           + h2o_time_lag + sonic_temperature + air_temperature + air_pressure + air_density + air_heat_capacity + air_molar_volume 
           + water_vapor_density + es + specific_humidity + RH + Tdew + u_unrot + v_unrot + w_unrot + u_rot + v_rot + w_rot 
           + wind_dir + yaw + pitch + u_star_ + TKE + L + `_z_minus_d__slash_L` + bowen_ratio + T_star_ + x_peak + x_offset + x_10__pecent_ x_30__percent_ 
           + x_50__percent_ + x_70__pecent_ +x_90__percent_ + un_Tau + Tau_scf + un_H + H_scf + un_LE + LE_scf + un_co2_flux + un_h2o_flux 
           + w_slash_ts_cov + w_slash_co2_cov + w_slash_h2o_cov + co2_1, data = teaching_tbl)
summary(mod3)
coef(mod3)
resid(mod3)
confint(mod3)
anova(mod3)
anova(mod3, mod2)
plot(mod3)
#для корреляционного анализа возьмем переменные участвующие в линейной модели
cor_teaching_tbl = select(teaching_tbl, h2o_flux~ DOY, used_records, Tau, qc_Tau, rand_err_Tau, H, qc_H, rand_err_H, LE, qc_LE, rand_err_LE,
                          co2_flux, qc_h2o_flux, rand_err_co2_flux, rand_err_h2o_flux, H_strg, co2_v_minus_adv, h2o_v_minus_adv,
                          co2_molar_density, co2_mole_fraction, co2_mixing_ratio, h2o_molar_density, h2o_mole_fraction, h2o_mixing_ratio,
                          h2o_time_lag, sonic_temperature, air_temperature, air_pressure, air_density, air_heat_capacity, air_molar_volume, 
                          water_vapor_density, es, specific_humidity, RH, Tdew, u_unrot,v_unrot, w_unrot, u_rot, v_rot, w_rot, 
                          wind_dir, yaw, pitch, u_star_, TKE, L, `_z_minus_d__slash_L`, bowen_ratio, T_star_, x_peak, x_offset, x_10__pecent_, x_30__percent_, 
                          x_50__percent_, x_70__pecent_, x_90__percent_, un_Tau, Tau_scf,un_H,H_scf,un_LE ,LE_scf, un_co2_flux,un_h2o_flux, 
                          w_slash_ts_cov, w_slash_co2_cov, w_slash_h2o_cov, co2_1)
#таблица коэффициентов корреляций
cor_td = cor(cor_teaching_tbl) %>% as.data.frame
#построим графики по 3 модели
#Построение точек по значениями обучающей выборки и наложение предсказанных значений по 3 модели
qplot(h2o_flux , h2o_flux, data = teaching_tbl) + geom_line(aes(y = predict(mod3, teaching_tbl)))
#Построение точек по значением тестирующей выборки и наложение предсказанных значений по 3 модели
qplot(h2o_flux , h2o_flux, data = testing_tbl) + geom_line(aes(y = predict(mod3, testing_tbl)))
#Примеры
qplot(DOY, h2o_flux, data = testing_tbl) + geom_line(aes(y = predict(mod3, testing_tbl)))
qplot(Tau, h2o_flux, data = testing_tbl) + geom_line(aes(y = predict(mod3, testing_tbl)))
qplot(co2_flux, h2o_flux, data = testing_tbl) + geom_line(aes(y = predict(mod3, testing_tbl)))