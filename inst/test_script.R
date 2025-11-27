# Типичный пайплайн
library(tidyverse)
library(pogodaR)


# 1. загрузка всех файлов из исходной папки,
# По умолчанию будут загружены столбцы с температурой, глубиной снежного покрова и количеством осадков
source_data <- read_rp5_folder('tests/testthat/test_data/')|>
  separate_date_rp5() # разделение столбца с датами

# 2. подсчет пропусков
source_data |>
  na_count_rp5()

# 3. В сырых данных необходимо заменить пропуски в температурах.
source_data_replaced_na <- source_data |>
  na_fill_temp_rp5()

# перепроверить, что пропуски очищены
source_data_replaced_na |>
  na_count_rp5()


# 3.1. Количество переходов температур через 0 каждый месяц -
# явления оттаивания-замерзания.
daily_trans_of_temp_acr_0 <- source_data_replaced_na |>
  group_by(Year, Month) |>
  num_of_transitions(T, 0)


# Результаты удобно изучать визуально:
ggplot(daily_trans_of_temp_acr_0, aes(Year, Num_of_transitions))+
  geom_col()+
  theme_minimal()+
  facet_wrap(.~Month)+
  labs(y = 'Количество переходов температур через 0°C',
       x = 'Годы',
       caption = 'Измерения проводились раз в 3 часа.\n Источник данных: rp5.ru')



# 4. агрегация до суточного разрешения
daily <- source_data_replaced_na |>
  aggregate_rp5('day')


# 5. подсчет пропусков
daily |>
  na_count_rp5()

# 6. Замена пропусков в данных суточного разрешения
# (имеет смысл только для снега: если измерения осадков пропущены, скорее всего, их просто не было.
# для суточного разрешения считается сумма осадков. Пропуски в температурах удобнее заполнять в сырых данных.)

daily_clean <- daily |>
  na_fill_sn_rp5(10)

daily_clean |>
  na_count_rp5()



# 6.1 количество переходов среднесуточных температур через 0

t_avg_cross_zero <- daily_clean |>
  dplyr::group_by(Year, Month) |>
  num_of_transitions(sss_avg, 0)

# Результаты удобно изучать визуально:
ggplot(t_avg_cross_zero, aes(Year, Num_of_transitions))+
  geom_col()+
  theme_minimal()+
  facet_wrap(.~Month)+
  labs(y = 'Количество переходов среднесуточных температур\n через 0°C',
       x = 'Годы',
       caption = 'Источник данных: rp5.ru')




# 7. частота переходов температур через 0 градусов в месяц
daily_clean |>
  freq_of_transitions(T_avg, 0) |>
  ggplot(aes(Year, Freq_of_transitions))+
  geom_col()+
  theme_minimal()+
  facet_wrap(.~Month)+
  labs(y = 'Частота переходов температур\n через 0°C',
       x = 'Годы',
       caption = 'Источник данных: rp5.ru')


# 8. Перейдем к месячному разрешению
monthly <- daily_clean |>
  aggregate_rp5('month') |>
  mutate(Tavg_smoothed = means_smooth(T_avg))  # добавим сглаженный скользящим средним параметр


# 10. Расчет линейного тренда среднемесячных температур
# с группировкой
trends_by_month <- monthly |>
  dplyr::group_by(Month) |>
  linear_trend(Year, Tavg_smoothed)

# Визуализируем результат
graph_with_trends(data = monthly,
              trend = trends_by_month,
              divide_by_month = TRUE,
              y = T_avg,
              y_smoothed = Tavg_smoothed,
              geom_type = 'line',
              x_label = 'Годы',
              y_label = 'Температура, °C')


# 11. Перейдем к годовому разрешения и визуализируем тренды

annualy <- monthly |>
  aggregate_rp5('year') |>
  mutate(Tavg_smoothed = means_smooth(T_avg, window = 5))

annualy_trend <- annualy |>
  linear_trend(Year, T_avg)

graph_with_trends(data = annualy,
                  trend = annualy_trend,
                  y = T_avg,
                  y_smoothed = Tavg_smoothed)




























