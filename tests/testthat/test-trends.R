# тестирование функции поиска тренда

test_that("to find linear trend", {
  test_data <- read_rp5_folder('test_data/') |>   # путь к файлам
    separate_date_rp5() |>              # разбить дату на отдельные столбцы
    na_fill_temp_rp5() |>               # заполнить пропуски в температурах
    aggregate_rp5('day') |>             # агрегировать до суточного разрешения
    na_fill_sn_rp5(10) |>               # заполнить пропуски в измерениях снега
    aggregate_rp5('month') |>           # агрегировать до месячного разрешения
    dplyr::mutate(Tavg_smoothed = means_smooth(T_avg))

  # с группировкой по месяцам
  trends_by_month <- test_data |>
    dplyr::group_by(Month) |>
    linear_trend(Year, Tavg_smoothed)

  # функция должна возвращать тиббл
  expect_s3_class(trends_by_month, "tbl_df")
  # проверка названий колонок
  expect_identical(names(trends_by_month), c("Month", "r2", "p_value", "b", "intercept", "equation"))

  # без группировки
  result_without_grops <- test_data |>
    linear_trend(Year, Tavg_smoothed)

  # функция должна возвращать тиббл
  expect_s3_class(result_without_grops, "tbl_df")
  # проверка названий колонок
  expect_identical(names(result_without_grops), c("r2", "p_value", "b", "intercept", "equation"))


})
