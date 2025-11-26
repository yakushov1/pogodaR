# тестирование  агрегации до суточного разрешения
test_that("aggregate_rp5 works with all resolutions", {
  path <- list.files('test_data/')[1]
  test_data <- read_rp5_csv(paste('test_data/', path, sep='')) |>
    separate_date_rp5()|>
    na_fill_temp_rp5()

  # Тестируем все три разрешения
  daily <- aggregate_rp5(test_data, 'day')
  monthly <- aggregate_rp5(test_data, 'month')
  yearly <- aggregate_rp5(test_data, 'year')

  expect_s3_class(daily, "tbl_df")
  expect_s3_class(monthly, "tbl_df")
  expect_s3_class(yearly, "tbl_df")

  # Проверяем группирующие столбцы для каждого разрешения
  expect_true(all(c("Year", "Month", "Day") %in% names(daily)))
  expect_true(all(c("Year", "Month") %in% names(monthly)))
  expect_true("Year" %in% names(yearly))

})


# тест сообщений об ошибках
test_that("aggregate_rp5 validates inputs correctly", {
  test_data <- read_rp5_folder('test_data/') |>
    separate_date_rp5()

  # Неправильное разрешение
  expect_error(aggregate_rp5(test_data, 'invalid'),
               "Choose correct resolution")

  # Данные без нужных столбцов
  empty_data <- tibble::tibble(Year = 2020, Month = 1, Day = 1)
  expect_error(aggregate_rp5(empty_data, 'day'),
               "No temperature, precipitation or snow depth columns found")

  # Смешанные сырые и агрегированные данные
  mixed_data <- test_data |> dplyr::mutate(T_avg = T)
  expect_error(aggregate_rp5(mixed_data, 'day'),
               "Data contains both raw and aggregated columns")
})


# тест математической корректности
test_that("aggregate_rp5 calculates correctly", {
  # Создаем контролируемые данные
  test_data <- tibble::tibble(
    Year = rep(2020, 6),
    Month = rep(1, 6),
    Day = rep(1, 6),
    T = c(1, 2, 3, 4, 5, 6),
    RRR = c(0.1, 0.2, 0, 0, 0.1, 0),
    sss = c(10, 10, 10, 10, 10, 10)
  )

  result <- aggregate_rp5(test_data, 'day')

  # Проверяем вычисления
  expect_equal(result$T_avg, 3.5)        # mean(1:6)
  expect_equal(result$RRR_sum, 0.4)      # sum(0.1, 0.2, 0, 0, 0.1, 0)
  expect_equal(result$sss_avg, 10)       # mean(rep(10, 6))
  expect_equal(result$N, 6)              # 6 наблюдений
})


# проверка агрегации уже агрегированных данных
test_that("aggregate_rp5 works with pre-aggregated data", {
  # Сначала агрегируем до суточного, потом до месячного
  daily_data <- read_rp5_folder('test_data') |>
    separate_date_rp5() |>
    na_fill_temp_rp5() |>
    aggregate_rp5('day')

  # Теперь агрегируем агрегированные данные
  monthly_from_daily <- aggregate_rp5(daily_data, 'month')

  # должен возвращать датафрейм
  expect_s3_class(monthly_from_daily, "tbl_df")

  # с заданными столбцами
  expect_true(all(c("Year", "Month", "N", "T_avg", "RRR_sum", "sss_avg") %in% names(monthly_from_daily)))
})




