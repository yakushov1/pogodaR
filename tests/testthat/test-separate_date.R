# тестирование функции для поиска трендов в среднемесячных значениях

test_that("testing separate_date_rp5", {
  path <- list.files('test_data/')[1]
  result <- read_rp5_csv(paste('test_data/', path, sep='')) |>
    separate_date_rp5()

  # функция должна возвращать тиббл
  expect_s3_class(result, "tbl_df")
  # проверка названий колонок
  expect_identical(names(result), c("Year", "Month", "Day", "T", "RRR", "sss"))
  # проверка типов колонок
  expect_type(result$Year, "double")
  expect_type(result$Month, "double")
  expect_type(result$Day, "integer")

})
