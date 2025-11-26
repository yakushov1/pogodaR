# подсчет количества пропусков
test_that("testing na's counter", {
  path <- list.files('test_data/')[1]
  result <- read_rp5_csv(paste('test_data/', path, sep='')) |>
    na_count_rp5()

  # функция должна возвращать тиббл
  expect_s3_class(result, "tbl_df")
  # проверка названий колонок
  expect_identical(names(result), c("Variable", "Missing_per_variable"))
  # проверка типов колонок
  expect_type(result$Variable, "character")
  expect_type(result$Missing_per_variable, "integer")

  # проверка, что при дефолтном считывании должно быть 3 строки для каждого из параметров
  expected_vars <- c("RRR", "T", "sss")
  expect_true(all(expected_vars %in% result$Variable))
  expect_equal(nrow(result), length(expected_vars))

  # Проверка, что все counts - неотрицательные числа
  expect_true(all(result$Missing_per_variable >= 0))
})


# заполнение пропусков в необработанной и неагрегированной температуре -  исходный столбец заменяется
test_that("testing na's counter", {
  path <- list.files('test_data/')[1]
  result <- read_rp5_csv(paste('test_data/', path, sep=''),
                         col_select = "T") |>
    separate_date_rp5() |>
    na_fill_temp_rp5()

  # функция должна возвращать тиббл
  expect_s3_class(result, "tbl_df")
  # проверка названий колонок
  expect_identical(names(result), c("Year", "Month", "Day", "T"))
  # Проверка, что все пропуски заполнились
  expect_true(all(is.na(result$T)==FALSE ))
})


# заполнение пропусков в необработанной неагрегированной температуре - создается новый столбец
test_that("testing na's counter", {
  path <- list.files('test_data/')[1]
  result <- read_rp5_csv(paste('test_data/', path, sep=''),
                         col_select = "T") |>
    separate_date_rp5() |>
    na_fill_temp_rp5(replace_original = FALSE)

  # функция должна возвращать тиббл
  expect_s3_class(result, "tbl_df")
  # проверка названий колонок
  expect_identical(names(result), c("Year", "Month", "Day", "T", "Temperature_interpolated"))
  # Проверка, что все пропуски заполнились
  expect_true(all(is.na(result$Temperature_interpolated)==FALSE ))
})


# заполнение пропусков в среднесуточной температуре
test_that("testing na's counter", {
  path <- list.files('test_data/')[1]
  result <- read_rp5_csv(paste('test_data/', path, sep=''),
                         col_select = "T") |>
    separate_date_rp5() |>
    aggregate_rp5('day') |>
    na_fill_temp_rp5()

  # функция должна возвращать тиббл
  expect_s3_class(result, "tbl_df")
  # проверка названий колонок
  expect_identical(names(result), c("Year", "Month", "Day", "N", "T_avg"))
  # Проверка, что все пропуски заполнились
  expect_true(all(is.na(result$T_avg)==FALSE))
})


