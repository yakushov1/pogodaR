# тестирование функций считывания файлов
# в простом случае - все параметры по дефолту
test_that("read_rp5_csv works with default parameters", {
  path <- list.files('test_data/')[1]
  result <- read_rp5_csv(paste('test_data/', path, sep=''),
                         skip = 6,
                         delim = ';',
                         local_time_position = 1,
                         col_select = c('T', 'RRR', 'sss'),
                         encoding = NULL,
                         suppress_warnings = T)

  # функция должна возвращать тиббл
  expect_s3_class(result, "tbl_df")
  # проверка названий колонок
  expect_identical(names(result), c("Local_time", "T", "RRR", "sss"))
  # проверка типов колонок
  expect_type(result$Local_time, "character")
  expect_type(result$T, "double")
  expect_type(result$RRR, "double")
  expect_type(result$sss, "double")
})



# если выбран только один столбец, например, Т
test_that("read_rp5_csv works with single column selection - temperature only", {
  path <- list.files('test_data/')[1]
  result <- read_rp5_csv(paste('test_data/', path, sep=''),
                         skip = 6,
                         delim = ';',
                         local_time_position = 1,
                         col_select = c('T'),
                         encoding = NULL,
                         suppress_warnings = T)

  # функция должна возвращать тиббл
  expect_s3_class(result, "tbl_df")
  # проверка названий колонок
  expect_identical(names(result), c("Local_time", "T"))
  # проверка типов колонок
  expect_type(result$Local_time, "character")
  expect_type(result$T, "double")
})



# считывание всех файлов в папке
test_that("read_rp5_folder with default parameters", {
  result <- read_rp5_folder('test_data/')

  # функция должна возвращать тиббл
  expect_s3_class(result, "tbl_df")
  # проверка названий колонок
  expect_identical(names(result), c("Local_time", "T", "RRR", "sss"))
  # проверка типов колонок
  expect_type(result$Local_time, "character")
  expect_type(result$T, "double")
  expect_type(result$RRR, "double")
  expect_type(result$sss, "double")
})


