# базовая функциональность
test_that("find_transitions correctly identifies transitions", {
  test_data <- tibble::tibble(
    temp = c(1, -1, 2, -2, 3),  # переходы: + -> - -> + -> - -> +
    day = 1:5
  )

  result <- find_transitions(test_data, temp, 0)

  expect_equal(result$chng, c(0, 1, 2, 3, 4))
  expect_equal(result$more_then_threshold, c(TRUE, FALSE, TRUE, FALSE, TRUE))
})

test_that("find_transitions works with positive threshold", {
  test_data <- tibble::tibble(value = c(4, 6, 3, 7, 2))
  result <- find_transitions(test_data, value, 5)  # порог = 5

  expect_equal(result$more_then_threshold, c(FALSE, TRUE, FALSE, TRUE, FALSE))
})


# остановить выполнение, если есть NA
test_that("find_transitions stops on NA values", {
  test_data <- tibble::tibble(temp = c(1, NA, 3, 4))

  expect_error(
    find_transitions(test_data, temp, 0),
    "Data contains 1 NA values"
  )
})


# проверка работы с группированным датафреймом
test_that("find_transitions works with grouped data", {
  test_data <- tibble::tibble(
    month = rep(1:2, each = 4),
    temp = c(1, -1, 2, -2, -1, 1, -2, 2)  # разные паттерны по месяцам
  )

  result <- test_data |>
    dplyr::group_by(month) |>
    find_transitions(temp, 0)

  expect_equal(result$chng, c(0, 1, 2, 3, 0, 1, 2, 3))
})



# правильно ли считает количество переходов через порог?
test_that("num_of_transitions counts correctly", {
  test_data <- tibble::tibble(
    temp = c(1, -1, 2, -2, 3),  # 4 перехода
    day = 1:5
  )

  result <- num_of_transitions(test_data, temp, 0)
  expect_equal(result$Num_of_transitions, 4)
})

test_that("num_of_transitions returns 0 for no transitions", {
  test_data <- tibble::tibble(temp = rep(5, 5))  # все значения выше порога, то есть переходов нет

  result <- num_of_transitions(test_data, temp, 0)
  expect_equal(result$Num_of_transitions, 0)
})



# расчет частоты переходов
test_that("freq_of_transitions calculates frequency correctly", {
  test_data <- tibble::tibble(
    Year = 2023,
    Month = 1,
    temp = c(1, -1, 1, -1, 1)  # 4 перехода за 5 дней
  )

  result <- freq_of_transitions(test_data, temp, 0)

  expect_equal(result$Num_of_transitions, 4)
  expect_equal(result$days_in_month, 5)
  expect_equal(result$Freq_of_transitions, 4/5)
})




# частота переходов через порог в разные месяцы
test_that("freq_of_transitions handles different month lengths", {
  test_data <- tibble::tibble(
    Year = rep(2023, 6),
    Month = rep(1:2, each = 3),  # 3 дня в каждом "месяце"
    temp = rep(c(1, -1, 1), 2)   # по 2 перехода в каждом месяце
  )

  result <- freq_of_transitions(test_data, temp, 0)

  expect_equal(result$Num_of_transitions, c(2, 2))
  expect_equal(result$days_in_month, c(3, 3))
  expect_equal(result$Freq_of_transitions, c(2/3, 2/3))
})

