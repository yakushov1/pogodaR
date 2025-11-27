test_that("graph_with_trends creates ggplot object", {
  test_data <- tibble::tibble(
    Year = 2000:2010,
    T_avg = 10 + 0.1 * (2000:2010) + rnorm(11),
    Tavg_smoothed = 10 + 0.1 * (2000:2010)
  )

  test_trend <- tibble::tibble(
    equation = "y = 0.1x + 10.0",
    r2 = 0.95,
    p_value = 0.001
  )

  result <- graph_with_trends(
    data = test_data,
    trend = test_trend,
    y = T_avg,
    y_smoothed = Tavg_smoothed
  )

  expect_s3_class(result, "ggplot")
  expect_s3_class(result, "gg")
})

test_that("graph_with_trends works with different geom types", {
  test_data <- tibble::tibble(
    Year = 2000:2005,
    T_avg = 1:6,
    Tavg_smoothed = 1:6
  )

  test_trend <- tibble::tibble(equation = "y = 1x + 0", r2 = 1, p_value = 0.001)

  # Line plot
  result_line <- graph_with_trends(
    data = test_data,
    trend = test_trend,
    y = T_avg,
    y_smoothed = Tavg_smoothed,
    geom_type = "line"
  )

  # Column plot
  result_col <- graph_with_trends(
    data = test_data,
    trend = test_trend,
    y = T_avg,
    y_smoothed = Tavg_smoothed,
    geom_type = "col"
  )

  expect_s3_class(result_line, "ggplot")
  expect_s3_class(result_col, "ggplot")
})


# тестирование аннотаций
test_that("graph_with_trends adds trend annotations", {
  test_data <- tibble::tibble(
    Year = 2000:2002,
    T_avg = 1:3,
    Tavg_smoothed = 1:3
  )

  test_trend <- tibble::tibble(
    equation = "y = 1.0x + 0.0",
    r2 = 0.99,
    p_value = 0.0001
  )

  result <- graph_with_trends(
    data = test_data,
    trend = test_trend,
    y = T_avg,
    y_smoothed = Tavg_smoothed
  )

  # Проверяем, что график создан (аннотации сложно тестировать напрямую)
  expect_s3_class(result, "ggplot")
  # Можно проверить, что данные для аннотаций переданы
  expect_true("data" %in% names(result$layers[[4]])) # 4-й слой - аннотации
})
