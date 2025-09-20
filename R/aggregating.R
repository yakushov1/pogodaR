#' Агрегация данных о температуре, количестве осадков и высоте снежного покрова
#' @description
#' Вычисляет среднюю температуру, сумму осадков и среднюю высоту снежного покрова для заданного разрешения: годового,
#' месячного или суточного. Пропущенные значения исключает из расчетов. Работает с тибблом - результатом
#' последовательного выполнения функций \code{\link{read_rp5_csv}} или \code{\link{read_rp5_folder}} и
#' \code{\link{separate_date_rp5}}.
#'
#'
#'
#' @param df a tibble: тиббл, полученный после выполнения функций \code{\link{read_rp5_csv}} или \code{\link{read_rp5_folder}} и
#' \code{\link{separate_date_rp5}}. Должен содержать в себе столбцы: обязательно - Year, Month, Day;
#' опционально - любой из T, RRR, sss или в любом сочетании.
#' Для других столбцов агрегация производиться не будет, даже если они присутствуют в датафрейме.
#'
#' @param resolution character: до какого разрешения агрегировать данные? До суточного: resolution = 'day',
#' месячного: resolution = 'month', годового: resolution = 'year'.
#'
#' @returns a tibble: Тиббл со столбцами Year, Month, Day (в зависимости от выбранного разрешения),
#' N - количество наблюдений в группе (обязательно), и, в зависимости от исходного набора столбцов
#'  T_avg - средняя температура, RRR_sum - сумма осадков, sss_avg - средняя высота снежного покрова.
#' @export
#'
#' @examples
#' \dontrun{
#' aggregate_rp5(df, 'day')}
aggregate_rp5 <- function(df, resolution){
  if (!resolution %in% c('day', 'month', 'year')) {
    return("Error: Choose correct resolution for aggregating: 'year', 'month' or 'day'")
  }

  # Группировка
  group_vars <- switch(resolution,
                       "day" = c("Year", "Month", "Day"),
                       "month" = c("Year", "Month"),
                       "year" = "Year")

  result <- df |>
    dplyr::group_by(dplyr::across(dplyr::all_of(group_vars))) |>
    dplyr::summarise(N = dplyr::n(),
                     dplyr::across(dplyr::any_of(c("T", "sss")), list('avg' = ~ round(mean(.x, na.rm = TRUE), 2))),
                     dplyr::across(dplyr::any_of(c("RRR")), list('sum' = ~ round(sum(.x, na.rm = TRUE), 2))),
                     .groups = 'drop')

  return(result)
}


