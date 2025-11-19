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
#' \code{\link{separate_date_rp5}}. Должен содержать в себе столбцы:
#' Year, Month, Day - для агрегации до суточного разрешения (работа с сырыми данными);
#' Year, Month - для агрегации до месячного разрешения (работа с предобработанными данными суточного разрешения);
#' Year - для агрегации до годового разрешения;
#' опционально - любой из T, RRR, sss (или T_avg, RRR_sum, sss_avg,
#' если предварительно агрегировали данные с помощью \code{\link{aggregate_rp5}}) или в любом сочетании.
#' Остальные столбцы учитываться не будут, даже если они присутствуют в датафрейме.
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
#' # Примеры полного пайплайна:
#' # агрегация до суточного разрешения:
#' read_rp5_folder('path') |>
#'   separate_date_rp5() |> # не забудьте разделить компоненты даты
#'   na_fill_temp_rp5() |>  # опционально, но рекомендуется обработать пропуски
#'   aggregate_rp5('day')
#'
#' # агрегация до месячного или годового разрешения
#' # (рекомендуется предварительно агрегировать до суточного и обработать пропуски)
#' read_rp5_folder('path') |>
#'   separate_date_rp5() |> # не забудьте разделить компоненты даты
#'   na_fill_temp_rp5() |>  # опционально, но рекомендуется обработать пропуски
#'   aggregate_rp5('month') # можно заменить на 'year'
#'}
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
                     dplyr::across(dplyr::any_of(c("T_avg", "sss_avg")), ~round(mean(.x, na.rm = TRUE), 2)),
                     dplyr::across(dplyr::any_of(c("T", "sss")), list("avg" = ~round(mean(.x, na.rm = TRUE), 2))),
                     dplyr::across(dplyr::any_of(c("RRR_sum")), ~round(sum(.x, na.rm = TRUE), 2)),
                     dplyr::across(dplyr::any_of(c("RRR")), list("sum" = ~round(sum(.x, na.rm = TRUE), 2))),
                     .groups = 'drop')

  return(result)
}


