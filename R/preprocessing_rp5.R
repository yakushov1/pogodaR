#' Date column processing: output of year, month, day in separate columns
#'
#' @param df a tibble: the result of executing the read_rp5_csv() or read_rp5_excel() functions
#' @param col_with_local_time integer: the ordinal number of the column containing information about the local time. By default : 1.
#' @param format_of_local_time a character: a string with the time format. By default: "%d.%m.%Y %R". Read more: https://lubridate.tidyverse.org/reference/as_date.html
#'
#' @returns a tibble
#' @export
#'
#' @examples
#' \dontrun{
#' process_date_rp5(df, col_with_local_time = 1, format_of_local_time = "%d.%m.%Y %R")}
process_date_rp5 <- function(df,
                             col_with_local_time = 1,
                             format_of_local_time = "%d.%m.%Y %R"){

  # Получаем имя колонки c датой
  col_name <- names(df)[col_with_local_time]

  # Обрабатываем данные
  df <- df |>
    dplyr::rename(Local_time = dplyr::all_of(col_name)) |>
    dplyr::mutate(Local_time = lubridate::as_datetime(.data$Local_time, format = format_of_local_time)) |>
    dplyr::mutate(Year = lubridate::year(.data$Local_time),
                  Month = as.factor(lubridate::month(.data$Local_time)),
                  Day = lubridate::mday(.data$Local_time),
                  .before = 'Local_time') |>
    dplyr::mutate(Sn_depth = stringr::str_replace_all(.data$sss, ";", ""))
    dplyr::select(!.data$Local_time)

  return(df)
}
