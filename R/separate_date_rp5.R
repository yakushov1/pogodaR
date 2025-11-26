#' Выделить год, месяц и день из столбца Local_time в отдельные столбцы.
#'
#' @description
#' Выделить год, месяц и день из столбца Local_time в отдельные столбцы. Используйте для обработки
#' датафреймов, полученных с помощью \code{\link{read_rp5_csv}} или \code{\link{read_rp5_folder}}.
#'
#'
#' @param df a tibble: результат выполнения функций \code{\link{read_rp5_csv}} или
#' \code{\link{read_rp5_folder}}.
#' Датафрейм обязательно должен содержать столбец Local_time в исходном формате, предоставляемом rp5.
#' @param format_of_local_time a character: формат времени, по умолчанию: "%d.%m.%Y %R".
#'  Подробности: \code{\link[lubridate]{as_date}}
#' @returns a tibble: тиббл со столбцами Year, Month, Day, T, RRR, sss
#' @export
#'
#' @examples
#' \dontrun{
#' # в большинстве случаев достаточно запустить
#' separate_date_rp5(ваш_датафрейм)
#'
#' # можно настроить формат исходной даты
#' separate_date_rp5(df,
#'                   format_of_local_time = "%d.%m.%Y %R")}
separate_date_rp5 <- function(df,
                             format_of_local_time = "%d.%m.%Y %R"){

  df <- df |>
    dplyr::mutate(Local_time = lubridate::as_datetime(.data$Local_time, format = format_of_local_time)) |>
    dplyr::mutate(Year = lubridate::year(.data$Local_time),
                  Month = lubridate::month(.data$Local_time),
                  Day = lubridate::mday(.data$Local_time),
                  .before = 'Local_time') |>
    dplyr::select(!dplyr::all_of('Local_time'))

  return(df)
}


