#' Подсчет пропущенных значений в каждом измерении
#' @description
#' Работает с тибблом, возвращенным после считывания исходных csv-файлов
#' (функции \code{\link{read_rp5_csv}} или \code{\link{read_rp5_folder}}) и разделения даты
#' на отдельные столбцы (функция \code{\link{separate_date_rp5}}). Можно работать с исходными данными, можно -
#' с агрегированными (\code{\link{aggregate_rp5}}).
#' Обратите внимание: при агрегации (функция \code{\link{aggregate_rp5}}) пропущенные значения удаляются из расчетов.
#' Это означает, что если в какой-либо переменной после агрегации до суточного разрешения
#' в итоговом датафрейме присутствуют пропуски, в конкретном дне не было ни одного наблюдения. То же самое
#' относится и к агрегации до месячного и годового разрешения (если есть пропуски,
#' то не было ни одного измерения в месяц)
#'
#' @param df тиббл, содержащий столбцы с параметрами:
#' агрегированные данные - результат \code{\link{aggregate_rp5}} - T_avg, sss_avg, RRR_sum (в любом сочетании)
#' неагрегированные данные - T, sss, RRR (в любых сочетаниях)
#' в отдельных столбцах, для которых нужно посчитать количество пропусков.
#'
#'
#' @returns тиббл со столбцами Variable - название параметра, и Missing_per_variable
#' - количество пропущенных значений для этого параметра
#' @export
#'
#' @examples
#' \dontrun{
#' # Подсчет пропусков в агрегированных данных
#' df <- read_rp5_folder('path_to_your_folder') |>
#'       separate_date_rp5() |>
#'       aggregate_rp5('day') |>
#'       na_count_rp5()
#' # Подсчет пропусков в неагрегированных данных
#' df <- read_rp5_folder('path_to_your_folder') |>
#'       separate_date_rp5() |>
#'       na_count_rp5() }
na_count_rp5 <- function(df){
  df |>
    tidyr::pivot_longer(cols = dplyr::any_of(c("T_avg", "sss_avg", "RRR_sum", "T", "sss", "RRR")),
                        names_to = 'Variable',
                        values_to = 'Par') |>
    dplyr::group_by(.data$Variable) |>
    dplyr::summarise(Missing_per_variable = sum(is.na(.data$Par)))

}



