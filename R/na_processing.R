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

# обычно, если пропущен RRR в необработанных данных, то это просто 0.
# снег измеряется не каждый час, а хотя бы один раз в сутки.
# поэтому снег лучше агрегировать до суточного значения, а потом обрабатывать пропуски
# температура - если пропущена, то заполнять линейной интерполяцией. Имеет смысл заполнять сырые данные, потом агрегировать


#' Заполнение пропущенных значений температур
#' @description
#' Выполняется для "сырых" данных, но применим и к агрегированным. Заполнение пропусков методом линейной интерполяции
#' (пакет zoo). По умолчанию пропуски в начале или конце датафрейма заполняются соседними значениями.
#'
#' @param df a tibble: Должен содержать столбцы с температурой (ожидаются названия T или T_avg)
#' @param replace_original LOGICAL: заменять ли исходный столбец новым столбцом без пропусков? По умолчанию TRUE
#'
#' @returns a tibble:  исходный тиббл с заполненными пропусками в столбце с температурами, либо с новым столбцом, в котором заполнены пропуски (исходный столбец с температурами остается неизменным)
#' @export
#'
#' @examples
#' \dontrun{
  #' # Подсчет пропусков в агрегированных данных
  #' df <- read_rp5_folder('path') |>
  #' separate_date_rp5() |>
  #'   na_fill_temp_rp5(replace_original = F) }
na_fill_temp_rp5 <- function(df, replace_original = TRUE){
  # Определяем имя столбца с температурой
  temp_col <- NULL
  if("T" %in% names(df)) {
    temp_col <- "T"
  } else if ("T_avg" %in% names(df)) {
    temp_col <- "T_avg"
  } else {
    stop("The column with temperature values was not found. The names 'T' or 'T_avg' were expected")
  }

  # Создаем интерполированные значения
  interpolated <- zoo::na.approx(df[[temp_col]], na.rm = FALSE, rule = 2)

  if(replace_original == TRUE){
    df[[temp_col]] <- interpolated
  } else {
    df$Temperature_interpolated <- interpolated
  }

  return(df)

}


