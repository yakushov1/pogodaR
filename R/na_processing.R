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
#' read_rp5_folder('path') |>
#'       separate_date_rp5() |>
#'       na_fill_temp_rp5(replace_original = F) }
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



# обработка пропусков в  снеге (суточное разрешение)
# логика: если снега нет долго после низкого значения, значит, он растаял (заменить на 0)
# если между измеренными значениями промежуток небольшой, то заменить средним значением

#' Обработка пропущенных значений глубины снежного покрова (только данные суточного разрешения)
#' @description
#' Логика работы: если значения глубины снежного покрова пропущены длительное время (заданный порог) - он растаял.
#' Такие пропуски стоит заменить на 0.
#' Если число подряд следующих пропусков менее порога - заменить на последнее значение перед пропусками.
#' Работает только с датафреймом суточного разрешения! (должны быть столбцы Year, Month, Day)
#'
#' @param df a tibble: данные суточного разрешения со столбцами Year, Month, Day и sss_svg.
#' @param threshold numeric: пороговое значение длины "окна" с пропусками
#' @param replace_original logical: заменять ли исходный столбец? по умолчанию TRUE
#'
#' @returns a tibble: исходный тиббл, где пропуски в sss_avg заменены (либо добавлен новый столбец sss_replaced - удобно для диагностики)
#' @export
#'
#' @examples
#' \dontrun{
#' read_rp5_folder('path') |>
#'       separate_date_rp5() |>
#'       na_fill_temp_rp5(replace_original = F) |>
#'       aggregate_rp5('day') |>
#'       na_fill_sn_rp5(10, replace_original = F)}
na_fill_sn_rp5 <- function(df, threshold, replace_original = TRUE){

  df <- df |>
    dplyr::arrange(.data$Year, .data$Month, .data$Day)


  na_position <- df |>
    dplyr::select(.data$sss_avg) |> # только нужный столбец для экономии памяти
    dplyr::mutate(order_number = c(1:nrow(df))) |>  # зададим всем строкам порядковые номера
    dplyr::filter(!is.na(.data$sss_avg)) |> # оставим только непропущенные значения
    dplyr::mutate(first_na_position = dplyr::lag(.data$order_number)+1, # позиция первого NA в серии подряд следующих пропусков
                  last_na_position = .data$order_number - 1, # позиция последнего NA в серии подряд следующих пропусков
                  na_count = .data$order_number - .data$first_na_position) |> # количество пропусков подряд
    dplyr::filter(.data$na_count > {threshold}) # можно менять пороговое значение


  indices_to_replace <- unlist(
    purrr::map2(na_position$first_na_position, na_position$last_na_position, ~ .x:.y)
  )

  if(replace_original == TRUE){
    df$sss_avg[indices_to_replace] <- 0 # заменить на 0 все строки, где снег был пропущен подряд более, чем threshold

    df <- df |>
      tidyr::fill(.data$sss_avg, .direction = "down") # оставшиеся пропуски заполнить предыдущим непропущенным числом
  } else{
    df$sss_replaced = df$sss_avg
    df$sss_replaced[indices_to_replace] <- 0

    df <- df |>
      tidyr::fill(.data$sss_replaced, .direction = "down")
  }
  return(df)

}


