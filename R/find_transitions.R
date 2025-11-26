#' Найти переход заданного параметра через пороговое значение
#' @description
#' Служебная функция.
#' Вычисляет факт перехода необходимого параметра через заданное пороговое значение.
#' Результат используется другими функциями внутри пакета.
#' По умолчанию удаляет пропущенные значения в параметре, поэтому NA предварительно необходимо обрабатывать.
#' @param df тиббл, содержащий как минимум 1 столбец - параметр, для которого будут вычисляться переходы (например, с температурой). Тиббл предварительно можно сгруппировать (например, вычислить переходы параметра через порог для каждого года, месяца и дня)
#' @param parameter a character: название столбца, для которого будут вычисляться переходы через пороговое значение threshold
#' @param threshold a numeric: пороговое значение
#'
#' @returns a tibble: содержит группирующие столбцы, столбец, для которого вычислялись переходы, а также столбец chng - порядковый номер перехода параметра через заданное пороговое значение.
#' @export
#'
#' @examples
#' \dontrun{
#' Найти переходы температур через 0 градусов
#' find_transitions(dataframe, T_avg, 0)}
find_transitions <- function(df, parameter, threshold){


  # Проверяем наличие NA
  na_count <- sum(is.na(dplyr::pull(df, {{parameter}})))

  if (na_count > 0) {
    stop("Data contains ", na_count, " NA values. Use na_fill functions first.")
  }



  df |>
    dplyr::mutate(
      more_then_threshold = {{parameter}} > {{threshold}}, # логический столбец, параметр больше порога? (TRUE | FALSE)
      chng = cumsum(.data$more_then_threshold != dplyr::lag(.data$more_then_threshold, default = dplyr::first(.data$more_then_threshold)))
    )

}


#' Посчитать количество переходов параметра через заданный порог
#' @description
#' Подсчет количества переходов параметра через заданный порог.
#' По умолчанию удаляет пропущенные значения в параметре, поэтому NA предварительно необходимо обрабатывать.
#' Может применяться после группировки датафрейма, например, для расчета количества переходов в каждом месяце, году и т.д.
#' @param df - тиббл, содержащий как минимум 1 столбец - параметр, для которого будут вычисляться переходы (например, с температурой). Тиббл предварительно можно сгруппировать (например, вычислить переходы параметра через порог для каждого года, месяца и дня)
#' @param parameter a character: название столбца, для которого будут вычисляться переходы через пороговое значение threshold
#' @param threshold a numeric: пороговое значение
#'
#' @returns  a tibble: содержит группирующие столбцы, столбец, для которого вычислялись переходы, а также столбец Num_of_transitions - количество переходов параметра через заданное значение
#' @export
#'
#' @examples
#' \dontrun{
#' Найти переходы температур через 0 градусов
#' num_of_transitions(dataframe, T_avg, 0)
#'
#' #' Найти количество переходов температур через 0 градусов
#' dataframe |>
#' group_by(Year, Month) |>
#' num_of_transitions(T_avg, 0)}
num_of_transitions <-  function(df, parameter, threshold){
  df |>
    find_transitions({{parameter}}, {{threshold}}) |>
    dplyr::summarise(Num_of_transitions = max(.data$chng))

}



#
#' Частота переходов среднесуточного параметра через пороговое значение (в месяц)
#' @description
#' Рассчитывает частоту как количество переходов параметра через заданное пороговое значение, поделенное на продолжительность месяца (в днях)
#'
#'
#' @param df a tibble: датафрейм со столбцами Year, Month, Параметр (например, T_avg). (опционально Day. Необходимо использовать датафрейм суточного разрешения, то есть одно измерение параметра должно соответствовать одним суткам)
#' @param parameter a character: название столбца - интересующего параметра
#' @param threshold numeric: пороговое значение, через которое совершаются переходы
#'
#' @returns a tibble: датафрейм со столбцами:
#' Year
#' Month
#' Num_of_transitions - количество переходов через заданный порог
#' days_in_month - количество дней в месяце (рассчитывается по входным данным)
#' Freq_of_transitions = Num_of_transitions/days_in_month  : частота переходов (переходов в сутки)
#' @export
#'
#' @examples
#' \dontrun{
#' # пример полного папйплайна
#' read_rp5_folder('path') |>    # считать все файлы в папке
#'     separate_date_rp5() |>    # разбить даты на отдельные столбцы
#'     na_fill_temp_rp5() |>     # заполнить пропуски в исходных значениях температуры
#'     aggregate_rp5('day') |>   # агрегировать данные до суточного разрешения
#'     na_fill_sn_rp5(10) |>     # заполнить пропуски в измерениях глубин снежного покрова
#'     freq_of_transitions(T_avg, 0)} # рассчитать частоту и количество переходов среднесуточных температур через 0
freq_of_transitions <- function(df, parameter, threshold){

  transitions <- df |>
    dplyr::group_by(.data$Year, .data$Month) |>
    num_of_transitions({{parameter}}, {{threshold}})

  days_count <- df |>
    dplyr::group_by(.data$Year, .data$Month) |>
    dplyr::summarise(days_in_month = dplyr::n(), .groups = "drop")

  dplyr::left_join(transitions, days_count, by = c("Year", "Month")) |>
    dplyr::mutate(Freq_of_transitions = .data$Num_of_transitions / .data$days_in_month)
}



