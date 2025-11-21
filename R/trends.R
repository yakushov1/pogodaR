#' Расчет скользящего среднего
#'
#' @param column : столбец для сглаживания
#' @param window : ширина окна
#'
#' @returns vector: вектор, который можно сохранить в отдельный столбец
#' @export
#'
#' @examples
#' \dontrun{
#' df |>
#'   means_smooth(T_avg)
#'   }
means_smooth <- function(column, window = 10) {
  pracma::movavg(column, n=window, type=c("s"))
}


#' Расчет параметров линейного тренда
#' @description
#' Расчет линейного тренда для сглаженного параметра и создание уравнения тренда.
#' Используется для нанесения уравнения на график, а также для построения сводной таблицы результатов анализа трендов.
#' Можно применять как к группированному датафрейму (линейный тренд будет вычислен для каждой группы отдельно),
#' так и без группировки.
#'
#' @param df a tibble: источник данных.
#' @param x предиктор (в текущем контексте это временная метка, например, год. Если год в данных один - используйте месяц, если он один - дату и т.д.)
#' @param y заисимая переменная (в текущем контексте это чаще всего сглаженный параметр, например средняя температура, сглаженная скользящим средним)
#'
#' @returns a tibble: датафрейм со столбцами:
#' r2 - коэффициент детерминации
#' p-value - для угла наклона регрессионной прямой (коэффииент b, или slope)
#' intercept - независимый член регрессионного уравнения
#' equation - уравнение линейного тренда
#' Опционально добавится столбец с группирующей переменной
#' @export
#'
#' @examples
#' \dontrun{
#' # простое применение
#' linear_trend(your_data, Year, Tavg_smoothed)
#'
#' # Шаблон полного пайплайна:
#' read_rp5_folder('inst/extdata/') |>   # путь к файлам
#'   separate_date_rp5() |>              # разбить дату на отдельные столбцы
#'   na_fill_temp_rp5() |>               # заполнить пропуски в температурах
#'   aggregate_rp5('day') |>             # агрегировать до суточного разрешения
#'   na_fill_sn_rp5(10) |>               # заполнить пропуски в измерениях снега
#'   aggregate_rp5('month') |>           # агрегировать до месячного разрешения
#'   dplyr::mutate(Tavg_smoothed = means_smooth(T_avg)) |>   # применить сглаживание скользящим средним
#'   linear_trend(Year, Tavg_smoothed)   # рассчитать линейный тренд для сглаженного параметра
#'
#' # расчет тренда, например, январских температур за все имеющиеся годы
#' monthly |> # данные месячного разрешения (после шага aggregate_rp5('month') из предыдущего примера)
#'   dplyr::filter(Month == 1) |>
#'   linear_trend(Year, Tavg_smoothed)
#'
#' # расчет тренда внутри одного года:
#' monthly |>
#'   dplyr::filter(Year == 2023) |>
#'   linear_trend(Month, Tavg_smoothed)
#'
#'
#' # расчет тренда для каждого года в исходном датафрейме
#'  monthly |>
#'    dplyr::group_by(Year) |>
#'    linear_trend(Month, Tavg_smoothed)
#'    }

linear_trend <- function(df, x, y) {
  # служебная функция линейного тренда:
  service_function <- function(data, x, y){
    x_vec <- dplyr::pull(data, {{x}})
    y_vec <- dplyr::pull(data, {{y}})

    lin_model <- stats::lm(y_vec ~ x_vec)

    result <- dplyr::tibble(
      r2 = round(summary(lin_model)$r.squared, 2),
      p_value = round(summary(lin_model)$coefficients[2,4], 4),
      b = round(stats::coef(lin_model)[2],2),
      intercept = round(stats::coef(lin_model)[1],2)) |>
      dplyr::mutate(equation = dplyr::case_when(
        .data$intercept == 0 ~ paste0("y = ", .data$b, "x"),
        .data$intercept < 0 ~ paste0("y = ", .data$b, "x - ", abs(.data$intercept)),
        .default = paste0("y = ", .data$b, "x + ", .data$intercept)
      ))
    return(result)
  }

  # если датафрейм сгруппирован, линейный тренд вычислится для каждой группы
    df |>
      dplyr::group_modify(~service_function(.x, {{x}}, {{y}}))
}
