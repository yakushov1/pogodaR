#' График трендов параметра для каждого месяца
#'
#' @param monthly_data a tibble: тиббл с данными месячного разрешения. Должен содержать столбцы Year, Month, а также столбец с интересующим параметром и его сглаженной версией
#' @param monthly_trend  a tibble: тиббл с параметрами линейного тренда для каждого месяца (результат функции \code{\link{linear_trend}})
#' @param y : название столбца с параметром (например, T_avg)
#' @param y_smoothed : сглаженный параметр (например, Tavg_smoothed)
#' @param geom_type : тип геометрии. По умолчанию 'line' - линейный график. Другой вариант - col - столбчатый график.
#' @param x_label : подпись оси Х.
#' @param y_label : подпись оси Y.
#'
#' @returns ggplot2 график
#' @export

#' @examples
#' \dontrun{
#' monthly_graph(monthly, trends_by_month, T_avg, Tavg_smoothed)
#' }
#'
monthly_graph <- function(monthly_data,
                          monthly_trend,
                          y,
                          y_smoothed,
                          geom_type = 'line',
                          x_label,
                          y_label){

  monthly_total <- monthly_data |>
    dplyr::left_join(monthly_trend, by = c('Month'))

  graph <- ggplot2::ggplot(monthly_total, ggplot2::aes(.data$Year, {{y}}))+
    ggplot2::geom_line(alpha = 0.5)+
    ggplot2::facet_wrap(~.data$Month, scales = 'free')+
    ggplot2::facet_wrap(~.data$Month, scales = 'free') +
    ggplot2::theme_minimal() +
    ggplot2::scale_x_continuous(labels = scales::number_format(accuracy = 1, big.mark = ""))+
    ggplot2::labs(x = x_label, y = y_label)

  # Добавляем geom в зависимости от типа
  if (geom_type == "col"){
    graph <- graph +
      ggplot2::geom_col(ggplot2::aes(y = {{y_smoothed}}), color = 'lightblue')+
      ggplot2::geom_smooth(ggplot2::aes(y = {{y_smoothed}}), method = 'lm')
  } else if (geom_type == 'line'){
    graph <- graph +
      ggplot2::geom_line(ggplot2::aes(y = {{y_smoothed}}), color = 'blue')+
      ggplot2::geom_smooth(ggplot2::aes(y = {{y_smoothed}}), method = 'lm')
  }

  # таблички с параметрами линейного тренда
  graph <- graph+
    ggplot2::geom_label(ggplot2::aes(x = -Inf,
                                     y = Inf,
                                     label = paste0(.data$equation, "\nR\u00b2 = ", .data$r2, "\np-value =", .data$p_value)),
                        hjust = 0,
                        vjust = 1,
                        size = 3,
                        fill='white',
                        alpha = 0.1)

  return(graph)
}
