#' График трендов
#' @description
#' График трендов для данных месячного или годового разрешения
#'
#' @param data a tibble: тиббл с агрегированными данными. Должен содержать столбцы Year, (Month - для данных месячного разрешения), а также столбец с визуализируемым параметром
#' @param trend a tibble: тиббл с параметрами тренда.
#' @param divide_by_month logical, default = FALSE: если данные месячного разрешения, установить TRUE, чтобы график разбивался на фасетки по месяцам.
#' @param y название столбца с параметром для визуализации
#' @param y_smoothed название столбца со сглаженным параметром для визуализации. Если вы не использовали сглаживание, укажите то же имя столбца, что и в y.
#' @param geom_type тип геометрии. По умолчанию 'line', возможен вариант 'col'
#' @param x_label подпись оси x. По умолчанию 'Years'
#' @param y_label подпись оси y. По умолчанию 'Temperature, °C'
#'
#' @returns ggplot2 график
#' @export
#'
#' @examples
#' \dontrun{
# среднемесячные графики с параметром
#'graph_with_trends(data = monthly,
#'                  trend = trends_by_month,
#'                  devide_by_month = TRUE,
#'                  y = T_avg,
#'                  y_smoothed = Tavg_smoothed,
#'                  geom_type = 'line',
#'                  x_label = 'Годы',
#'                  y_label = "температуры")
# для среднегодовых значний параметров
#'graph_with_trends(data = annualy,
#'                  trend = annualy_trend,
#'                  y = T_avg,
#'                  y_smoothed = T_avg,
#'                  geom_type = 'col')}



graph_with_trends <- function(data,
                          trend,
                          divide_by_month = FALSE,
                          y,
                          y_smoothed,
                          geom_type = 'line',
                          x_label = 'Years',
                          y_label ='Temperature, \u00b0C'){


  y_name <- deparse(substitute(y)) # для вставки в легенду

  if (y_name %in% c('T', 'T_avg')){
    y_name_for_caption = 'Temperature'
  } else if (y_name %in% c('sss', 'sss_avg')){
    y_name_for_caption = 'Snow depth'
  } else {
    y_name_for_caption = 'y'
  }


  graph <- ggplot2::ggplot(data, ggplot2::aes(.data$Year, {{y}}))+
    ggplot2::geom_line(alpha = 0.5)+
    ggplot2::theme_minimal() +
    ggplot2::scale_x_continuous(labels = scales::number_format(accuracy = 1, big.mark = ""))+
    ggplot2::labs(x = x_label,
                  y = y_label,
                  caption = paste("<span style='color:gray50;'>", y_name_for_caption, "</span> <br>
                 <span style='color:blue;'>smoothed by a moving average.</span><br>
                 <span style='color:red;'>linear trend</span>"))+
    ggplot2::theme(
                   plot.caption = ggtext::element_markdown(
                     hjust = 0,
                     size = 9,
                     lineheight = 1.2
                   )
                 )



  # Добавляем geom в зависимости от типа
  if (geom_type == "col"){
    graph <- graph +
      ggplot2::geom_col(ggplot2::aes(y = {{y_smoothed}}), fill = 'lightblue')+
      ggplot2::geom_smooth(ggplot2::aes(y = {{y_smoothed}}), method = 'lm', color = 'red')
  } else if (geom_type == 'line'){
    graph <- graph +
      ggplot2::geom_line(ggplot2::aes(y = {{y_smoothed}}), color = 'blue')+
      ggplot2::geom_smooth(ggplot2::aes(y = {{y_smoothed}}), method = 'lm', color = 'red')
  }

  # разбивать ли по месяцам?

  if (divide_by_month){
    graph <- graph +
      ggplot2::facet_wrap(~.data$Month, scales = 'free')
  }


  # таблички с параметрами линейного тренда
  graph <- graph +
    ggplot2::geom_label(
      data = trend,  # отдельный датафрейм с подписями
      ggplot2::aes(x = -Inf, y = Inf,
                   label = paste0(.data$equation, "\nR\u00b2 = ", .data$r2, "\np-value = ", .data$p_value)),
      hjust = 0,
      vjust = 1,
      size = 3,
      fill = 'white',
      alpha = 0.5
    )


  return(graph)
}
