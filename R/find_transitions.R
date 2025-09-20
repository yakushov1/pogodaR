#' Find the number of parameter transitions through the threshold level
#'
#' @param df a tibble containing the columns: year, month, day, parameter (for example, temperature)
#' @param parameter a character: the name of the column to search for the number of crossings over the threshold
#' @param threshold a numeric: threshold value
#'
#' @returns a tibble: Year, Month, Num_of_transitions - the number of parameter transitions through the threshold level
#' @export
#'
#' @examples
#' \dontrun{
#' counting the number of temperature transitions after 0 degrees
#' find_transitions(dataframe, T, 0)}
find_transitions <- function(df, parameter, threshold){
  df |>
    dplyr::filter(is.na({{parameter}})==F) |>
    dplyr::group_by(.data$Year, .data$Month) |>
    dplyr::mutate(
      more_then_zero = {{parameter}} > {{threshold}}, # логический столбец, параметр больше 0? (TRUE | FALSE)
      chng1 = cumsum(.data$more_then_zero != dplyr::lag(.data$more_then_zero, def = dplyr::first(.data$more_then_zero)))
    ) |>
    dplyr::summarise(Num_of_transitions = max(.data$chng1))
}



