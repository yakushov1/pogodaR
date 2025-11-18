#' Импорт csv файла, скачанного с сайта  \href{https://rp5.ru/}{rp5.ru}
#' @description
#' Высокоуровневая обертка для функции
#' \code{\link[readr]{read_delim}}, адаптированной для импорта csv файла,
#' скачанного с сайта \href{https://rp5.ru/}{rp5.ru}. При импорте
#' происходит автоматическое удаление метаданных и отбор столбцов с местным временем (в любом случае),
#' и (опционально) температурой (T),
#' количеством осадков (RRR), высотой снежного покрова (sss). Cтолбцы со второго по последний (по умолчанию
#' T, RRR, sss)
#' автоматически очищаются от лишних символов, остаются только дробные или целые числа.
#'
#' @param path character: путь к csv файлу.
#' @param skip integer: количество строк в csv-файле, которые необходимо пропустить при импорте.
#' (обычно в верхних строках при скачивании с сайта rp5.ru хранятся метаданные).
#' По умолчанию `skip=6`
#' @param delim character: разделитель. По умолчанию `delim=';'`
#' @param local_time_position integer: позиция столбца с местным временем. По умолчанию = 1
#' @param encoding character: кодировка csv-файла.По умолчанию `encoding = NULL`,
#'  и функция пытается автоматически распознать кодировку, указанную в имени файла.
#'  При скачивании csv с rp5.ru информация о кодировке записана в названии, поэтому для автоматического
#'  распознавания кодировки лучше не переименовывать файл. Если возникают ошибки, вы можете установить
#'  значение вручную. Варианты кодировок,
#' в которых можно скачать csv с сайта rp5.ru: "utf-8", unicode" или "ANSI"
#' (в таком случае нужно указывать "windows-1251")
#' @param col_select character: отбор необходимых столбцов. Например col_select = c('T', 'RRR', 'sss').
#'  По умолчанию отбирает столбцы с местным временем,
#'  температурой (столбец T), количеством выпавших осадков (столбец RRR) и высотой снежного покрова
#'  (столбец sss). По стандарту rp5 местное время располагается в первом столбце,
#'  но можно указать и номер другого столбца. Автоматически переименуется в Local_time для унификации.
#'
#'
#' @param suppress_warnings bool: по умолчанию предупреждения не отображаются в консоли.
#' Для отладки вы можете их включить: `suppress_warnings = T`
#' @returns A tibble: тиббл со столбцами Local_time (обязательно),  и (опционально) T, RRR, sss
#' @export
#'
#' @examples
#' \dontrun{
#' # В самом распространенном случае вам нужно просто выполнить:
#' read_csv_rp5("путь к вашему файлу")
#'
#'
#' # Каждый параметр функции можно настроить:
#' read_csv_rp5("path/to/your/file",
#'               skip = 6,
#'               delim = ";",
#'               local_time_position = 1,
#'               col_select = c('T', 'RRR', 'sss'),
#'               encoding = "UTF-8",
#'               suppress_warnings = F)}
read_rp5_csv <- function(path,
                         skip = 6,
                         delim = ';',
                         local_time_position = 1,
                         col_select = c('T', 'RRR', 'sss'),
                         encoding = NULL,
                         suppress_warnings = T){

  # основная функция
  inner_func <- function() {

    if (is.null(encoding)) {
    encoding <- stringr::str_extract(path, "(?<=\\.)(ansi|utf8|unic)(?=\\.)")

    if (encoding == 'ansi'){
      encoding = 'windows-1251'
    }

    if (encoding == 'unic'){
      encoding = 'unicode'
    }

    if (encoding == 'utf8'){
      encoding = 'utf-8'
    }

    }

    df <- readr::read_delim(file = path,
                            delim = delim,
                            skip = skip,
                            col_select = c(Local_time = local_time_position,
                                           dplyr::all_of(col_select)),
                            show_col_types = FALSE,
                            locale = readr::locale(encoding = encoding)) |>
      dplyr::mutate(dplyr::across(2:dplyr::last_col(),
                           ~ readr::parse_number(as.character(.x))))
    return(df)
  }


  # настройка отключения предупреждений
  if (suppress_warnings) {
    suppressWarnings(inner_func())
      }
   else {
    inner_func()
   }

}







#' Импорт всех csv файлов из папки и объединение их в один тиббл
#' @description
#' Обертка над функцией \code{\link{read_rp5_csv}}, позволяющая импортировать сразу все csv-файлы из папки
#' и объединение из в один тиббл (датафрейм). Предназначено для csv с сайта \href{https://rp5.ru/}{rp5.ru}.
#' Во всех csv-файлах должны присутствовать столбцы, указанные в `col_select` (по умолчанию это
#' местное время, температура (T), количество осадков (RRR) и высота снежного покрова (sss)).
#'
#'
#' @param path character: путь к папке с csv файлами.
#' @param skip integer: количество строк в csv-файле, которые необходимо пропустить при импорте.
#' (обычно в верхних строках при скачивании с сайта rp5.ru хранятся метаданные).
#' По умолчанию `skip=6`
#' @param delim character: разделитель. По умолчанию `delim=';'`
#' @param local_time_position integer: позиция столбца с местным временем. По умолчанию = 1
#' По стандарту rp5 местное время располагается в первом столбце,
#'  но можно указать и номер другого столбца. Автоматически переименуется в Local_time для унификации.
#' @param encoding character: кодировка csv-файла.По умолчанию `encoding = NULL`,
#'  и функция пытается автоматически распознать кодировку, указанную в имени файла.
#'  При скачивании csv с rp5.ru информация о кодировке записана в названии, поэтому для автоматического
#'  распознавания кодировки лучше не переименовывать файл. Если возникают ошибки, вы можете установить
#'  значение вручную. Варианты кодировок,
#' в которых можно скачать csv с сайта rp5.ru: "utf-8", unicode" или "ANSI"
#' (в таком случае нужно указывать "windows-1251")
#' @param col_select character: отбор необходимых столбцов. Например col_select = c('T', 'RRR', 'sss').
#'  По умолчанию отбирает столбцы с местным временем,
#'  температурой (столбец T), количеством выпавших осадков (столбец RRR) и высотой снежного покрова
#'  (столбец sss).
#'
#'
#' @param suppress_warnings bool: по умолчанию предупреждения не отображаются в консоли.
#' Для отладки вы можете их включить: `suppress_warnings = T`
#'
#' @returns tibble со столбцами, указанными в col_select
#' @export
#'
#' @examples
#' \dontrun{
#' # в самом распространенном случае вам необходимо выполнить
#' read_rp5_folder("путь_до_вашей_папки")
#'
#' # при необходимости можно настроить поведение функции:
#' read_rp5_folder("path/to/your/folder",
#'                  skip = 6,
#'                  delim = ";",
#'                  local_time_position = 1,
#'                  col_select = c('T', 'RRR', 'sss'),
#'                  encoding = NULL,
#'                  suppress_warnings = T)  }
#'
read_rp5_folder <- function(path,
                            skip = 6,
                            delim = ';',
                            local_time_position = 1,
                            col_select = c('T', 'RRR', 'sss'),
                            encoding = NULL,
                            suppress_warnings = T){

  paths <- list.files(path = path, full.names = T, pattern = ".csv$")

  read_and_process <- function(path_to_file){
    df <- read_rp5_csv(path_to_file, skip, delim, local_time_position, col_select, encoding, suppress_warnings)
    return(df)
  }

  result <- paths |>
    purrr::map_dfr(read_and_process)

  print(paste(length(paths), "csv files were combined"))
  return(result)

}





