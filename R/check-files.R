#' Spell Check
#'
#' Perform a spell check on document files or plain text.
#'
#' @rdname spell_check_files
#' @family spelling
#' @inheritParams spell_check_package
#' @param path path to file or to spell check
#' @param ignore character vector with words which will be added to the [hunspell::dictionary]
#' @export
#' @examples # Example files
#' files <- list.files(system.file("examples", package = "knitr"),
#'   pattern = "\\.(Rnw|Rmd|html)$", full.names = TRUE)
#' spell_check_files(files)
spell_check_files <- function(path, ignore = character(), lang = "en_US"){
  stopifnot(is.character(ignore))
  dict <- hunspell::dictionary(lang, add_words = ignore)
  path <- normalizePath(path, mustWork = TRUE)
  lines <- lapply(sort(path), spell_check_file_one, dict = dict)
  summarize_words(path, lines)
}

spell_check_file_one <- function(path, dict){
  if(grepl("\\.r?md$",path, ignore.case = TRUE))
    return(spell_check_file_md(path, dict = dict))
  if(grepl("\\.(rnw|snw)$",path, ignore.case = TRUE))
    return(spell_check_file_knitr(path = path, format = "latex", dict = dict))
  if(grepl("\\.(tex)$",path, ignore.case = TRUE))
    return(spell_check_file_plain(path = path, format = "latex", dict = dict))
  if(grepl("\\.(html?)$",path, ignore.case = TRUE))
    return(spell_check_file_plain(path = path, format = "html", dict = dict))
  if(grepl("\\.(xml)$",path, ignore.case = TRUE))
    return(spell_check_file_plain(path = path, format = "xml", dict = dict))
  if(grepl("\\.(pdf)$",path, ignore.case = TRUE))
    return(spell_check_file_pdf(path = path, format = "text", dict = dict))
  return(spell_check_file_plain(path = path, format = "text", dict = dict))
}

#' @rdname spell_check_files
#' @export
#' @param text character vector with plain text
spell_check_text <- function(text, ignore = character(), lang = "en_US"){
  stopifnot(is.character(ignore))
  dict <- hunspell::dictionary(lang, add_words = ignore)
  bad_words <- hunspell::hunspell(text, dict = dict)
  words <- sort(unique(unlist(bad_words)))
  out <- data.frame(word = words, stringsAsFactors = FALSE)
  out$found <- lapply(words, function(word) {
    which(vapply(bad_words, `%in%`, x = word, logical(1)))
  })
  out
}

spell_check_plain <- function(text, dict){
  bad_words <- hunspell::hunspell(text, dict = dict)
  vapply(sort(unique(unlist(bad_words))), function(word) {
    line_numbers <- which(vapply(bad_words, `%in%`, x = word, logical(1)))
    paste(line_numbers, collapse = ",")
  }, character(1))
}

spell_check_file_text <- function(file, dict){
  spell_check_plain(readLines(file), dict = dict)
}

spell_check_file_rd <- function(rdfile, dict){
  text <- tools::RdTextFilter(rdfile)
  spell_check_plain(text, dict = dict)
}

spell_check_file_md <- function(path, dict){
  words <- parse_text_md(path)
  words$startline <- vapply(strsplit(words$position, ":", fixed = TRUE), `[[`, character(1), 1)
  bad_words <- hunspell::hunspell(words$text, dict = dict)
  vapply(sort(unique(unlist(bad_words))), function(word) {
    line_numbers <- which(vapply(bad_words, `%in%`, x = word, logical(1)))
    paste(words$startline[line_numbers], collapse = ",")
  }, character(1))
}

spell_check_file_knitr <- function(path, format, dict){
  latex <- remove_chunks(path)
  words <- hunspell::hunspell_parse(latex, format = format, dict = dict)
  text <- vapply(words, paste, character(1), collapse = " ")
  spell_check_plain(text, dict = dict)
}

spell_check_file_plain <- function(path, format, dict){
  lines <- readLines(path, warn = FALSE)
  words <- hunspell::hunspell_parse(lines, format = format, dict = dict)
  text <- vapply(words, paste, character(1), collapse = " ")
  spell_check_plain(text, dict = dict)
}

spell_check_file_pdf <- function(path, format, dict){
  lines <- pdftools::pdf_text(path)
  words <- hunspell::hunspell_parse(lines, format = format, dict = dict)
  text <- vapply(words, paste, character(1), collapse = " ")
  spell_check_plain(text, dict = dict)
}
