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
spell_check_files <- function(path, ignore = character(), lang = "en_GB"){
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
  return(spell_check_file_plain(path = path, format = "text", dict = dict))
}

#' @rdname spell_check_files
#' @export
#' @param text character vector with plain text
spell_check_text <- function(text, ignore = character(), lang = "en_GB"){
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

#' @useDynLib spelling, .registration = TRUE
#' @importFrom Rcpp sourceCpp
spell_check_file_roxygen <- function(path, dict, global_options = list()) {

  parsed <- roxygen2::parse_file(file = path, global_options = global_options)

  lines <- readLines(path)
  is_roxygen <- grep("^[[:space:]]*#+'", lines)
  roxygen_lines <- lines[is_roxygen]

  # Some roxygen tags (such as param) have a name and a description, we only
  # want to spell check the latter.
  extract_text <- function(x) {
    if (is.list(x) && exists("description", x)) {
      return(x[["description"]])
    }
    x
  }

  # roxygen tags that contain text
  text_tags <- c("concept", "describeIn", "description", "details", "field", "note", "param", "return", "section", "slot", "title")
  parse_block <- function(tags) {
    text <- unlist(lapply(tags[names(tags) %in% text_tags], extract_text))
    if (length(text) == 0) {
      return(data.frame(word = character(), line = integer(), start = integer(), stringsAsFactors = FALSE))
    }

    # blank out rd tags, tag list derived from RdTextFilter
    # https://github.com/wch/r-source/blob/89ec1150299f7be62b839d5d5eb46bd9a63653bd/src/library/tools/R/Rdtools.R#L113-L126
    rd_tags <- c("S3method", "S4method", "command", "code", "docType", "email", "encoding", "file", "keyword", "link", "linkS4class", "method", "pkg", "var")
    re <- paste0("\\\\(", paste0(collapse = "|", rd_tags), ")[^}]+}")
    text <- blank_matches(text, re)
    bad_words <- hunspell::hunspell(text, dict = dict)
    res <- find_word_positions(roxygen_lines, unique(sort(unlist(bad_words))))

    # Fix line numbers for real file.
    res$line <- is_roxygen[res$line]

    vapply(split(res$line, res$word), paste, character(1), collapse = ", ")
  }

  unlist(lapply(parsed, parse_block))
}

blank_matches <- function(str, re) {
  m <- gregexpr(re, str)
  blanks <- function(n) strrep(" ", n)
  regmatches(str, m) <- Map(blanks, lapply(regmatches(str, m), nchar))
  str
}
