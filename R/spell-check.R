#' Spell Checking
#'
#' Perform a spell check on manual pages, vignettes, description files and other formats.
#'
#' The `spell_check_package()` checks the package manual pages, rmd/rnw vignettes, and text
#' fields in the DESCRIPTION file. The file `tools/wordlist.txt` can be used to whitelist
#' custom words in your package, which will be added to the dictionary before checking.
#'
#' Hunspell includes dictionaries for `en_US` and `en_GB` by default. Other languages
#' require installation of a custom dictionary, see [hunspell][hunspell::hunspell] for details.
#'
#' @export
#' @rdname spell_check
#' @param path path to file or package root directory containing the `DESCRIPTION` file
#' @param vignettes spell check `rmd` and `rnw` files in the `vignettes` folder
#' @param ignore character vector with words to ignore in [hunspell][hunspell::hunspell]
#' @param lang string with dictionary language string for [hunspell::dictionary][hunspell::dictionary]
spell_check_package <- function(path = ".", vignettes = TRUE, lang = "en_US"){
  if(inherits(path, 'package')){
    pkg <- path
  } else {
    description <- normalizePath(file.path(path, "DESCRIPTION"), mustWork = TRUE)
    pkg <- as.list(read.dcf(description)[1,])
    names(pkg) <- tolower(names(pkg))
    pkg$path <- dirname(description)
  }

  # Add custom words to the ignore list
  wordfile <- normalizePath(file.path(path, "tools/wordlist.txt"), mustWork = FALSE)
  wordlist <- if(file.exists(wordfile))
    unlist(strsplit(readLines(wordfile, warn = FALSE), " ", fixed = TRUE))
  author <- strsplit(pkg$author, " ", fixed = TRUE)[[1]]
  ignore <- c(pkg$package, author, hunspell::en_stats, wordlist)

  # Create the hunspell dictionary object
  dict <- hunspell::dictionary(lang, add_words = ignore)

  # Check Rd manual files
  rd_files <- list.files(file.path(pkg$path, "man"), "\\.rd$", ignore.case = TRUE, full.names = TRUE)
  rd_lines <- lapply(sort(rd_files), spell_check_file_rd, dict = dict)

  # Check 'DESCRIPTION' fields
  pkg_fields <- c("title", "description")
  pkg_lines <- lapply(pkg_fields, function(x){
    spell_check_file_text(textConnection(pkg[[x]]), dict = dict)
  })

  # Combine
  all_sources <- c(rd_files, pkg_fields)
  all_lines <- c(rd_lines, pkg_lines)

  if(isTRUE(vignettes)){
    # Markdown vignettes
    md_files <- list.files(file.path(pkg$path, "vignettes"), pattern = "\\.r?md$", ignore.case = TRUE, full.names = TRUE)
    md_lines <- lapply(sort(md_files), spell_check_file_md, dict = dict)

    # Sweave vignettes
    rnw_files <- list.files(file.path(pkg$path, "vignettes"), pattern = "\\.[rs]nw$", ignore.case = TRUE, full.names = TRUE)
    rnw_lines <- lapply(sort(rnw_files), spell_check_file_knitr, format = "latex", dict = dict)

    # Combine
    all_sources <- c(all_sources, md_files, rnw_files)
    all_lines <- c(all_lines, md_lines, rnw_lines)
  }
  summarize_words(all_sources, all_lines)
}

# Find all occurences for each word
summarize_words <- function(file_names, found_line){
  words_by_file <- lapply(found_line, names)
  bad_words <- sort(unique(unlist(words_by_file)))
  out <- lapply(bad_words, function(word) {
    index <- which(vapply(words_by_file, `%in%`, x = word, logical(1)))
    reports <- vapply(index, function(i){
      paste0(basename(file_names[i]), ":", found_line[[i]][word])
    }, character(1))
  })
  structure(out, names = bad_words, class = "spellcheck")
}

#' @rdname spell_check
#' @export
#' @examples # Example files
#' files <- list.files(system.file("examples", package = "knitr"),
#'   pattern = "\\.(Rnw|Rmd|html)$", full.names = TRUE)
#' spell_check_files(files)
spell_check_files <- function(path, ignore = character(), lang = "en_US"){
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

#' @rdname spell_check
#' @export
#' @param text character vector with plain text
spell_check_text <- function(text, ignore = character(), lang = "en_US"){
  dict <- hunspell::dictionary(lang, add_words = ignore)
  spell_check_plain(text, dict)
}

spell_check_plain <- function(text, dict){
  bad_words <- hunspell::hunspell(text, dict = dict)
  vapply(sort(unique(unlist(bad_words))), function(word) {
    line_numbers <- which(vapply(bad_words, `%in%`, x = word, logical(1)))
    paste(line_numbers, collapse = ",")
  }, character(1))
}

#' @export
print.spellcheck <- function(x, ...){
  words <- names(x)
  fmt <- paste0("%-", max(nchar(words), 0) + 3, "s")
  pretty_names <- sprintf(fmt, words)
  cat(sprintf(fmt, "  WORD"), "  FOUND IN\n", sep = "")
  for(i in seq_along(x)){
    cat(pretty_names[i])
    cat(paste(x[[i]], collapse = paste0("\n", sprintf(fmt, ""))))
    cat("\n")
  }
  invisible(x)
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
