#' Spell Check Package
#'
#' Runs a spell check on text fields in the package description file and manual pages.
#' Hunspell includes dictionaries for `en_US` and `en_GB` by default. Other languages
#' require installation of a custom dictionary, see [hunspell][hunspell::hunspell] for details.
#'
#' @export
#' @rdname spell_check
#' @param pkg package description, can be path or package name. Passed to [devtools::as.package].
#' @param ignore character vector with words to ignore. Passed to [hunspell][hunspell::hunspell].
#' @param dict a dictionary object or language string. Passed to [hunspell][hunspell::hunspell].
spell_check_package <- function(pkg = ".", ignore = character(), dict = "en_US"){

  pkg <- devtools::as.package(pkg)
  ignore <- c(pkg$package, hunspell::en_stats, ignore)

  # Check Rd manual files
  rd_files <- list.files(file.path(pkg$path, "man"), "\\.rd$", ignore.case = TRUE, full.names = TRUE)
  rd_lines <- lapply(sort(rd_files), spell_check_file_rd, ignore = ignore, dict = dict)

  # Markdown vignettes
  md_files <- list.files(file.path(pkg$path, "vignettes"), pattern = "\\.r?md$", ignore.case = TRUE, full.names = TRUE)
  md_lines <- lapply(sort(md_files), spell_check_file_md, ignore = ignore, dict = dict)

  # Check 'DESCRIPTION' fields
  pkg_fields <- c("title", "description")
  pkg_lines <- lapply(pkg_fields, function(x){
    spell_check_file_text(textConnection(pkg[[x]]), ignore = ignore, dict = dict)
  })

  # Combine
  all_sources <- c(rd_files, md_files, pkg_fields)
  all_lines <- c(rd_lines, md_lines, pkg_lines)
  words_by_file <- lapply(all_lines, names)
  bad_words <- sort(unique(unlist(words_by_file)))

  # Find all occurences for each word
  out <- lapply(bad_words, function(word) {
    index <- which(vapply(words_by_file, `%in%`, x = word, logical(1)))
    reports <- vapply(index, function(i){
      paste0(basename(all_sources[i]), ":", all_lines[[i]][word])
    }, character(1))
  })
  structure(out, names = bad_words, class = "spellcheck")
}

#' @rdname spell_check
#' @export
#' @param path file with supported file extension
spell_check_file <- function(path, ignore = character(), dict = "en_US"){
  stop("not yet implemented")
}

#' @rdname spell_check
#' @export
#' @param text character vector with plain text
spell_check_text <- function(text, ignore = character(), dict = "en_US"){
  bad_words <- hunspell::hunspell(text, ignore = ignore, dict = dict)
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
    cat(paste(x[[i]], collapse = ", "))
    cat("\n")
  }
  invisible(x)
}

spell_check_file_text <- function(file, ignore, dict){
  spell_check_text(readLines(file), ignore = ignore, dict = dict)
}

spell_check_file_rd <- function(rdfile, ignore, dict){
  text <- tools::RdTextFilter(rdfile)
  spell_check_text(text, ignore = ignore, dict = dict)
}

spell_check_file_md <- function(path, ignore, dict){
  words <- parse_text_md(path)
  words$startline <- vapply(strsplit(words$position, ":", fixed = TRUE), `[[`, character(1), 1)
  bad_words <- hunspell::hunspell(words$text, ignore = ignore, dict = dict)
  vapply(sort(unique(unlist(bad_words))), function(word) {
    line_numbers <- which(vapply(bad_words, `%in%`, x = word, logical(1)))
    paste(words$startline[line_numbers], collapse = ",")
  }, character(1))
}
