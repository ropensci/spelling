#' Package Spell Checking
#'
#' Automatically spell-check package description, documentation, and vignettes.
#'
#' Parse and spell check R package manual pages, rmd/rnw vignettes, and text fields in the
#' `DESCRIPTION` file. Use the [WORDLIST][get_wordlist] file to allow custom words in your
#' package, which will be added to the dictionary when spell checking.
#'
#' The [spell_check_setup] function adds a unit test to your package which automatically
#' runs a spell check on documentation and vignettes during `R CMD check`. By default this
#' unit test never fails; it merely prints potential spelling errors to the console.
#'
#' Hunspell includes dictionaries for `en_US` and `en_GB` by default. Other languages
#' require installation of a custom dictionary, see [hunspell][hunspell::hunspell] for details.
#'
#' @export
#' @rdname spell_check_package
#' @name spell_check_package
#' @aliases spelling
#' @family spelling
#' @param pkg path to package root directory containing the `DESCRIPTION` file
#' @param vignettes also check all `rmd` and `rnw` files in the pkg `vignettes` folder
#' @param lang dictionary string for [hunspell][hunspell::dictionary],
#' usually either `"en_US"` or `"en_GB"`.
#' @param use_wordlist ignore words in the package [WORDLIST][get_wordlist] file
spell_check_package <- function(pkg = ".", vignettes = TRUE, lang = "en_GB", use_wordlist = TRUE){
  # Get package info
  pkg <- as_package(pkg)

  # Add custom words to the ignore list
  add_words <- if(isTRUE(use_wordlist))
    get_wordlist(pkg$path)
  author <- strsplit(pkg$author, " ", fixed = TRUE)[[1]]
  ignore <- unique(c(pkg$package, author, hunspell::en_stats, add_words))

  # Create the hunspell dictionary object
  dict <- hunspell::dictionary(lang, add_words = sort(ignore))

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

as_package <- function(pkg){
  if(inherits(pkg, 'package'))
    return(pkg)
  path <- pkg
  description <- if(file.exists(file.path(path, "DESCRIPTION.in"))){
    file.path(path, "DESCRIPTION.in")
  } else {
    normalizePath(file.path(path, "DESCRIPTION"), mustWork = TRUE)
  }
  pkg <- as.list(read.dcf(description)[1,])
  names(pkg) <- tolower(names(pkg))
  pkg$path <- dirname(description)
  return(pkg)
}

# Find all occurences for each word
summarize_words <- function(file_names, found_line){
  words_by_file <- lapply(found_line, names)
  bad_words <- sort(unique(unlist(words_by_file)))
  out <- data.frame(
    word = bad_words,
    stringsAsFactors = FALSE
  )
  out$found <- lapply(bad_words, function(word) {
    index <- which(vapply(words_by_file, `%in%`, x = word, logical(1)))
    reports <- vapply(index, function(i){
      paste0(basename(file_names[i]), ":", found_line[[i]][word])
    }, character(1))
  })
  structure(out, class = c("summary_spellcheck", "data.frame"))
}

#' @export
print.summary_spellcheck <- function(x, ...){
  if(!nrow(x)){
    cat("No spelling errors found.\n")
    return(invisible())
  }
  words <- x$word
  fmt <- paste0("%-", max(nchar(words), 0) + 3, "s")
  pretty_names <- sprintf(fmt, words)
  cat(sprintf(fmt, "  WORD"), "  FOUND IN\n", sep = "")
  for(i in seq_len(nrow(x))){
    cat(pretty_names[i])
    cat(paste(x$found[[i]], collapse = paste0("\n", sprintf(fmt, ""))))
    cat("\n")
  }
  invisible(x)
}

#' @export
#' @aliases spell_check_test
#' @rdname spell_check_package
#' @param error make `R CMD check` fail when spelling errors are found.
#' Default behaviour only prints spelling errors to the console at the end of `CMD check`.
spell_check_setup <- function(pkg = ".", vignettes = TRUE, lang = "en_GB", error = FALSE){
  # Get package info
  pkg <- as_package(pkg)
  update_wordlist(pkg$path, vignettes = vignettes, lang = lang)
  dir.create(file.path(pkg$path, "tests"), showWarnings = FALSE)
  writeLines(sprintf("spelling::spell_check_test(vignettes = %s, lang = %s, error = %s)",
    deparse(vignettes), deparse(lang), deparse(error)), file.path(pkg$path, "tests/spelling.R"))
  cat(sprintf("Updated %s\n", file.path(pkg$path, "tests/spelling.R")))
  try(add_to_description(pkg))
}

#' @export
spell_check_test <- function(vignettes = TRUE, lang = "en_GB", error = FALSE){
  out_save <- readLines(system.file("templates/spelling.Rout.save", package = 'spelling'))
  code <- paste(">", readLines("spelling.R"), collapse = "\n")
  out_save <- sub("@INPUT@", code, out_save, fixed = TRUE)
  writeLines(out_save, "spelling.Rout.save")

  # Try to find pkg source directory
  pkg_dir <- list.files("../00_pkg_src", full.names = TRUE)
  if(!length(pkg_dir)){
    # This is where it is on e.g. win builder
    check_dir <- dirname(getwd())
    if(grepl("\\.Rcheck$", check_dir)){
      source_dir <- sub("\\.Rcheck$", "", check_dir)
      if(file.exists(source_dir))
        pkg_dir <- source_dir
    }
  }
  if(!length(pkg_dir)){
    warning("Failed to find package source directory")
    return(invisible())
  }
  results <- spell_check_package(pkg_dir, vignettes = vignettes, lang = lang)
  if(nrow(results)){
    if(isTRUE(error)){
      output <- sprintf("Potential spelling errors: %s\n", paste(results$word, collapse = ", "))
      stop(output, call. = FALSE)
    } else {
      cat("Potential spelling errors:\n")
      print(results)
    }
  }
  cat("All Done!\n")
}

add_to_description <- function(pkg){
  if(!grepl("spelling", pkg$suggests)){
    desc <- normalizePath(file.path(pkg$path, "DESCRIPTION"), mustWork = TRUE)
    lines <- readLines(desc, warn = FALSE)
    out <- if(!any(grepl("^Suggests", lines))){
      c(lines, "Suggests:\n    spelling")
    } else {
      sub("^Suggests:", "Suggests:\n    spelling,", lines)
    }
    writeLines(out, desc)
  }
}
