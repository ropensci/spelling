#' Update Wordlist
#'
#' Read and update the wordlist included with a package. The wordlist is stored in
#' the file `inst/WORDLIST` in the source package and is be used to whitelist
#' custom words that will be added to the dictionary when spell checking.
#'
#' @rdname wordlist
#' @name wordlist
#' @export
#' @param confirm show changes and ask confirmation before writing changes
#' @inheritParams spell_check
update_wordlist <- function(path = ".", vignettes = TRUE, lang = "en_US", confirm = TRUE){
  old_words <- sort(get_wordlist())
  new_words <- sort(names(spell_check_package(path, vignettes = vignettes, lang = lang, use_wordlist = FALSE)))
  if(isTRUE(all.equal(old_words, new_words))){
    cat("No changes required to wordlist\n")
  } else {
    words_added <- new_words[is.na(match(new_words, old_words))]
    words_removed <- old_words[is.na(match(old_words, new_words))]
    if(length(words_added)){
      cat(sprintf("The following words were added to the wordlist:\n%s\n",
          paste(" -", words_added, collapse = "\n")))
    }
    if(length(words_removed)){
      cat(sprintf("The following words were removed from the wordlist:\n%s\n",
          paste(" -", words_removed, collapse = "\n")))
    }
    if(isTRUE(confirm)){
      cat("Are you sure you want to update the wordlist?")
      if (!interactive() || utils::menu(c("Yes", "No")) != 1){
        return(invisible())
      }
    }

    # Save as UTF-8
    wordfile <- get_wordfile(path)
    dir.create(dirname(wordfile), showWarnings = FALSE)
    writeLines(enc2utf8(new_words), wordfile, useBytes = TRUE)
  }
}

#' @rdname wordlist
#' @export
get_wordlist <- function(path = "."){
  wordfile <- get_wordfile(path)
  out <- if(file.exists(wordfile))
    unlist(strsplit(readLines(wordfile, warn = FALSE, encoding = "UTF-8"), " ", fixed = TRUE))
  as.character(out)
}

get_wordfile <- function(path){
  normalizePath(file.path(path, "inst/WORDLIST"), mustWork = FALSE)
}
