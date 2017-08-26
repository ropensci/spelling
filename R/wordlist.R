#' Update WORDLIST file
#'
#' Read and update the wordlist included with a package. The wordlist is stored in
#' the file `inst/WORDLIST` in the source package and is used to whitelist custom words
#' that will be added to the dictionary when spell checking.
#'
#' @rdname wordlist
#' @name wordlist
#' @export
#' @param confirm show changes and ask confirmation before adding new words to the list
#' @inheritParams spell_check
update_wordlist <- function(path = ".", vignettes = TRUE, lang = "en_US", confirm = TRUE){
  wordfile <- get_wordfile(path)
  old_words <- sort(get_wordlist(path))
  new_words <- sort(names(spell_check_package(path, vignettes = vignettes, lang = lang, use_wordlist = FALSE)))
  if(isTRUE(all.equal(old_words, new_words))){
    cat(sprintf("No changes required to %s\n", wordfile))
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
    if(isTRUE(confirm) && length(words_added)){
      cat("Are you sure you want to update the wordlist?")
      if (!interactive() || utils::menu(c("Yes", "No")) != 1){
        return(invisible())
      }
    }

    # Save as UTF-8
    dir.create(dirname(wordfile), showWarnings = FALSE)
    writeLines(enc2utf8(new_words), wordfile, useBytes = TRUE)
    cat(sprintf("Added %d and removed %d words in %s\n", length(words_added), length(words_removed), wordfile))
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
