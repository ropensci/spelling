#' The WORDLIST file
#'
#' The package wordlist file is used to allow custom words which will be added to the
#' dictionary when spell checking. It is stored in `inst/WORDLIST` in the source package
#' and must contain one word per line in UTF-8 encoded text.
#'
#' The [update_wordlist] function runs a full spell check on a package, shows the results,
#' and then prompts to add the found words to the package wordlist. Obviously you should
#' check closely that these legitimate words and not actual spelling errors. It also
#' removes words from the wordlist that no longer appear as spelling errors, either because
#' they have been removed from the documentation or added to the `lang` dictionary.
#'
#' @rdname wordlist
#' @name wordlist
#' @family spelling
#' @export
#' @param confirm show changes and ask confirmation before adding new words to the list
#' @param pkg path to package (or bookdown folder)
#' @inheritParams spell_check_package
update_wordlist <- function(pkg = ".", vignettes = TRUE, confirm = TRUE){
  wordfile <- get_wordfile(pkg)
  old_words <- sort(get_wordlist(pkg))
  new_words <- sort(spell_check_package(pkg, vignettes = vignettes, use_wordlist = FALSE)$word)
  if(isTRUE(all.equal(old_words, new_words))){
    cat(sprintf("No changes required to %s\n", wordfile))
  } else {
    words_added <- new_words[is.na(match(new_words, old_words))]
    words_removed <- old_words[is.na(match(old_words, new_words))]
    if(length(words_added)){
      cat(sprintf("The following words will be added to the wordlist:\n%s\n",
          paste(" -", words_added, collapse = "\n")))
    }
    if(length(words_removed)){
      cat(sprintf("The following words will be removed from the wordlist:\n%s\n",
          paste(" -", words_removed, collapse = "\n")))
    }
    if(isTRUE(confirm) && length(words_added)){
      cat("Are you sure you want to update the wordlist?")
      if (utils::menu(c("Yes", "No")) != 1){
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
get_wordlist <- function(pkg = "."){
  wordfile <- get_wordfile(pkg)
  out <- if(file.exists(wordfile))
    unlist(strsplit(readLines(wordfile, warn = FALSE, encoding = "UTF-8"), " ", fixed = TRUE))
  as.character(out)
}

get_wordfile <- function(path){
  normalizePath(file.path(path, "inst/WORDLIST"), mustWork = FALSE)
}
