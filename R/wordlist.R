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
#' @param ... parameters passed to the different methods.
#' @rdname wordlist
#' @name wordlist
#' @family spelling
#' @export
#' @param confirm show changes and ask confirmation before adding new words to the list
#' @inheritParams spell_check_package
update_wordlist <- function(pkg = ".", ...) {
  UseMethod("update_wordlist", pkg)
}

#' @export
#' @importFrom utils menu
#' @rdname wordlist
update_wordlist.character <- function(pkg = ".", ..., confirm = TRUE) {
  stopifnot("pkg is not a string" = is.character(pkg) && length(pkg) == 1)
  stopifnot("pkg is not an existing directory" = file_test("-d", pkg))
  if (file_test("-f", file.path(pkg, "DESCRIPTION"))) {
    return(update_wordlist(as_package(pkg), ...))
  }

  old_words <- get_wordlist(pkg)
  settings <- parse_settings(root = pkg)
  new_names <- names(settings)[names(settings) != "ignore"]
  new_names[new_names == "default"] <- settings$default
  new_words <- vapply(
    new_names, FUN.VALUE = vector(mode = "list", length = 1),
    new_words = spell_check_project(root = pkg, lang = "update"),
    FUN = function(x, new_words) {
      issues <- new_words$word[new_words$lang == x]
      if (length(issues) == 0) {
        return(list(character(0)))
      }
      list(sort(issues, method = "radix"))
    }
  )
  names(new_words)[names(new_words) == settings$default] <- "default"
  if (identical(old_words, new_words)) {
    message("No new words to add")
    return(invisible(NULL))
  }
  wordfile <- get_wordfile(pkg)
  for (i in names(wordfile)) {
    all.equal(old_words[[i]], new_words[[i]])
    if (identical(old_words[[i]], new_words[[i]])) {
      next
    }
    words_added <- new_words[[i]][!new_words[[i]] %in% old_words[[i]]]
    if (length(words_added)) {
      message(
        sprintf(
          "The following words will be added to the %s wordlist:\n%s\n",
          i, paste(" -", words_added, collapse = "\n")
        )
      )
    }
    words_removed <- old_words[[i]][!old_words[[i]] %in% new_words[[i]]]
    if (length(words_removed)) {
      message(
        sprintf(
          "The following words will be removed the %s wordlist:\n%s\n",
          i, paste(" -", words_removed, collapse = "\n")
        )
      )
    }
    if (isTRUE(confirm) && length(words_added) || length(words_removed)) {
      message(sprintf("Are you sure you want to update the %s wordlist?", i))
      if (menu(c("Yes", "No")) != 1) {
        next
      }
    }
    # Save as UTF-8
    dir.create(dirname(wordfile[i]), showWarnings = FALSE)
    writeLines(enc2utf8(new_words[[i]]), wordfile[i], useBytes = TRUE)
    message(
      sprintf(
        "Added %d and removed %d words in %s\n", length(words_added),
        length(words_removed), wordfile[i]
      )
    )
  }
}

#' @export
#' @rdname wordlist
update_wordlist.package <- function(
  pkg = ".", vignettes = TRUE, confirm = TRUE, inst = FALSE, ...
) {
  wordfile <- get_wordfile(pkg$path)
  old_words <- sort(get_wordlist(pkg), method = "radix")
  new_words <- sort(
    spell_check_package(
      pkg$path, vignettes = vignettes, inst = inst, use_wordlist = FALSE
    )$word,
    method = "radix"
  )
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
get_wordlist <- function(pkg = ".") {
  UseMethod("get_wordlist", pkg)
}

#' @rdname wordlist
#' @export
#' @importFrom utils file_test
get_wordlist.character <- function(pkg = ".") {
  stopifnot("pkg is not a string" = is.character(pkg) && length(pkg) == 1)
  stopifnot("pkg is not an existing directory" = file_test("-d", pkg))
  if (
    file_test("-f", file.path(pkg, "DESCRIPTION")) &&
    !file_test("-f", file.path(pkg, "spell_check.json"))
  ) {
    return(get_wordlist(as_package(pkg)))
  }
  wordfile <- get_wordfile(path = pkg)
  vapply(
    wordfile, FUN.VALUE = vector(mode = "list", length = 1),
    FUN = function(x) {
      if (!file_test("-f", x)) {
        return(list(character(0)))
      }
      out <- readLines(x, warn = FALSE, encoding = "UTF-8")
      if (length(out) == 0) {
        return(list(character(0)))
      }
      out <- unlist(strsplit(out, " ", fixed = TRUE))
      list(sort(out, method = "radix"))
    }
  )
}

#' @rdname wordlist
#' @export
get_wordlist.package <- function(pkg) {
  wordfile <- get_wordfile(pkg$path)
  out <- if(file.exists(wordfile))
    unlist(strsplit(readLines(wordfile, warn = FALSE, encoding = "UTF-8"), " ", fixed = TRUE))
  as.character(out)
}

#' @importFrom utils file_test
get_wordfile <- function(path) {
  if (!file_test("-f", file.path(path, "spell_check.json"))) {
    return(normalizePath(file.path(path, "inst", "WORDLIST"), mustWork = FALSE))
  }
  settings <- parse_settings(root = path)
  extra_lang <- vapply(
    names(settings)[!names(settings) %in% c("default", "ignore")],
    FUN = normalize_lang, FUN.VALUE = character(1)
  )
  wordfile <- paste0("WORDLIST", c("", paste0("_", extra_lang)))
  wordfile <- normalizePath(file.path(path, "inst", wordfile), mustWork = FALSE)
  names(wordfile) <- c("default", names(extra_lang))
  return(wordfile)
}
