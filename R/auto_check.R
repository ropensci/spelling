#' Auto Spell Check
#'
#' Setup automatic spell checking as a unit test for `CMD check`.
#'
#' The [spell_test_run()] function is intended to be called as a unit test triggered
#' via running `CMD check` on a package. Use [spell_test_setup()] to automatically
#' create the unit test that does this.
#'
#' @export
#' @family spelling
#' @rdname autocheck
#' @inheritParams spell_check
spell_test_run <- function(vignettes = TRUE, lang = "en_US"){
  pkg_dir <- list.files("../00_pkg_src", full.names = TRUE)
  if(!length(pkg_dir)){
    warning("Failed to find package source directory")
    return(invisible())
  }
  results <- spell_check_package(pkg_dir, vignettes = vignettes, lang = lang)
  if(length(results))
    stop(sprintf("Potential spelling errors: %s\nIf these are false positives, run spelling::update_wordlist()",
                 paste(names(results),collapse = ", ")))
  cat("All good.\n")
}

#' @export
#' @rdname autocheck
spell_test_setup <- function(path = ".", vignettes = TRUE, lang = "en_US"){
  update_wordlist(path)
  dir.create(file.path(path, "tests"), showWarnings = FALSE)
  writeLines(sprintf("spelling::spell_test_run(vignettes = %s, lang = %s)",
                     deparse(vignettes), deparse(lang)), file.path(path, "tests/spelling.R"))
}
