# This file is used by R CMD check when:
# _R_CHECK_CRAN_INCOMING_USE_ASPELL_=TRUE
# _R_CHECK_CRAN_INCOMING_=TRUE
if(file.exists(file.path(dir, 'inst', 'WORDLIST'))){
  Rd_files <- vignettes <- R_files <- description <- list(
    ignore = readLines(file.path(dir, 'inst', 'WORDLIST'))
  )
}

