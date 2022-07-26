# spelling

[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](http://www.repostatus.org/badges/latest/active.svg)](http://www.repostatus.org/#active)
[![Build Status](https://travis-ci.org/ropensci/spelling.svg?branch=master)](https://travis-ci.org/ropensci/spelling)
[![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/ropensci/spelling?branch=master&svg=true)](https://ci.appveyor.com/project/jeroen/spelling)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/spelling)](http://cran.r-project.org/package=spelling)
[![CRAN RStudio mirror downloads](http://cranlogs.r-pkg.org/badges/spelling)](http://cran.r-project.org/web/packages/spelling/index.html)

> Tools for Spell Checking in R

Spell checking common document formats including latex, markdown, manual pages,
and description files. Includes utilities to automate checking of documentation and 
vignettes as a unit test during 'R CMD check'. Both British and American English are 
supported out of the box and other languages can be added. In addition, packages may
define a 'wordlist' to allow custom terminology without having to abuse punctuation.

## Spell Check Single Files

The function `spell_check_files` automatically parses known text formats and only spell checks text blocks, not code chunks.

```r
spell_check_files('README.md', lang = 'en_US')
#   WORD       FOUND IN
# AppVeyor   README.md:5
# CMD        README.md:12
# RStudio    README.md:8
```

For more information about the underlying spelling engine and how to add 
support for other languages, see the [hunspell package](https://docs.ropensci.org/hunspell/articles/intro.html#hunspell-dictionaries).

![screenshot](https://jeroen.github.io/images/rs-hunspell.png)

## Spell Check a Package

Spell check documentation, description, readme, and vignettes of a package:

```r
spell_check_package("~/workspace/V8")
# DESCRIPTION does not contain 'Language' field. Defaulting to 'en-US'.
#   WORD          FOUND IN
# ECMA          V8.Rd:16, description:2,4
# emscripten    description:5
# htmlwidgets   JS.Rd:16
# JSON          V8.Rd:33,38,39,57,58,59,121
# jsonlite      V8.Rd:42
# Ooms          V8.Rd:41,121
# th            description:3
# Xie           JS.Rd:26
# Yihui         JS.Rd:26
```

Review these words and then update the wordlist to allow them:


```r
update_wordlist("~/workspace/V8")
# The following words will be added to the wordlist:
#  - ECMA
#  - emscripten
#  - htmlwidgets
#  - JSON
#  - jsonlite
#  - Ooms
#  - th
#  - Xie
#  - Yihui
# Are you sure you want to update the wordlist?
# 1: Yes
# 2: No
```

Then these will no longer be marked as errors:

```r
> spell_check_package("~/workspace/V8")
No spelling errors found.
```

## Automate Package Spell Checking

Use `spell_check_setup()` to add a unit test to your package which automatically runs a spell check on documentation and vignettes during `R CMD check` if the environment variable `NOT_CRAN` is set to `TRUE`. By default this unit test never fails; it merely prints potential spelling errors to the console.


```r
spell_check_setup("~/workspace/V8")
# Adding 'Language: en-US' to DESCRIPTION
# No changes required to /Users/jeroen/workspace/V8/inst/WORDLIST
# Updated /Users/jeroen/workspace/V8/tests/spelling.R
```

Note that the `NOT_CRAN` variable is automatically set to 1 on Travis and in devtools or RStudio, otherwise you need to set it yourself:

```sh
export NOT_CRAN=1
R CMD check V8_1.5.9000.tar.gz
# * using log directory ‘/Users/jeroen/workspace/V8.Rcheck’
# * using R version 3.5.1 (2018-07-02)
# * using platform: x86_64-apple-darwin15.6.0 (64-bit)
# ...
# ...
# * checking tests ...
#   Running ‘spelling.R’
#   Comparing ‘spelling.Rout’ to ‘spelling.Rout.save’ ... OK
#   Running ‘testthat.R’
#  OK
```
