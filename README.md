# spelling

[![Build Status](https://travis-ci.org/ropensci/spelling.svg?branch=master)](https://travis-ci.org/ropensci/spelling)
[![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/ropensci/spelling?branch=master&svg=true)](https://ci.appveyor.com/project/jeroen/spelling)
[![Coverage Status](https://codecov.io/github/ropensci/spelling/coverage.svg?branch=master)](https://codecov.io/github/ropensci/spelling?branch=master)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/spelling)](http://cran.r-project.org/package=spelling)
[![CRAN RStudio mirror downloads](http://cranlogs.r-pkg.org/badges/spelling)](http://cran.r-project.org/web/packages/spelling/index.html)

> Tools for Spell Checking in R

Automated spell checking for common document formats including latex,
markdown, manual pages, and description files

## Installation

```r
devtools::install_github("ropensci/spelling")
```

## Getting started

Spell check documentation, description and vignettes of a package:

```r
spell_check_package("~/workspace/V8")
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
