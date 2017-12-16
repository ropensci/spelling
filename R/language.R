# Very simple right now:
# Convert dashes to underscore
# Convert 'en' to 'en_US'
# Convert e.g. 'de' to 'de_DE'
normalize_lang <- function(lang = NULL){
  if(!length(lang) || !nchar(lang)){
    message("DESCRIPTION does not contain 'Language' field. Defaulting to 'en-US'.")
    lang <- "en-US"
  }
  if(tolower(lang) == "en" || tolower(lang) == "eng"){
    message("Found ambiguous language 'en'. Defaulting to 'en-US")
    lang <- "en-US"
  }
  if(nchar(lang) == 2){
    oldlang <- lang
    lang <- paste(tolower(lang), toupper(lang), sep = "_")
    message(sprintf("Found ambiguous language '%s'. Defaulting to '%s'", oldlang, lang))
  }
  gsub("-", "_", lang, fixed = TRUE)
}
