#include <Rcpp.h>
#include <cstring>
using namespace Rcpp;

// [[Rcpp::export]]
Rcpp::DataFrame find_word_positions(CharacterVector lines,
                                    CharacterVector words) {
  std::vector<const char*> found_words;
  std::vector<int> found_lines;
  std::vector<int> found_starts;

  for (int i = 0; i < words.size(); ++i) {
    const char* word = words.at(i);
    size_t len = strlen(word);
    bool found = false;
    for (int j = 0; j < lines.size(); ++j) {
      const char* line = lines.at(j);
      for (const char* p = line; (p = strstr(p, word)) != NULL; ++p) {
        if ((p == line) || (p != NULL && !isalnum(p[-1]))) {
          if (!isalnum(p[len])) {
            found = true;
            found_words.push_back(word);
            found_lines.push_back(j + 1);
            found_starts.push_back((int)(p - lines.at(j)) + 1);
          }
          p += len;
        }
      }
    }
    if (!found) {
      found_words.push_back(word);
      found_lines.push_back(NA_INTEGER);
      found_starts.push_back(NA_INTEGER);
    }
  }
  return DataFrame::create(_["word"] = found_words, _["line"] = found_lines,
                           _["start"] = found_starts,
                           Rcpp::_["stringsAsFactors"] = false);
}
