#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::interfaces()]]
// [[Rcpp::export("findFirstGE")]]
IntegerVector findFirstGE(NumericVector x,
                          NumericVector target,
                          bool forward = true) {
  const double eps = 1E-8; // precision of comparison

  auto ge = [eps](double x1, double x2) {
    return x1 > x2 || fabs(x1 - x2) < eps;
  };

  int xn = x.size();
  if (xn == 0) return Rcpp::IntegerVector();
  int tn = target.size();

  Rcpp::IntegerVector result(tn, NA_INTEGER);
  int totalFound = 0;

  int starti, endi, stepi;
  if (forward) {
    starti = 0; endi = xn - 1; stepi = +1;
  } else {
    starti = xn - 1; endi = 0; stepi = -1;
  }
  // Cut the sequence if it has a level tail to save time and achieve a better result.
  while (endi != starti && x[endi] == x[endi-stepi])
    endi -= stepi;
  while (starti != endi && x[starti] == 0 && x[starti+stepi] == 0)
    starti += stepi;

  for (int i = starti; forward ? i <= endi : i >= endi; i += stepi) {
    // skip when x has elements left but all targets are finished
    if (totalFound == tn) return result;

    for (int t = 0; t < tn; t++) {
      if (target[t] != 0 && i != endi && x[i] == x[i+stepi]) continue; // skip if values are equal

      if (IntegerVector::is_na(result[t]) && // value not set, yet
          ge(x[i], target[t])) { // threshold found
        if (!NumericVector::is_na(target[t])) { // skip NA in target
            result[t] = i + 1; // R is 1-indexed
        }
        totalFound++;
      }
    }
  }

  return result;
}
