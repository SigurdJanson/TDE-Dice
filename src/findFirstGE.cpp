#include <Rcpp.h>
using namespace Rcpp;

//' findFirstGE
//'
//' Find the first index `i` in `x` for which `x[i] >= target` holds true.
//' @param x A numeric vector with monotonously increasing values.
//' @param target A numeric vector.
//' @details This is a helper function for quantile functions.
//' It also works outside the value range [0, 1], but has been tested
//' most extensively within this range.
//'
//' The comparison uses a precision of 1E-8.
//' @returns For each target it returns the first `x[i]` (from left to right)
//' for which `x[i] >= target` holds true.
//' If `target` is greater than any `x`, it returns `NA`.
// [[Rcpp::export]]
IntegerVector findFirstGE(NumericVector x, NumericVector target,
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
  //Rcout << \"starti : \" << starti << \" -|- endi : \" << endi << \"\\n\" << \"stepi : \" << stepi;

  for (int i = starti; forward ? i <= endi : i >= endi; i += stepi) {
    // skip when x has elements left but all targets are finished
    if (totalFound == tn) return result;

    for (int t = 0; t < tn; t++) {
      //if (!forward)
      //  Rcout << \"x[i] : \" << x[i] << \" -|- target[t] : \" << target[t] << \"\\n\";

      if (IntegerVector::is_na(result[t]) && ge(x[i], target[t])) { // x[i] >= target[t]) {
        if (!NumericVector::is_na(target[t])) // skip NA in target
          result[t] = i + 1; // R is 1-indexed
        totalFound++;
      }
    }
  }

  //    if (!forward)
  //      Rcout << \"The value of totalFound : \" << totalFound << \"\\n\";
  return result;
}
