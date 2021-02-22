# include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector CD(NumericVector x, NumericVector y) {
    int n = x.size();
    NumericVector output(4);

    for (int i=0; i < n; ++i) {
        for (int j=i+1; j<n; ++j){
            if (((x[i] < x[j]) && (y[i] < y[j])) || ((x[i] > x[j]) && (y[i] > y[j]))) {
                output[0] += 1;
            } else if ((x[i] == x[j]) && (y[i] != y[j])) {
                output[2] += 1;
            } else if ((x[i] != x[j]) && (y[i] == y[j])) {
                output[3] += 1; 
            } else if ((x[i] == x[j]) && (y[i] == y[j])) {
            ;
            } else {
                output[1] += 1;
            }
        }
    }

   return output; 
}
