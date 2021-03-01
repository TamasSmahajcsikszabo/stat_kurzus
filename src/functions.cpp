# include <Rcpp.h>
# include <math.h>
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

// [[Rcpp::export]]
double ASED(NumericVector p1, NumericVector p2) {
    int n = p1.size();
    double estimate=0;

    for (int i=0; i<n; i++) {
        double dist = p2[i] - p1[i];
        dist = dist * dist;
        estimate = estimate + dist;
    }
    return estimate / n;
}


// [[Rcpp::export]]
NumericMatrix ASED_matrix(NumericMatrix input) {
    int nrow = input.nrow();
    /* int ncol = input.ncol(); */
    /* CharacterVector names = rownames(input); */
    NumericMatrix output;

    for (int i=0; i<nrow; i++){
        for (int j=0; j<nrow; j++) {
            if (i == j) {
                output(i,j) = 0;
            } else {
                NumericVector p1 = input(i,_);
                NumericVector p2 = input(j,_);
                output(i,j) = ASED(p1, p2);
            }
        }
    }
    /* colnames(output)=names; */
    /* rownames(output)=names; */
    return output;
}
