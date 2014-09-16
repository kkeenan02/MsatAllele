#include <Rcpp.h>
using namespace Rcpp;

// getAllele C++
// This function replicates the get.allele function from the MsatAllele package

// [[Rcpp::export]]
int getAll(NumericVector fragRef , double frag){
  int n = fragRef.size();
  LogicalVector all = fragRef.size();
  for(int i = 0; i < n; i++){
    if(fragRef[i] >= frag + 0.8 || fragRef[i] <= frag - 0.8){
      all[i] = TRUE;
    } else {
      all[i] = FALSE;
    }
  }
  NumericVector nw = fragRef.size();
  int nwn = nw.size();
  int i = 0;
  for(int j = 0; j < nwn; j++){
    double dif = nw[j+1] - nw[j];
    if(dif >= 0.4){
      break;
    }
    i = i + 1;
  }
  return i;
}
