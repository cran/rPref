#include <Rcpp.h>
using namespace Rcpp;

#include "pref-classes.h"
#include "bnl.h"

// --------------------------------------------------------------------------------------------------------------------------------


// External wrapper for (top k) preference selection
// v is just a index vector and f operates on indices
// [[Rcpp::export]]
NumericVector pref_select_impl(DataFrame scores, List serial_pref, int topk) {
  
  NumericVector col1 = scores[0];  
  int ntuples = col1.size();
  
  // De-Serialize preference
  pref* p = CreatePreference(serial_pref, scores, 0);
  
  // Create index vector
  std::vector<int> v(ntuples);
  for (int i=0; i<ntuples; i++) v[i] = i;
  
  // Execute algorithm
  std::list<int> res;
  if (topk == -1) { 
    res = bnl_internal(v, p);
  } else {
    if (topk >= ntuples) return(wrap(v)); // Entire dataset
    res = bnl_topk_internal(v, p, topk);
  }
  
  // Return result
  return(NumericVector(res.begin(), res.end()));
}


// --------------------------------------------------------------------------------------------------------------------------------
// --------------------------------------------------------------------------------------------------------------------------------


// Standard-BNL
std::list<int> bnl_internal(std::vector<int>& v, pref* p) {
  
  std::list<int> window;
  std::list<int>::iterator j;
  std::list<int>::iterator j_del;
  
  window.push_back(v[0]);    
  
  bool dominated;
  int ntuples = v.size();
   
  for(int i=1; i<ntuples; ++i) {
    
    dominated = false;
    for (j=window.begin(); j != window.end(); ++j) {
      if (p->cmp(*j, v[i])) { // *j (window element) is better
        dominated = true;
        break; 
      } else if (p->cmp(v[i], *j)) { // v[i] (picked element) is better
        // delete j
        j_del = j;
        ++j;
        window.erase(j_del);
        --j;
      }
    }
    if (!dominated) {
      window.push_back(v[i]);
    }
  }
  
  // return Window as a numeric list
  return window;
  
}

// --------------------------------------------------------------------------------------------------------------------------------


// Internal top-k BNL
std::list<int> bnl_topk_internal(std::vector<int>& v, pref* p, int topk) {


  std::list<int> window;
  std::list<int>::iterator j;
  std::list<int>::iterator j_del;
  
  int ntuples = v.size();
  std::vector<int> new_v(ntuples);
  
  window.push_back(v[0]);   
  int wsize=1;
  
  bool dominated;
  int count = 0;
  
  // If k >= number of tuples, return the entire vector (as a list!)
  if (topk >= ntuples) return(std::list<int>(v.begin(), v.end()));
  
  for(int i=1; i<ntuples; ++i) {
    
    dominated = false;
    
    for (j=window.begin(); j != window.end(); ++j) {
      if (p->cmp(*j, v[i])) { //v[j] (window element) is better
        dominated = true;
        break; 
      } else if (p->cmp(v[i], *j)) { // v[i] (picked element) is better
        // delete j and add j to new_v
        j_del = j;
        new_v[count] = *j;
        count++;
		    ++j;
        window.erase(j_del);
		    --j;
        wsize--;
      }
    }
    if (!dominated) {
      window.push_back(v[i]);
      wsize++;
    } else {
	    new_v[count] = v[i];
	    count++;
	  }
  }

  v.clear(); // this is not needed anymore

  if (wsize >= topk) {
	  window.resize(topk);
  } else {
	  new_v.resize(count);
	  std::list<int> res = bnl_topk_internal(new_v, p, topk - wsize);
    window.splice(window.end(), res);
  }
  return(window);
}


// --------------------------------------------------------------------------------------------------------------------------------



