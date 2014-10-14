#include <Rcpp.h>
using namespace Rcpp;

#include "pref-classes.h"

// --------------------------------------------------------------------------------------------------------------------------------

// Preferences classes methods and de-serzialization of prefs

bool scorepref::cmp(int i, int j) {
  return(data[i] < data[j]);
}

bool scorepref::eq(int i, int j) {
	return(data[i] == data[j]);
}

bool reversepref::cmp(int i, int j) {
	return(p->cmp(j, i));
}

bool reversepref::eq(int i, int j) {
	return(p->eq(i, j));
}

bool complexpref::eq(int i, int j) {
	return(p1->eq(i, j) && p2->eq(i, j));
}


bool prior::cmp(int i, int j) {
	return(p1->cmp(i, j) || (p1->eq(i, j) && p2->cmp(i, j)));
}


bool pareto::cmp(int i, int j) {
	return((p1->cmp(i, j) && (p2->cmp(i, j) || p2->eq(i, j))) ||
		(p2->cmp(i, j) && (p1->cmp(i, j) || p1->eq(i, j))));
}

bool intersectionpref::cmp(int i, int j) {
	return(p1->cmp(i, j) && p2->cmp(i, j));
}


bool unionpref::cmp(int i, int j) {
	return(p1->cmp(i, j) || p2->cmp(i, j));
}


// --------------------------------------------------------------------------------------------------------------------------------


pref* CreatePreference(List pref_lst, DataFrame& scores, int current_id) {
    
  char pref_kind = as<char>(pref_lst["kind"]);
  
  
  if (pref_kind == '*' || pref_kind == '&' || pref_kind == '|' || pref_kind == '+') {
    
    // Binary complex preference
    
    complexpref* res = NULL;
    
    switch(pref_kind) {
      case '*': res = new pareto(); break;
      case '&': res = new prior(); break;
      case '|': res = new intersectionpref(); break;
      case '+': res = new unionpref(); break;
    }
    
    res->p1 = CreatePreference(as<List>(pref_lst["p1"]), scores, current_id);
    res->p2 = CreatePreference(as<List>(pref_lst["p2"]), scores, res->p1->highest_id + 1);
    res->highest_id = res->p2->highest_id;
    return(res);
    
    
  } else if (pref_kind == '-') {
    
    reversepref* res = new reversepref();
    res->p = CreatePreference(as<List>(pref_lst["p"]), scores, current_id);
    return(res);    
    
  } else if (pref_kind == 's') {
    
    // Score (base) preference
    
    scorepref* res = new scorepref();
    res->data = scores[current_id]; 
    res->highest_id = current_id;
    return(res);
    
  } 
  
  stop("Error during de-serialization of preference: Unexpected preference!");
  return (new pareto());
  
}
