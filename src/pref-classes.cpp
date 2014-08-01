#include <Rcpp.h>
using namespace Rcpp;

#include "pref-classes.h"

// --------------------------------------------------------------------------------------------------------------------------------

// Preferences classes and de-serzialization of prefs

class scorepref : public pref {
public:

  NumericVector data;
  
  bool cmp(int i, int j) {
    return(data[i] < data[j]);
  }
  
  bool eq(int i, int j) {
    return(data[i] == data[j]);
  }
};


class reversepref : public pref {
public:

  pref *p;
  
  bool cmp(int i, int j) {
    return(p->cmp(j,i));
  }
  
  bool eq(int i, int j) {
    return(p->eq(i,j));
  }
};


class complexpref : public pref {
public:

  pref *p1;
  pref *p2;
  
  bool eq(int i, int j) {
    return(p1->eq(i,j) && p2->eq(i,j));
  }  
};


class prior : public complexpref {  
public:

  bool cmp(int i, int j) {
    return( p1->cmp(i,j) || ( p1->eq(i,j) && p2->cmp(i,j) ) );
  }
};


class pareto : public complexpref {
public:
  
  bool cmp(int i, int j) {
    return( ( p1->cmp(i,j) && (p2->cmp(i,j) || p2->eq(i,j)) ) || 
            ( p2->cmp(i,j) && (p1->cmp(i,j) || p1->eq(i,j)) )     );
  }
};

class intersectionpref : public complexpref {
public:
  
  bool cmp(int i, int j) {
    return( p1->cmp(i,j) && p2->cmp(i,j) );
  }
};

class unionpref : public complexpref {
public:
  
  bool cmp(int i, int j) {
    return( p1->cmp(i,j) || p2->cmp(i,j) );
  }
};


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
