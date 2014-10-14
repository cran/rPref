
class pref {
public:
  int highest_id;

	virtual bool cmp(int, int) = 0;
	virtual bool eq(int, int) = 0;
}; 

class scorepref : public pref {
public:
  NumericVector data;

	bool cmp(int i, int j);
	bool eq(int i, int j);
};

class complexpref : public pref {
public:
  pref *p1;
	pref *p2;

	bool eq(int i, int j);
};

// Common Superclass for Pareto and intersection 
class productpref : public complexpref {};

class pareto : public productpref {
public:
	bool cmp(int i, int j);
};

class unionpref : public complexpref {
public:
	bool cmp(int i, int j);
};

class prior : public complexpref {
public:
	bool cmp(int i, int j);
};

class intersectionpref : public productpref {
public:
	bool cmp(int i, int j);
};

class reversepref : public pref {
public:
	pref *p;

	bool cmp(int i, int j);
	bool eq(int i, int j);
};

// Derserialize preference 
pref* CreatePreference(List, DataFrame&, int);
