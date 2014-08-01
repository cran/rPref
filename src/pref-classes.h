

class pref {
public:

  int highest_id;
  
  virtual bool cmp(int, int) = 0;
  virtual bool eq(int, int) = 0;
};

// Derserialize preference 
pref* CreatePreference(List, DataFrame&, int);
