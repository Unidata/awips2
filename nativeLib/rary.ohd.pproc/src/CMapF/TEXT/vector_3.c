#include <math.h>
#include "vector_3.h"
                    
double x_product(double * a,double * b,double * c){
int xp_nd[]={1,2,0,1};
double norm=0;
int k,l;
  for (k=0,l=1;k<3;k++,l++) {
    c[k] = a[xp_nd[k]] * b[xp_nd[l]] - a[xp_nd[l]] * b[xp_nd[k]];
    norm += c[k]*c[k];
  }
  return sqrt(norm);
}

vector_3d v_sum(vector_3d a,vector_3d b) {
vector_3d result;
int k;
   for (k=0;k<3;k++) {
      result.v[k] = a.v[k] + b.v[k];
   }
   return result;
}

double norm (vector_3d a) {
double result = 0;
int k;
   for (k=0;k<3;k++) {
      result += a.v[k] * a.v[k];
   }
   return sqrt(result);
}
