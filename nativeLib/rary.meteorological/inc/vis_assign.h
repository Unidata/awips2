
#ifndef _vis_assign_h
#define _vis_assign_h

#ifdef __cplusplus
extern "C" {
#endif

void use_goodness_values(int ug);

void vis_assign(float lats[], float lons[], int goodness[],
                float dist[], int ns);

#ifdef __cplusplus
};
#endif

#endif
