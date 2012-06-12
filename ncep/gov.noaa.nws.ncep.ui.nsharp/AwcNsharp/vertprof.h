#include "winline.h"
#ifndef VERT_PROF
void InitSaveVertProfBg(Widget pv_vertprof);
float ARCCOSSINE (float x);
void GetWindProfAtCurs(int istart,int jstart, int nsta, 
	  float staxywrk[][200], unsigned levels[], float actva[][39], int wnddirs[],
	  int wndspds[], int wndhts[], int *wndcnt, int slashcount,
	  int MinSearchRadius, float clng, float clat);
void InitProfWindow(Widget pv_vertprof);
void DrawPVVertWinds(int wndcnt,int wnddirs[], int wndspds[], int wndhts[],
  Widget pv_vertprof);
void DrawPVHodograph(int wndcnt, int wnddirs[], int wndspds[], int wndhts[],
  Widget pv_hodo);
void calcsfc_6kmshear(int dirupr,int spdupr,int dirlwr,int spdlwr, float *shear);
void setmaxwindinhodo(float pvsndg[][3][200], unsigned levels[], int pvnstns, int nlvls);
#define HODO_WDTH 175
#define HODO_HGHT 250
#define VP_WDTH 175
/* #define VP_HGHT 575 */
#define VP_HGHT 525
/* #define MAXWND 50.0 */
#define VERT_PROF
/* #define PI 3.14159 */
#endif
