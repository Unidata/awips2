/*
	File:		res_cbs.h
	Date:		8/30/1994
	Author:		Dale Shelton, Paul Taylor (1/13/1998)
	
	Purpose:	Provide support for the Reservoir DS.
*/


#ifndef res_cbs_h
#define res_cbs_h

typedef struct ResCoordWindowStruct
{
   float	ur_lat;
   float	ur_lon;
   
   float	ul_lat;
   float	ul_lon;
      
   float	ll_lat;
   float	ll_lon;
   
   float	lr_lat;
   float	lr_lon;
   
} ResCoordWindow;



void	ShowResDs(Widget w, char *lid);	
int	res_load(const char *lid);
void	res_callbacks(void);
void	res_add_btns(void);
void	res_del_conf();
void	res_delete();
void	res_close();
void	res_save();

void	resassoc_callbacks(void);
void	resassoc_show(Widget w, XtPointer ptr, XtPointer cbs);

void	resassoc_assoc();
void	resassoc_clear();
void	resassoc_close();


void		resassoc_loadXmList(Widget w, XtPointer ptr, XtPointer cbs);


ResCoordWindow*	resassoc_computeResCoordWindow(float lat, float lon,
					       float square_radius);

void	resassoc_toggleSwitch(Widget w, XtPointer ptr, XtPointer cbs);

void	resassoc_loadDefaults(void);


#endif
