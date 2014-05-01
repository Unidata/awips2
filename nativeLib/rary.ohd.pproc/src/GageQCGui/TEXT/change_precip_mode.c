#include <Xm/Xm.h>

#include "gageqc_gui.h"

int dcmode = 2;
int tcmode = 2;
int rsmode = 1;

void change_rsmode ( Widget w,
                     XtPointer data,
                     XtPointer call_data )
{

   extern int change_pcpn_flag;
   extern int change_rpcpn_flag;

   rsmode = ( int ) data;

   change_pcpn_flag=-1;
   change_rpcpn_flag=-1;

   if(rsmode==0)
      change_rpcpn_flag=1;

   else
      change_pcpn_flag=1;

   get_legend();

   send_expose();
   return;
}

void change_tcmode ( Widget w,
                     XtPointer data,
                     XtPointer call_data)
{
   tcmode=(int)data;
   send_expose();
}

void change_dcmode ( Widget w,
                     XtPointer data,
                     XtPointer call_data)
{
   dcmode=(int)data;

   send_expose();
}

void change_pcpn_edit_mode ( Widget w,
		             XtPointer data,
		             XtPointer call_data )
{
   extern Widget pcpn_widget;
   extern int edit_stations_flag;
   extern int qpf_on;

   XtDestroyWidget(pcpn_widget);

   pcpn_widget=NULL;
   edit_stations_flag=-1;
   qpf_on=-1;

   send_expose();
}

