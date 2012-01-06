/*******************************************************************************
* FILENAME:
* NUMBER OF MODULES:
* GENERAL INFORMATION:
*   MODULE 1:
* DESCRIPTION:
*
* ORIGINAL AUTHOR:
* CREATION DATE:
* ORGANIZATION:
* MACHINE:
* MODIFICATION HISTORY:
*   MODULE #        DATE         PROGRAMMER        DESCRIPTION/REASON
*
********************************************************************************
*/
#include <Xm/Xm.h>

#include "gageqc_defs.h"
#include "gageqc_gui.h"
#include "gageqc_types.h"
#include "map_library.h"

/*******************************************************************************
* MODULE NUMBER:
* MODULE NAME:
* PURPOSE:
*
* ARGUMENTS:
*   TYPE   DATA TYPE   NAME                 DESCRIPTION/UNITS
*
* RETURNS:
*   DATA TYPE   NAME                        DESCRIPTION
*
* APIs UTILIZED:
*   NAME                                    HEADER FILE DESCRIPTION
*
* LOCAL DATA ELEMENTS (OPTIONAL):
*   DATA TYPE  NAME                         DESCRIPTION
*
* DATA FILES AND/OR DATABASE:
*
* ERROR HANDLING:
*    ERROR CODE                             DESCRIPTION
*
********************************************************************************
*/

extern int isom;
extern int pcp_flag;
extern int pcp_in_use[];
extern int points_flag;
extern int grids_flag;
extern int map_flag;
extern int pcpn_day;
extern int pcpn_time;
extern Widget diswidget [];
extern Widget rowcol1;
extern Widget rpbutton;
extern struct zdata zdata[];
extern time_t btim;

void change_z_time ( Widget w,
                     XtPointer data,
                     XtPointer call_data)
{
   int num_qc_days;
   int time_pos,i;
   Arg args[10];
   time_t tget;
   struct tm *gm = NULL;

   /* Retrieve the number of days to QC data for. */
   num_qc_days = get_num_days_to_qc ( );

   /* 24 hour or 6 hour time step */

   if((int)data==0)
   {
      pcp_flag--;
   }
   else if((int)data==1)
   {
      pcp_flag++;
   }

   if(pcp_flag < 0)
   {
      pcp_flag=0;
   }

   if(pcp_flag >= num_qc_days *4)
   {
      pcp_flag=num_qc_days *4-1;
   }

   pcpn_day=pcp_flag/4;
   pcpn_time=3-(pcp_flag-pcpn_day*4);
   time_pos=100+pcp_flag;

   for(i=1;i<7;i++)
   {
      XtSetSensitive(diswidget[i],True);
   }

   if(pcp_in_use[time_pos]==-1)
   {
      for(i=1;i<7;i++)
      {
         XtSetSensitive(diswidget[i],False);
      }
   }

   if(points_flag==1 && pcp_in_use[time_pos]==-1)
   {
      i=0;
   }
   else if(points_flag==1 && grids_flag==-1 && map_flag==-1)
   {
      i=0;
   }
   else if(points_flag==-1 && grids_flag==1 && map_flag==-1)
   {
      i=1;
   }
   else if(points_flag==-1 && grids_flag==-1 && map_flag==1)
   {
      i=2;
   }
   else if(points_flag==1 && grids_flag==1 && map_flag==-1)
   {
      i=3;
   }
   else if(points_flag==1 && grids_flag==-1 && map_flag==1)
   {
      i=4;
   }
   else if(points_flag==-1 && grids_flag==-1 && map_flag==-1)
   {
      i=5;
   }

   XtSetArg(args[0],XmNmenuHistory,diswidget[i]);
   XtSetValues(rowcol1,args,1);

   if(pcp_in_use[time_pos]==-1 &&
      zdata[pcpn_day].used[pcpn_time] != 0)
   {
      XtSetSensitive(rpbutton,True);
   }
   else
   {
      XtSetSensitive(rpbutton,False);
   }

   tget=btim-pcpn_day*86400;

   gm=gmtime(&tget);

   isom=gm->tm_mon;

   set_freezing_arrow_sensitivity ( );

   send_expose ( );
}

void set_freezing_arrow_sensitivity ( )
{
   int num_qc_days;
   extern int pcp_flag;
   extern int pcpn_time_step;
   Widget up_arrow;
   Widget down_arrow;

   /* Retrieve the number of days to QC data for. */
   num_qc_days = get_num_days_to_qc ( );

   /* Retrieve the widgets corresponding to the up and down
      time step arrows on the edit precip gages gui. */
   get_freezing_time_step_arrows ( & up_arrow, & down_arrow );

   /* 6 or 24 hour mode? */
   if ( pcpn_time_step == 0 )
   {
      if (pcp_flag + 1 >= num_qc_days * 4 )
      {
         /* Grey out the down arrow. */
         XtSetSensitive ( down_arrow, False );
      }
      else
      {
         /* Make sure that the down arrow is available. */
        XtSetSensitive ( down_arrow, True );
      }

      if (pcp_flag - 1  < 0)
      {
         /* Grey out the up arrow. */
         XtSetSensitive ( up_arrow, False );
      }
      else
      {
        /* Make sure the up arrow is available. */
        XtSetSensitive ( up_arrow, True );
      }
   }
   else
   {
      if (pcp_flag + 4 >= num_qc_days * 4)
      {
         /* Grey out the down arrow. */
         XtSetSensitive ( down_arrow, False );
      }
      else
      {
         /* Make sure that the down arrow is available. */
        XtSetSensitive ( down_arrow, True );
      }

      if ( pcp_flag - 4 < 0 )
      {
          /* Grey out the up arrow. */
          XtSetSensitive ( up_arrow, False );
      }
      else
      {
          XtSetSensitive ( up_arrow, True );
      }
   }

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob82/ohd/pproc_lib/src/GageQCGui/RCS/change_freezing_time.c,v $";
 static char rcs_id2[] = "$Id: change_freezing_time.c,v 1.2 2007/05/23 21:50:35 whfs Exp $";}
/*  ===================================================  */

}
