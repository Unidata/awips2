#include "gui.h"
#include "sharp95.h"

#define         MAXLINE 50

void checkbox_Callback ( Widget, long, XmToggleButtonCallbackStruct* );
void printdialog_cancel_cb ( Widget, XtPointer, XtPointer );
void print_sounding_text ( char *file );

extern Widget toplevel;
Widget print_dialog=NULL;

static int COLRMODE=0;
/*  static int PRINTTEXT=0;  This variable NOT USED !!! ??? */
Widget          prter_nameW;
Widget          psfl_nameW;
Widget          txtfl_nameW;
char    PSFILE[256];

#if 0/*chin */
int _prtSetXWPdev ( char *wname )
{
/*printf("override print xwp function\n");*/
return(0);
}

/*=====================================================================*/

int _prtSetPSDev ( char *wname )
{
PSFILE[0] = '\0';
strcpy (PSFILE, wname);
if ( PSFILE[0] == '\0' ) strcpy(PSFILE,"nsharp.ps");
/*printf("override print ps function %s\n",PSFILE);*/
return(0);
}
#endif
/*=====================================================================*/

void print_selection ( Widget w )
{
static char wname[]="print_panel";
/*---------------------------------------------------------------------*/
if( ! print_dialog)
   print_dialog = (Widget)NxmPrt_create(wname,toplevel, printdialog_ok_cb);

NxmPrt_prtWPopup();
}

/*=====================================================================*/
/* ARGSUSED */
void printdialog_cancel_cb ( Widget w, XtPointer clnt, XtPointer call )
{
    XtUnmanageChild (print_dialog);
}

/*=====================================================================*/
/* ARGSUSED */
void checkbox_Callback ( Widget w, long which,
			 XmToggleButtonCallbackStruct *call )
{

        switch(which)
           {
           case 0:
                if ( call->set == 1 )
                        COLRMODE = 0;
                if ( call->set == 0)
                        COLRMODE = 1;
                break;
           case 1:
/*                if ( call->set == 1 )
                   PRINTTEXT = 0;
                else
                   PRINTTEXT = 1;  NOT used */
                break;
           default:
                printf("Unknown selection %d\n", (int)which);
           }
}

/*=====================================================================*/

void printdialog_ok_cb ( void )
{
char            command[256];
/*---------------------------------------------------------------------*/
/*
if(_colorMode == 2)
   COLRMODE = 1;
else
   COLRMODE = 0;
*/
COLRMODE = 1;

print_sounding_ps (COLRMODE);
sprintf(command,"cp %s %s",config.filename,PSFILE);
system(command);

unlink(config.filename);

}

/*=====================================================================*/

void print_sounding_text ( char *file )
{
int i;
FILE *f;
/*---------------------------------------------------------------------*/
if ( (sndgp == NULL ) || ( sndgp->numlev == 0) ) return;

if((f = fopen(file,"w")) != NULL)
   {
   fprintf(f,"   LEVEL     HGHT     TEMP     DWPT     WDIR     WSPD        OMEG\n");

   for(i=0;i<sndgp->numlev;i++)
      {
      fprintf(f,"%8.2f %8.2f %8.2f %8.2f %8.2f %8.2f %11.6f\n",
         sndgp->sndg[i].pres,sndgp->sndg[i].hght,sndgp->sndg[i].temp,
	 sndgp->sndg[i].dwpt,sndgp->sndg[i].drct,sndgp->sndg[i].sped,
	 sndgp->sndg[i].omega);
      }
   fclose(f);
   }
}
