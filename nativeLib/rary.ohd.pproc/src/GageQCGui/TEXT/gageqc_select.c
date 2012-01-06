#include <Xm/Xm.h>

#include "gageqc_gui.h"

void source_select ( Widget w, 
		     XtPointer client_data,
		     XtPointer call_data )
{
   extern int dflag [ ];
   extern int tsmax;
   int i;
   Cardinal argcount;
   Arg args[10];
   extern Widget bwidget [];

   dflag[(int)client_data]=-dflag[(int)client_data];

   if((int)client_data==tsmax+1) {

                 if(dflag[tsmax+1]==1){

                            for(i=1;i<tsmax+2;i++){

                                    dflag[i]=1;
                                    argcount=1;
                                    XtSetArg(args[0],XmNset,True);
                                    XtSetValues(bwidget[i],args,argcount);

                                  }

                          }
                  else{

                            for(i=1;i<tsmax+2;i++){

                                    dflag[i]=-1;
                                    argcount=1;
                                    XtSetArg(args[0],XmNset,False);
                                    XtSetValues(bwidget[i],args,argcount);

                                  }

                          }

               }


send_expose();
}

void quality_select ( Widget w, 
		      XtPointer client_data,
		      XtPointer call_data )
{
/*
good=0
Bad=1
Manual=2
Questionable=3
Estimate=5
Time Distributed=6
All=9

*/

int i;
extern int funct [ ];
extern int qflag [ ];
Cardinal argcount;
Arg args[10];
extern Widget awidget[];

i=funct[(int)client_data];

qflag[i]=-qflag[i];

if(i==9) {

                 if(qflag[9]==1){

                            for(i=0;i<9;i++){

                                    qflag[funct[i]]=1;
                                    argcount=1;
                                    XtSetArg(args[0],XmNset,True);
                                    XtSetValues(awidget[i],args,argcount);
                                  }

                          }

                  else{

                            for(i=0;i<9;i++){

                                    qflag[funct[i]]=-1;
                                    argcount=1;
                                    XtSetArg(args[0],XmNset,False);
                                    XtSetValues(awidget[i],args,argcount);

                                  }

                          }

               }

   send_expose();
}

void quality_select_temperature ( Widget w, 
		                  XtPointer client_data,
		                  XtPointer call_data )
{
/*
good=0
Bad=1
Manual=2
Questionable=3
Estimate=5
Time Distributed=6
All=9

*/

int i;
extern int funct [ ];
extern int qflag [ ];
Cardinal argcount;
Arg args[10];
extern Widget awidget[];

i=funct[(int)client_data];

qflag[i]=-qflag[i];

if(i==9) {

                 if(qflag[9]==1){

                            for(i=0;i<9;i++){

                                    if( i==5 )
                                    {
                                       continue;
                                    }

                                    qflag[funct[i]]=1;
                                    argcount=1;
                                    XtSetArg(args[0],XmNset,True);
                                    XtSetValues(awidget[i],args,argcount);
                                  }

                          }

                  else{

                            for(i=0;i<9;i++){

                                    if( i==5 )
                                    {
                                       continue;
                                    }

                                    qflag[funct[i]]=-1;
                                    argcount=1;
                                    XtSetArg(args[0],XmNset,False);
                                    XtSetValues(awidget[i],args,argcount);

                                  }

                          }

               }

   send_expose();

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob82/ohd/pproc_lib/src/GageQCGui/RCS/gageqc_select.c,v $";
 static char rcs_id2[] = "$Id: gageqc_select.c,v 1.3 2007/10/03 18:46:37 lawrence Exp $";}
/*  ===================================================  */

}
