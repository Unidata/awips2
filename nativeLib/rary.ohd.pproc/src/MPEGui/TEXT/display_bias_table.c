/******************************************************************************
* FILENAME:            display_bias_table.c
* NUMBER OF MODULES:   9
* GENERAL INFORMATION:
*   MODULE 1:          display_bias_table
* DESCRIPTION:         This routine create the main bias window, 
*		       which has information for all radars.
*
*   MODULE 2:          radarBiasTable
* DESCRIPTION:         This routine create the bias table for 
*		       the selected radar.
*
*   MODULE 3:          quitBiasTableCallback
* DESCRIPTION:         This routine called when selecting "Close" button
*		       from multi-radar main bias window. 
*
*   MODULE 4:          quitRadarBiasTableCallback
* DESCRIPTION:         This routine called when selecting "Close" button
*		       from single-radar bias window.
*
*   MODULE 5:          applyThresholdChangeCallback
* DESCRIPTION:         This routine called when selecting "Apply" button
*		       from single-radar bias window.
*
*   MODULE 6:          applyBiasChangeCallback
* DESCRIPTION:         This routine called when selecting "Apply" button 
*		       from multi-radar main bias window. 
*   
*   MODULE 7:          validateInputCallback
* DESCRIPTION:         This routine called while changing text field with 
*		       bias value in multi-radar main bias window. 
*
*   MODULE 8:	       setModifyCallback  
* DESCRIPTION:         This routine is called when changing text field 
*		       with bias value in multi-radar main bias window.
* 
*   MODULE 9:          toggleYesNo
* DESCRIPTION:         This routine is called when pushing "Yes" or "No" button 
* 		       on multi-radar main  bias window. 
*
* ORGANIZATION:        OHD / HSEB
* MACHINE:             HP Unix / Dell Linux
* MODIFICATION HISTORY:
*   MODULE #        DATE         PROGRAMMER        DESCRIPTION/REASON
*          ALL      3/21/2001    Mark Glaudemans   Revision as per DR #7596
*          ALL      2/26/2002    Bryon Lawrence    Revision for merging
* 						   Hydromap / MPE applications.
*          ALL      12/15/2004   Bryon Lawrence    Added the include of
*                                                  read_ab_zrcoef.h for the
*                                                  read_ab_zrcoef prototype.
*******************************************************************************
*/
#include <ctype.h>
#include <Xm/AtomMgr.h>
#include <Xm/CascadeB.h>
#include <Xm/Protocols.h>

#include "DbmsDefs.h"
#include "display_bias_table.h"
#include "mpe_log_utils.h"
#include "read_ab_zrcoef.h"
#include "read_default_bias.h"
#include "restore_bias_RFCW.h"
#include "rfcwide.h"
#include "rfcwide_interface.h"
#include "stage3.h"
#include "stage3_globals.h"
#include "update_bias_RFCW.h"

/* Global variables */
static Boolean biasShellUp = False;

Widget   radarId[60];
Widget   bias[60];
Widget   modify[60];
Widget   biasTableMainForm;
Widget   biasTableInfo;
Widget   thresholdTf;

float    oldBias[60] = {0};
static   int radarBias_displayed = 0;

/*******************************************************************************
* MODULE NUMBER: 1
* MODULE NAME:   display_bias_table
* PURPOSE:       This routine Creates GUI to display the bias information      
*      		 as read from the TBD table calling function: callback          
*      		 from Display Bias Table option          
*
* ARGUMENTS:
* TYPE   DATA TYPE    NAME       DESCRIPTION/UNITS     
* Input  Widget       w          The identifier of the widget generating this
*                                callback.
* Input  XtPointer    clientdata Programmer-specified data passed into this
*                                callback routine.
* Input  XtPointer    calldata   An informational structure specific to this
*                                type of call back.
*
* RETURNS:
*   None
*
* APIs UTILIZED:
*   NAME         HEADER FILE                DESCRIPTION
*
* LOCAL DATA ELEMENTS (OPTIONAL):
*   DATA TYPE  NAME                         DESCRIPTION
*
* DATA FILES AND/OR DATABASE:
*    Not Applicable
*
* ERROR HANDLING:
*    None
*
********************************************************************************
*/

#define NUMBER_OF_COLUMN_HEADINGS 5

void display_bias_table ( Widget w , XtPointer clientdata , 
                          XtPointer calldata )
{   
   Atom         wmAtom;
   Widget   biasTable;
   Widget   biasTableForm1;
   Widget   biasFrame,biasLabels;
   Widget   biasSw;
   Widget   aBias[40], bBias[40]; 
   Widget   otherBias[40], otherOffice[40];
   Widget   biasFrameA[40], biasFrameB[40];
   Widget   otherbiasFrame[40];
   Widget   otherofficeFrame[40];
   Widget   closePb,applyPb, timeTf;
//   Widget helpPb;

   static  char *biasLabelStrings[ NUMBER_OF_COLUMN_HEADINGS ]=
      {" Radar ", " Bias : Manually Specified ",
      " A ", " B ", " Bias : Other Office "};
   
   static  int  labelPositionArray[NUMBER_OF_COLUMN_HEADINGS]= 
                                  {5,90,315,385,455};
   
   int bias_found;
   int i;
   float other_bias_value;
   char rid[RADAR_ID_LEN + 1];
   char biasStr[8];
   char aBiasStr[4];
   char bBiasStr[4];
   char yesNoStr[2];
   char datetimeTf[40];
   char office_id[RFC_LEN + 1];
   char otherbiasStr[8];
   XmString  str;

   /* Check if there is an instance of the bias table already
      displayed. */
   if ( biasShellUp == True )
   {
      return;
   }
   
   yesNoStr[1] = '\0';
   rid[RADAR_ID_LEN] = '\0';
   aBiasStr[4] = '\0';
   bBiasStr[4] = '\0';
   memset(office_id, '\0', RFC_LEN + 1 );
   memset(datetimeTf, '\0', 40);
   memset(biasStr, '\0', 8);
   memset(otherbiasStr, '\0', 8);
   
   biasTable  = XtVaCreatePopupShell("Edit Bias Table",
				   transientShellWidgetClass,
				   toplevel, 
				   XmNheight, 600,
                                   XmNwidth, 650,
				   NULL);
   bias_shell = biasTable;				      

   /* Add protocol to the close button on the window frame. */ 
   wmAtom = XmInternAtom ( ( XtDisplay ( biasTable ) ), "WM_DELETE_WINDOW", 
                           False );
   XmAddWMProtocolCallback ( biasTable, wmAtom, quitBiasTableCallback,
                             ( Widget ) biasTable );
   
   biasTableMainForm = XtVaCreateManagedWidget("biasTableForm",
					       xmFormWidgetClass,
					       biasTable,
					       NULL);
   
   /* Create the "CLOSE" button. */
   closePb = XtVaCreateManagedWidget(" CLOSE  ",
				     xmPushButtonWidgetClass,
				     biasTableMainForm,
				     XmNwidth, 80,
				     XmNtopAttachment, XmATTACH_FORM,
				     XmNtopOffset, 5,
				     XmNleftAttachment, XmATTACH_FORM,
				     XmNleftOffset, 5,     
				     NULL); 
   XtAddCallback(closePb ,XmNactivateCallback,
		 quitBiasTableCallback, (Widget)bias_shell);	      	      
   
   /* Create the "APPLY" button. */
   applyPb = XtVaCreateManagedWidget(" APPLY ",
				     xmPushButtonWidgetClass,
				     biasTableMainForm,
				     XmNwidth, 80,
				     XmNtopAttachment, XmATTACH_FORM,
				     XmNtopOffset, 5,
				     XmNleftAttachment, XmATTACH_FORM,
				     XmNleftOffset, 90,
				     NULL);
   XtAddCallback(applyPb, XmNactivateCallback,
		 applyBiasChangeCallback, NULL);
   
/*   helpPb = XtVaCreateManagedWidget(" HELP ",
				    xmPushButtonWidgetClass,
				    biasTableMainForm,
				    XmNwidth, 80,
				    XmNtopAttachment, XmATTACH_FORM,
				    XmNtopOffset, 5,
				    XmNleftAttachment, XmATTACH_FORM,
				    XmNleftOffset, 175,
				    NULL);*/
//   XtAddCallback(helpPb,XmNactivateCallback,
//		 popup_help_window,"EDITBIASTABLE");	
   
   /* Add the time text field.  This is not editable. */
   timeTf = XtVaCreateManagedWidget("",
				    xmTextWidgetClass,
				    biasTableMainForm,
				    XmNwidth, 150,
				    XmNtopAttachment, XmATTACH_FORM,
				    XmNtopOffset, 5,
				    XmNrightAttachment, XmATTACH_FORM,
				    XmNrightOffset, 5,
				    XmNeditable, FALSE,
				    XmNcursorPositionVisible, FALSE,
				    XmNshadowThickness,0,
				    XmNtraversalOn, FALSE,	
				    NULL);
   
   biasTableDate = timeTf;
   sprintf(datetimeTf,"%s", date_st3.lldate);
   XmTextSetString(timeTf, datetimeTf);
   
   /* Create the bias column headings. */ 
   for(i = 0; i < NUMBER_OF_COLUMN_HEADINGS; i++)
   {
      biasFrame = XtVaCreateManagedWidget("",
					  xmFrameWidgetClass,
					  biasTableMainForm,
					  XmNshadowType, XmSHADOW_ETCHED_OUT,
					  XmNshadowThickness, 5,
					  XmNtopAttachment, XmATTACH_FORM,
					  XmNtopOffset, 45,
					  XmNx,labelPositionArray[i],
					  NULL);
      
      biasLabels = XtVaCreateManagedWidget(biasLabelStrings[i],
					   xmLabelWidgetClass,
					   biasFrame,
					   NULL);		       
   }
   
   
   biasSw = XtVaCreateManagedWidget("biasScrolledWindow",
				    xmScrolledWindowWidgetClass,
				    biasTableMainForm,
				    XmNscrollingPolicy, XmAUTOMATIC,
				    XmNtopAttachment, XmATTACH_FORM,
				    XmNtopOffset, 100,
				    XmNbottomAttachment,XmATTACH_FORM,
				    XmNwidth, 650,
				    NULL);
   
   
   biasTableForm1 = XtVaCreateManagedWidget("biasTableForm",
					    xmFormWidgetClass,
					    biasSw,		      
					    NULL);
   
   /*---------------------------------------------------------*/
   /*  read data from database tables for Bias Table Display  */
   /*---------------------------------------------------------*/
   
   for (i = 0; i < NRADARS; i++)
   {  		   	             
      abzerocoef.mlt_zrcoef = 0.0;
      abzerocoef.pwr_zrcoef = 0.0;		
      
      memset ( rid, '\0', RADAR_ID_LEN + 1);
      strncpy ( rid, nexrad[i].id, RADAR_ID_LEN);
      read_radar_results_table(rid, datetime);
      
      sprintf(yesNoStr,"%s",radarresultdata.edit_bias_values);	   
      read_bias_table_param(rid);
      
      if (iflarad[i] == 0 || iflarad[i] == 2) read_ab_zrcoef(rid, i);
      
      str = XmStringCreateLocalized ( rid );

      radarId[i] = XtVaCreateManagedWidget(rid,
					   xmPushButtonWidgetClass,
					   biasTableForm1,
					   XmNleftAttachment, XmATTACH_FORM,
					   XmNleftOffset, 5, 
					   XmNheight,35,
					   XmNwidth,70,
					   XmNy,(37*i),
                                           XmNlabelType,XmSTRING,
                                           XmNlabelString,str,
					   NULL);

      XtAddCallback(radarId[i], XmNactivateCallback,
		    radarBiasTable, NULL);	
      
      sprintf(biasStr,"%-1.2f",radarresultdata.rw_bias_val_used);
      oldBias[i] = radarresultdata.rw_bias_val_used;
      
      bias[i] = XtVaCreateManagedWidget("",
					xmTextWidgetClass,
					biasTableForm1,
					XmNheight,35,
					XmNwidth, 50,
					XmNy,(37*i),
					XmNleftAttachment, XmATTACH_WIDGET,
					XmNleftWidget,radarId[i],
					XmNleftOffset,35,
					NULL);  	
      XmStringFree ( str );

      XmTextSetString(bias[i], biasStr);
      
      XtAddCallback(bias[i], XmNmodifyVerifyCallback,
		    validateInputCallback, NULL);
      XtAddCallback(bias[i], XmNvalueChangedCallback,
		    setModifyCallback, (int *)i);
      
      if (!strcmp(yesNoStr, "y"))
	 str = XmStringCreateLocalized("   YES   ");
      else
	 str = XmStringCreateLocalized("   NO    ");  
      
      modify[i] = XtVaCreateManagedWidget("",
					  xmPushButtonWidgetClass,
					  biasTableForm1,
					  XmNlabelString, str,
					  XmNheight,35,
					  XmNwidth, 70,
					  XmNrecomputeSize, FALSE,
					  XmNy,(37*i),
					  XmNleftAttachment, XmATTACH_WIDGET,
					  XmNleftWidget,bias[i],
					  XmNleftOffset,50,
					  NULL);	
      XtAddCallback(modify[i], XmNactivateCallback,
		    toggleYesNo, (int *)i);  	    
      XmStringFree ( str ) ;
      
      
      biasFrameA[i] = XtVaCreateManagedWidget("",
					      xmFrameWidgetClass,
					      biasTableForm1,
					      XmNshadowType, XmSHADOW_ETCHED_OUT,
					      XmNshadowThickness, 5,
					      XmNleftAttachment, XmATTACH_WIDGET,
					      XmNleftWidget,modify[i],
					      XmNleftOffset,25,
					      XmNheight,35,
					      XmNwidth, 50,
					      XmNy,(37*i),			
					      NULL);
      
      aBias[i] = XtVaCreateManagedWidget("",
					 xmLabelWidgetClass,
					 biasFrameA[i],
					 XmNcursorPositionVisible, FALSE,
					 NULL);
      
      if (abzerocoef.mlt_zrcoef == 0.0)
      {
	 XmString labelString;
	 labelString = XmStringCreateLocalized("N/A");
	 XtVaSetValues(aBias[i],
		       XmNlabelString, labelString,
		       NULL);
	 XmStringFree(labelString);
      }
      
      else
      {	
	 XmString labelString;
	 sprintf(aBiasStr, "%-3.0f", abzerocoef.mlt_zrcoef);
	 labelString = XmStringCreateLocalized(aBiasStr);
	 XtVaSetValues(aBias[i],
		       XmNlabelString, labelString,
		       NULL);
	 XmStringFree(labelString);
      }	
      
      
      biasFrameB[i] = XtVaCreateManagedWidget("",
					      xmFrameWidgetClass,
					      biasTableForm1,
					      XmNshadowType, XmSHADOW_ETCHED_OUT,
					      XmNshadowThickness, 5,
					      XmNheight,35,
					      XmNwidth, 50,
					      XmNy,(37*i),
					      XmNleftAttachment, XmATTACH_WIDGET,
					      XmNleftWidget,aBias[i],
					      XmNleftOffset,20,		
					      NULL);	
      
      bBias[i] = XtVaCreateManagedWidget("",
					 xmLabelWidgetClass,
					 biasFrameB[i],		
					 NULL);
      
      if (abzerocoef.pwr_zrcoef == 0.0)
      {
	 XmString labelString;
	 labelString = XmStringCreateLocalized("N/A");
	 XtVaSetValues(bBias[i],
		       XmNlabelString, labelString,
		       NULL);
	 XmStringFree(labelString);
      }
      
      else
      {	
	 XmString labelString;
	 sprintf(bBiasStr, "%-1.2f", abzerocoef.pwr_zrcoef);
	 labelString = XmStringCreateLocalized(bBiasStr);
	 XtVaSetValues(bBias[i],
		       XmNlabelString, labelString,
		       NULL);
	 XmStringFree(labelString);	
      }

      bias_found = get_rfc_bias_value ( rid, office_id, &other_bias_value ); 

      /* New Additions on May 8, 2007.  These fields show the bias from the
         parent RFC if it is available. */
      otherbiasFrame[i] = XtVaCreateManagedWidget("",
					      xmFrameWidgetClass,
					      biasTableForm1,
					      XmNshadowType, XmSHADOW_ETCHED_OUT,
					      XmNshadowThickness, 5,
					      XmNheight,35,
					      XmNwidth, 50,
					      XmNy,(37*i),
					      XmNleftAttachment, XmATTACH_WIDGET,
					      XmNleftWidget,bBias[i],
					      XmNleftOffset,20,		
					      NULL);	
      otherBias[i] = XtVaCreateManagedWidget("",
					 xmLabelWidgetClass,
					 otherbiasFrame[i],		
					 NULL);

      if ( bias_found == 0)
      {
	 XmString labelString;
	 labelString = XmStringCreateLocalized("N/A");
	 XtVaSetValues(otherBias[i],
		       XmNlabelString, labelString,
		       NULL);
	 XmStringFree(labelString);
      }
      else
      {	
	 XmString labelString;
	 sprintf(otherbiasStr, "%-1.2f", other_bias_value);
	 labelString = XmStringCreateLocalized(otherbiasStr);
	 XtVaSetValues(otherBias[i],
		       XmNlabelString, labelString,
		       NULL);
	 XmStringFree(labelString);	
      }

      otherofficeFrame[i] = XtVaCreateManagedWidget("",
					      xmFrameWidgetClass,
					      biasTableForm1,
					      XmNshadowType, XmSHADOW_ETCHED_OUT,
					      XmNshadowThickness, 5,
					      XmNheight,35,
					      XmNwidth, 50,
					      XmNy,(37*i),
					      XmNleftAttachment, XmATTACH_WIDGET,
					      XmNleftWidget,otherBias[i],
					      XmNleftOffset,20,		
					      NULL);	

      otherOffice[i] = XtVaCreateManagedWidget("",
					 xmLabelWidgetClass,
					 otherofficeFrame[i],		
					 NULL);

      if ( bias_found == 0)
      {
	 XmString labelString;
	 labelString = XmStringCreateLocalized("N/A");
	 XtVaSetValues(otherOffice[i],
		       XmNlabelString, labelString,
		       NULL);
	 XmStringFree(labelString);
      }
      else
      {	
	 XmString labelString;
	 labelString = XmStringCreateLocalized(office_id);
	 XtVaSetValues(otherOffice[i],
		       XmNlabelString, labelString,
		       NULL);
	 XmStringFree(labelString);	
      }
      
   }
   
   
   if (NRADARS > 18)
   {
      XtVaSetValues(biasTable, 
		    XmNheight,900,
		    XmNmaxHeight,900,
		    XmNminHeight,900,
		    XmNwidth,650,
		    XmNmaxWidth,650,
		    XmNminWidth,650,
		    NULL);      
      biasShellUp = True;
   }
   
   else
   {
      if ( NRADARS <= 3 )
      {
         XtVaSetValues(biasTable,
		       XmNheight, 250,
		       XmNmaxHeight, 250,
		       XmNminHeight, 250,
		       XmNwidth,650,
		       XmNmaxWidth,650,
		       XmNminWidth,650,
		       NULL);
      }
      else
      {
         XtVaSetValues(biasTable,
		       XmNheight,(NRADARS*65) - 10,
		       XmNmaxHeight,(NRADARS*65) - 10,
		       XmNminHeight,(NRADARS*65) - 10,
		       XmNwidth,650,
		       XmNmaxWidth,650,
		       XmNminWidth,650,
		       NULL);
      }

      biasShellUp = True;           
   }

   XtPopup(biasTable, XtGrabNone);
   
   return;   
}

/*******************************************************************************
* MODULE NUMBER: 2
* MODULE NAME:   radarBiasTable
* PURPOSE:       This routine create the bias table for 
*		 the selected radar.      
*
* ARGUMENTS:
* TYPE   DATA TYPE    NAME       DESCRIPTION/UNITS     
* Input  Widget       w          The identifier of the widget generating this
*                                callback.
* Input  XtPointer    clientdata Programmer-specified data passed into this
*                                callback routine.
* Input  XtPointer    calldata   An informational structure specific to this
*                                type of call back.
*
* RETURNS:
*   None
*
* APIs UTILIZED:
*   NAME         HEADER FILE                DESCRIPTION
*
* LOCAL DATA ELEMENTS (OPTIONAL):
*   DATA TYPE  NAME                         DESCRIPTION
*
* DATA FILES AND/OR DATABASE:
*    Not Applicable
*
* ERROR HANDLING:
*    None
*
********************************************************************************
*/

void radarBiasTable ( Widget w , XtPointer clientdata , XtPointer calldata )
{
   Atom         wmAtom;
   Widget       biasTableInfoMainForm;
   Widget       radarInfoTf;
   Widget       memspan[10],npairs[10];
   Widget       sumgag[10], sumrad[10];
   Widget       biasum[10];
   Widget       timeTf, closeInfoPb;
   Widget       thresholdLb, helpInfoPb;
   Widget       frame [ 10 ], frame2 [ 10 ], frame3 [10], frame4 [ 10 ], 
                frame5 [ 10 ];
   Widget       thresholdFrame;
   Widget	header1_1, header1_2, header1_3, header1_4, header1_5;
   Widget       header2_1, header2_3, header2_4;
   int          i,j = 0;
   int          index;
   static int   tfIndex = 0;
   char         col1[16],col2[12],col3[12],col4[12];
   char         col5[7];
   char         datetimeTf[40];
   char         rid[4];
   char         threshold[9];
   XmString     labelString;   
   XmString  str = XmStringCreateLocalized("NPairs Threshold = ");
  
   rid[3]   = '\0';   
   memset(datetimeTf, '\0', 40);
   memset(threshold,  '\0', 9);
   memset(col1, '\0', 16);
   memset(col2, '\0', 12);
   memset(col3, '\0', 12);
   memset(col4, '\0', 12);
   memset(col5, '\0', 7);

   
   if (radarBias_displayed)
   {
     logMessage("single radar bias table already displayed...\n");
      return;
   }
   else
      radarBias_displayed = 1;
      
   for (i = 0; i < NRADARS; i++)
   {  
      if(strcmp(XtName(w), XtName(radarId[i])) == 0)
      {
	 index = i;
	 break;
      }
   }
   
   biasTableInfo = XtVaCreatePopupShell(XtName(w),
				        transientShellWidgetClass,
				        toplevel, XmNdeleteResponse,
                                        XmDO_NOTHING, NULL);

   /* Add protocol to the close button on the window frame. */ 
   wmAtom = XmInternAtom ( ( XtDisplay ( biasTableInfo ) ), "WM_DELETE_WINDOW", 
                           False );
   XmAddWMProtocolCallback ( biasTableInfo, wmAtom, quitRadarBiasTableCallback,
                             ( Widget ) biasTableInfo );

   biasTableInfoMainForm = XtVaCreateManagedWidget("",
						   xmFormWidgetClass,
						   biasTableInfo,
						   NULL);	
   
   closeInfoPb = XtVaCreateManagedWidget(" CLOSE   ",
					 xmPushButtonWidgetClass,
					 biasTableInfoMainForm,
					 XmNwidth, 80,
					 XmNtopAttachment, XmATTACH_FORM,
					 XmNtopOffset, 5,
					 XmNleftAttachment, XmATTACH_FORM,
					 XmNleftOffset, 5,     
					 NULL);
   XtAddCallback(closeInfoPb, XmNactivateCallback,
                 quitRadarBiasTableCallback, (Widget)biasTableInfo);
   
   /* Apply button for NPAIRS Threshold. This will not be
      until changes are made in the db to support changing
      NPAIRS Threshold fo a radar.
      
      applyInfoPb[applyPbIndex] = XtVaCreateManagedWidget(" APPLY ",
      xmPushButtonWidgetClass,
      biasTableInfoMainForm,
      XmNwidth, 80,
      XmNtopAttachment, XmATTACH_FORM,
      XmNtopOffset, 5,
      XmNleftAttachment, XmATTACH_FORM,
      XmNleftOffset, 90,
      NULL);
      XtAddCallback(applyInfoPb[applyPbIndex], XmNactivateCallback,
      applyThresholdChangeCallback, (int)applyPbIndex);
      */
   
   helpInfoPb = XtVaCreateManagedWidget(" HELP ",
					xmPushButtonWidgetClass,
					biasTableInfoMainForm,
					XmNwidth, 80,
					XmNtopAttachment, XmATTACH_FORM,
					XmNtopOffset, 5,
					XmNleftAttachment, XmATTACH_FORM,
					XmNleftOffset, 90,
					NULL);  
//   XtAddCallback(helpInfoPb, XmNactivateCallback,
//	         popup_help_window, "BIASTABLE"); 
   
   thresholdLb = XtVaCreateManagedWidget("",
					 xmLabelWidgetClass,
					 biasTableInfoMainForm,
					 XmNtopWidget,closeInfoPb,
					 XmNtopAttachment, XmATTACH_WIDGET,   
					 XmNtopOffset, 15,
					 XmNleftAttachment, XmATTACH_FORM,
					 XmNleftOffset, 5,
					 XmNlabelString, str,
					 NULL);

   XmStringFree ( str ) ;
   
   thresholdFrame = XtVaCreateManagedWidget("",
					    xmFrameWidgetClass,
					    biasTableInfoMainForm,
					    XmNshadowType, XmSHADOW_ETCHED_OUT,
					    XmNshadowThickness, 5,
					    XmNtopAttachment, XmATTACH_WIDGET,   
					    XmNtopOffset, 40,
					    XmNleftWidget,thresholdLb,
					    XmNleftAttachment, XmATTACH_WIDGET,
					    XmNleftOffset, 5,
					    NULL);         
   
   thresholdTf = XtVaCreateManagedWidget("            ",
					 xmLabelWidgetClass,
					 thresholdFrame,
					 NULL);
   
   sprintf(threshold,"%d",read_default_bias());
   
   labelString = XmStringCreateLocalized(threshold);
   XtVaSetValues(thresholdTf,
	         XmNlabelString, labelString,
		 NULL); 
   XmStringFree(labelString);     
   /*
   sprintf(defaultBias,"%d",read_default_bias());
   XmTextSetString(thresholdTf,defaultBias);	
   */
   tfIndex++; 			
   
   timeTf = XtVaCreateManagedWidget("",
				    xmTextWidgetClass,
				    biasTableInfoMainForm,
				    XmNwidth, 145,
				    XmNtopAttachment, XmATTACH_FORM,
				    XmNtopOffset, 2,
				    XmNrightAttachment, XmATTACH_FORM,
				    XmNrightOffset, 5,
				    XmNcursorPositionVisible, FALSE,
				    XmNshadowThickness,0,
				    XmNtraversalOn, FALSE,	
				    NULL);
   
   sprintf(datetimeTf, "%s", date_st3.lldate);
   XmTextSetString(timeTf, datetimeTf);			
   
   radarInfoTf = XtVaCreateManagedWidget("",
					 xmFormWidgetClass,
					 biasTableInfoMainForm,
					 XmNeditable, FALSE,
					 XmNtraversalOn, FALSE,
					 XmNcursorPositionVisible, FALSE,
					 XmNwidth, 650,
					 XmNy, 100,			
					 NULL);
   
   for (i = 0; i < 10; i++)
   { 
      if ( i == 0 )
      {
         frame [ i ] = XtVaCreateManagedWidget("",
		      		         xmFrameWidgetClass,
				         radarInfoTf,
				         XmNshadowType, XmSHADOW_ETCHED_OUT,
				         XmNshadowThickness, 2,
				         XmNleftAttachment, XmATTACH_FORM,
				         XmNleftOffset,10,
				         XmNheight,25,
				         XmNwidth, 105,
				         XmNy, 50,			
				         NULL);
      }
      else
      {
         frame [ i ] = XtVaCreateManagedWidget("",
		      		         xmFrameWidgetClass,
				         radarInfoTf,
				         XmNshadowType, XmSHADOW_ETCHED_OUT,
				         XmNshadowThickness, 2,
				         XmNleftAttachment, XmATTACH_FORM,
				         XmNleftOffset,10,
				         XmNheight,25,
				         XmNwidth, 105,
				         XmNy,((35*i) + 50),			
				         NULL);
      }
      
      memspan[i] = XtVaCreateManagedWidget("",
					   xmLabelWidgetClass,
					   frame [ i ],		
				           XmNleftAttachment, XmATTACH_WIDGET,
                                           XmNleftWidget, frame [ i ] ,
                                           XmNleftOffset , 5 ,
                                           XmNalignment , XmALIGNMENT_CENTER ,
					   NULL);
      
      if ( i == 0 )
      {
         frame2 [ i ] = XtVaCreateManagedWidget("",
			   	          xmFrameWidgetClass,
				          radarInfoTf,
				          XmNshadowType, XmSHADOW_ETCHED_OUT,
				          XmNshadowThickness, 2,
				          XmNleftAttachment, XmATTACH_WIDGET,
				          XmNleftOffset,20,
				          XmNleftWidget, frame [ i ],
				          XmNheight,25,
				          XmNwidth, 65,
				          XmNy, 50,			
				          NULL);	   
      }
      else
      {
         frame2 [ i ] = XtVaCreateManagedWidget("",
			   	          xmFrameWidgetClass,
				          radarInfoTf,
				          XmNshadowType, XmSHADOW_ETCHED_OUT,
				          XmNshadowThickness, 2,
				          XmNleftAttachment, XmATTACH_WIDGET,
				          XmNleftOffset,20,
				          XmNleftWidget, frame [ i ],
				          XmNheight,25,
				          XmNwidth, 65,
				          XmNy,((35*i)+50),			
                                          NULL ) ;
      }
      
      npairs[i] = XtVaCreateManagedWidget("",
					  xmLabelWidgetClass,
					  frame2 [ i ],
				          XmNleftAttachment, XmATTACH_WIDGET,
                                          XmNleftWidget, frame2 [ i ] ,
                                          XmNleftOffset , 5 ,
                                          XmNalignment , XmALIGNMENT_CENTER ,
					  NULL);
      
      if ( i == 0 )
      {
          frame3 [ i ] = XtVaCreateManagedWidget("",
				           xmFrameWidgetClass,
				           radarInfoTf,
				           XmNshadowType, XmSHADOW_ETCHED_OUT,
				           XmNshadowThickness, 2,
				           XmNleftAttachment, XmATTACH_WIDGET,
				           XmNleftOffset,20,
				           XmNleftWidget, frame2 [ i ],
				           XmNheight,25,
				           XmNwidth, 65,
				           XmNy,50,			
				           NULL);
      }
      else
      {
          frame3 [ i ] = XtVaCreateManagedWidget("",
				           xmFrameWidgetClass,
				           radarInfoTf,
				           XmNshadowType, XmSHADOW_ETCHED_OUT,
				           XmNshadowThickness, 2,
				           XmNleftAttachment, XmATTACH_WIDGET,
				           XmNleftOffset,20,
				           XmNleftWidget, frame2 [ i ],
				           XmNheight,25,
				           XmNwidth, 65,
				           XmNy,((35*i)+50),			
                                           NULL ) ;
      }
      
      sumgag[i] = XtVaCreateManagedWidget("",
					  xmLabelWidgetClass,
					  frame3 [ i ],	
				          XmNleftAttachment, XmATTACH_WIDGET,
                                          XmNleftWidget, frame3 [ i ] ,
                                          XmNleftOffset , 5 ,
                                          XmNalignment , XmALIGNMENT_CENTER ,
					  NULL);
      
      if ( i == 0 )
      {
         frame4 [ i ] = XtVaCreateManagedWidget("",
				          xmFrameWidgetClass,
				          radarInfoTf,
				          XmNshadowType, XmSHADOW_ETCHED_OUT,
				          XmNshadowThickness, 2,
				          XmNleftAttachment, XmATTACH_WIDGET,
				          XmNleftOffset,20,
				          XmNleftWidget, frame3 [ i ],
				          XmNheight,25,
				          XmNwidth, 65,
				          XmNy, 50,			
				          NULL);
      }
      else
      {
         frame4 [ i ] = XtVaCreateManagedWidget("",
				          xmFrameWidgetClass,
				          radarInfoTf,
				          XmNshadowType, XmSHADOW_ETCHED_OUT,
				          XmNshadowThickness, 2,
				          XmNleftAttachment, XmATTACH_WIDGET,
				          XmNleftOffset,20,
				          XmNleftWidget, frame3 [ i ],
				          XmNheight,25,
				          XmNwidth, 65,
				          XmNy,((35*i)+50),			
                                          NULL ) ;
      } 

      sumrad[i] = XtVaCreateManagedWidget("",
					  xmLabelWidgetClass,
					  frame4 [ i ],
				          XmNleftAttachment, XmATTACH_WIDGET,
                                          XmNleftWidget, frame4 [ i ] ,
                                          XmNleftOffset , 5 ,
                                          XmNalignment , XmALIGNMENT_CENTER ,
					  NULL);
      
      
      if ( i == 0 )
      {
         frame5 [ i ] = XtVaCreateManagedWidget("",
				          xmFrameWidgetClass,
				          radarInfoTf,
				          XmNshadowType, XmSHADOW_ETCHED_OUT,
				          XmNshadowThickness, 2,
				          XmNleftAttachment, XmATTACH_WIDGET,
				          XmNleftOffset,20,
				          XmNleftWidget, frame4 [ i ],
				          XmNheight,25,
				          XmNwidth, 65,
				          XmNy, 50,			
				          NULL);
      }
      else
      {
         frame5 [ i ] = XtVaCreateManagedWidget("",
				          xmFrameWidgetClass,
				          radarInfoTf,
				          XmNshadowType, XmSHADOW_ETCHED_OUT,
				          XmNshadowThickness, 2,
				          XmNleftAttachment, XmATTACH_WIDGET,
				          XmNleftOffset,20,
				          XmNleftWidget, frame4 [ i ],
				          XmNheight,25,
				          XmNwidth, 65,
				          XmNy,((35*i)+50),			
                                          NULL ) ;
      }
      
      biasum[i] = XtVaCreateManagedWidget("",
					  xmLabelWidgetClass,
					  frame5 [ i ],
				          XmNleftAttachment, XmATTACH_WIDGET,
                                          XmNleftWidget, frame5 [ i ] ,
                                          XmNleftOffset , 5 ,
                                          XmNalignment , XmALIGNMENT_CENTER ,
					  NULL);
      
   }
   
   for (i = 0; i < NRADARS; i++)
   { 
      if ((strcmp(XtName(w), nexrad[i].id) == 0))
      {  	 
         header1_1  = XtVaCreateManagedWidget("Memory Span",
                                              xmLabelWidgetClass,
                                              radarInfoTf,
					      XmNtopAttachment, XmATTACH_FORM,
					      XmNtopOffset, 0,
                                              XmNleftAttachment,
                                              XmATTACH_OPPOSITE_WIDGET,
                                              XmNleftWidget, frame [ 0 ] ,
                                              NULL);  
					      
         header2_1  = XtVaCreateManagedWidget("(hrs)",
                                              xmLabelWidgetClass,
                                              radarInfoTf,
					      XmNtopAttachment, XmATTACH_WIDGET,
                                              XmNtopWidget, header1_1,
					      XmNtopOffset, 0,
                                              XmNleftAttachment,
                                              XmATTACH_OPPOSITE_WIDGET,
                                              XmNleftWidget, header1_1,
                                              NULL);  
					      
	 header1_2  = XtVaCreateManagedWidget("NPairs",
                                              xmLabelWidgetClass,
                                              radarInfoTf,
					      XmNtopAttachment, XmATTACH_FORM,
					      XmNtopOffset, 0,
                                              XmNleftAttachment,
                                              XmATTACH_OPPOSITE_WIDGET,
					      XmNleftWidget, frame2 [ 0 ],
                                              NULL);      
					       
	 header1_3  = XtVaCreateManagedWidget("Mean Gage",
                                              xmLabelWidgetClass,
                                              radarInfoTf,
					      XmNtopAttachment, XmATTACH_FORM,
					      XmNtopOffset, 0,
                                              XmNleftAttachment,
                                              XmATTACH_OPPOSITE_WIDGET,
					      XmNleftWidget, frame3 [ 0 ],
                                              NULL); 

         header2_3  = XtVaCreateManagedWidget("(mm/hrs)",
                                              xmLabelWidgetClass,
                                              radarInfoTf,
					      XmNtopAttachment, XmATTACH_WIDGET,
                                              XmNtopWidget, header1_3,
					      XmNtopOffset, 0,
                                              XmNleftAttachment,
                                              XmATTACH_OPPOSITE_WIDGET,
                                              XmNleftWidget, header1_3,
                                              NULL);  
					      
					          
	 header1_4  = XtVaCreateManagedWidget("Mean Radar",
                                              xmLabelWidgetClass,
                                              radarInfoTf,
					      XmNtopAttachment, XmATTACH_FORM,
					      XmNtopOffset, 0,
                                              XmNleftAttachment,
                                              XmATTACH_OPPOSITE_WIDGET,
					      XmNleftWidget, frame4 [ 0 ],
                                              NULL);  

         header2_4  = XtVaCreateManagedWidget("(mm/hrs)",
                                              xmLabelWidgetClass,
                                              radarInfoTf,
					      XmNtopAttachment, XmATTACH_WIDGET,
                                              XmNtopWidget, header1_4,
					      XmNtopOffset, 0,
                                              XmNleftAttachment,
                                              XmATTACH_OPPOSITE_WIDGET,
                                              XmNleftWidget, header1_4,
                                              NULL);  
					      
					      
	header1_5  = XtVaCreateManagedWidget("Bias",
                                              xmLabelWidgetClass,
                                              radarInfoTf,
					      XmNtopAttachment, XmATTACH_FORM,
					      XmNtopOffset, 0,
                                              XmNleftAttachment,
                                              XmATTACH_OPPOSITE_WIDGET,
					      XmNleftWidget, frame5 [ 0 ],
                                              NULL);          						              				                                                  

	 strcpy(rid,nexrad[i].id);
         read_bias_table_param(rid);
	 
	 for (j = 0; j < 10; j++)
	 {
	    sprintf(col1, "%-13.3f", biasdata.mem_span[j]);
	    labelString = XmStringCreateLocalized(col1);
	    XtVaSetValues(memspan[j],
			  XmNlabelString, labelString,
			  NULL); 
	    XmStringFree(labelString);
	    
	    sprintf(col2, "%-10.2f", biasdata.num_pairs[j]);
	    labelString = XmStringCreateLocalized(col2);
	    XtVaSetValues(npairs[j],
			  XmNlabelString, labelString,
			  NULL); 		      
	    XmStringFree(labelString);
	    
	    sprintf(col3, "%-10.2f", biasdata.sumgag[j]);
	    labelString = XmStringCreateLocalized(col3);
	    XtVaSetValues(sumgag[j],
			  XmNlabelString, labelString,
			  NULL); 		      
	    XmStringFree(labelString);	 
	    
	    sprintf(col4, "%-10.2f", biasdata.sumrad[j]);
	    labelString = XmStringCreateLocalized(col4);
	    XtVaSetValues(sumrad[j],
			  XmNlabelString, labelString,
			  NULL); 		      
	    XmStringFree(labelString);	    
	    
	    sprintf(col5, "%-10.2f", biasdata.bias[j]);
	    labelString = XmStringCreateLocalized(col5);
	    XtVaSetValues(biasum[j],
			  XmNlabelString, labelString,
			  NULL); 		      
	    XmStringFree(labelString);
	    
	 }	  	 	 		     
      }
   }
   
   XtPopup(biasTableInfo,XtGrabNone);   
   
   XtVaSetValues(biasTableInfo, 
		 XmNmaxHeight, 520,
		 XmNmaxWidth, 650,
		 NULL); 
   
   return;     		
}
/*******************************************************************************
* MODULE NUMBER: 3
* MODULE NAME:   quitBiasTableCallback
* PURPOSE:       This routine called when selecting "Close" button
*		 from multi-radar main bias window.          
*
* ARGUMENTS:
* TYPE   DATA TYPE    NAME       DESCRIPTION/UNITS     
* Input  Widget       w          The identifier of the widget generating this
*                                callback.
* Input  XtPointer    clientdata Programmer-specified data passed into this
*                                callback routine.
* Input  XtPointer    calldata   An informational structure specific to this
*                                type of call back.
*
* RETURNS:
*   None
*
* APIs UTILIZED:
*   NAME         HEADER FILE                DESCRIPTION
*
* LOCAL DATA ELEMENTS (OPTIONAL):
*   DATA TYPE  NAME                         DESCRIPTION
*
* DATA FILES AND/OR DATABASE:
*    Not Applicable
*
* ERROR HANDLING:
*    None
*
********************************************************************************
*/

void quitBiasTableCallback ( Widget w, XtPointer clientdata , 
                             XtPointer calldata)   
{
   Widget shell = (Widget) clientdata ;
   
  logMessage("closing multi-radar bias table...\n");
   
   XtPopdown(shell);
   XtDestroyWidget(shell);

   biasShellUp = False;
   
   return;
}

/*******************************************************************************
* MODULE NUMBER: 4
* MODULE NAME:   quitRadarBiasTableCallback
* PURPOSE:       This routine called when selecting "Close" button
*		 from single-radar bias window.         
*
* ARGUMENTS:
* TYPE   DATA TYPE    NAME       DESCRIPTION/UNITS     
* Input  Widget       w          The identifier of the widget generating this
*                                callback.
* Input  XtPointer    clientdata Programmer-specified data passed into this
*                                callback routine.
* Input  XtPointer    calldata   An informational structure specific to this
*                                type of call back.
*
* RETURNS:
*   None
*
* APIs UTILIZED:
*   NAME         HEADER FILE                DESCRIPTION
*
* LOCAL DATA ELEMENTS (OPTIONAL):
*   DATA TYPE  NAME                         DESCRIPTION
*
* DATA FILES AND/OR DATABASE:
*    Not Applicable
*
* ERROR HANDLING:
*    None
*
********************************************************************************
*/

void quitRadarBiasTableCallback(Widget w, XtPointer clientdata , XtPointer calldata)
  
{
   Widget shell = ( Widget ) clientdata ;
  logMessage("closing single radar bias table...\n");
   XtPopdown(shell);
   XtDestroyWidget(shell);
   
   radarBias_displayed = 0;
   biasShellUp = False;
   
   return;
}

/*******************************************************************************
* MODULE NUMBER: 5
* MODULE NAME:   applyThresholdChangeCallback
* PURPOSE:       This routine called when selecting "Apply" button
*		 from single-radar bias window.         
*
* ARGUMENTS:
* TYPE   DATA TYPE    NAME       DESCRIPTION/UNITS     
* Input  Widget       w          The identifier of the widget generating this
*                                callback.
* Input  XtPointer    clientdata Programmer-specified data passed into this
*                                callback routine.
* Input  XtPointer    calldata   An informational structure specific to this
*                                type of call back.
*
* RETURNS:
*   None
*
* APIs UTILIZED:
*   NAME         HEADER FILE                DESCRIPTION
*
* LOCAL DATA ELEMENTS (OPTIONAL):
*   DATA TYPE  NAME                         DESCRIPTION
*
* DATA FILES AND/OR DATABASE:
*    Not Applicable
*
* ERROR HANDLING:
*    None
*
********************************************************************************
*/

void applyThresholdChangeCallback(Widget w, XtPointer clientdata, XtPointer calldata)
  
{
   
   
   /*
   value = atoi(XmTextGetString(thresholdTf[pbId]));
   update_threshold_RFCW(value);
   */
   
  logMessage("calling applyThresholdChangeCallback() currently does nothing\n");
   
   return;   
}
/*******************************************************************************
* MODULE NUMBER: 6
* MODULE NAME:   applyBiasChangeCallback
* PURPOSE:       This routine Creates GUI to display the bias information      
*      		 as read from the TBD table calling function: callback          
*      		 from Display Bias Table option          
*
* ARGUMENTS:
* TYPE   DATA TYPE    NAME       DESCRIPTION/UNITS     
* Input  Widget       w          The identifier of the widget generating this
*                                callback.
* Input  XtPointer    clientdata Programmer-specified data passed into this
*                                callback routine.
* Input  XtPointer    calldata   An informational structure specific to this
*                                type of call back.
*
* RETURNS:
*   None
*
* APIs UTILIZED:
*   NAME         HEADER FILE                DESCRIPTION
*
* LOCAL DATA ELEMENTS (OPTIONAL):
*   DATA TYPE  NAME                         DESCRIPTION
*
* DATA FILES AND/OR DATABASE:
*    Not Applicable
*
* ERROR HANDLING:
*    None
*
********************************************************************************
*/

void applyBiasChangeCallback ( Widget w , XtPointer clientdata , XtPointer calldata)
  
{
   int 		i;
   char 	biasStr[8];
   float 	value;
   char      	* text = NULL ;
   XmString  	label; 
   
   memset(biasStr, '\0', 8);

   for (i = 0; i < NRADARS; i++)
   {    	    
      sprintf(biasStr, "%-1.2f", oldBias[i]);
      XtVaGetValues(modify[i], XmNlabelString, &label, NULL); 
      XmStringGetLtoR(label, XmFONTLIST_DEFAULT_TAG, &text);
      
      if(strcmp(text,"   NO    ") == 0)	 
      {
	 restore_bias_RFCW(XtName(radarId[i]),datetime);
      }
      
      else
      {
         /*
        logMessage("updating RWRadarResult table record for radar=%s  new bias value=%s\n",
	 XtName(radarId[i]),XmTextGetString(bias[i]));
	 */	     
	 value = atof(XmTextGetString(bias[i]));  
	 update_bias_RFCW(XtName(radarId[i]), datetime, &value);     
      }

      XtFree ( text ) ;
      text = NULL ;
   }
   
   return;
}

/*******************************************************************************
* MODULE NUMBER: 7
* MODULE NAME:   validateInputCallback
* PURPOSE:       This routine called while changing text field with 
*		 bias value in multi-radar main bias window. 
* ARGUMENTS:
* TYPE   DATA TYPE    NAME       DESCRIPTION/UNITS     
* Input  Widget       w          The identifier of the widget generating this
*                                callback.
* Input  XtPointer    clientdata Programmer-specified data passed into this
*                                callback routine.
* Input  XtPointer    calldata   An informational structure specific to this
*                                type of call back.
*
* RETURNS:
*   None
*
* APIs UTILIZED:
*   NAME         HEADER FILE                DESCRIPTION
*
* LOCAL DATA ELEMENTS (OPTIONAL):
*   DATA TYPE  NAME                         DESCRIPTION
*
* DATA FILES AND/OR DATABASE:
*    Not Applicable
*
* ERROR HANDLING:
*    None
*
********************************************************************************
*/
void validateInputCallback(Widget w , XtPointer clientdata , XtPointer calldata)
  
{ 
   int len;
   XmTextVerifyCallbackStruct *cbs =
      (XmTextVerifyCallbackStruct *)calldata;
   
   
   len = XmTextGetLastPosition(w);
   if (cbs->startPos < cbs->currInsert)
      return;
   
   if (len > 3)
   {
      cbs->doit = FALSE;
      return;
   }
   
   for (len = 0; len < cbs->text->length; len++)
   {
      if ((!isdigit (cbs->text->ptr[len])) &&
	 (cbs->text->ptr[len] != '.'))
      {	    
         int i;
	 for(i = len;(i+1) < cbs->text->length;i++)
	    cbs->text->ptr[i] = cbs->text->ptr[i+1];
	 cbs->text->length--;
	 len--;
      }
   }
   
   if (cbs->text->length == 0)
      cbs->doit = FALSE;
   
   return;
}

/*******************************************************************************
* MODULE NUMBER: 8
* MODULE NAME:   setModifyCallback
* PURPOSE:       This routine is called when changing text field 
*		 with bias value in multi-radar main bias window.
*
* ARGUMENTS:
* TYPE   DATA TYPE    NAME       DESCRIPTION/UNITS     
* Input  Widget       w          The identifier of the widget generating this
*                                callback.
* Input  XtPointer    clientdata Programmer-specified data passed into this
*                                callback routine.
* Input  XtPointer    calldata   An informational structure specific to this
*                                type of call back.
*
* RETURNS:
*   None
*
* APIs UTILIZED:
*   NAME         HEADER FILE                DESCRIPTION
*
* LOCAL DATA ELEMENTS (OPTIONAL):
*   DATA TYPE  NAME                         DESCRIPTION
*
* DATA FILES AND/OR DATABASE:
*    Not Applicable
*
* ERROR HANDLING:
*    None
*
********************************************************************************
*/

void setModifyCallback ( Widget w , XtPointer clientdata , XtPointer calldata)
   
{   
   XmString  yesStr;
   int       i = (int)clientdata;
   
   yesStr = XmStringCreateLocalized("   YES   ");
   XtVaSetValues(modify[i], XmNlabelString, yesStr, NULL);
   XmStringFree(yesStr);  
   
   return;
}

/*******************************************************************************
* MODULE NUMBER: 9
* MODULE NAME:   toggleYesNo   
* PURPOSE:       This routine is called when pushing Yes or No button 
*		 on multi-radar main bias window.
*
* ARGUMENTS:
* TYPE   DATA TYPE    NAME       DESCRIPTION/UNITS     
* Input  Widget       w          The identifier of the widget generating this
*                                callback.
* Input  XtPointer    clientdata Programmer-specified data passed into this
*                                callback routine.
* Input  XtPointer    calldata   An informational structure specific to this
*                                type of call back.
*
* RETURNS:
*   None
*
* APIs UTILIZED:
*   NAME         HEADER FILE                DESCRIPTION
*
* LOCAL DATA ELEMENTS (OPTIONAL):
*   DATA TYPE  NAME                         DESCRIPTION
*
* DATA FILES AND/OR DATABASE:
*    Not Applicable
*
* ERROR HANDLING:
*    None
*
********************************************************************************
*/
void toggleYesNo ( Widget w , XtPointer clientdata , XtPointer calldata)
  
{
   int       i      = ( int) clientdata;
   XmString  noStr  = XmStringCreateLocalized("   NO    ");
   XmString  yesStr = XmStringCreateLocalized("   YES   ");

   char      *text;
   XmString  label;
   char      biasStr[8];
   
   XtVaGetValues(w, XmNlabelString, &label, NULL); 
   XmStringGetLtoR(label, XmFONTLIST_DEFAULT_TAG, &text);
   
   if (strcmp(text,"   YES   ") == 0)
   {   
      sprintf(biasStr, "%-1.2f", oldBias[i]);
      XmTextSetString(bias[i], biasStr);
      XtVaSetValues(w,
                    XmNlabelString,noStr,
		    NULL);
   }
   
   else
   {
      XtVaSetValues(w, XmNlabelString, yesStr, NULL);
   }
   
   XmStringFree(label);
   XmStringFree(noStr);
   XmStringFree(yesStr);
   XtFree(text);
   
   return;   

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob83/ohd/pproc_lib/src/MPEGui/RCS/display_bias_table.c,v $";
 static char rcs_id2[] = "$Id: display_bias_table.c,v 1.22 2007/10/18 18:07:13 lawrence Exp $";}
/*  ===================================================  */

}


