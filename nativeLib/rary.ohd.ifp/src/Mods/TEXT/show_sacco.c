/* ********************************************************** */
/*                                                            */
/*	FILE:		show_sacco.c                          */
/*                                                            */
/*	Sacco Display                                         */
/*                                                            */
/*	Coded by:	W. Kwock                               */
/*			A. Vo                                 */
/*	Date:		03/15/01                              */
/*      Modified by:                                          */
/*                      05/15/01 A. Vo added a CO day         */
/*                      display message on SACCO window       */
/*                                                            */ 
/* ********************************************************** */  
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include <memory.h>


#include <X11/Intrinsic.h>
#include <X11/Shell.h>

#include <Xm/Xm.h>
#include <Xm/DialogS.h>
#include <Xm/Form.h>
#include <Xm/PushB.h>
#include <Xm/ScrollBar.h> 
#include <Xm/Text.h>
#include <Xm/TextF.h>
#include <Xm/FileSB.h>
#include <Xm/Frame.h>
#include <Xm/LabelG.h>
#include <Xm/Scale.h>




/* ***************************************** */
/* ************* Header files ************** */
/* ***************************************** */
#include "mods_plot.h"
#include "libXs.h"
#include "Mods_everythingStruct.h"
#include "sacco.h"


int scaleChanged(float newval, float curval);
void Scale_ChangeCB ();
void SaccoCallbacks(Widget);
void CancelBtnCB  ();
void Text_ChangeCB (Widget ,Widget,  XtPointer);
void SetTickMark (Widget ,float);
void OKBtnCB (); 
void mm2inch(int, float[], float[]);
float degCtoF(float );
extern void ErrorDialog(Widget, char *);
extern void createSelectionCB(Widget, Mods_everythingStruct *, XmPushButtonCallbackStruct *);
extern void show_fileViewDS();
extern void getnonuniversaltechs(int *NOSNOW, int *NOFRZE, int *upsc, int *upwe,
                     int *IPRSAC, int *IPRSNW, int *ICRTRO, int *IPRHY, int *IOUTYP);
extern int isModDatesValid(Mods_everythingStruct *);
extern void ShowErrorDialog( Widget , char * );
/* ************************************************************** */
/* SACCO Control Dialog can be called by a standalone application    */
/* or by a callbacks function from any type of button             */
/* ************************************************************** */

Mods_everythingStruct   *sacData;
Widget          wt[7],ws[7],wl[7];
void    show_saccoDS(Widget w, Mods_everythingStruct *data)
{

	/* ********************************************************** */
	/*   Create SACCO Control Dialog for the first time only and  */
	/*   just manage it when called  and unmanage it when don't   */
	/*   need. 						      */
	/* ********************************************************** */
        int         i;
        
        
        sacData = data;
      
        
        create_saccoScaleDS(XtParent(w));
        SaccoCallbacks(w);
        XtManageChild(saccoScaleFO); 
        XtManageChild(saccoScaleDS);
        
        return;

}

/* ******************************************* */
/* Add callbacks(): To add all callbacks */
/* for the SACCO  Dialog          */
/* ******************************************* */
extern Mods_everythingStruct *sacData;
void SaccoCallbacks(Widget w)
{
    
    int             initialScaleValue;
    int             scaleMinimum;
    int             scaleMaximum;
    int             scaleDecimalPoints = 1;
    int             scaleFactor = 1;
    char            string[20];
    XmString        xmStr;
    XmString        xmMsgStr;
    char            text[20],  LblTxt[30] , UnitTxt[20];
    char            dispTxt[80];
    int             i, scaleInputError = 0;
    WaterLevelType  *WaterLvl;
  
    float           retval[6], tmpMaxfgix, tmpCurrentfgix,tmpNewfgix, 
                    retval1[6],
                    retval2[6];
    int             size, j;
    int             n;
    
    /* Load the settings of the non-universal techniques from the X-window
       property.  (Hank  2005-03-07)*/
    int nosnow;
    int nofrze;
    int upsc;
    int upwe;
    int iprsac;
    int iprsnw;
    int icrtro;
    int iprhy;
    int ioutyp;
    getnonuniversaltechs(&nosnow, &nofrze, &upsc, &upwe, &iprsac, &iprsnw,
        &icrtro, &iprhy, &ioutyp);
        
    
    /* initialize newWaterlevel */
    for(j = 0 ;j< 7;j++)
    {     
        sacData->WaterLvl->NewWaterLevels[j] = 
                   sacData->WaterLvl->CurrentWaterLevels[j];
	
    }
    /* MR 1991  */
    /* text widgets */
    wt[0]=uztwcCurrentValuetext;
    wt[1]=uzfwcCurrentValuetext;
    wt[2]=lztwcCurrentValuetext;
    wt[3]=lzfscCurrentValuetext;
    wt[4]=lzfpcCurrentValuetext;
    wt[5]=adimcCurrentValuetext;
    wt[6]=fgixCurrentValuetext;
    /* Scale widgets */
    ws[0]=uztwcScale;
    ws[1]=uzfwcScale;
    ws[2]=lztwcScale;
    ws[3]=lzfscScale;
    ws[4]=lzfpcScale;
    ws[5]=adimcScale;
    ws[6]=fgixScale;
     /* Label widgets */
    wl[0]= uztwcInitValueLB;
    wl[1]= uzfwcInitValueLB;
    wl[2]= lztwcInitValueLB;
    wl[3]= lzfscInitValueLB;
    wl[4]= lzfpcInitValueLB;
    wl[5]= adimcInitValueLB;
    wl[6]= fgixInitValueLB;
    
    
    sprintf(dispTxt,"%s","Initial Values displayed are for the Carryover day");
    xmMsgStr = XmStringCreateSimple(dispTxt);
    XtVaSetValues(DisplayLbl,XmNlabelString,xmMsgStr,NULL) ;
    XmStringFree(xmMsgStr) ;
    /* Check for Input unit Metric unit = 1, English Unit = 0 */
    /* check units: metric (mm ) or English (inches) */
    if(sacData->units->SAC_Units != UNITS_METRIC)
    {
       sprintf(UnitTxt,"%s"," Unit = English"); 
       
       xmStr = XmStringCreateSimple(UnitTxt) ;
       
       XtVaSetValues(UnitLbl,XmNlabelString,xmStr,NULL) ;
       XmStringFree(xmStr) ;
      
        size = 6;
        /* convert mm to inch */
        mm2inch(size,sacData->WaterLvl->MaxWaterLevels,retval);
        mm2inch(size,sacData->WaterLvl->CurrentWaterLevels,retval1);
        mm2inch(size,sacData->WaterLvl->NewWaterLevels,retval2);
        /* convert Celsius to Fahrenheit */
        
        if (sacData->WaterLvl->CurrentWaterLevels[6] != -999.0)
        {
           tmpMaxfgix = degCtoF(sacData->WaterLvl->MaxWaterLevels[6]);
           tmpCurrentfgix = degCtoF(sacData->WaterLvl->CurrentWaterLevels[6]);
           tmpNewfgix = degCtoF(sacData->WaterLvl->NewWaterLevels[6]);
           sacData->WaterLvl->MaxWaterLevels[6] = tmpMaxfgix;
           sacData->WaterLvl->CurrentWaterLevels[6] = tmpCurrentfgix;
           sacData->WaterLvl->NewWaterLevels[6] = tmpNewfgix;
           
        }
        
        /* Store new value back to the array data */
        for( i=0; i< 6; i++)
        {   
            if(sacData->WaterLvl->MaxWaterLevels[i] != -999 ||
                sacData->WaterLvl->CurrentWaterLevels[i] != -999)
            {
                sacData->WaterLvl->MaxWaterLevels[i] = retval[i];
                sacData->WaterLvl->CurrentWaterLevels[i] = retval1[i];
                sacData->WaterLvl->NewWaterLevels[i] = retval2[i];
            }
            
        }
    }
    else
    { 
       
       sprintf(UnitTxt,"%s"," Unit = Metric"); 
       xmStr = XmStringCreateSimple(UnitTxt) ;
       
       XtVaSetValues(UnitLbl,XmNlabelString,xmStr,NULL) ;
       XmStringFree(xmStr) ;
    
    }
    
    /* check waterlevel values: currentwaterlevel must < maxwaterlevel */
    
    for(i=0;i<7;i++)
    {
    
       if(sacData->WaterLvl->CurrentWaterLevels[i] > sacData->WaterLvl->MaxWaterLevels[i] ){
           sacData->WaterLvl->CurrentWaterLevels[i] = -999.0;
       }
       XtVaSetValues(ws[i],XmNshowValue,		FALSE,	NULL);
       XtAddCallback (ws[i],XmNvalueChangedCallback,(XtCallbackProc)Scale_ChangeCB,wt[i]) ;
       XtAddCallback (wt[i],XmNactivateCallback,(XtCallbackProc)Text_ChangeCB,ws[i]) ;
    }
    
    XtAddCallback (OKButton,XmNactivateCallback,OKBtnCB,NULL) ;
    XtAddCallback (CancelButton,XmNactivateCallback,CancelBtnCB,NULL) ;
    XtAddCallback (ViewButton,XmNactivateCallback,(XtCallbackProc)show_fileViewDS,NULL) ;
   
    xmStr = XmStringCreateSimple(sacData->WaterLvl->ID) ;
    XtVaSetValues(Opname,XmNlabelString,xmStr,NULL) ;
    XmStringFree(xmStr) ;
   
    for(n=0;n<7;n++){
       memset(string,'\0',20);
       memset(text,'\0',20);
       
       if(sacData->WaterLvl->CurrentWaterLevels[n] == -999.0 ||
       sacData->WaterLvl->MaxWaterLevels[n] == -999.0)
       {
           XtSetSensitive(ws[n], FALSE); 
           XtSetSensitive(wt[n], FALSE); 
      
       }
       sprintf (text,"%.2f",sacData->WaterLvl->CurrentWaterLevels[n]);
    
       XmTextSetString(wt[n],text) ;
       strcpy(LblTxt,"In Val:"); 
       strcat(LblTxt,text) ;
       xmStr = XmStringCreateSimple(LblTxt) ;
       XtVaSetValues(wl[n],XmNlabelString,xmStr,NULL) ;
       XmStringFree(xmStr) ;


       XtVaSetValues(ws[n],XmNmaximum,(int)(sacData->WaterLvl->MaxWaterLevels[n]*100),
                              XmNvalue,(int)(sacData->WaterLvl->CurrentWaterLevels[n]*100),NULL) ;
       sprintf(string, "%.2f",sacData->WaterLvl->CurrentWaterLevels[n]);
       XmTextFieldSetString(wt[n], string);
	
       SetTickMark (ws[n],sacData->WaterLvl->MaxWaterLevels[n]) ;
    }    
    
    /* Make the fgix window unsensitive if the nofrze flag is 1. 
       Hank Herr 2005-03-07*/
    if (nofrze == 1)
   {
       XtSetSensitive(ws[6], FALSE); 
       XtSetSensitive(wt[6], FALSE); 
      
   }
  
}



/*************************************************/
void Scale_ChangeCB (Widget Scale, Widget CurrentValuetext, XtPointer call_data)
{ 

  Display         *display;
  float           displayValue;
  char		  string[50];
  int             i;
  int             decimal_conversion = 1;
  short           decimal_points;  /* number of decimal points */
  XmScaleCallbackStruct *cbs = (XmScaleCallbackStruct *) call_data;
  
  if(cbs->reason == XmCR_VALUE_CHANGED)
  {
     XtVaGetValues(Scale, XmNdecimalPoints, &decimal_points, NULL);
     sprintf(string,"%.2f",cbs->value/100.0) ;
     XmTextSetString (CurrentValuetext,string) ;
  }
  
} 


/*************************************************/
void CancelBtnCB (Widget CancelButton, XtPointer call_client, XtPointer call_data)
{
    extern Mods_everythingStruct *sacData;
  
    sacData->WaterLvl->ActionFlag = -1 ;
    /* Toggle button on modwier window */
    
    XmToggleButtonSetState(sacData->widgetData->dataEntryToggle,
			                          FALSE, FALSE);
    XtDestroyWidget(saccoScaleDS);    
    saccoScaleDS = NULL;                                          
  
}

/*************************************************/

void Text_ChangeCB (Widget TextBox,Widget Scale, XtPointer call_data)
{ 
    float      value ;
    char       *text ;
    char       string[20];
    int        n;    
    Display    *display;
    extern     Mods_everythingStruct *sacData;
    
    XtVaGetValues (TextBox,XmNvalue,&text,NULL) ;
    sscanf(text,"%f",&value);
    for(n=0; n<7; n++){
       if(Scale == ws[n]){
     
           if(value > sacData->WaterLvl->MaxWaterLevels[n]){
            
               ErrorDialog(XtParent(Scale), "Value is out bound.  Will set to Max level");
          
               value = sacData->WaterLvl->MaxWaterLevels[n];    
            
           }
           if( value < 0.0){
               ErrorDialog(XtParent(Scale), "Negative value enter. Reset to Zero");
               value = 0.0;       
            
           }
       }
    }
  
    sprintf(string,"%.2f",value) ;
    XmTextSetString (TextBox,string) ;
    XmScaleSetValue (Scale,(int)(value*100.0)) ;
    XtFree (text) ;
} 

/*************************************************/
void SetTickMark (Widget widgetScale,float maxValue)
{
    int           i;
    char          chs[8] ;
    int minValue;
    float TempValue,TmpMinValue ;
    
    XtVaGetValues(widgetScale, XmNminimum, &minValue, NULL);
    
    TmpMinValue = (float) (minValue/100.0) ;
    TempValue = (maxValue - TmpMinValue) / 10.0 ;
    
    for (i=10; i>=0; i--)
    {         
        sprintf (chs,"%.2f",(float)(TempValue * i + TmpMinValue)) ;
        strcat(chs, "-");
        XtVaCreateManagedWidget (chs,xmLabelGadgetClass,widgetScale,NULL) ;
    }
   
}



/*************************************************/

void OKBtnCB (Widget OKButton,XtPointer client_data ,XtPointer call_data)  
{ 
  
    int num, i;
    int noAction[7], noActionflag = 1;
    int myval, n;
    extern Mods_everythingStruct *sacData;
    int isValidDate;
    char *buf;
    float value;
    /*Widget  wt[7],ws[7];*/
   
    
isValidDate=isModDatesValid(sacData) ;


switch (isValidDate){/*pop up the error message*/
	case -1:
		ShowErrorDialog (OKButton,"***start month is invalid***");
		break ;
	case -2:
		ShowErrorDialog (OKButton,"***start year is invalid***");
		break ;
	case -3:
		ShowErrorDialog (OKButton,"***start day is invalid***");
		break ;
	case -4:
		ShowErrorDialog (OKButton,"***start hour is invalid***");
		break ;
	case -5:
		ShowErrorDialog (OKButton,"***end month is invalid***");
		break ;
	case -6:
		ShowErrorDialog (OKButton,"***end year is invalid***");
		break ;
	case -7:
		ShowErrorDialog (OKButton,"***end day is invalid***");
		break ;
	case -8:
		ShowErrorDialog (OKButton,"***end hour is invalid***");
		break ;
	case -9:
		ShowErrorDialog (OKButton,"***valid month is invalid***");
		break ;
	case -10:
		ShowErrorDialog (OKButton,"***valid year is invalid***");
		break ;
	case -11:
		ShowErrorDialog (OKButton,"***valid day is invalid***");
		break ;
	case -12:
		ShowErrorDialog (OKButton,"***valid hour is invalid***");
}

if (isValidDate < 0) /*mod date is invalid.  Stop create mod*/
	return ;
    /*MR 1991    */
    for (n = 0;n<7;n++){ 
       XtVaGetValues (wt[n],XmNvalue,&buf,NULL) ;
       if(sscanf(buf,"%f",&value)) {
          sacData->WaterLvl->NewWaterLevels[n] = value ;
       
       }else {    
    
          XmScaleGetValue(ws[n],&num);
          sacData->WaterLvl->NewWaterLevels[n] = num/100.0 ;
       }
       noAction[n] = scaleChanged(sacData->WaterLvl->NewWaterLevels[n],
                              sacData->WaterLvl->CurrentWaterLevels[n]);
    
    }
    XtFree (buf) ;
    /*end MR 1991 */
    for(i = 0;i<7;i++)
    {
        noActionflag = noActionflag * noAction[i];
    }  
    
    /* scale value has been changed */
    if(noActionflag == FALSE) {       
        createSelectionCB( (Widget)OKBtnCB, sacData, NULL); 
    }
    sacData->WaterLvl->ActionFlag = 1 ;
    XmToggleButtonSetState(sacData->widgetData->dataEntryToggle,
			                          FALSE, FALSE);
    XtDestroyWidget(saccoScaleDS);    
    saccoScaleDS = NULL; 
                                    
} 

int scaleChanged(float newval, float curval)
{
    float tdel = 0.0;
   
    if(fabs(curval) != 999.0){
        tdel = newval - curval;   
    }
    /*value has changed */
    if(fabs(tdel) >= 0.009){
        return FALSE;
    }
    else{
        return TRUE;
    }   
}

/* convert from mm  to inches  */
void mm2inch(int arrysize, float inval[],float outval[])
{
    int i;
   
    for(i = 0; i<arrysize; i++)
    {
       outval[i] = inval[i]/25.4;      
    }
}


float degCtoF(float degc)

{
float degF;

   degF = (degc * 9.0/5.0) + 32.0;
   return( degF);

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/Mods/RCS/show_sacco.c,v $";
 static char rcs_id2[] = "$Id: show_sacco.c,v 1.6 2006/04/18 15:30:01 aivo Exp $";}
/*  ===================================================  */

}

