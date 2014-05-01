/* ***************************************** */
/* ************* AV 3/03/01   ************** */
/* ***************************************** */
#include <stdio.h>
#include <stdlib.h>
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


/* ***************************************** */
/* ************* Header files an extern routines ************** */
/* ***************************************** */

#include "fileView.h"
#include "show_fileView.h"
#include "mods_plot.h"
#include "libXs.h"
#include "Mods_everythingStruct.h"






extern int getFilesize(char *);
extern int get_apps_defaults(char *, int *, char *, int *);

void ErrMsg(Widget , char *);

/* ************************************************************** */
/* SACCO Control Dialog can be called by a standalone application    */
/* or by a callbacks function from any type of button             */
/* ************************************************************** */
void DeleteWhiteSpace(char *);

void    show_fileViewDS(Widget w)
{
        extern Mods_everythingStruct   *sacData;
	int     len, len2;
	char    saccoFilename[40];
	char    SettingsPath[100];
        

	/* ********************************************************** */
	/*   Create SACCO Control Dialog for the first time only and     */
	/*   just manage it when called  and unmanage it when don't   */
	/*  need. 						      */
	/* ********************************************************** */
        memset(SettingsPath, '\0', 100);
        memset(saccoFilename, '\0', 40);
        len = strlen("ifp_sacco_dir");
        
        get_apps_defaults("ifp_sacco_dir", &len, SettingsPath, &len2);
        /* get_apps_defaults("climate_file_name", &flen, saccoFilename, &flen2);*/
        /* need to add code here to change filename. Noneed to read from a token */
        strncpy(saccoFilename,sacData->WaterLvl->Seg,strlen(sacData->WaterLvl->Seg));
        DeleteWhiteSpace(saccoFilename);
       
        strcat(saccoFilename,".sac");
	
        strcat(SettingsPath, "/");
	strcat(SettingsPath, saccoFilename);

        saccoViewDS = NULL;
       
	if (! saccoViewDS )
    	{
       		create_saccoViewDS(XtParent(w));
       		sacco_callbacks();
	}

 	if (! XtIsManaged(saccoViewDS))
   	{
                XtManageChild(saccoViewFO);
      		XtManageChild(saccoViewDS);
   	}
       
        
        showTextfile(XtParent(w), SettingsPath);

        return;

}

/* ******************************************* */
/*  all callbacks */
/* for the view Control Dialog          */
/* ******************************************* */

void sacco_callbacks() 
{

	/* ************************************* */
	/* Adding Callbacks for SACCO view push button */
	/* ************************************* */

	 
	 XtAddCallback(saccoViewPB, XmNactivateCallback, saccoViewClose_CB, (XtPointer) 0);
	 


}


void showTextfile(Widget widget,char *filePathname)
{

FILE   *fp;
int    fileSize;
char   *ptext;
char   pathname[100];
XmFontList   fontlist;
XFontStruct   *viewfont;

memset(pathname,'\0',100);

viewfont = XLoadQueryFont(XtDisplay(saccoViewtext),"*-misc-fixed-medium-r-normal--15-140-75-75-c-90-*");
fontlist = XmFontListCreate(viewfont, "chset1");
        
XtVaSetValues( saccoViewtext, XmNfontList, fontlist, NULL );


if((fp = fopen (filePathname, "r+")) == NULL)
{
   strcpy(pathname,"Failed to open File: ");
   strcat(pathname,filePathname);
   
   printf("Open File %s failed \n",filePathname);
   
   ErrMsg( widget, pathname);
   
   return;
}
memset(pathname,'\0',100);
fileSize =  getFilesize(filePathname);

if ( fileSize == 0) 
{
   strcpy(pathname,"There is no text in ");
   strcat(pathname,filePathname);
   
   ErrMsg( widget, pathname);
   printf("There is no text in %s \n",filePathname);
     
   return;

}

ptext = (char *)malloc(fileSize +1 );

memset(pathname,'\0',100);
if ( ! fread( ptext, 1, fileSize, fp))
{
   strcpy(pathname,"Failed to read file: ");
   strcat(pathname,filePathname);
  
   ErrMsg( widget, pathname);
   
   printf("Read File %s failed \n",filePathname);
   
   return;
}


XmTextSetString (saccoViewtext, ptext);
free(ptext);
if(fontlist != NULL)
   XmFontListFree(fontlist);


}



void saccoViewClose_CB (Widget w, XtPointer ptr, XtPointer cbs)

{     
       
        XtUnmanageChild(saccoViewDS); 
        XtDestroyWidget(saccoViewDS); 
       

}

void DeleteWhiteSpace(char *fName)
{


    int  i;
    
    for(i = 0; i < strlen(fName); i++)
    {
    
        if(fName[i] ==' ')
        {      
           fName[i] = '\0';
           break;
        }
    }
    

}

void ErrMsg(Widget widget, char *msg)
{
	static Widget	msgBox;
	Arg		arg[4];
	int		ac;
	
        msgBox = NULL;
	if ( ! msgBox )
	{
		ac = 0;
		XtSetArg(arg[ac], XmNtitle, "Message Window"); ac++;
		XtSetArg(arg[ac], XmNdialogType, XmDIALOG_ERROR); ac++;
		XtSetArg(arg[ac], XmNdialogStyle, XmDIALOG_PRIMARY_APPLICATION_MODAL); ac++; 
	
		
		msgBox = XmCreateMessageDialog(widget, "msgBox", arg, ac);
		XtUnmanageChild(XmMessageBoxGetChild(msgBox, XmDIALOG_HELP_BUTTON));
		XtUnmanageChild(XmMessageBoxGetChild(msgBox, XmDIALOG_CANCEL_BUTTON));
	

	}
	ac = 0;
	XtSetArg(arg[ac], XmNmessageString,
		 XmStringCreateLtoR(msg,
				    (XmStringCharSet)XmFONTLIST_DEFAULT_TAG)); ac++;
	XtSetValues(msgBox, arg, ac);
	
	XtManageChild(msgBox);
	XtPopup(XtParent(msgBox), XtGrabNone);


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/Mods/RCS/show_fileView.c,v $";
 static char rcs_id2[] = "$Id: show_fileView.c,v 1.3 2002/02/11 19:46:26 dws Exp $";}
/*  ===================================================  */

}
