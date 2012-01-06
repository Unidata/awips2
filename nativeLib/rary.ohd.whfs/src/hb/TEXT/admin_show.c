/*
	File:		admin_show.c
	Date:		11/09/1994
	Author:		Dale Shelton
	
	Purpose:	Provide support for the Administration DS.
	
*/


#include <Xm/Xm.h>
#include <Xm/Protocols.h>
#include <Xm/PushB.h>
#include <Xm/Text.h>
#include "Admin.h"
#include "DbmsUtils.h"
#include "Xtools.h"
#include "admin_cbs.h"
#include "admin.h"
#include "hybase.h"

#include "time_convert.h"
/****
#include "sqlhdr.h"
***/   

void	ShowAdmDs(Widget w)
{
	Atom		atom;
	
	
	if (! admDS)
	{
		create_admDS(GetTopShell(w));
		
		/*
			Add callbacks.
		*/
		atom = XmInternAtom(XtDisplay(admDS), "WM_DELETE_WINDOW", False);
		XmAddWMProtocolCallback(admDS, atom, admin_close, NULL);
		XtAddCallback(admokPB, XmNactivateCallback, admin_save, NULL);
		XtAddCallback(admclosePB, XmNactivateCallback, admin_close, NULL);
		
		XtAddCallback(aphoneTxt,    XmNmodifyVerifyCallback, (XtCallbackProc)num_filter, (XtPointer)INTEGERS_AND_HYPHENS);
		XtAddCallback(tenyrTxt,     XmNmodifyVerifyCallback, (XtCallbackProc)num_filter, (XtPointer)INTEGERS_AND_SLASHES);
		XtAddCallback(oneyrTxt,     XmNmodifyVerifyCallback, (XtCallbackProc)num_filter, (XtPointer)INTEGERS_AND_SLASHES);
		XtAddCallback(acd404Txt,    XmNmodifyVerifyCallback, (XtCallbackProc)alphanum_filter, (XtPointer)UPPERCASE_AND_HYPHENS);
		XtAddCallback(aregionnoTxt, XmNmodifyVerifyCallback, (XtCallbackProc)num_filter, (XtPointer)INTEGERS);
		XtAddCallback(apasswordTE,  XmNmodifyVerifyCallback, (XtCallbackProc)alphanum_filter, (XtPointer)MIXEDCASE);
	}
	
	if (! XtIsManaged(admDS))
	{
		XtManageChild(admFM);
		XtManageChild(admDS);
		admin_load();
	}
	
	return;
}



void	admin_save(Widget w, XtPointer ptr, XtPointer cbs)
{
	Admin		admin;
	char		msg[MAX_BUF_LEN],
			*buf;
	date_t	date;
	/* DbStatus *dbStatusPtr = NULL ; */

	
	/*
		Initialize the admin data structure.
	*/
	memset(&admin, '\0', sizeof(admin));
	
	
        /*
                Associate XmText widget values with
                the appropriate field in the
                Reservoir struct.
        */
        if ( (buf = XmTextGetString(hnameTxt)) )
	{
        	if (IsNull(CHAR, buf) == NOTNULL)
                	strcpy(admin.focalpoint, buf);
		XtFree(buf);
	}


        if ( (buf = XmTextGetString(acd404Txt)) )
	{
        	if (IsNull(CHAR, buf) == NOTNULL)
                	strcpy(admin.cd404, buf);
		XtFree(buf);
	}


        if ( (buf = XmTextGetString(aofcTxt)) )
	{
        	if (IsNull(CHAR, buf) == NOTNULL)
                	strcpy(admin.ofc, buf);
 		XtFree(buf);
	}


        if ( (buf = XmTextGetString(aphoneTxt)) )
	{
        	if (IsNull(CHAR, buf) == NOTNULL)
                	strcpy(admin.phone, buf);
 		XtFree(buf);
	}


        if ( (buf = XmTextGetString(aregionTxt)) )
	{
        	if (IsNull(CHAR, buf) == NOTNULL)
                	strcpy(admin.region, buf);
		XtFree(buf);
	}


        if ( (buf = XmTextGetString(aregionnoTxt)) )
	{
        	if (IsNull(CHAR, buf) == NOTNULL)
                	strcpy(admin.regno, buf);
 		XtFree(buf);
	}

        if ( (buf = XmTextGetString(ahsanumTxt)) )
	{
        	if (IsNull(CHAR, buf) == NOTNULL)
                	admin.hsa_num = atoi(buf);
 		XtFree(buf);
	}


        if ( (buf = XmTextGetString(ahsaTxt)) )
	{
        	if (IsNull(CHAR, buf) == NOTNULL)
                	strcpy(admin.hsa, buf);
 		XtFree(buf);
	}


        if ( (buf = XmTextGetString(apasswordTE)) )
	{
        	if (IsNull(CHAR, buf) == NOTNULL)
                	strcpy(admin.hb_password, buf);
 		XtFree(buf);
	}


   if ( (buf = XmTextGetString(tenyrTxt)) )
	{
      if ( strlen(buf) == 0)
      {
         SetNull(LONG, (void *) &admin.tenyr);
      }
		else if (four_digit_year(buf) != 0 )
		{
		  sprintf(msg, "Invalid date '%s' entered, 4 digit year required.\n", 
			  buf);
		  ErrorDialog(admDS, msg);
		  /* leave function, do not save data */
		  return;
		}
		else
		{
        	  if ( USA_date_to_date_t(buf, &date) != 0 )
        	  {
		    sprintf(msg, "Invalid date '%s' entered, check month and day.\n", 
			    buf);
		    ErrorDialog(admDS, msg);
		    /* leave function, do not save data */
		    return;
        	  }
        	  else
        	    admin.tenyr = date;
        	}
        	
        	XtFree(buf);
	}


        if ( (buf = XmTextGetString(oneyrTxt)) )
        {
           if ( strlen(buf) == 0)
           {
              SetNull(LONG, (void *) &admin.oneyr);
           }
	        else if (four_digit_year(buf) != 0 )
           {
              sprintf(msg, "Invalid date '%s' entered, 4 digit year required.\n", 
                      buf);
              ErrorDialog(admDS, msg);
              /* leave function, do not save data */
              return;
           }
           else
           {
              if ( USA_date_to_date_t(buf, &date) != 0 )
              {
                 sprintf(msg, "Invalid date '%s' entered, check month and day.\n", 
                         buf);
                 ErrorDialog(admDS, msg);
                 /* leave function, do not save data */
                 return;
              }
              else
                 admin.oneyr = date;
           }
        	
		     XtFree(buf);
        }
	

	/*
		Update database.
	*/
	if (recordCount("admin", " ") > 0)
		UpdateAdmin(&admin, " ");
	else
		PutAdmin(&admin);
      
/**** This is how you get and print SQL codes
   dbStatusPtr = GetAdminDbStatus();
   printf("sqlcode = %ld\n", dbStatusPtr->sql_code);   
****/
	return;
}


void	admin_close(Widget w, XtPointer ptr, XtPointer cbs)
{
   if(XtIsManaged(admDS))
   {
      XtDestroyWidget(admDS);
      admDS = NULL;
   }
   
   return;
}



void	admin_load(void)
{
	Admin	*admin;
	char	time_buf[MAX_BUF_LEN];
   char  hsa_num[4];

	XtRemoveCallback(ahsanumTxt,XmNmodifyVerifyCallback, (XtCallbackProc)num_filter, (XtPointer)INTEGERS);
	
	admin = GetAdmin("");
	if (ListCount(&admin->list))
	{	
		XmTextSetString(hnameTxt,     admin->focalpoint);
		XmTextSetString(aofcTxt,      admin->ofc);
		XmTextSetString(aphoneTxt,    admin->phone);
		XmTextSetString(aregionTxt,   admin->region);
		XmTextSetString(aregionnoTxt, admin->regno);
		XmTextSetString(acd404Txt,    admin->cd404);
		XmTextSetString(ahsaTxt,      admin->hsa);
		XmTextSetString(apasswordTE,  admin->hb_password);
		
      DataToString(&admin->hsa_num, SHORT, hsa_num, "%3d", "");
		XmTextSetString(ahsanumTxt,   hsa_num);

		date_t_to_USA_date ( admin->tenyr, time_buf );
		XmTextSetString(tenyrTxt, time_buf);
		
		date_t_to_USA_date ( admin->oneyr, time_buf );
		XmTextSetString(oneyrTxt, time_buf);
		
		
		/*
			Free memory.
		*/
		FreeAdmin(admin);
	}

	XtAddCallback(ahsanumTxt,   XmNmodifyVerifyCallback, (XtCallbackProc)num_filter, (XtPointer)INTEGERS);
	
	return;
}

