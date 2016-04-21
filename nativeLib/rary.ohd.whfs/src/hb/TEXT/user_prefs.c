/*
	File:		user_prefs.c
	Date:		11/28/1994
	Author:		Dale Shelton
	
	Purpose:	This file contains the functions
			that implement management of HydroBase
			preferences for the current user.
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include "GeneralUtil.h"
#include "hybase.h"
#include "Location.h"
#include "UserPrefs.h"
#include "user_prefs.h"
#include "Xtools.h"

long	get_preference(const char *pref)
{
	UserPrefs	*up;
	char		where[MAX_WHERE_LEN],
	   		*userid;
	long		val = 0 ;
	
	
	if ((userid = (char *)getlogin()) != NULL)
	{
		sprintf(where, " WHERE userid = '%s' ", userid);
		if ((up = GetUserPrefs(where)) != NULL)
		{
			if (strcmp(TITLE, pref) == 0)
			   	val = up->title;
			
			if (strcmp(SORT_LIST, pref) == 0)
			   	val = up->sortlist;
			
			if (strcmp(FIELD_LIST, pref) == 0)
			   	val = up->fieldlist;
			
			FreeUserPrefs(up);
		}
	}
	
	return(val);
}


/*
	Set the value of the TITLE preference
	variable.
*/
void	set_preference(const char *pref, long code)
{
   UserPrefs	*up;
   
   char		where[MAX_WHERE_LEN],
      		*userid;
   
   int		update;
   
   update = False;
   if ((userid = (char *)getlogin()) != NULL)
   {
      sprintf(where, " WHERE userid = '%s' ", userid);
      if ((up = GetUserPrefs(where)) != NULL)
      {
	 update = True;
      }
      else
      {
	 up = (UserPrefs *) malloc(sizeof(UserPrefs));
	 strcpy(up->userid, userid);
      }
      
      if (strcmp(pref, TITLE) == 0)
	 up->title = code;
      
      if (strcmp(pref, SORT_LIST) == 0)
	 up->sortlist = code;
      
      if (strcmp(pref, FIELD_LIST) == 0)
	 up->fieldlist = code;
      
      
      if (update)
	 UpdateUserPrefs(up, where);
      else
	 PutUserPrefs(up);
      
      
      /*
      	  Free any allocated memory.
      */
      if (update)
         FreeUserPrefs(up);
   }
   
   return;
}


long	get_title_preference(void)
{
	return(get_preference(TITLE));
}


void	set_title_preference(long code)
{
	set_preference(TITLE, code);
	return;
}


long	get_sort_preference(void)
{
	return(get_preference(SORT_LIST));
}


void	set_sort_preference(long code)
{
   	set_preference(SORT_LIST, code);
 	return;  
}


long	get_field_preference(void)
{
	return(get_preference(FIELD_LIST));
}


void	set_field_preference(long code)
{
 	set_preference(FIELD_LIST, code);
	return;
}


void	set_window_title(Widget w, char *title, char *lid)
{
	Location	*loc;
	char		where[MAX_WHERE_LEN],
			buf[MAX_BUF_LEN];
	long		code;
	
	
	/*
		Set memory buf to initial
		starting state.
	*/
	memset(buf, '\0', sizeof(buf));
	if (*title)
		strcpy(buf, title);
	
	
	/*
		Get user prefs for window title
		and make determination about the
		appropriate title string.		
	*/
	
	
	if (*lid)
	{
		code = get_title_preference();
			
		if (code & TITLE_HB5)
		{
			strcat(buf, "  -  ");
			strcat(buf, lid);
		}
	
		/*
			Determine if name flag is set.
		*/	
		if (code & TITLE_NAME)
		{
			sprintf(where, " WHERE lid = '%s' ", lid);
			if ((loc = GetLocation(where)) != NULL)
			{
				if (loc->name)
				{
					strcat(buf, " - ");
					strcat(buf, loc->name);
				}
			
				/*
					Free any alloc'd memory.
				*/
				FreeLocation(loc);
			}	
		}
	}
		
	
	/*
		Set window title and return.
	*/
	SetTitle(w, buf);
	return;
}

void	set_main_window_title(Widget w, const char *appName)
{
	char		buf[BUFSIZ],
			dataBaseName[128],
			serverName[128];	
	int		gad_token_len=0, gad_value_len=0;
	
	
	
	/*
		Set the variable to NULL 
	*/
	memset(dataBaseName, '\0', sizeof(dataBaseName) );
	memset(serverName, '\0', sizeof(serverName) );
	
	
	/*
		get the environmental variables
	*/
	gad_token_len = strlen("db_name");
	get_apps_defaults("db_name", &gad_token_len, dataBaseName, &gad_value_len);
	
	
	/*
		Copy the application name, database name, and serverName
	*/
	strcpy(buf, appName);
	strcat(buf, " on ");
	strcat(buf, dataBaseName);
	
	
	/*
		Set window title and return.
	*/
	SetTitle(w, buf);
	return;
}
