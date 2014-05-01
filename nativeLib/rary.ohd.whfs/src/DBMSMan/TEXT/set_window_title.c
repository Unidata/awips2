/*

       File:                       set_window_title.c
       Date of last revision:      03/05/2003
       Author:                     Gautam Sood
       
       Purpose:                    This file contains the function that 
                                   set's the window title to the LID and
                                   station name.
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include "set_window_title.h"
#include "Location.h"

void set_title(Widget w, char *title, char *lid)
{

      char        where[MAX_WHERE_LEN],
                  buf[MAX_BUF_LEN];
      Location *loc;


      memset(buf,'\0', sizeof(buf));
      if (*title)
         strcpy(buf, title);
 
      strcat(buf, "  -  ");
      strcat(buf, lid);
      strcat(buf, " - ");
     
      sprintf(where, " WHERE lid = '%s' ", lid);
      if ((loc = GetLocation(where)) != NULL)
        if (loc->name)
          strcat(buf, loc->name);
      FreeLocation(loc);
      
      SetTitle(w, buf);
      return;
}
