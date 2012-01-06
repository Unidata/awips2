
/*******************************************************************************
* FILENAME:          damDisplayControl_show.c
* NUMBER OF MODULES:   ??
* GENERAL INFORMATION:
********************************************************************************
*/

#include <dirent.h>
#include <memory.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>

#include <X11/Intrinsic.h>
#include <Xm/Xm.h>
#include <Xm/Form.h>
#include <Xm/ScrolledW.h>

#include "ArealDisplayControl.h"
#include "ArealDataAttr.h"
#include "damDisplayControl.h"
#include "damDisplayControl_show.h"

#include "GeneralUtil.h"
#include "hv_mainCallbacks.h"
#include "List.h"
#include "map_defines.h"
#include "map_library.h"
#include "map_resource.h"
#include "Xtools.h"
#include "drawDams.h"
#include "DamMaster.h"

#define TEXT_BOX_SIZE 10

static int drawDamFlag = 0 ; /* By default dams are not plotted. */
static Boolean showDamId   = False ; /* By default dam icons do not have ID labels. */
static Boolean showDamName = True ; /* By default dam icons have name labels. */
static Boolean showDamIcon = True ; /* By default dam icons are shown. */


/* define variable which is used as external by the
   drawDams functions to pass its data outside */

DamReportList * damreportlistHead = NULL ;

/*******************************************************************************
*/
void dam_display_show ( Widget w )
{
   
   static char * center_lat_token = "hv_center_lat" ;
   static char * center_lon_token = "hv_center_lon" ;
   int reply_len ;
   int request_len ;
   int status = 0 ;
   char latlon_value [ 20 ] ;

    if ( ( damDisplayControlDS == NULL ) || ( ! damDisplayControlDS ) )
    {

        /* create all the widgets */
        create_damDisplayControlDS ( GetTopShell ( w ) );

        /* set up the callbacks */
        dam_dc_AddCallbacks( );
        
        /* set toggle button defaults */
        XmToggleButtonSetState ( damDisplayIdTB, False, False );	
        XmToggleButtonSetState ( damDisplayNameTB, True, False );	
        XmToggleButtonSetState ( damDisplayIconTB, True, False );	
        XmToggleButtonSetState ( enableLatLonTB, False, False );	

        /* set Dam Volume text box initially to 100,000 */
        XmTextSetString ( damFilterValueTE , "100000" ) ;

        /* set Lat and Lon Offset text boxes initially to 1 */
        XmTextSetString ( damLatOffsetTE , "2.0" ) ;
        XmTextSetString ( damLonOffsetTE , "2.0" ) ;

        request_len = strlen ( center_lat_token ) ;
        status = get_apps_defaults ( center_lat_token , & request_len ,
                                     latlon_value , & reply_len ) ;
        XmTextSetString ( damLatCenterTE , latlon_value ) ;

        request_len = strlen ( center_lon_token ) ;
        status = get_apps_defaults ( center_lon_token , & request_len ,
                                     latlon_value , & reply_len ) ;
        XmTextSetString ( damLonCenterTE , latlon_value ) ;
    }
   
    XtManageChild ( damDisplayControlDS ) ; 
    XtManageChild ( damDisplayControlFO ) ;

    XtPopup( damDisplayControlDS , XtGrabNone ) ;

    
    return ;
}

/******************************************************************************/

void dam_dc_AddCallbacks ( )
{
   Atom		atom ;
   
   /* Window manager callbacks. */
   atom = XmInternAtom(XtDisplay(damDisplayControlDS),
		       "WM_DELETE_WINDOW", False);
   XmAddWMProtocolCallback(damDisplayControlDS, atom,
			   dam_close_display, NULL);
   
   /* Close button callback */
   XtAddCallback(dam_closeDisplayPB, XmNactivateCallback,
                 dam_close_display, NULL);
		 
   /* Map Data button callback */
   XtAddCallback(dam_mapDisplayPB, XmNactivateCallback,
                 dam_MapData, NULL);

   /* Clear Data button callback */
   XtAddCallback(dam_clearDisplayPB , XmNactivateCallback ,
                 dam_clear_display , NULL ) ;

   /* Display option toggle button callbacks */
   XtAddCallback(damDisplayIdTB , XmNvalueChangedCallback,
                 setDisplayOptions, NULL ) ;
   XtAddCallback(damDisplayNameTB, XmNvalueChangedCallback,
                 setDisplayOptions, NULL);
   XtAddCallback(damDisplayIconTB, XmNvalueChangedCallback,
                 setDisplayOptions, NULL);

   XtAddCallback(damFilterValueTE, XmNmodifyVerifyCallback,
                (XtCallbackProc)num_filter, (XtPointer)INTEGERS_AND_DECIMALS);

   XtAddCallback(damLatCenterTE, XmNmodifyVerifyCallback,
                (XtCallbackProc)num_filter, (XtPointer)INTEGERS_AND_DECIMALS);

   XtAddCallback(damLonCenterTE, XmNmodifyVerifyCallback,
                (XtCallbackProc)num_filter, (XtPointer)INTEGERS_AND_DECIMALS_AND_HYPHENS);

   XtAddCallback(damLatOffsetTE, XmNmodifyVerifyCallback,
                (XtCallbackProc)num_filter, (XtPointer)INTEGERS_AND_DECIMALS);

   XtAddCallback(damLonOffsetTE, XmNmodifyVerifyCallback,
                (XtCallbackProc)num_filter, (XtPointer)INTEGERS_AND_DECIMALS);

   return;
}

/******************************************************************************/
void setDisplayOptions ( Widget w , XtPointer clientdata , 
                                XtPointer calldata )
{

   if ( XmToggleButtonGetState(damDisplayIdTB) )
      showDamId = True;
   else
      showDamId = False;
      
   if ( XmToggleButtonGetState(damDisplayNameTB) )
      showDamName = True;
   else
      showDamName = False;
   
   if ( XmToggleButtonGetState(damDisplayIconTB) )
      showDamIcon = True;
   else
      showDamIcon = False;

}


/******************************************************************************/
void dam_close_display ( Widget w , XtPointer clientdata , 
                                XtPointer calldata )
{
    XtPopdown ( damDisplayControlDS ) ;
    return ;
}

/******************************************************************************/

void dam_clear_display ( Widget w , XtPointer clientdata ,
                                XtPointer calldata )
{

   /* set the cursor to a watch while the request is being processed */
   SetCursor ( w , XC_watch ) ;
   
   drawDamFlag = 0 ;

   /* Call the redraw function to re paint the map without Dam icons */
   redrawMap();

   /* Clear the watch cursor. */
   UnsetCursor ( w ) ;

}

/*****************************************************************************
   dam_MapData()
   callback for Map Data pushbutton
   **************************************************************************/
void dam_MapData ( Widget w , XtPointer clientdata ,
                          XtPointer calldata )
{

   DamMaster *dmHead = NULL ;
   DamMaster *dmNode = NULL ;
   
   char ndid [ NID_ID_LEN + 1 ] ;
   char name [ DAMREPORTLIST_NAME_LEN + 1 ] ;
   char * whereClause ;
   static char previousWhereClause [ BUFSIZ ] = " ";
   double lat = 0.0 ;
   double lon = 0.0 ;
   int returnCode = 0 ;

   /* set the cursor to a watch while the request is being processed */
   SetCursor ( w , XC_watch ) ;
   
   drawDamFlag = 1 ;

   /* Process the user request by reading gui options and retrieving data. */
   
   whereClause = buildWhereClause();
   /* if the where clause has changed then query the database */
   if ( strcmp(whereClause, previousWhereClause) != 0 )
   {
      strcpy (previousWhereClause, whereClause);

      if ( damreportlistHead != NULL )
      {
         FreeDamReportList( damreportlistHead );
         damreportlistHead = NULL;
      }

      returnCode = switchDatabases ( DAMCAT_DATABASE );
      if (returnCode == 0)
      {
         if ( ( dmHead = GetDamMaster(whereClause) ) != NULL )
         {
            printf("Number of Dams retrieved=%d\n", ListCount(&dmHead->list));

            dmNode = ( DamMaster * ) ListFirst ( & dmHead->list ) ;

            while ( dmNode != NULL )
            {
               strncpy(ndid, dmNode->nidid, NID_ID_LEN);
               strncpy(name, dmNode->dam_name, DAMREPORTLIST_NAME_LEN);
               lat = dmNode->latitude_dam ;
               lon = dmNode->longitude_dam ;

               damreportlistHead = dude( damreportlistHead, ndid, name, lat, lon );

               dmNode = ( DamMaster * ) ListNext ( & dmNode->node ) ;
            }
      
            FreeDamMaster(dmHead);
            dmHead = NULL;

         } /* if the head of the DamMaster linked list is not null */

         returnCode = switchDatabases ( IHFS_DATABASE );
         if (returnCode != 0)
            printf("Unable to open the IHFS database!\n");

      } /* if switching to DamCrest database was successful */
      else
         printf("Unable to open the DamCrest database!\n");
      
   } /* if where clause changed */
 

   /* Display the retrieved data.  This is done by calling the redraw function */
   redrawMap();

   /* Clear the watch cursor. */
   UnsetCursor ( w ) ;

   return ;
}

/******************************************************************************/

DamReportList * dude( DamReportList *inputPtr,
                      char *damId,
                      char *damName,
                      double damLat,
                      double damLong)
{

   DamReportList	* onedamreportPtr = NULL;
   DamReportList	* outputPtr = NULL;

   if ((onedamreportPtr = (DamReportList *)malloc(sizeof(DamReportList))) != NULL)
   {      
      /* copy fields */
	 
      /* Copy the nidid and dam name into the current Dam report structure. */
      memset  ( onedamreportPtr->nidid , '\0', NID_ID_LEN + 1 ) ;
      strncpy ( onedamreportPtr->nidid , damId , NID_ID_LEN ) ;
      memset  ( onedamreportPtr->name , '\0', DAMREPORTLIST_NAME_LEN + 1 ) ;
      strncpy ( onedamreportPtr->name , damName , DAMREPORTLIST_NAME_LEN ) ;

      onedamreportPtr->use = 1 ;

      /* Copy the latitude and longitude into the current report
         structure being processed. */
      onedamreportPtr->latitude = damLat ;
      onedamreportPtr->longitude = damLong ;


		/* initialize the list */

		if (inputPtr == NULL)
		{
		    outputPtr = onedamreportPtr;
		    ListInit(&outputPtr->list);
		}
		else
		    outputPtr = inputPtr;


		/* add the data to the list */
	 
		ListAdd(&outputPtr->list, &onedamreportPtr->node);
	}
	else
	{
		fprintf(stderr, "Error allocating for DamReportList.\n");
	}

   return outputPtr ;

}

/******************************************************************************/

char * buildWhereClause( )
{
   char damVolume [ TEXT_BOX_SIZE + 1 ] ;
   static char where_clause [ BUFSIZ ] ;
   char *valstr=NULL;			/* Used to get string from text widget */
   char *filterLatLon;


   memset ( damVolume, '\0', TEXT_BOX_SIZE + 1 ) ;
	if ( (valstr = XmTextGetString(damFilterValueTE)) )
	{
		strcpy(damVolume, valstr);
		XtFree(valstr);
		valstr=NULL;
	}

	switch(GetMenuPos(damFilterOperatorOM))
	{
   case 0:
      sprintf ( where_clause , " WHERE max_storage > %s ", damVolume );
      break;
   
   case 1:
      sprintf ( where_clause , " WHERE max_storage = %s ", damVolume );
      break;
   
   case 2:
      sprintf ( where_clause , " WHERE max_storage < %s ", damVolume );
      break;
   
   case 3:
      sprintf ( where_clause , " " );
      break;
   
   }

   if ( XmToggleButtonGetState(enableLatLonTB) )
   {
      filterLatLon = buildLatLonFilter(); 
      if ( strcmp (filterLatLon, "") != 0)
      {
         if ( GetMenuPos(damFilterOperatorOM) == 3 )
            strcat ( where_clause , "WHERE ");
         else
            strcat ( where_clause , "AND ");

         strcat ( where_clause , filterLatLon);
      }
   }

   return where_clause;

}

/******************************************************************************/

char * buildLatLonFilter ( )
{
   static char filterString [ BUFSIZ ] ;

   char latCenterText [ TEXT_BOX_SIZE + 1 ] = "";
   char latOffsetText [ TEXT_BOX_SIZE + 1 ] = "";
   char lonCenterText [ TEXT_BOX_SIZE + 1 ] = "";
   char lonOffsetText [ TEXT_BOX_SIZE + 1 ] = "";
   char *valstr=NULL;			/* Used to get string from text widget */

   double latCenter = 0;
   double latOffset = 0;
   double lonCenter = 0;
   double lonOffset = 0;
   double southernLat = 0;
   double northernLat = 0;
   double easternLon = 0;
   double westernLon = 0;

   memset ( latCenterText, '\0', TEXT_BOX_SIZE + 1 ) ;
	if ( (valstr = XmTextGetString(damLatCenterTE)) )
	{
		strcpy(latCenterText, valstr);
		XtFree(valstr);
		valstr=NULL;
	}
   
   memset ( latOffsetText, '\0', TEXT_BOX_SIZE + 1 ) ;
	if ( (valstr = XmTextGetString(damLatOffsetTE)) )
	{
		strcpy(latOffsetText, valstr);
		XtFree(valstr);
		valstr=NULL;
	}
   
   memset ( lonCenterText, '\0', TEXT_BOX_SIZE + 1 ) ;
	if ( (valstr = XmTextGetString(damLonCenterTE)) )
	{
		strcpy(lonCenterText, valstr);
		XtFree(valstr);
		valstr=NULL;
	}
   
   memset ( lonOffsetText, '\0', TEXT_BOX_SIZE + 1 ) ;
	if ( (valstr = XmTextGetString(damLonOffsetTE)) )
	{
		strcpy(lonOffsetText, valstr);
		XtFree(valstr);
		valstr=NULL;
	}

   latCenter = atof ( latCenterText );
   latOffset = atof ( latOffsetText );
   lonCenter = atof ( lonCenterText );
   lonOffset = atof ( lonOffsetText );
   
   southernLat = latCenter - latOffset;
   northernLat = latCenter + latOffset;
   easternLon = lonCenter + lonOffset;
   westernLon = lonCenter - lonOffset;
   
   sprintf ( filterString, " latitude_dam  >= %.2f AND latitude_dam  <= %.2f AND "
                           " longitude_dam >= %.2f AND longitude_dam <= %.2f ",
                           southernLat, northernLat, westernLon, easternLon );


   return filterString;

}

/***********************************************************************/

int switchDatabases ( int databaseToOpen )
{
   static char * db_name_token = "db_name" ;
   static char * damcat_db_name_token = "damcat_db_name" ;

   int reply_len ;
   int request_len ;
   int status = 0 ;
   int returnCode = 0 ;
   char db_name [ 20 ] ;
   char damcat_db_name  [ 20 ] ;
   char databaseName  [ 20 ] ;


   if ( databaseToOpen == IHFS_DATABASE )
   {
      /* Get the name of the IHFS database. */
      request_len = strlen ( db_name_token ) ;

      status = get_apps_defaults ( db_name_token , & request_len ,
                                   db_name , & reply_len ) ;

      if ( status != 0 )
      {
         fprintf ( stdout , "Could not retrieve name of IHFS database.\n" ) ;
         return 1 ;
      }
      else
         strcpy ( databaseName, db_name);
   }
   else
   {
      /* Get the name of the DamCrest database. */
      request_len = strlen ( damcat_db_name_token ) ;

      status = get_apps_defaults ( damcat_db_name_token , & request_len ,
                                   damcat_db_name , & reply_len ) ;

      if ( status != 0 )
      {
         fprintf ( stdout , "Could not retrieve name of DamCrest database.\n" ) ;
         return 1 ;
      }
      else
         strcpy ( databaseName, damcat_db_name);

   }
   
   /* Connect to the IHFS or DamCrest database. */
   status = OpenDbms ( databaseName ) ;  

   if ( status != 0  )
   {
      printf ( "The call to routine \"OpenDbms\" returned a\n"
               "status code of %d trying to open %s\n" , status, databaseName ) ;
      returnCode = 1 ;
   }

   return returnCode ;
}

/***********************************************************************/

int getDamDrawingState ( )
{
   return drawDamFlag ;
}

/***********************************************************************/

Boolean showDamIdLabel ( )
{
   return showDamId ;
}

/***********************************************************************/

Boolean showDamNameLabel ( )
{
   return showDamName ;
}

/***********************************************************************/

Boolean showDamIconSymbol ( )
{
   return showDamIcon ;
}

/*****************************************************************************
   FreeDamReportList()
   ***************************************************************************/
void FreeDamReportList ( DamReportList * sp )
{
   DamReportList * nextPtr = NULL ;

   if ( sp != NULL )
   {
      sp = ( DamReportList * ) ListFirst ( & sp->list ) ;

      while ( sp != NULL )
      {
         nextPtr = ( DamReportList * ) ListNext ( & sp->node ) ;
         free ( ( void * ) sp ) ;
         sp = nextPtr ;
      }
   }
}
