/*******************************************************************************
* FILENAME:             pointcontrol_datasource.c
* NUMBER OF MODULES:    1
* GENERAL INFORMATION:
*    MODULE 1:          get_DatasrcList 
* DESCRIPTION:          Retrieves point data source information from the
*                       TelemType table in the IHFS database. Adds three
*                       additional types - Observer, DCP, and Undefined.
*                       All data source information is stored in the
*                       datasrcbuf member of the pc_petsdata structure.
*                        
* ORIGINAL AUTHOR:      Unknown
* CREATION DATE:        Unknown
* ORGANIZATION:         OHD-11 HSEB
* MACHINE:              HP-UX / Redhat Linux
* MODIFICATION HISTORY:
*   MODULE #        DATE         PROGRAMMER        DESCRIPTION/REASON
*          1        12/31/03     Bryon Lawrence    Added documentation.
********************************************************************************
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "List.h"
#include "pointcontrol_datasource.h"
#include "pointcontrol_options.h"
#include "pointcontrol_pets.h"
#include "TelmType.h"


/*******************************************************************************
* MODULE NUMBER: 1
* MODULE NAME:      get_AdHocDataSourceList 
* PURPOSE:          Retrieves point data source information from the
*                   TelemType table in the IHFS database. Adds three
*                   additional types - Observer, DCP, and Undefined.
*                   All data source information is stored in the
*                   datasrcbuf member of the pc_petsdata structure.
*
* ARGUMENTS:
*   None
*
* RETURNS:
*   DATA TYPE   NAME    DESCRIPTION
*   int         cnt     The number of data sources stored in the 
*                       pc_petsdata structure.
*
* APIs UTILIZED:
*   NAME            HEADER FILE         DESCRIPTION
*   FreeTelmType    TelmType.h          Deallocates the linked list of TelmType
*                                       information.
*   get_PcPETSdata  pointcontrol_pets.h Returns a pointer to the pc_petsdata
*                                       structure.
*   GetTelmType     TelmType.h          Retrieves rows from the TelmType table
*                                       and presents the results as a linked
*                                       list.
*   ListCount       List.h              Returns the number of nodes in a
*                                       linked list.
*   ListFirst       List.h              Returns the first node in a linked
*                                       list.
*
* LOCAL DATA ELEMENTS:
*   DATA TYPE         NAME         DESCRIPTION
*   TelmType        * tHead        A pointer to the head of the linked list
*                                  containing TelmType table rows.
*   TelmType        * tPtr         A pointer to a node in the linked list 
*                                  containing TelmType table information.
*   int		      n            Used to keep track of the number of
*                                  data sources which did not come from the
*                                  TelmType table e.g. Observer.
*   int               cnt          Contains the total of all TelmType and
*                                  non TelmType data sources.
*   int               first        Indicates if this is the first time this
*                                  routine has been called.  If it is, get
*                                  the TelmType table information.
*   pc_options_struct pc_options   Contains the options selected by the user
*                                  on the point data control GUI.
*   pc_pets_struct  * pc_petsdata  Points to the pc_petsdata structure.  This
*                                  structure contains information relevant to
*                                  the user-select PETS information.
*
* DATA FILES AND/OR DATABASE:
* Queries the TelmType table in the IHFS database.
*
* ERROR HANDLING:
*    ERROR CODE                             DESCRIPTION
*              -1                           Either data could not be
*                                           retrieved from the TelmType table 
*                                           or dynamic memory could not be
*                                           allocated for the datasrcbuf member
*                                           of the pc_petsdata structure.
* positive number                           The number of datasources stored
*                                           in the datasrcbug member of the
*                                           pc_petsdata.
********************************************************************************
*/

int get_AdHocDataSourceList()
{
	
   TelmType * tHead = NULL ;
   TelmType * tPtr = NULL ;
   int		n ;
   static int count = 0 ;
   static int first = 1 ;
   pc_pets_struct * pc_petsdata = NULL ;
   pc_options_struct * pc_options = get_pc_options();

   /* Make sure that the DataSource information is loaded only once.
	This should be done the first time this routine is called. */
    if ( first == 1 )
    {
        pc_petsdata = get_pc_petsdata ( ) ;

       /* load Telemtype data into data source list widget
	 and prepend "Observer" and "DCP" items into current list,
	 and append "Undefined" at the end.  This accounts for the
	 3 extra items allocated for below.
	 The entries for this list must all be less than TYPE_LEN
	 specified for the telem type table.  */

       tHead = GetTelmType ( "" ) ;

       if ( tHead == NULL )
       {
           fprintf ( stderr , "\nIn routine \"get_AdHocDataSourceList\":\n"
                            "Could not load Telemtype data.\n" ) ;
           return -1 ;
       }

       count = ListCount(&tHead->list);

       pc_petsdata->adhoc_datasrcbuf = (Char100Array *) malloc 
                                                (sizeof (Char100Array) *
                                                (count + 3));
       n = 0;
       strcpy(pc_petsdata->adhoc_datasrcbuf[n++], OBSERVER_STRING);
       strcpy(pc_petsdata->adhoc_datasrcbuf[n++], DCP_STRING);

       if (tHead != NULL)
       {
           tPtr = ( TelmType * ) ListFirst( & tHead->list ) ;

           while ( tPtr )
    	   {
		       strcpy(pc_petsdata->adhoc_datasrcbuf[n++], tPtr->type);
		       tPtr = (TelmType *) ListNext (&tPtr->node);
       	   }

    	   FreeTelmType(tHead);
       }

       strcpy(pc_petsdata->adhoc_datasrcbuf[n++], UNDEFINED_STRING);

       count = n ;
       first = 0 ;
    }   
    
    
    
    /* now allocate instances of datasource for later use with pc_options */
    
    if ( pc_options->data_sources_chosen_array == NULL )
    {
        pc_options->data_sources_chosen_array = (DataSource *)
                                      malloc(sizeof(DataSource) *  count);
    
        if ( pc_options->data_sources_chosen_array == NULL )
        {
            fprintf ( stderr , "\nIn routine 'get_AdHocADataSourceList':\n"
                               "Could not allocate memory for the array of\n"
                               "data sources.\n" ) ;
            return -1 ;
        }
    
        pc_options->data_sources_chosen_count = 0 ;
    }
     
    
     
    return count ;
}
