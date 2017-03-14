/************************************************************************
   preset_saveupdate_show.c
   
   PURPOSE   
   Defines and contains the callbacks for the preset save or update dialog shell.
   
 *********************************************************************/

#include <stdlib.h>
#include <Xm/Xm.h>
#include <Xm/Protocols.h>
#include <Xm/Text.h>

#include "DbmsDefs.h"
#include "geoutil.h"
#include "pointcontrol.h"
#include "pointcontrol_mgr.h"
#include "pointcontrol_show.h"
#include "PointDataPresets.h"
#include "preset_saveupdate.h"
#include "preset_saveupdate_show.h"
#include "Xtools.h"

static PointDataPresets * pPresetHead = NULL ;
 
static void ok_saveasnewCB(Widget w, XtPointer xtptr1, XtPointer xtptr2)
{
   Boolean match_found = False ;
   char * edited_preset_descr = NULL ;
   char * edited_preset_id = NULL ;
   char * edited_preset_rank = NULL ;
   char * unedited_preset_id  = NULL ;
   char * preset_string = NULL ;
   char where [ 100 ] ;
   int status ;
   PointDataPresets * pPresetNode = NULL ;
   PointDataPresets presetNode ;

   /* Build a PointDataPresetNode. Strip off all unecessary leading and
      trailing spaces. Check for invalid or blank values. */
   edited_preset_id = XmTextGetString ( saveupdate_idTX ) ;

   strip_lblanks ( edited_preset_id ) ;
   strip_tblanks ( edited_preset_id ) ;

   if ( strlen ( edited_preset_id ) == 0 )
   {
      ErrorDialog ( w , "Invalid Preset Identifier." ) ;
      XtFree ( edited_preset_id ) ;
      edited_preset_id = NULL ;
      return ;
   }

   edited_preset_descr = XmTextGetString ( saveupdate_desTX ) ;

   strip_lblanks ( edited_preset_descr ) ;
   strip_tblanks ( edited_preset_descr ) ;

   if ( strlen ( edited_preset_descr ) == 0 )
   {
      ErrorDialog ( w , "Invalid Preset Description." ) ;
      XtFree ( edited_preset_descr ) ;
      edited_preset_descr = NULL ;
      XtFree ( edited_preset_id ) ;
      edited_preset_id = NULL ;
      return ;
   }

   edited_preset_rank = XmTextGetString ( saveupdate_rankTX ) ; 

   strip_lblanks ( edited_preset_rank ) ;
   strip_tblanks ( edited_preset_rank ) ;

   if ( strlen ( edited_preset_rank ) == 0 )
   {
      ErrorDialog ( w , "Invalid Preset Rank." ) ;
      XtFree ( edited_preset_rank ) ;
      edited_preset_rank = NULL ;
      XtFree ( edited_preset_descr ) ;
      edited_preset_descr = NULL ;
      XtFree ( edited_preset_id ) ;
      edited_preset_id = NULL ;
      return ;
   }

   /* Get the preset_id of the currently selected  preset_group. */
   unedited_preset_id = XmTextGetString ( saveupdate_curidTX ) ;

   strip_lblanks ( unedited_preset_id ) ;
   strip_tblanks ( unedited_preset_id ) ;

   /* Retrieve the current setting on the pointdata control GUI.
      These will be represented as a string of option/value pairs. */

   set_timefields ( ) ;
   preset_string = build_pointdata_preset_string_from_options ( ) ;

   if ( preset_string != NULL )
   {
       memset ( presetNode.preset_id , '\0' , PRESET_ID_LEN + 1 ) ; 
       memset ( presetNode.descr , '\0' , PRESET_DESCR_LEN + 1 ) ;
       memset ( presetNode.preset_string , '\0' , PRESET_STRING_LEN + 1 ) ;

       strncpy ( presetNode.preset_id , edited_preset_id , PRESET_ID_LEN ) ; 
       strncpy ( presetNode.descr , edited_preset_descr , PRESET_DESCR_LEN ) ;
       strncpy ( presetNode.preset_string , preset_string , 
                 PRESET_STRING_LEN ) ;
       presetNode.preset_rank = atoi ( edited_preset_rank ) ;

      /* Loop over the linked list of PointDataPresets structures. Compare
         each preset id with the one the user has edited.  If there is a
         match than do a upate.  Otherwise, perform an insert. */
      if ( pPresetHead != NULL )
      {
         pPresetNode = ( PointDataPresets * ) 
                       ListFirst ( & pPresetHead->list ) ; 

         while ( pPresetNode != NULL )
         {
            status = strcmp ( pPresetNode->preset_id , edited_preset_id ) ;

            if ( status == 0 )
            {
               match_found = True ;
               break ;
            }

            pPresetNode = ( PointDataPresets * )
                          ListNext ( & pPresetNode->node ) ;
         }
      }

      if ( match_found == True )
      {
         /* Perform an update */
         sprintf ( where , " WHERE preset_id = '%s' " , edited_preset_id ) ;
         status = UpdatePointDataPresets ( & presetNode , where ) ;

         if ( status != 0 )
         {
            fprintf ( stderr , "\nIn routine 'ok_saveasnewCB':\n"
                               "Could not update the record in the\n"
                               "PointDataPresets table. SQLCODE = %d\n" ,
                               status  ) ;
            ErrorDialog ( w , "Could not update the preset group in the\n"
                              "PointDataPresets table.\n" ) ;
         }

      }
      else
      {
         /* Perform an insert */
         status = PutPointDataPresets ( & presetNode ) ;
       
         if ( status != 0 )
         {
            fprintf ( stderr , "\nIn routine 'ok_saveasnewCB':\n"
                               "Could not insert the record in the\n"
                               "PointDataPresets table. SQLCODE = %d\n" ,
                               status ) ;
            ErrorDialog ( w , "Could not insert the preset group into the\n"
                              "PointDataPresets table.\n" ) ;
         }

      }

      if ( preset_string != NULL )
      {
          free ( preset_string ) ;
          preset_string = NULL  ;
      }
   }
  
   XtFree ( edited_preset_rank ) ;
   edited_preset_rank = NULL ;
   XtFree ( edited_preset_descr ) ;
   edited_preset_descr = NULL ;
   XtFree ( edited_preset_id ) ;
   edited_preset_id = NULL ;
   XtFree ( unedited_preset_id ) ;
   unedited_preset_id = NULL ;

   /* Update the updated preset groups list in the pointdata control
      GUI. */
   load_presetOptionList ( True , presetNode.preset_id , False ) ;

   XtUnmanageChild(saveasnewFO);
   XtUnmanageChild(saveasnewDS);

   return ;
}

static void cancel_saveasnewCB(Widget w, XtPointer xtptr1, XtPointer xtptr2)
{
   XtUnmanageChild(saveasnewFO);
   XtUnmanageChild(saveasnewDS);
   
   return;
}

void load_saveasnew ( PointDataPresets * pPresetListHead , 
                      PointDataPresets * pPresetNode )
{
   char value_str [ 10 ] ;

   pPresetHead = pPresetListHead ;

   if ( pPresetNode != NULL )
   {   
      sprintf ( value_str , "%d" , pPresetNode->preset_rank ) ;

      /* Set the text fields to reflect the currently selected preset 
         group. */
      XmTextSetString ( saveupdate_curidTX , pPresetNode->preset_id ) ;
      XmTextSetString ( saveupdate_curdesTX , pPresetNode->descr ) ;
      XmTextSetString ( saveupdate_currankTX , value_str ) ; 

      XmTextSetString ( saveupdate_idTX , pPresetNode->preset_id ) ;
      XmTextSetString ( saveupdate_desTX , pPresetNode->descr ) ;
      XmTextSetString ( saveupdate_rankTX , value_str ) ; 
   }
   else
   {
      XmTextSetString ( saveupdate_curidTX , "" ) ;
      XmTextSetString ( saveupdate_curdesTX , "" ) ;
      XmTextSetString ( saveupdate_currankTX , "" ) ; 

      XmTextSetString ( saveupdate_idTX , "" ) ;
      XmTextSetString ( saveupdate_desTX , "" ) ;
      XmTextSetString ( saveupdate_rankTX , "" ) ; 
   }

}

void add_saveasnew_callbacks()
{
   Atom	wmAtom ;
   
   XtAddCallback ( ok_saveasnewPB , XmNactivateCallback , ok_saveasnewCB , 
                   NULL ) ;
   XtAddCallback ( cancel_saveasnewPB , XmNactivateCallback , 
                   cancel_saveasnewCB , NULL ) ;
   
   wmAtom = XmInternAtom(XtDisplay(saveasnewDS), "WM_DELETE_WINDOW", False);
   XmAddWMProtocolCallback(saveasnewDS, wmAtom, cancel_saveasnewCB, NULL);
   
   return;
}
