/*  Mods_globalDefs.h */

#ifndef Mods_globalDefs_h
#define Mods_globalDefs_h

#include <stdio.h>
#include <time.h>
#include <X11/Intrinsic.h>
#include <X11/Shell.h>
#include <Xm/Xm.h>
#include <Xm/RowColumn.h>
#include <Xm/ScrolledW.h>
#include <Xm/BulletinB.h>
#include <Xm/PushB.h>
#include <Xm/Label.h>
#include <Xm/Form.h>
#include <Xm/List.h>
#include <Xm/DialogS.h>
#include <Xm/MessageB.h>
#include <Xm/Text.h>
#include <Xm/TextF.h>
#include <X11/StringDefs.h>


#define NONE_SELECTED   0
#define MAX_RUN_PERIOD	744


#define YES     1
#define NO      0

#define NONE_SELECTED   0

#define RERUN_SEGMENT   0
#define NEXT_SEGMENT    1
#define RUN_FINISHED    2

#define SKIP                            -1
#define NO_VALUE_SELECTED               -9
#define MAX_MODS                        100
#define MAX_NUM_SETQMEAN_VALUES         744
#define ALL_OPERATIONS                  -99
#define MAX_CARD_LENGTH                 70
#define MAX_NUMBER_OF_SELECTABLE_DATES  20
#define MAX_OPERATIONS                  100
/*      *************** GLOBAL FLAGS **************     */

int             stay_in_CEX25;
int             segment_run_type;



int             dialogMessageIsUp;      /* Flag (TRUE or FALSE) indicating a Message Dialog is active   */
int             leftParen_count;
int             rightParen_count;
int             isExpression;           /* Flag (YES or NO) indicating an expression for SETQMEAN...    */
int             num_prev_QMean_values;  /* Number of values previously entered in SETQMEAN              */
int             available_QMean_values; /* Number of values currently available to  SETQMEAN            */
int             currentModIndex;        /* Index for *Mod_array[]...                                    */
int             oldModIndex;            /* Index for *Mod_array[] entering the current Mods creation    */
					/*      cycle; if 'Cancel All' is selected, it is used to       */
					/*      set all Mod_array[] pointers to NULL beginning with     */
					/*      Mod_array[oldModIndex]...                               */
int             modListItem_selected;   /* The item_position of the item selected in the                */
					/*      new_mods_listWidget...                                  */


int             deciding_to_save_modDeletions;
int             make_modDeletions_fromFile;



int		operations_number[MAX_OPERATIONS]; /* Must be same as MAX_OPERATIONS and be same in ifp_globals.h
//					  and mods_plot.h
//                                       */



/*      ***************** END GLOBAL FLAGS **************     */

Widget  global_toplevel;
Widget  tulplot_w;               /* Shell widget for Tulsa plot table    */
Widget  graph_w;                 /* Shell widget for Tulsa plot graph    */
Widget  other_mods_shellWidget;  /* Shell widget for Other Mods...       */
Widget  prevSelected_date_widget;/* Added by (TEA) 03/10/93...           */

#endif
