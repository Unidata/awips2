/*******************************************************************************
 * FILENAME:            delete_polygons_show.c
 * NUMBER OF MODULES:
 * GENERAL INFORMATION:
 *   MODULE 1:
 * DESCRIPTION:
 *
 * ORIGINAL AUTHOR:     Bryon Lawrence
 * CREATION DATE:       July 13, 2005
 * ORGANIZATION:        OHD/HSEB/WHFS
 * MACHINE/OS:             IBM / Redhat Linux
 * MODIFICATION HISTORY:
 *   MODULE #        DATE         PROGRAMMER        DESCRIPTION/REASON
 *         
 ********************************************************************************
 */

#include <dirent.h>
#include <string.h>
#include <stdlib.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <Xm/AtomMgr.h>
#include <Xm/Protocols.h>
#include <Xm/Text.h>
#include <Xm/Xm.h>
#include <unistd.h>

#include "polygon_RFCW.h"
#include "delete_polygons.h"
#include "delete_polygons_show.h"
#include "display_field.h"
#include "draw_precip_poly_RFCW.h"
#include "GeneralUtil.h"
#include "map.h"
#include "map_library.h"
#include "mpe_log_utils.h"
#include "pointcontrol_pets.h"
#include "post_functions.h"
#include "stage3.h"
#include "Xtools.h"

static char polygon_dir [ 100 ] = { '\0' };
static const char * poly_dir_token = "rfcwide_drawpre_dir";
static int poly_first = 1;

/* get_polygons routine. */
enum action_names
{   Sub, Set, Snow, Raise, Lower, Scale, Num_Actions};

/* Specifies the precedence of the actions that can be performed on a polygon.
 The -1 is a sentinel that indicates that no precedence was specified 
 for the given action. */
static int action_precedence [Num_Actions ] = { -1, -1, -1, -1, -1, -1 };
static const char * action_names [Num_Actions ] = { "Sub", "Set", "Snow",
        "Raise", "Lower", "Scale" };

/* Contains the draw precedence of each field that can
 be substituted in a polygon. The -1 is a sentinel that indicates that
 no precedence was specified for the given field. */
static int sub_precedence [NUM_COLORUSE_ITEMS ] = { -1 };

/*******************************************************************************
 * MODULE NUMBER: 1
 * MODULE NAME:  init_action_order
 * PURPOSE:      This routine determines the order in which to apply 
 *               edit polygons.  It first checks the 
 *
 * ARGUMENTS:
 *   TYPE   DATA TYPE   NAME                 DESCRIPTION/UNITS
 *   None
 *
 * RETURNS:
 *   DATA TYPE                DESCRIPTION
 *   int                      A flag indicating whether or not
 *                            an edit polygon action order has been
 *                            specified.
 *                            0 = No order specified
 *                            1 = An order was specified.  The result is
 *                                stored in the action_precedence array.
 *
 * APIs UTILIZED:
 *   NAME                     HEADER FILE     DESCRIPTION
 *   get_apps_defaults        GeneralUtil.h   Retrieves the value of an
 *                                            application configurable token.
 *
 * LOCAL DATA ELEMENTS (OPTIONAL):
 *  DATA TYPE  NAME          DESCRIPTION
 *  char *     action_token  The name of the token containing the polygon
 *                           action order.
 *  char *     pToken        Used for parsing the action order token.
 *  char       reply         Contains the value of the action order token.
 *  int        i             Loop index variable.
 *  int        order         The order of the action.  A polygon with an order
 *                           of 0 is drawn the lowest on the stack while a
 *                           polygon with a of 5 is drawn at the top of the
 *                           stack.
 *  int        reply_len     The number of characters in the value
 *                           of the action order token.
 *  int        request_len   The number of characters in the name of the
 *                           action order token.
 *  int        status        Contains return codes from get_apps_defaults
 *                           and string comparisons.
 *
 * DATA FILES AND/OR DATABASE:
 * None
 *
 * ERROR HANDLING:
 *    ERROR CODE                             DESCRIPTION
 *    None
 *
 ********************************************************************************
 */
static int init_action_order()
{
    static const char * action_token = "mpe_polygon_action_order";
    char * pToken = NULL;
    char reply [ 200 ];
    int i;
    int order = 0;
    int reply_len;
    int request_len;
    int status;

    for (i = 0; i < Num_Actions; ++i)
    {
        action_precedence [ i ] = -1;
    }

    request_len = strlen(action_token);
    status = get_apps_defaults( ( char * ) action_token, &request_len, reply,
            &reply_len);
    if (reply_len > 0)
    {
        status = strcmp(reply, "None");

        if (status != 0)
        {
            pToken = strtok(reply, ",");

            while (pToken != NULL)
            {
                for (i = 0; i < Num_Actions; ++i)
                {
                    status = strcmp(pToken, action_names [ i ]);

                    if (status == 0)
                    {
                        action_precedence [ order ] = i;
                        ++order;
                    }
                }

                pToken = strtok(NULL, ",");
            }
        }
    }

    if (order > 0)
    {
        status = 1;
    }
    else
    {
        status = 0;
    }

    return status;
}

/*******************************************************************************
 * MODULE NUMBER: 2
 * MODULE NAME:   init_substitute_order
 * PURPOSE:       Reads the token indicating the order, if any, to be applied
 *                to substitute polygons.  That is should a substituted
 *                RMOSAIC always be drawn on top of a substituted 
 *                SATPRE polygon?
 *
 * ARGUMENTS:
 *   TYPE   DATA TYPE   NAME                 DESCRIPTION/UNITS
 *   None
 *
 * RETURNS:
 *   DATA TYPE         DESCRIPTION
 *   int               Indicates whether or not an order was specified
 *                     for edit polygon substitutions.
 *
 * APIs UTILIZED:
 *   NAME               HEADER FILE          DESCRIPTION
 *   get_apps_defaults  GeneralUtil.h        Retrieves the values of 
 *                                           application configurable tokens.
 *
 * LOCAL DATA ELEMENTS (OPTIONAL):
 *   DATA TYPE  NAME             DESCRIPTION
 *   char *     sub_token        The name of the token to retrieve the
 *                               substitute order from. 
 *   char **    fieldnames       The names of all of the fields which may be 
 *                               substituted into a sub polygon.
 *   char *     pToken           Used to parse the field names from the
 *                               substitute token value string.
 *   char       reply            Used to contain the value of the
 *                               edit polygon substitute token value string.
 *   int        i                A loop index variable.
 *   int        request_len      The length in characters of the substitute
 *                               token name.
 *   int        reply_len        The length in characters of the value of the
 *                               substitute token name.
 *   int        status           Contains the return codes from 
 *                               string comparison operations.
 *   int        order            Contains the draw order of the sub polygons.
 *                               0 the polygon is drawn first with subsequent
 *                               higher ranked polygons drawn on top of it.
 *
 *
 * DATA FILES AND/OR DATABASE:
 *   None
 *
 * ERROR HANDLING:
 *    ERROR CODE                             DESCRIPTION
 *   None
 ********************************************************************************
 */
static int init_substitute_order()
{
    static const char * sub_token = "mpe_polygon_field_order";
    const char ** fieldnames = NULL;
    char * pToken = NULL;
    char reply [ 200 ];
    int i;
    int request_len;
    int reply_len;
    int status;
    int order = 0;

    for (i = 0; i < NUM_COLORUSE_ITEMS; ++i)
    {
        sub_precedence [ i ] = -1;
    }

    request_len = strlen(sub_token);
    status = get_apps_defaults( (char *) sub_token, &request_len, reply,
            &reply_len);

    if (reply_len > 0)
    {
        status = strcmp(reply, "None");

        if (status != 0)
        {
            fieldnames = get_mpe_field_names();
            pToken = strtok(reply, ",");

            while (pToken != NULL)
            {
                for (i = 0; i < NUM_COLORUSE_ITEMS; ++i)
                {
                    status = strcmp(pToken, fieldnames [ i ]);

                    if (status == 0)
                    {
                        sub_precedence [ i ] = order;
                        ++order;
                    }
                }

                pToken = strtok(NULL, ",");
            }
        }
    }

    if (order > 0)
    {
        status = 1;
    }
    else
    {
        status = 0;
    }

    return status;
}

/*******************************************************************************
 * MODULE NUMBER: 3
 * MODULE NAME:   order_polygons
 * PURPOSE:       Given edit polygon action and substitute rules,
 *                This routine reorders a linked list of edit polygons
 *                (rubber_poly_data structures) to conform to these rules.
 *
 * ARGUMENTS:
 *   TYPE   DATA TYPE   NAME                 DESCRIPTION/UNITS
 *   Input  List *      pPolyList            Points to the head and tail 
 *                                           nodes in a linked list as well
 *                                           as keeping track of the number
 *                                           of polygons in the list.
 * RETURNS:
 *   DATA TYPE   NAME                        DESCRIPTION
 *   None
 *
 * APIs UTILIZED:
 *   NAME                   HEADER FILE      DESCRIPTION
 *   init_action_order      static linkage   Reads, parses and stores 
 *                                           the value of the action
 *                                           order token. 
 *   init_substitute_order  static linkage   Reads, parses and stores
 *                                           the value of the substitute
 *                                           order token.
 *   ListAdd                List.h           Adds a node to a linked list.   
 *   ListDelete             List.h           Deletes a node from a
 *                                           linked list.
 *   ListFirst              List.h           Lists the first node in a linked
 *                                           list.
 *   ListInsert             List.h           Inserts a node into a linked list.
 *   ListNext               List.h           Lists the next node in a linked
 *                                           list.
 *   ListNth                List.h           Lists the Nth node in a linked
 *                                           list.
 *
 * LOCAL DATA ELEMENTS (OPTIONAL):
 *   DATA TYPE          NAME               DESCRIPTION
 *   enum action_names  action             The possible edit polygon actions.
 *   int                action_order       The draw order of a polygon action.
 *   int                field1_precedence  The draw order of the first sub
 *                                         field.
 *   int                field2_precedence  The draw order of the second sub
 *                                         field.
 *   int                first              Indicates whether or not the tokens
 *                                         containing the action and sub orders
 *                                         need to be read in (only read in on
 *                                         the first call to this routine 
 *                                         and then buffer).
 *   int                i                  Loop index.
 *   int                node_number        The number of the current node
 *                                         being processed.
 *   int                num_swaps          the number of swaps performed ...
 *                                         when there are zero swaps then
 *                                         the algorithm is complete.
 *   int                sub_field          The field to be substituted. 
 *   int                sub_order          Indicates if there is a substitute
 *                                         order.
 *   rubber_poly_data * pNode              Points to a node in the linked list
 *                                         of edit polygons.
 *   rubber_poly_data * pNodeN             Points to a node in the linked list
 *                                         of edit polygons.
 *   rubber_poly_data * pTemp              Points to a node in the linked list
 *                                         of edit polygons.
 *   rubber_poly_data * pNextNode          Points to the next node in the
 *                                         linked list of edit polygons.
 *   rubber_poly_data * pNextTemp          Points to the next node in the linked
 *                                         list of edit polygons.
 *
 * DATA FILES AND/OR DATABASE:
 *   None
 *
 * ERROR HANDLING:
 *    ERROR CODE                             DESCRIPTION
 *   None
 ********************************************************************************
 */
void order_polygons(List * pPolyList)
{
    enum action_names action;
    static int action_order = 0;
    int field1_precedence;
    int field2_precedence;
    static int first = 1;
    int i;
    int node_number;
    int num_swaps;
    int sub_field;
    static int sub_order = 0;
    rubber_poly_data * pNode = NULL;
    rubber_poly_data * pNodeN = NULL;
    rubber_poly_data * pTemp = NULL;
    rubber_poly_data * pNextNode = NULL;
    rubber_poly_data * pNextTemp = NULL;

    if (first == 1)
    {
        /* Determine if there are any user-specified polygon orderings. */
        first = 0;
        action_order = init_action_order();
        sub_order = init_substitute_order();
    }

    if ( (action_order == 1 ) || (sub_order == 1 ))
    {
        /* Polygon precedence does matter ... */
        if (action_order == 1)
        {
            /* Some action ordering exists. */
            node_number = 1;

            for (i = 0; i < Num_Actions; ++i)
            {
                action = ( enum action_names ) action_precedence [ i ];

                if (action == -1)
                    break;

                /* Walk through the polygon list looking for this
                 *                element. */
                pNode = ( rubber_poly_data * ) ListFirst(pPolyList);

                while (pNode != NULL)
                {
                    pTemp = ( rubber_poly_data * ) ListNext( &pNode->node);
                    switch (action)
                    {
                        case Sub:

                            if (pNode->sub_flag == 1)
                            {
                                pNodeN = ( rubber_poly_data * ) ListNth(
                                        pPolyList, node_number);
                                if (pNodeN != pNode)
                                {
                                    ListDelete(pPolyList, &pNode->node);
                                    ListInsert(pPolyList, &pNodeN->node,
                                            &pNode->node);
                                }

                                ++node_number;
                            }

                            break;

                        case Set:

                            if (pNode->set_flag == 1)
                            {
                                pNodeN = ( rubber_poly_data * ) ListNth(
                                        pPolyList, node_number);
                                if (pNodeN != pNode)
                                {
                                    ListDelete(pPolyList, &pNode->node);
                                    ListInsert(pPolyList, &pNodeN->node,
                                            &pNode->node);
                                }

                                ++node_number;
                            }

                            break;

                        case Snow:

                            if (pNode->snow_flag == 1)
                            {
                                pNodeN = ( rubber_poly_data * ) ListNth(
                                        pPolyList, node_number);
                                if (pNodeN != pNode)
                                {
                                    ListDelete(pPolyList, &pNode->node);
                                    ListInsert(pPolyList, &pNodeN->node,
                                            &pNode->node);
                                }

                                ++node_number;
                            }

                            break;

                        case Raise:

                            if (pNode->raise_flag == 1)
                            {
                                pNodeN = ( rubber_poly_data * ) ListNth(
                                        pPolyList, node_number);
                                if (pNodeN != pNode)
                                {
                                    ListDelete(pPolyList, &pNode->node);
                                    ListInsert(pPolyList, &pNodeN->node,
                                            &pNode->node);
                                }

                                ++node_number;
                            }

                            break;

                        case Lower:

                            if (pNode->lower_flag == 1)
                            {
                                pNodeN = ( rubber_poly_data * ) ListNth(
                                        pPolyList, node_number);
                                if (pNodeN != pNode)
                                {
                                    ListDelete(pPolyList, &pNode->node);
                                    ListInsert(pPolyList, &pNodeN->node,
                                            &pNode->node);
                                }

                                ++node_number;
                            }

                            break;
                        case Scale:

                            if (pNode->scale_flag == 1)
                            {
                                pNodeN = ( rubber_poly_data * ) ListNth(
                                        pPolyList, node_number);
                                if (pNodeN != pNode)
                                {
                                    ListDelete(pPolyList, &pNode->node);
                                    ListInsert(pPolyList, &pNodeN->node,
                                            &pNode->node);
                                }

                                ++node_number;
                            }

                            break;

                        default:

                            flogMessage(stdout,
                                    "Unrecognized action %d in order "
                                        "polygons.\n", action);
                    }

                    pNode = pTemp;
                }
            }
        }

        /* Action ordering complete. */
        if (sub_order == 1)
        {

            do
            {
                num_swaps = 0;

                pNode = ( rubber_poly_data * ) ListFirst(pPolyList);

                while (pNode != NULL)
                {
                    if (pNode->sub_flag == 1)
                    {
                        sub_field = pNode->draw_source;
                        field1_precedence = sub_precedence [ sub_field ];

                        if (field1_precedence != -1)
                        {
                            pTemp
                                    = ( rubber_poly_data * ) ListNext( &pNode->node);

                            while (pTemp != NULL)
                            {
                                if (pTemp->sub_flag == 1)
                                {
                                    sub_field = pTemp->draw_source;
                                    field2_precedence
                                            = sub_precedence [ sub_field ];

                                    if (field2_precedence != -1)
                                    {
                                        if (field1_precedence
                                                > field2_precedence)
                                        {
                                            ++num_swaps;

                                            /* Swap Nodes. */
                                            pNextNode
                                                    = ( rubber_poly_data * ) ListNext( &pNode->node);
                                            pNextTemp
                                                    = ( rubber_poly_data * ) ListNext( &pTemp->node);

                                            ListDelete(pPolyList, &pNode->node);
                                            ListDelete(pPolyList, &pTemp->node);

                                            if (pNextTemp != NULL)
                                            {
                                                ListInsert(pPolyList,
                                                        &pNextTemp->node,
                                                        &pNode->node);
                                            }
                                            else
                                            {
                                                ListAdd(pPolyList, &pNode->node);
                                            }

                                            if (pNextNode != NULL)
                                            {
                                                if (pTemp != pNextNode)
                                                {
                                                    ListInsert(pPolyList,
                                                            &pNextNode->node,
                                                            &pTemp->node);
                                                }
                                                else
                                                {
                                                    ListInsert(pPolyList,
                                                            &pNode->node,
                                                            &pTemp->node);
                                                }

                                            }
                                            else
                                            {
                                                ListAdd(pPolyList, &pTemp->node);
                                            }

                                            pTemp = pNode;
                                        }
                                    }

                                }

                                pTemp
                                        = ( rubber_poly_data * ) ListNext( &pTemp->node);
                            }
                        }

                    }

                    pNode = ( rubber_poly_data * ) ListNext( &pNode->node);
                }

            } while (num_swaps > 0);
        }
    }
}

/*******************************************************************************
 * MODULE NUMBER: 4
 * MODULE NAME:   get_display_fieldname
 * PURPOSE:       Returns the name of the field currently displayed in the
 *                Hydroview/MPE viewer.
 *
 * ARGUMENTS:
 *   TYPE   DATA TYPE              NAME        DESCRIPTION/UNITS
 *   Input  enum DisplayFieldData  field_type  The field displayed in Hydroview.
 *   Output char *                 field       The string name of the field.
 *
 * RETURNS:
 *   None
 *
 * APIs UTILIZED:
 *   NAME                  HEADER FILE       DESCRIPTION
 *   get_mpe_fieldnames    mpe_fieldnames.h  Returns a list of all of the
 *                                           possible Hydroview/MPE fields.
 *
 * LOCAL DATA ELEMENTS:
 *   DATA TYPE     NAME           DESCRIPTION
 *   const char *  fieldname      Contains the name of the MPE field being
 *                                displayed in Hydroview/MPE.
 *   const char ** fieldnames     The list of field names returned from
 *                                the call to get_mpe_fieldnames.
 *
 * DATA FILES AND/OR DATABASE:
 *   None
 *
 * ERROR HANDLING:
 *   None
 ********************************************************************************
 */
void get_display_fieldname(enum DisplayFieldData field_type, char * field)
{
    const char * fieldname = NULL;
    const char ** fieldnames = NULL;

    fieldnames = get_mpe_field_names();
    fieldname = fieldnames [ field_type ];

    strncpy(field, fieldname, POLY_VALUE_LENGTH);

}

/*******************************************************************************
 * MODULE NUMBER: 5
 * MODULE NAME:   load_polygon_list
 * PURPOSE:       Retrieves draw polygons from a polygon file and lists them
 *                in the list widget on the delete polygons GUI.
 *
 * ARGUMENTS:
 *   None
 *
 * RETURNS:
 *   None
 *
 * APIs UTILIZED:
 *   NAME                  HEADER FILE             DESCRIPTION
 *   free_polygon_list     draw_precip_poly_RFCW.h Releases memory associated
 *                                                 with the list of polygons
 *                                                 read from the polygon file.
 *   get_display_fieldname draw_polygons_show.h    Retrieves the name of the
 *                                                 field currently displayed
 *                                                 in Hydroview/MPE.
 *   get_poly_action       draw_precip_poly_RFCW.h Gets the action associated
 *                                                 with the polygon. 
 *   get_polygons          delete_polygons_show.h  Retrieves a list of 
 *                                                 polygons from the polygons
 *                                                 file.
 *   ListCount             List.h                  Returns the number of nodes
 *                                                 in a linked list.
 *   ListFirst             List.h                  Lists the first node in the
 *                                                 linked list.
 *   loadXmList100         Xtools.h                Loads the delete poygons
 *                                                 GUI list with polygon
 *                                                 information.
 *
 * LOCAL DATA ELEMENTS (OPTIONAL):
 *   DATA TYPE          NAME            DESCRIPTION
 *   Boolean            state           The state of a list.  Are items 
 *                                      selected? 
 *   char               action [ ]      The polygon action.
 *   char               fieldname [ ]   The name of the field being displayed.
 *   char               value [ ]       The value of the polygon.
 *   char               visible         The polygon visibility flag.
 *   Char100Array *     pPolyInfo       The polygon info to be loaded to the
 *                                      delete polygons list.
 *   enum DisplayFieldData display_field_type The field being displayed.
 *   int                i               Loop index.
 *   int                item_count      The number of items in the
 *                                      delete polygons list. 
 *   int                num_positions   The number of items selected in the
 *                                      delete polygons list.
 *   int *              position_list   An array of the positions of items
 *                                      selected in the delete polygons list.
 *   int                num_polys       The number of polygons read from the
 *                                      polygon file.
 *   int                selected_position The polygon selected from the 
 *                                        delete polygons list widget.
 *   List               PolyList          A list structure.
 *   rubber_poly_data * pPolyHead         Points to the head node in the
 *                                        polygon linked list.
 *   rubber_poly_data * pPolyNode         Points to a node in the polygon 
 *                                        linked list.
 *
 * DATA FILES AND/OR DATABASE:
 *   Reads the contents of a draw polygon file.
 *   These files reside in the directory pointed to by the edit_poly
 *   token.  The format of the filename is DrawPolyXXXXXXXYYYYMMDDHHz.
 *   where XXXXXXX is the name of the product, YYYY is the year, MM is the 
 *   month, DD is the day, and HH is the hour.
 *
 *    ERROR CODE                             DESCRIPTION
 * ERROR HANDLING:
 *   None.
 *
 ********************************************************************************
 */
static void load_polygon_list()
{
    Boolean state;
    char action [POLY_ACTION_LENGTH + 1 ] = { '\0' };
    char fieldname [POLY_VALUE_LENGTH + 1 ] = { '\0' };
    char persistent;
    char value [POLY_VALUE_LENGTH + 1 ] = { '\0' };
    char visible;
    Char100Array * pPolyInfo = NULL;
    enum DisplayFieldData display_field_type;
    int i;
    int item_count;
    int num_positions;
    int * position_list = NULL;
    int num_polys;
    int selected_position = -1;
    List PolyList;
    rubber_poly_data * pPolyHead = NULL;
    rubber_poly_data * pPolyNode = NULL;

    display_field_type = rad_data [ 0 ].field_type;

    /* Load the field type into the field text box. */
    /* Get the fieldname corresponding to the field. */
    get_display_fieldname(display_field_type, fieldname);
    XmTextSetString(productTX, ( char * ) fieldname );

    /* Load the date into the date field. */
    XmTextSetString(datetimeTX, date_st3.dttm);

    /* Retrieve the polygons for the currently displayed product and hour. */
    get_polygons(display_field_type, &PolyList, date_st3.cdate);

    /* Get the currently selected item. */
    state = XmListGetSelectedPos(deletepolyLI, &position_list, &num_positions);

    if (state == True)
    {
        selected_position = position_list [ 0 ];
        XtFree( ( char * ) position_list );
        position_list = NULL;
    }

    /* Delete all of the list items. */
    XmListDeleteAllItems(deletepolyLI);

    pPolyHead = ( rubber_poly_data * ) ListFirst( &PolyList);

    if (pPolyHead != NULL)
    {
        num_polys = ListCount( &PolyList);

        if (num_polys > 0)
        {
            pPolyInfo = ( Char100Array * ) malloc(num_polys
                    * sizeof(Char100Array));

            if (pPolyInfo == NULL)
            {
                return;
            }

            pPolyNode = pPolyHead;
            i = 0;

            while (pPolyNode != NULL)
            {
                get_poly_action(pPolyNode, action, value);

                if (pPolyNode->visible == True)
                {
                    visible = 'T';
                }
                else
                {
                    visible = 'F';
                }

                if (pPolyNode->persistent == True)
                {
                    persistent = 'T';
                }
                else
                {
                    persistent = 'F';
                }

                sprintf(pPolyInfo [ i++ ],
                        "%d            %c         %c       %s    %s",
                        pPolyNode->polygon_number, visible, persistent, action,
                        value);

                pPolyNode = ( rubber_poly_data * ) ListNext( &pPolyNode->node);
            }

            /* Load the option sets into the option set scrolled list on the
             pointcontrol GUI. */
            loadXmList100(deletepolyLI, pPolyInfo, num_polys);

            /* Deallocate the memory used by the pPolyInfo array. */
            free(pPolyInfo);
            pPolyInfo = NULL;

            /* Free the memory used by the linked list of Polygon data. */
            free_polygon_list(pPolyHead);
            pPolyHead = NULL;

            /* Select an item in the list. */
            XtVaGetValues(deletepolyLI, XmNitemCount, &item_count, NULL);

            if (state == True && item_count > 0)
            {
                /* Make sure the selected position still exists. */
                if (selected_position <= item_count)
                {
                    XmListSelectPos(deletepolyLI, selected_position, False);
                }
                else
                {
                    XmListSelectPos(deletepolyLI, 1, False);
                }
            }
            else if (item_count > 0)
            {
                XmListSelectPos(deletepolyLI, 1, False);
            }
        }
    }
}

void update_polygon_list()
{
    if ( (deletepolyLI != NULL ) && (XtIsManaged(deletepolyLI) == True ))
    {
        load_polygon_list();
    }
}

const char * check_for_polygon_file(enum DisplayFieldData field,
        const char * cdate, int * status)
{
    char fieldname [POLY_VALUE_LENGTH + 1 ] = { '\0' };
    static char filename [POLY_FILENAME_LENGTH + 1 ] = { '\0' };
    int exists;
    int length;
    struct stat file_stat;

    /* Get the directory the polygon files reside in. */
    if (poly_first == 1)
    {
        poly_first = 0;

        /* Do things here which only need to be done once per
         run of Hydroview/MPE (such as calling get_apps_defaults. */
        /* Get the directory in which the precip polygon files reside. */
        length = strlen(poly_dir_token);
        get_apps_defaults( ( char * ) poly_dir_token, &length, polygon_dir,
                &length);
    }

    /* Get the fieldname corresponding to the field. */
    get_display_fieldname(field, fieldname);

    /* Build the polygon filename. */
    sprintf(filename, "%s/DrawPoly%s%sz", polygon_dir, fieldname, cdate);

    /* Check to see if the file exists. */
    exists = stat(filename, &file_stat);

    if (exists == 0)
    {
        *status = 1;
    }
    else
    {
        *status = 0;
    }

    return filename;
}

static void read_poly_data(FILE * pPolygonFile, List * pPolyList,
        const char * filename, Boolean persistent)
{
    char action [POLY_ACTION_LENGTH + 1 ] = { '\0' };
    char value [POLY_VALUE_LENGTH + 1 ] = { '\0' };

    const char ** fieldnames = NULL;
    float hrapx;
    float hrapy;
    int i;
    int j;
    int nitems;
    int num_points;
    int poly_order;
    int record_number = 0;
    int status;
    int visible;
    rubber_poly_data * pPolyNode = NULL;

    /* Don't do this here.  The get_polygons routines takes care
     of the list initialization. */
    /* ListInit ( pPolyList ); */
    nitems = fscanf(pPolygonFile, "%d %s %s %d %d", &poly_order, action, value,
            &num_points, &visible);
    ++record_number;

    while ( !feof(pPolygonFile) && (nitems != EOF ))
    {
        if (nitems != 5)
        {
            flogMessage(stderr, "\nIn routine 'get_polygons':\n"
                "encountered a read error in file %s "
                "at record %d.\n", filename, record_number);
            return;
        }

        /* Allocate the node in the polygon linked list. */
        pPolyNode = ( rubber_poly_data * ) malloc(sizeof(rubber_poly_data));

        if (pPolyNode == NULL)
        {
            flogMessage(stderr, "\nIn routine 'get_polygons':\n"
                "Could not allocate a node in the linked\n"
                "list of rubber_poly_data structures.\n");
            return;
        }
        /* Initialize the polygon node. */
        memset(pPolyNode, 0, sizeof(rubber_poly_data));

        /* Initialize the variables which will define the HRAP box which
         completely encloses the polygon. */
        pPolyNode->minx = 9999;
        pPolyNode->miny = 9999;
        pPolyNode->maxx = 0;
        pPolyNode->maxy = 0;

        /* Allocate space for the hrap point arrays. */
        pPolyNode->hrap = ( HRAP *) malloc(num_points * sizeof(HRAP));

        if (pPolyNode->hrap == NULL)
        {
            flogMessage(stderr, "In routine 'get_polygons':\n"
                "Could not allocate memory for the"
                "polygon points array.\n");
            return;
        }

        /* Copy the header information to the rubber_poly_data
         structure. */
        pPolyNode->polygon_number = poly_order;
        pPolyNode->npoints = num_points;

        for (i = 0; i < Num_Actions; ++i)
        {
            status = strcmp(action_names [ i ], action);

            if (status == 0)
            {
                switch (i)
                {
                    case Sub:

                        pPolyNode->sub_flag = True;
                        fieldnames = get_mpe_field_names();

                        for (j = 0; j < NUM_COLORUSE_ITEMS; ++j)
                        {
                            status = strcmp(fieldnames [ j ], value);

                            if (status == 0)
                            {
                                pPolyNode->draw_source = j;
                                break;
                            }
                        }

                        if (j == NUM_COLORUSE_ITEMS)
                        {
                            flogMessage(stderr, "In routine 'get_polygons':\n"
                                "Unrecognized value %s.\n", value);
                        }

                        break;

                    case Set:

                        pPolyNode->draw_source = display_subValue;
                        pPolyNode->draw_precip_value = atof(value);
                        pPolyNode->set_flag = True;
                        break;

                    case Snow:

                        pPolyNode->draw_source = display_subValue;
                        pPolyNode->draw_precip_value = atof(value);
                        pPolyNode->snow_flag = True;
                        break;

                    case Lower:

                        pPolyNode->draw_source = display_subValue;
                        pPolyNode->draw_precip_value = atof(value);
                        pPolyNode->lower_flag = True;
                        break;

                    case Raise:

                        pPolyNode->draw_source = display_subValue;
                        pPolyNode->draw_precip_value = atof(value);
                        pPolyNode->raise_flag = True;
                        break;

                    case Scale:

                        pPolyNode->draw_source = display_subValue;
                        pPolyNode->draw_precip_value = atof(value);
                        pPolyNode->scale_flag = True;
                        break;

                    default:

                        flogMessage(stderr, "In routine 'get_polygons':\n"
                            "Reached default case in "
                            "switch statement. Logic "
                            "error.\n");
                        break;
                }

                break;
            }

        }

        if (i == Num_Actions)
        {
            flogMessage(stderr, "In routine 'get_polygons':\n"
                "Unrecognized action %s.\n", action);
        }

        if (visible == 0)
        {
            pPolyNode->visible = False;
        }
        else
        {
            pPolyNode->visible = True;
        }

        pPolyNode->persistent = persistent;

        for (i = 0; i < num_points; ++i)
        {
            /* Read in the points of the polygon. */
            nitems = fscanf(pPolygonFile, "%f %f", &hrapx, &hrapy);
            ++record_number;

            if (nitems == EOF || nitems != 2)
            {
                flogMessage(stderr, "\nIn routine 'get_polygons':\n"
                    "An error was encountered at line %d "
                    "of file %s.\n", record_number, filename);
                return;
            }

            pPolyNode->hrap [ i ].x = hrapx;
            pPolyNode->hrap [ i ].y = hrapy;

            /* Add logic here to derive a HRAP box which completely
             encloses the polygon. */
            if (pPolyNode->hrap[i].x < pPolyNode->minx)
            {
                pPolyNode->minx = pPolyNode->hrap[i].x;
            }

            if (pPolyNode->hrap[i].y < pPolyNode->miny)
            {
                pPolyNode->miny = pPolyNode->hrap[i].y;
            }

            if (pPolyNode->hrap[i].x > pPolyNode->maxx)
            {
                pPolyNode->maxx = pPolyNode->hrap[i].x;
            }

            if (pPolyNode->hrap[i].y > pPolyNode->maxy)
            {
                pPolyNode->maxy = pPolyNode->hrap[i].y;
            }
        }

        ListAdd(pPolyList, &pPolyNode->node);
        nitems = fscanf(pPolygonFile, "%d %s %s %d %d", &poly_order, action,
                value, &num_points, &visible);
        ++record_number;
    }

    return;
}

void get_polygons(enum DisplayFieldData field, List * pPolyList,
        const char * cdate)
{
    char fieldname [POLY_VALUE_LENGTH + 1 ] = { '\0' };
    char filename [POLY_FILENAME_LENGTH + 1 ] = { '\0' };
    char persistent_filename [POLY_FILENAME_LENGTH + 1 ] = { '\0' };
    FILE * pPolygonFile = NULL;
    int exists;
    int length;
    int poly_order;
    rubber_poly_data * pPolyNode = NULL;
    struct stat file_stat;

    ListInit(pPolyList);

    /* Get the directory the polygon files reside in. */
    if (poly_first == 1)
    {
        poly_first = 0;

        /* Do things here which only need to be done once per
         run of Hydroview/MPE (such as calling get_apps_defaults. */
        /* Get the directory in which the precip polygon files reside. */
        length = strlen(poly_dir_token);
        get_apps_defaults( ( char * ) poly_dir_token, &length, polygon_dir,
                &length);
    }

    /* Get the fieldname corresponding to the field. */
    get_display_fieldname(field, fieldname);

    /* Build the polygon filename. */
    sprintf(filename, "%s/DrawPoly%s%sz", polygon_dir, fieldname, cdate);

    /* Build the persistent polygon filename. */
    sprintf(persistent_filename, "%s/DrawPoly%s", polygon_dir, fieldname);

    /* Check to see if the persistent polygon file exists. */
    exists = stat(persistent_filename, &file_stat);

    if (exists == 0)
    {
        pPolygonFile = fopen(persistent_filename, "r");

        if (pPolygonFile != NULL)
        {
            read_poly_data(pPolygonFile, pPolyList, persistent_filename, True);
            fclose(pPolygonFile);
            pPolygonFile = NULL;
        }
    }

    /* Check to see if the hourly polygon file exists. */
    exists = stat(filename, &file_stat);

    if (exists == 0)
    {
        pPolygonFile = fopen(filename, "r");

        if (pPolygonFile != NULL)
        {
            read_poly_data(pPolygonFile, pPolyList, filename, False);
            fclose(pPolygonFile);
            pPolygonFile = NULL;
        }
    }

    /* If the user has specified any polygon order preferences,
     reorder the list to reflect these preferences. */
    order_polygons(pPolyList);

    /* Set the polygons so that they are numbered sequentially from 1 to N,
     where N is the number of polygons in the linked list. */
    pPolyNode = ( rubber_poly_data * ) ListFirst(pPolyList);

    poly_order = 1;

    while (pPolyNode != NULL)
    {
        pPolyNode->polygon_number = poly_order++;
        pPolyNode = ( rubber_poly_data * ) ListNext( &pPolyNode->node) ;
    }

    return;
}

typedef struct SnowList
{
    Node node;
    char filename [ 100 ];
    List list;
} SnowList;

void get_snow_polygons(List * pSnowPolyList, const char * cdate)
{
    char * pChar = NULL;
    char fieldname [POLY_VALUE_LENGTH + 1 ] = { '\0' };
    char filename [POLY_FILENAME_LENGTH + 1 ] = { '\0' };
    char ** filenames = NULL;
    char persistent_filename [POLY_FILENAME_LENGTH + 1 ] = { '\0' };
    DIR * dirp = NULL;
    FILE * pPolygonFile = NULL;
    int exists;
    int first = 1;
    int poly_order;
    rubber_poly_data * pSnowPolyNode = NULL;
    struct dirent * dp = NULL;
    struct stat file_stat;
    int i = 0;
    int length = 0;

    SnowList * pSnowListHead = NULL;
    SnowList * pSnowListNode = NULL;
    SnowList * pSnowListTemp = NULL;

    rubber_poly_data * pSnowPolyHead = NULL;

    char command1[120];
    char command[120];
    char eachfile[120];
    FILE *fd = NULL;

    /* Get the directory the polygon files reside in. */
    if (poly_first == 1)
    {
        poly_first = 0;

        /* Do things here which only need to be done once per
         run of Hydroview/MPE (such as calling get_apps_defaults. */
        /* Get the directory in which the precip polygon files reside. */
        length = strlen(poly_dir_token);
        get_apps_defaults( ( char * ) poly_dir_token, &length, polygon_dir,
                &length);
    }

    memset(command1, 120, '\0');
    sprintf(command1, "rm -f %s%s%s%s", polygon_dir, "/snow_gages", cdate, "z");
    system(command1);

    ListInit(pSnowPolyList);

    /* Retrieve the contents of the polygon directory. Filter
     out the files for the given date and hour
     which contain snow polytgons. */

    if ((dirp = opendir(polygon_dir)) == NULL)
    {
        logMessage("Could not open or read directory %s.\n", polygon_dir);
        return;
    }

    do
    {
        if ((dp = readdir(dirp)) != NULL)
        {
            pChar = strstr(dp->d_name, "DrawPoly");

            if (pChar != NULL)
            {
                pChar = strstr(dp->d_name, cdate);

                if (pChar != NULL)
                {
                    pSnowListNode = (SnowList * ) malloc(sizeof(SnowList));

                    strcpy(pSnowListNode->filename, dp->d_name);

                    if (first == 1)
                    {
                        pSnowListHead = pSnowListNode;
                        ListInit( &pSnowListHead->list);
                        first = 0;
                    }

                    ListAdd( &pSnowListHead->list, &pSnowListNode->node);
                }
            }
        }
    } while (dp != NULL);

    closedir(dirp);
    dirp = NULL;

    if (pSnowListHead != NULL)
    {
        pSnowListNode = (SnowList *) ListFirst( &pSnowListHead->list);

        while (pSnowListNode != NULL)
        {
            strcpy(filename, polygon_dir);
            strcat(filename, "/");
            strcat(filename, pSnowListNode->filename);

            /* Check to see if any files with Snow polygon entries exists. */
            exists = stat(filename, &file_stat);

            if (exists == 0)
            {
                pPolygonFile = fopen(filename, "r");

                if (pPolygonFile != NULL)
                {
                    read_poly_data(pPolygonFile, pSnowPolyList, filename, False);
                    fclose(pPolygonFile);
                    pPolygonFile = NULL;
                }
            }

            pSnowListNode = (SnowList*) ListNext( &pSnowListNode->node);
        }//end for

        /* Free the SnowList Linked List. */
        pSnowListNode = (SnowList *) ListFirst( &pSnowListHead->list);

        while (pSnowListNode != NULL)
        {
            pSnowListTemp = ( SnowList * ) ListNext( &pSnowListNode->node);
            free(pSnowListNode);
            pSnowListNode = pSnowListTemp;
        }
    }

    pSnowPolyHead = ( rubber_poly_data * ) ListFirst(pSnowPolyList);

    if (pSnowPolyHead != NULL)
    {
        pSnowPolyNode = pSnowPolyHead;

        while (pSnowPolyNode != NULL)
        {
            if (pSnowPolyNode->snow_flag == False)
            {
                ListDelete(pSnowPolyList, &pSnowPolyNode->node);
                free(pSnowPolyNode);
                pSnowPolyNode = NULL;
                break;
            }

            pSnowPolyNode
                    = ( rubber_poly_data * ) ListNext( &pSnowPolyNode->node);
        }
    }

    return;
}

void write_polygons(const rubber_poly_data * pPolyHead,
        enum DisplayFieldData field, const char * cdate)
{
    char action [POLY_ACTION_LENGTH +1 ] = { '\0' };
    char fieldname[POLY_VALUE_LENGTH + 1 ] = { '\0' };
    char value [POLY_VALUE_LENGTH + 1 ] = { '\0' };
    char filename [POLY_FILENAME_LENGTH +1 ] = { '\0' };
    char persistent_filename [POLY_FILENAME_LENGTH + 1 ] = { '\0' };
    FILE * pPolygonFile = NULL;
    int exists;
    int hourly_count = 0;
    int i;
    int length;
    int num_points;
    int persistent_count = 0;
    int poly_order;
    int visible;
    rubber_poly_data * pPolyNode = NULL;
    struct stat file_stat;

    /*    char temp_file[120];
     FILE *temp_snow_file = NULL;
     */

    if (poly_first == 1)
    {
        poly_first = 0;

        /* Do things here which only need to be done once per
         run of Hydroview/MPE (such as calling get_apps_defaults. */
        /* Get the directory in which the precip polygon files reside. */
        length = strlen(poly_dir_token);
        get_apps_defaults( ( char * ) poly_dir_token, &length, polygon_dir,
                &length);
        /*       memset(temp_file, 120, '\0'); 
         sprintf(temp_file, "%s", polygon_dir);
         strcat(temp_file, "/temp.txt");
         */
    }

    /* Get the fieldname corresponding to the field. */
    get_display_fieldname(field, fieldname);
    //get_display_fieldname ( display_field_type, fieldname );
    sprintf(filename, "%s/DrawPoly%s%sz", polygon_dir, fieldname,
            date_st3.cdate);

    /* Determine how many persistent and hourly polygons there are to
     write out for this field. */
    pPolyNode = ( rubber_poly_data * ) pPolyHead;

    while (pPolyNode != NULL)
    {
        if (pPolyNode->persistent == True)
        {
            persistent_count++;
        }
        else
        {
            hourly_count++;
        }

        pPolyNode = ( rubber_poly_data * ) ListNext( &pPolyNode->node);
    }

    /* Build the name of the persistent polygon file. */
    sprintf(persistent_filename, "%s/DrawPoly%s", polygon_dir, fieldname);

    /* Build the name of the polygon file. Each node in the linked
     list of polygons may be from a different MPE field source. */
    sprintf(filename, "%s/DrawPoly%s%sz", polygon_dir, fieldname,
            date_st3.cdate);

    if (persistent_count == 0)
    {
        /* Stat the file to see if it is there. */
        exists = stat(persistent_filename, &file_stat);

        if (exists == 0)
        {
            /* Remove the file if it exists. */
            remove(persistent_filename);
        }
    }
    else
    {
        /* There are persistent polygons to write out. */
        /* Open it for writing. */
        pPolygonFile = fopen(persistent_filename, "w");

        if (pPolygonFile == NULL)
        {
            flogMessage(stderr, "\nIn routine 'write_draw_precip_data_RFCW':\n"
                "Could not open file %s to write polygon data "
                "to. Polygon operation stopped.\n", filename);
            return;
        }

        poly_order = 0;

        pPolyNode = ( rubber_poly_data * ) pPolyHead;

        while (pPolyNode != NULL)
        {
            if (pPolyNode->persistent == True)
            {
                /* Increment the polygon number and record number. */
                poly_order++;
                get_poly_action(pPolyNode, action, value);

                /* Initialize the polygon number of points. */
                num_points = pPolyNode->npoints;

                /* Initialize the visibility flag. */
                if (pPolyNode->visible == True)
                {
                    visible = 1;
                }
                else
                {
                    visible = 0;
                }

                /* Write the header to the polgon file. */
                fprintf(pPolygonFile, "%d %s %s %d %d\n", poly_order, action,
                        value, num_points, visible);

                /* Write the points to the polygon file. */
                for (i = 0; i < pPolyNode->npoints; ++i)
                {
                    fprintf(pPolygonFile, "%f %f\n", pPolyNode->hrap[i].x,
                            pPolyNode->hrap[i].y);
                }

            }

            pPolyNode = ( rubber_poly_data * ) ListNext( &pPolyNode->node);
        }

        /* Close the file. */
        fclose(pPolygonFile);
        pPolygonFile = NULL;

    }

    if (hourly_count == 0)
    {
        /* Stat the file to see if it is there. */
        exists = stat(filename, &file_stat);

        if (exists == 0)
        {
            /* Remove the file if it exists. */
            remove(filename);
        }

    }
    else
    {
        /* There are hourly polygons to write out. */
        /* Open it for writing. */
        pPolygonFile = fopen(filename, "w");

        if (pPolygonFile == NULL)
        {
            flogMessage(stderr, "\nIn routine 'write_draw_precip_data_RFCW':\n"
                "Could not open file %s to write polygon data "
                "to. Polygon operation stopped.\n", filename);
            return;
        }

        /* Write out the file. */
        poly_order = 0;

        pPolyNode = ( rubber_poly_data * ) pPolyHead;

        while (pPolyNode != NULL)
        {
            if (pPolyNode->persistent == False)
            {
                /* Increment the polygon number and record number. */
                poly_order++;
                get_poly_action(pPolyNode, action, value);

                /* Initialize the polygon number of points. */
                num_points = pPolyNode->npoints;

                /* Initialize the visibility flag. */
                if (pPolyNode->visible == True)
                {
                    visible = 1;
                }
                else
                {
                    visible = 0;
                }

                /* Write the header to the polgon file. */
                fprintf(pPolygonFile, "%d %s %s %d %d\n", poly_order, action,
                        value, num_points, visible);

                /* Write the points to the polygon file. */
                for (i = 0; i < pPolyNode->npoints; ++i)
                {
                    fprintf(pPolygonFile, "%f %f\n", pPolyNode->hrap[i].x,
                            pPolyNode->hrap[i].y);
                }
            }

            pPolyNode = ( rubber_poly_data * ) ListNext( &pPolyNode->node);
        }

        /* Close the file. */
        fclose(pPolygonFile);
        pPolygonFile = NULL;
    }

    return;
}

void add_polygon(rubber_poly_data * pPolyNode)
{
    enum DisplayFieldData display_field_type;
    rubber_poly_data * pPolyHead = NULL;
    List PolyList;
    char filename [POLY_FILENAME_LENGTH +1 ] = { '\0' };
    char fieldname[POLY_VALUE_LENGTH + 1 ] = { '\0' };
    int length = 0;

    display_field_type = rad_data [ 0 ].field_type;

    get_polygons(display_field_type, &PolyList, date_st3.cdate);

    /* Get the directory the polygon files reside in. */
    if (poly_first == 1)
    {
        poly_first = 0;

        /* Do things here which only need to be done once per
         run of Hydroview/MPE (such as calling get_apps_defaults. */
        /* Get the directory in which the precip polygon files reside. */
        length = strlen(poly_dir_token);
        get_apps_defaults( ( char * ) poly_dir_token, &length, polygon_dir,
                &length);
    }

    pPolyHead = ( rubber_poly_data * ) ListFirst( &PolyList);
    if (pPolyHead != NULL)
    {
        ListAdd( &PolyList, &pPolyNode->node);
    }
    else
    {
        /* Determine the correct location in the linked list to add the
         newly created polygon. */
        ListInit( &PolyList);
        ListAdd( &PolyList, &pPolyNode->node);
        pPolyHead = ( rubber_poly_data * ) ListFirst( &PolyList);
    }

    /* Write the updated polygons to the polygon file. */
    write_polygons(pPolyHead, display_field_type, date_st3.cdate);
    free_polygon_list(pPolyHead);
    pPolyHead = NULL;

    /* Update the map display. */
    turnOnMpeData();
    display_mpe_data( 0);
}

static void display_polygon(Widget w, XtPointer clientdata, XtPointer calldata)
{
    enum DisplayFieldData * display_field_type =
            ( enum DisplayFieldData * ) clientdata;
    Boolean state;
    int polygon_number;
    int position_count;
    int * position_list = NULL;
    List PolyList;

    rubber_poly_data * pPolyHead = NULL;
    rubber_poly_data * pPolyNode = NULL;

    state = XmListGetSelectedPos(deletepolyLI, &position_list, &position_count);

    if (state == True && position_count > 0)
    {
        polygon_number = position_list [ 0 ];

        XtFree( (char * ) position_list );
        position_list = NULL;

        /* Retrieve the polygons from the polygon file. */
        get_polygons( *display_field_type, &PolyList, date_st3.cdate);

        pPolyHead = ( rubber_poly_data * ) ListFirst( &PolyList);

        /* Walk through the linked list until the polygon with 
         the specified number is found. */
        pPolyNode = pPolyHead;

        while (pPolyNode != NULL)
        {
            if (pPolyNode->polygon_number == polygon_number)
            {
                if (pPolyNode->visible == True)
                {
                    free_polygon_list(pPolyHead);
                    pPolyHead = NULL;
                    return;
                }

                pPolyNode->visible = True;
                break;
            }

            pPolyNode = ( rubber_poly_data * ) ListNext( &pPolyNode->node);
        }

        /* Write the polygons to the polygon file. */
        write_polygons(pPolyHead, *display_field_type, date_st3.cdate);

        /* Free the polygon list. */
        free_polygon_list(pPolyHead);

        /*--------------------------------------------*/
        /*  redraw main window with polygons          */
        /*--------------------------------------------*/
        /* Indicate to the exposure routine that there is now
         Mpe data to plot. */
        turnOnMpeData() ;

        /* Force the Hmap_mpe application to redraw its map so that
         the new display will be visible to the user. */
        display_mpe_data( 0);
    }
}

/* Recover a specific polygon from the polygon list. */

static void undisplay_polygon(Widget w, XtPointer clientdata, XtPointer calldata)
{
    enum DisplayFieldData * display_field_type =
            ( enum DisplayFieldData * ) clientdata;
    Boolean state;
    int polygon_number;
    int position_count;
    int * position_list = NULL;
    List PolyList;
    rubber_poly_data * pPolyHead = NULL;
    rubber_poly_data * pPolyNode = NULL;

    state = XmListGetSelectedPos(deletepolyLI, &position_list, &position_count);

    if (state == True && position_count > 0)
    {
        polygon_number = position_list [ 0 ];
        XtFree( ( char * ) position_list );
        position_list = NULL;

        get_polygons( *display_field_type, &PolyList, date_st3.cdate);

        pPolyHead = ( rubber_poly_data * ) ListFirst( &PolyList);

        if (pPolyHead != NULL)
        {
            pPolyNode = pPolyHead;

            while (pPolyNode != NULL)
            {
                if (pPolyNode->polygon_number == polygon_number)
                {
                    if (pPolyNode->visible == False)
                    {
                        free_polygon_list(pPolyHead);
                        pPolyHead = NULL;
                        return;
                    }

                    pPolyNode->visible = False;
                    break;
                }

                pPolyNode = ( rubber_poly_data * ) ListNext( &pPolyNode->node);
            }

            /* Write the polygons to the polygon file. */
            write_polygons(pPolyHead, *display_field_type, date_st3.cdate);

            /* Free the polygon list. */
            free_polygon_list(pPolyHead);
            pPolyHead = NULL;

            /*--------------------------------------------*/
            /*  redraw main window with drawn in precip   */
            /*--------------------------------------------*/
            /* Indicate to the exposure routine that there is now
             Mpe data to plot. */
            turnOnMpeData() ;

            /* Force the Hmap_mpe application to redraw its map so that
             the new display will be visible to the user. */
            display_mpe_data( 0);
        }
    }
}

/* delete a specific polygon from the polygon list. */

static void delete_polygon(Widget w, XtPointer clientdata, XtPointer calldata)
{
    enum DisplayFieldData * display_field_type =
            ( enum DisplayFieldData * ) clientdata;
    Boolean state;
    int polygon_number;
    int position_count;
    int * position_list = NULL;
    List PolyList;
    rubber_poly_data * pPolyHead = NULL;
    rubber_poly_data * pPolyNode = NULL;

    state = XmListGetSelectedPos(deletepolyLI, &position_list, &position_count);

    if (state == True && position_count > 0)
    {
        polygon_number = position_list [ 0 ];
        XtFree( ( char * ) position_list );
        position_list = NULL;

        get_polygons( *display_field_type, &PolyList, date_st3.cdate);

        pPolyHead = ( rubber_poly_data * ) ListFirst( &PolyList);

        if (pPolyHead != NULL)
        {
            pPolyNode = pPolyHead;

            while (pPolyNode != NULL)
            {
                if (pPolyNode->polygon_number == polygon_number)
                {
                    ListDelete( &PolyList, &pPolyNode->node);
                    free(pPolyNode);
                    pPolyNode = NULL;
                    break;
                }

                pPolyNode = ( rubber_poly_data * ) ListNext( &pPolyNode->node);
            }

            /* Write the polygons to the polygon file. */
            pPolyHead = ( rubber_poly_data * ) ListFirst( &PolyList);

            write_polygons(pPolyHead, *display_field_type, date_st3.cdate);

            /* Free the polygon list. */
            free_polygon_list(pPolyHead);
            pPolyHead = NULL;

            /*--------------------------------------------*/
            /*  redraw main window with drawn in precip   */
            /*--------------------------------------------*/
            /* Indicate to the exposure routine that there is now
             Mpe data to plot. */
            turnOnMpeData() ;

            /* Force the Hmap_mpe application to redraw its map so that
             the new display will be visible to the user. */
            display_mpe_data( 0);
        }
    }
}

static void delete_all(Widget w, XtPointer client_data, XtPointer call_data)
{
    enum DisplayFieldData * display_field_type =
            ( enum DisplayFieldData * ) client_data;
    List PolyList;
    rubber_poly_data * pPolyHead = NULL;
    rubber_poly_data * pPolyNode = NULL;
    rubber_poly_data * pPolyTemp = NULL;

    get_polygons( *display_field_type, &PolyList, date_st3.cdate);

    pPolyHead = ( rubber_poly_data * ) ListFirst( &PolyList);

    if (pPolyHead != NULL)
    {
        /* Walk through linked list of polygons, deleting all polygons
         which are flagged to be deleted. */
        pPolyNode = pPolyHead;

        while (pPolyNode != NULL)
        {
            pPolyTemp = ( rubber_poly_data * ) ListNext( &pPolyNode->node);

            ListDelete( &PolyList, &pPolyNode->node);
            free(pPolyNode);
            pPolyNode = NULL;

            pPolyNode = pPolyTemp;
        }

        pPolyHead = ( rubber_poly_data * ) ListFirst( &PolyList);

        write_polygons(pPolyHead, *display_field_type, date_st3.cdate);

        load_polygon_list();

        /* Free the polygon list. */
        free_polygon_list(pPolyHead);
        pPolyHead = NULL;

        /*--------------------------------------------*/
        /*  redraw main window with drawn in precip   */
        /*--------------------------------------------*/
        /* Indicate to the exposure routine that there is now
         Mpe data to plot. */
        turnOnMpeData() ;

        /* Force the Hmap_mpe application to redraw its map so that
         the new display will be visible to the user. */
        display_mpe_data( 0);
    }
}

static void close_deletepolygon(Widget w, XtPointer clientdata,
        XtPointer calldata)
{
    XtPopdown(deletepolyDS);
    XtDestroyWidget(deletepolyDS);
    deletepolyDS = NULL;
    deletepolyLI = NULL;
}

void show_deletepolygonDS(Widget w, XtPointer clientdata, XtPointer calldata)
{
    Atom wmAtom;
    Widget top_level_shell;

    if (deletepolyDS == ( Widget ) NULL )
    {
        top_level_shell = _get_map_widget( 0);
        top_level_shell = GetTopShell(top_level_shell);
        create_deletepolyDS(top_level_shell);

        /* Add any window manager callbacks. */
        wmAtom = XmInternAtom( (XtDisplay(deletepolyDS)), "WM_DELETE_WINDOW",
                False);
        XmAddWMProtocolCallback(deletepolyDS, wmAtom, close_deletepolygon, NULL);

        /* Add the call backs. */
        XtAddCallback(deleteclosePB, XmNactivateCallback, close_deletepolygon,
                NULL);
        XtAddCallback(undisplayPB, XmNactivateCallback, undisplay_polygon,
                &rad_data[0].field_type);
        XtAddCallback(displayPB, XmNactivateCallback, display_polygon,
                &rad_data[0].field_type);
        XtAddCallback(deletePB, XmNactivateCallback, delete_polygon,
                &rad_data[0].field_type);
        XtAddCallback(deleteallPB, XmNactivateCallback, delete_all,
                &rad_data[0].field_type);
    }

    /* Load the entries in the polygon list. */
    load_polygon_list();
    XtManageChild(deletepolyFO);
    XtManageChild(deletepolyDS);

    /*  ==============  Statements containing RCS keywords:  */
    {
        static char
                rcs_id1[] =
                        "$Source: /fs/hseb/ob90/ohd/pproc_lib/src/MPEGui/RCS/delete_polygons_show.c,v $";
        static char rcs_id2[] =
                "$Id: delete_polygons_show.c,v 1.9 2008/01/11 17:11:27 lawrence Exp $";
    }
    /*  ===================================================  */

}
