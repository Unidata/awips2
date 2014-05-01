/* ***************************************** */
/*    File:           TSControl_show.c     */
/*    Date:           April 1995         */
/*    Author:         Sung Vo              */
/*    Purpose:        Provide support for  */
/*    the Time Series Control Dialog       */
/* **************************************************************/
/*    Modification History                                        */
/*  Date:        Author:            Purpose:                        */
/*  04-21-2004    Guoxian Zhou    add reorder_ts_CB function      */
/*                              and modify related functions to */
/*                              support reorder by lid/name     */
/*  06-2004        Guoxian Zhou    add new feature for station mode*/
/*                              to display two graphs in one    */
/*                              page.                           */
/*  04-2005             Bryon Lawrence  Added logic to make sure  */
/*                                      that widgets are properly */
/*                                      initialized upon start up */
/*                                      of TimeSeries   program   */
/* ****************************************************************/


/* ***************************************** */
/* ************* Header files ************** */
/* ***************************************** */

#include <Xm/Xm.h>
#include <X11/IntrinsicP.h>
#include "GeneralUtil.h"
#include "TimeSeries_show.h"
#include "TSControl_show.h"
#include "TSControl.h"
#include "TSgetinfo.h"
#include "TSutils.h"
#include "tabular_info.h"
#include "tsgen_info.h" 

/* ***************************************** */
/* external structures used in the Time      */
/* Series that defined in  TimeSeries_show.c */
/* ***************************************** */
 extern PAGE_INFO        TSpageinfo[MAX_PAGE_INFO];
 extern PAGE_DATA        TSpagedata[MAX_PAGE_DATA];

 extern PAGE_MGR        *PageMgr;
 extern GROUP_INFO      *GroupInfo;
 extern PAGE_INFO       *PageInfo;
 extern GRAPH_INFO      *GraphInfo;
 extern TRACE_INFO      *TraceInfo;

/* *********************************************** */
/* *** Local variables to TSControl_show.c for *** */
/* *** keeping track selection of start time   *** */
/* *** endtime from Time Series Control Window *** */
/* *********************************************** */
int    EndMUp_Selected,
    EndDn_selected,
    BeginUp_selected,
    BeginDn_selected;

 char    Bmonth = 1,
        Bday, 
        Byear, 
     Bhour; 

 char    Emonth = 1,
        Eday, 
        Eyear, 
     Ehour; 

    int    num_active_lids = 0;
    int    tabinfo_selected_pos;

/* *********************************************************************** */
/* reorder_mode = 0 or 1 depends on the check box on search options        */
/*            0 reorder by lid                                             */   
/*            1 reorder by name                                            */   
/* *********************************************************************** */
static int reorder_mode = 0;

/* ***************************************************** */
/* Group's structure for the Time Series  Control Window */
/* ***************************************************** */
typedef struct _GROUP_NAME
{

    int     ngroups;
    RussText name[MAX_GROUPS];
    RussText nameselected;
    RussText prev_nameselected;

}GROUP_NAME;

/* ******************************************************* */
/* PEDTSEP's structure for the Time Series Control Window  */
/* Duration/Type Source/Extremum                          */
/* ******************************************************* */
typedef struct _PEDTSEP
{

    RussText *display_name;
    RussText *name;
    int num_items;

}PEDTSEP;

/* ********************************************************* */
/* *** LISTID's structure to hold new list of Lid and    *** */
/* *** Location name that are dynamically filtered by    *** */
/* *** filter's option on Time Series Control Window     *** */
/* ********************************************************* */
typedef struct _LISTLID
{
    char lid[LOC_ID_LEN+1];
    char name[LOC_NAME_LEN+1];

} LISTLID;

/* ********************************************************* */
/* *** LIDDATA's structure to hold a list of Lid's data  *** */
/* *** that are used to record multiple selected Lid     *** */
/* *** on Time Series Control Window                     *** */
/* ********************************************************* */
typedef struct _LIDDATA
{
    char pe[3];
    int  dur;
} LIDDATA;


int lid_check(LIDDATA LidData1, LIDDATA LidData2);

/* ******************************************************* */
/* *** TIMEDATA's structure to hold current time from  *** */
/* *** Time Series Control Window                      *** */
/* ******************************************************* */
typedef struct  _TIMEDATA
{
    int EndmValue,
        EnddValue,
        EndyValue,
        EndhValue;

    int BegmValue,
        BegdValue,
        BegyValue,
        BeghValue;

    time_t    BeginTime,
        EndTime;

}TIMEDATA;

/* ************************************** */
/* Structures used for TSControl Dialog    */
/* ************************************** */

TRACE_INFO     TinfoPtr;
TRACE_INFO     tmpinfo;
TRACE_INFO     Tinfo[MAX_TRACES];

TIMEDATA     TimeData;
GROUP_NAME    Group;
PEDTSEP       PedTSep;
LISTLID        *LlidPtr = NULL;

char        keyhit = 0;


/* *************************************** */
/* StnClassHead and locviewHead are Global */
/* variable to allow subsequent searches   */
/* on the lid field.                       */
/* *************************************** */

LocView        *locviewHead  = NULL;
StnClass    *StnClassHead = NULL;
                                        
Boolean        class_options[10];

char        current_lid[9],
        last_lid[9];

char        statlist_lid[1];

char        current_name[LOC_NAME_LEN +1];

/* *************************************************** */
/* Search for and return the position that matches the */
/* requested location name.                            */
/* *************************************************** */
static int search_LocNameList(const char *str)
{
    int        len,
            i;
            
    char         *ptr;
    char         ch = '(';
    
    memset(current_name,'\0',strlen(current_name));

    len    = strlen(str);

    for (i = 0; i<num_active_lids; i++)
    {
        
        if (strncmp((LlidPtr+i)->name, str, len) == 0)
        {
            strcpy(current_lid,(LlidPtr+i)->lid);
            strcpy(current_name,(LlidPtr+i)->name);
            
            /* ************************************ */
            /* Truncate spaces and parenthesis      */
            /* in the case a location name has that.*/
            /* ************************************ */
            
            ptr = strchr(current_name, ch);
            if(ptr)
            {
                ptr--;
                
                while(isspace(*ptr))
                {
                    ptr--;
                }
                
                ++ptr;
                *ptr = '\0';
            }
            if(current_name == " ")
                break;
            
            return(++i);
        }
    
    
    }

    return(-1);
}

/* ********************************************************** */
/* Search and update the widget list for lid as characters    */
/* entered in the Search.                                     */
/* ********************************************************** */
static void TSC_Lookuplid (char *buf, XtPointer call_data)
{    
    int        pos;
    
    XmTextVerifyCallbackStruct *cbs = (XmTextVerifyCallbackStruct *) call_data;

    if (cbs->text->length == 0)
        return;
        
    
    /* ****************************************** */
    /* Search through the list and find the list  */
    /* position that matches the specified input  */
    /* string.                                    */
    /* ****************************************** */
    if ((pos = search_StnList(buf)) < 0)
        cbs->doit = False;
        

    /* ****************************************** */
    /* Reset the currently selected position in   */
    /* the location list.                         */
    /* ****************************************** */


    XmListSelectPos(TSC_StationLI, pos, True);
    XmListSetPos(TSC_StationLI, pos);


    /* ******************************************* */
        /* Clear TSC_PCodeLI list when first character */
    /* received in Search text widget.             */
    /* ******************************************* */

    if ( strlen(buf) == 1)
        XmListDeleteAllItems(TSC_PCodeLI);

    if ( strcmp ( buf, current_lid ) == 0)
    {
        strcpy (TinfoPtr.lid, current_lid); 
        sprintf(buf,"PE TypSrc Ext Dur %s",TinfoPtr.lid);
        SetLabel(TSC_PeTs,buf);
        load_Ingestfilter ( current_lid );

    }
    else
        XmListDeselectPos(TSC_StationLI,pos);

    return;
}

/* ********************************************************** */
/* Search and update the widget list for name as characters   */
/* entered in the Search.                                     */
/* ********************************************************** */
static void TSC_Lookupname(char *buf, XtPointer call_data) 
{      
    int pos;

    XmTextVerifyCallbackStruct *cbs = (XmTextVerifyCallbackStruct *) call_data;

    if (cbs->text->length == 0)
        return;
        
    /* ****************************************** */
    /* Search through the list and find the list  */
    /* position that matches the specified input  */
    /* string of location name.                   */
    /* ****************************************** */
        
    if ((pos = search_LocNameList(buf)) < 0)
        cbs->doit = False;    

    /* ****************************************** */
    /* Reset the currently selected position in   */
    /* the location name list.                    */
    /* ****************************************** */


    XmListSelectPos(TSC_StationLI, pos, True);
    XmListSetPos(TSC_StationLI, pos);


    /* ******************************************* */
        /* Clear TSC_PCodeLI list when first character */
    /* received in Search text widget.             */
    /* ******************************************* */

    if ( strlen(buf) == 1)
        XmListDeleteAllItems(TSC_PCodeLI);

    if(strcmp(buf, current_name) == 0)
    {
        strcpy (TinfoPtr.lid, current_lid ); 
        sprintf(buf,"PE TypSrc Ext Dur %s",TinfoPtr.lid);
        SetLabel(TSC_PeTs,buf);    
        load_Ingestfilter(current_lid);
    }
    else    
        XmListDeselectPos(TSC_StationLI,pos);
        
    return;
}



/* ***************************************** */
/* return days from the .apps_default file  */
/* for the time series begin/end time value  */
/* ***************************************** */
int get_DefaultTime( char *token)
{
    char    gad_value[128];
    int      gad_token_len=0, gad_value_len=0, rcfad=0;
    int        total = 1;

    gad_token_len  =  strlen (token);
    rcfad = get_apps_defaults(token, &gad_token_len, gad_value, &gad_value_len);


    if (rcfad != 0)
    {
        fprintf(stderr, "STATUS: There is no value currently set for the token \"%s\".\n", token);
        return -1;
    }
    else
    {
        char    *cptr = NULL ;

        char* tmp =    (char*)malloc((gad_value_len+1) * sizeof(char));

        memset(tmp, '\0', strlen(tmp));
        
        strcpy(tmp, gad_value);

        cptr = strtok(gad_value, "*");

        while (  cptr != NULL )
        {
            int i;
            for(i=0; i<strlen(cptr); i++)
            {
                if(!isdigit(cptr[i]))
                {
                    fprintf(stderr, "WARNING: The current value \"%s\" set for the token \"%s\"\n", tmp, token); 
                    fprintf(stderr, "         is not valid.\n"); 
                    return -1;
                }
            }
            total *= atoi(cptr);

            cptr = strtok(NULL, "*");
        }
        
        free(tmp);

        return total;
    }
}


/* ********************************/
/* Store time data structure      */
/* ********************************/
static void store_TStime ( TIMEDATA *TData)
{
    extern    PAGE_MGR      *PageMgr;

    char buf[ANSI_TIME_LEN];

    time_t Begin, End;

    sprintf(buf,"%.4d-%.2d-%.2d %.2d:%.2d:%.2d",TData->BegyValue,TData->BegmValue,
                         TData->BegdValue,TData->BeghValue, 0, 0);


    yearsec_ansi_to_timet (buf, &Begin);

    sprintf(buf,"%.4d-%.2d-%.2d %.2d:%.2d:%.2d",TData->EndyValue,TData->EndmValue,
                         TData->EnddValue,TData->EndhValue,0,0);

    yearsec_ansi_to_timet (buf, &End);

    TData->BeginTime   = Begin;
    TData->EndTime     = End;
    PageMgr->BeginTime = Begin;
    PageMgr->EndTime   = End;

}


/* ************************************************************** */
/* TS Control Dialog can be called by a standalone application    */
/* or by a callbacks function from any type of button             */
/* ************************************************************** */
void    show_TSControlDS(Widget w, TSGEN_INFO tsgen)
{

    extern PAGE_MGR      *PageMgr;
    extern GROUP_INFO      *GroupInfo;

    int    pos = 1;
    int    flag;
        int     manage_timeseries_gui = 0;
    char    lid[9];

    static int first = 1;
    
    
    char mode_token[]        = "timeseries_mode";
    char begintime_token[]    = "timeseries_begintime";
    char endtime_token[]    = "timeseries_endtime";
    int mode_token_length, mode_value_length;
    char mode_token_value[128];

    /* ********************************************************** */
    /*   Create TS Control Dialog for the first time only and     */
    /*   just manage it when called  and unmanage it when don't   */
    /*  need.                               */
    /* ********************************************************** */
    if (! TSControlDS && first == 1)
        {

        TSinit_memory ( );
           create_TSControlDS(GetTopShell(w));         
           TSControl_callbacks(); 

        int tmpDays;
        tmpDays = get_DefaultTime(begintime_token);
        if(tmpDays == -1)
            GroupInfo->days_back    = 5; /* default to 5 days */
        else
            GroupInfo->days_back    = tmpDays;
            
        tmpDays = get_DefaultTime(endtime_token);
        if(tmpDays == -1)
            GroupInfo->days_forward    = 3; /* default to 3 days */
        else
            GroupInfo->days_forward    = tmpDays;
            
        init_TStime( 0, 0);
        loadTSGrp_List();
        setTSStn_byPos( pos );

        PageMgr->standalone = 0;
        if ( tsgen.standalone )
        {
            SetLabel(TSC_ClosePB,"Exit");
            PageMgr->standalone = 1;
        }
         
        first = 0;
       }

    flag = 0;
    strcpy(lid,tsgen.lid);

    /* **************************************************** */
    /*  When there is no Lid is selected, List of LID on    */
    /*  the Time Series Control Dialog will be default      */
    /*  to all                                              */
    /* **************************************************** */
       
    if ( strlen (lid) <=2 ) /* No lid is selected from tsgen structure */
    {
        pos = 1;
        setTSStn_byPos ( pos );
        flag = 1;
        tsgen.pedtse_defined = 0;

    }
    else if ( tsgen.group_check )
    {
        pos = search_GroupList(lid); /* Check GroupName if matched lid */
        if ( pos >= 1 )
        {
            setTSGrp_byPos(  pos );
            strcpy(Group.nameselected,lid);
            flag = 1;

        }

    }

    if ( flag == 0) /* Check if Lid is in StnList */
    {
        pos = search_StnList(lid);
        if ( pos >= 1)
            setTSStn_byPos ( pos );
        else
        {
            set_stnclass_allon();
            pos = 1;
            setTSStn_byPos ( pos );

            pos = search_StnList(lid);

            if ( pos <= 0) pos = 1;

            setTSStn_byPos ( pos );
        }

    }

     if (! XtIsManaged(TSControlDS))
       {
              XtManageChild(TSC_FO);
              XtManageChild(TSControlDS);
        XtMoveWidget(TSControlDS, 5,0);

       }

    /* ************************************************ */
    /* Raise the Time Series Control Dialog when called */
    /* ************************************************ */

    XRaiseWindow(XtDisplay(TSControlDS), XtWindow(TSControlDS));

    if ( tsgen.pedtse_defined )
    {

        /* ******************************************************** */
        /*   Initilize start time & end time for TSControl Dialog.  */
        /*   Load the stattion list and hight light a selected Lid  */
        /* ******************************************************** */

        init_TStime( tsgen.Begin_time, tsgen.End_time);

        pos = load_hightlight( tsgen );

        /* ******************************************************** */
        /*  If the Lid is found in the station list then activate   */
        /*  the Time Series Control Display. Time Series Display    */
        /*  will  display as in either TAB mode or GRAPH  mode      */
        /*  depends on the tsgen.display_mode                       */
        /* ******************************************************** */
        if ( pos >= 1)
        {
            if ( tsgen.display_mode == GRAPH_TS)
                TSC_Graph_CB ( w, (XtPointer )NULL, (XtPointer ) NULL);
            else
                TSC_Tab_CB   ( w, (XtPointer )NULL, (XtPointer ) NULL);

        }

    }

    /* If the timeseries starts with STANDALONE,
     * need check the apps_defaults file if the 
     * customer sets up timeseries mode token value 
     * "timeseries_mode", if it is set to "group", 
     * then display in group mode.
     * added by guoxian zhou 05-2004
     */

    if ( tsgen.standalone )
    {
        int rcfad=0;
        mode_token_length = strlen(mode_token);
        rcfad = get_apps_defaults(mode_token, &mode_token_length, mode_token_value, &mode_value_length);

        if ( rcfad == 0)
        {
            /*Convert to lower case*/
            char* tmp =    (char*)malloc((mode_value_length+1) * sizeof(char));
            int i;
            
            memset(tmp, '\0', mode_value_length + 1);
            
            for(i=0; i < mode_value_length; i++)
                tmp[i] = tolower(mode_token_value[i]);

            if(!strcmp(tmp, "group"))
            {
                GroupInfo->GroupSelected = GROUP; 

                if (Group.ngroups <= 0)
                {
                    DeSensitize(TSC_GraphPB);
                    DeSensitize(TSC_TabPB);
                }
                else
                {
                    Sensitize(TSC_GraphPB);
                    Sensitize(TSC_TabPB);
                }

                 XtUnmanageChild(TSC_stnclassFO);
                  XtUnmanageChild(TSC_StationFO);
                  XtManageChild(TSC_GroupFO);

                SetMenuPos ( TSC_GroupStationOM, GROUP);
            }
            /* If user sets the token value to "station",
             * then do nothing since the default mode is station.
             * otherwise, print out warning message
             */
            else if(strcmp(tmp, "station"))
            {
                fprintf(stderr, "WARNING: The current value \"%s\" set for the token \"%s\"\n", mode_token_value, mode_token);
                fprintf(stderr, "         is not valid.\n"); 
            }

            free(tmp);
        }
        else
            fprintf(stderr, "STATUS: There is no value currently set for the token \"%s\".\n", mode_token);
    }

   /* Create the TimeSeries graph but do not manage it.  This is being
      done so that the TimeSeries graph widgets have resources allocated 
      to them.  This allows their states to be manipulated even before
      the TimeSeries graph has been first displayed. */
   show_TimeSeries ( w, (XtPointer) & manage_timeseries_gui, NULL );

   return;

}

/* ******************************************* */
/* TSControl_callbacks(): To add all callbacks */
/* for the Time Series Control Dialog          */
/* ******************************************* */
void TSControl_callbacks() 
{

    Atom    atom;
    atom = XmInternAtom(XtDisplay(TSControlDS),"WM_DELETE_WINDOW",False);

    /* ************************************* */
    /* Adding Callbacks for TSControl Dialog */
    /* ************************************* */

     XtAddCallback(TSC_ArrowEndUp,     XmNactivateCallback, TSC_ArrowEndUp_CB,     NULL);
     XtAddCallback(TSC_ArrowEndDown,   XmNactivateCallback, TSC_ArrowEndDown_CB,     NULL);
     XtAddCallback(TSC_ArrowBeginUp,   XmNactivateCallback, TSC_ArrowBeginUp_CB,     NULL);
     XtAddCallback(TSC_ArrowBeginDown, XmNactivateCallback, TSC_ArrowBeginDown_CB,     NULL);

     XtAddCallback(TSC_EndmonthPB,  XmNactivateCallback, TSC_Endmonth_CB,     NULL);
     XtAddCallback(TSC_EnddayPB,      XmNactivateCallback, TSC_Endday_CB,     NULL);
     XtAddCallback(TSC_EndyearPB,   XmNactivateCallback, TSC_Endyear_CB,     NULL);
     XtAddCallback(TSC_EndhourPB,      XmNactivateCallback, TSC_Endhour_CB,     NULL);

     XtAddCallback(TSC_BeginmonthPB,XmNactivateCallback, TSC_Beginmonth_CB, NULL);
     XtAddCallback(TSC_BegindayPB,  XmNactivateCallback, TSC_Beginday_CB,     NULL);
     XtAddCallback(TSC_BeginyearPB, XmNactivateCallback, TSC_Beginyear_CB,  NULL);
     XtAddCallback(TSC_BeginhourPB, XmNactivateCallback, TSC_Beginhour_CB,  NULL);

     XtAddCallback(TSC_GroupPB,     XmNactivateCallback, TSC_Group_CB,     NULL);
     XtAddCallback(TSC_StationPB,     XmNactivateCallback, TSC_Station_CB,     NULL);

     XtAddCallback(TSC_allPB,     XmNactivateCallback,     TSC_Class_allCB,      NULL);
     XtAddCallback(TSC_heightTB,     XmNvalueChangedCallback, TSC_Class_CB, (XtPointer) 0);
     XtAddCallback(TSC_tempTB,     XmNvalueChangedCallback, TSC_Class_CB, (XtPointer) 1);
     XtAddCallback(TSC_precipTB,     XmNvalueChangedCallback, TSC_Class_CB, (XtPointer) 2);
     XtAddCallback(TSC_snowTB,     XmNvalueChangedCallback, TSC_Class_CB, (XtPointer) 3);
     XtAddCallback(TSC_otherTB,     XmNvalueChangedCallback, TSC_Class_CB, (XtPointer) 4);

     XtAddCallback(TSC_GroupLI,     XmNsingleSelectionCallback,   TSC_GroupLI_CB,  NULL);
     XtAddCallback(TSC_PCodeLI ,    XmNmultipleSelectionCallback, TSC_PCodeLI_CB, NULL);

     XtAddCallback(TSC_lidTB,     XmNvalueChangedCallback, reorder_ts_CB, (XtPointer) 0);
     XtAddCallback(TSC_nameTB,     XmNvalueChangedCallback, reorder_ts_CB, (XtPointer) 1);

     XtAddCallback(TSC_StationLI,   XmNsingleSelectionCallback,   TSC_StationLI_CB,NULL);

     XtAddCallback(TSC_StationTX,   XmNmodifyVerifyCallback, TSC_SearchMode, NULL);

     XtAddEventHandler(TSC_StationTX, KeyPressMask,  False,  (XtEventHandler)TSCkey_CB,  NULL);

     XtAddCallback(TSC_GraphPB,     XmNactivateCallback, TSC_Graph_CB, NULL);
     XtAddCallback(TSC_TabPB,         XmNactivateCallback, TSC_Tab_CB,   NULL);

     XtAddCallback(TSC_ClosePB,     XmNactivateCallback, TSC_Close_CB, NULL);

     XmAddWMProtocolCallback(TSControlDS, atom,TSC_Close_CB, NULL);


}

/************************************************* */
/* loadTSGrp_List():  Load group information from  */
/* group_definition.cfg file that setup by user    */
/* Load only the Group's name but the details will */
/* be loaded later by other funtion                */
/*                                                 */
/************************************************* */
void loadTSGrp_List()
{
    FILE    *fp;
    char    *rbuf;

    char     buf[200],
        gad_value[128];

    int    pos,
        ngroups = 0;
    int        gad_token_len=0, gad_value_len=0, rcfad=0;


    /* ******************************************** */
    /* Get the application data directory path name */
    /* ******************************************** */

    gad_token_len = strlen("whfs_config_dir");
    rcfad = get_apps_defaults("whfs_config_dir", &gad_token_len, gad_value, &gad_value_len);

    if ( rcfad == 0 )
        sprintf(buf,"%s/timeseries/group_definition.cfg",gad_value);
    else
        sprintf(buf,"%s/timeseries/group_definition.cfg",".");
        
    if( (fp = fopen(buf,"r")) == NULL)
    {
        printf("Error: open %s Failed.\n",buf);
        return;

    }

    /* ********************************************* */
    /* Delete all items in Group List and  Load them */
    /* from group_definition.cfg file                */
    /* ********************************************* */
    XmListDeleteAllItems(TSC_GroupLI);
    while ( fgets(buf,200, fp) && (ngroups < MAX_GROUPS))
    {
        if(strstr (buf,"#"))
            ; /* skip comment lines */
        else if(strstr (buf,"Group:"))
        {
            rbuf = get_TSgroupName(buf);
            strcpy(Group.name[ngroups],rbuf);
            ngroups++;
        }
    }

    Group.ngroups = ngroups;
    pos = 1;

    loadXmList100(TSC_GroupLI, Group.name, ngroups);
    XmListSelectPos(TSC_GroupLI, pos, True);

    fclose(fp);
}

/***************************************************************** */
/* setTSGroup_byPos( int pos): Highlight the selected group's name */
/* based on user's selection                                       */
/***************************************************************** */
void setTSGrp_byPos(int pos)
{
    int     ngroups = 0;

    ngroups  =  Group.ngroups;

    XmListDeleteAllItems(TSC_GroupLI);

    loadXmList100(TSC_GroupLI, Group.name, ngroups);
    XmListSelectPos(TSC_GroupLI, pos, True);
    XmListSetPos(TSC_GroupLI, pos);

          XtUnmanageChild(TSC_StationFO);
          XtManageChild(TSC_GroupFO);
    SetMenuPos ( TSC_GroupStationOM, GROUP);
    GroupInfo->GroupSelected = GROUP;
        
    return;
}

/* ********************************************************************** */
/* setTSGrp_byLid(const char *lid) : Check if the selected station Lid    */ 
/* matches the group's name - If matches then Group's mode is active      */
/************************************************************************ */
void    setTSGrp_byLid(const char *lid)
{
    extern    GROUP_INFO    *GroupInfo;

    int     n, 
        pos = 0;

    for ( n = 0; n < Group.ngroups; n++)
    {
        if( strcmp(Group.name[n],lid) == 0)
        pos = (n + 1);
    }

    if ( pos )
    {
              XtUnmanageChild(TSC_StationFO);
              XtManageChild(TSC_GroupFO);
        SetMenuPos ( TSC_GroupStationOM, GROUP);
        setTSGrp_byPos ( pos );
        GroupInfo->GroupSelected = GROUP;
    }

}

/* ******************************************************** */
/*  search_GroupList(): return position of                  */
/*  a group's name that matches the request's name          */
/* ******************************************************** */
int    search_GroupList(const char *str)
{
    int        n,
            ngroups;
    
    ngroups = Group.ngroups;

    for (n = 0; n < ngroups; n++)
    {
        
        if (strcmp(Group.name[n], str) == 0)
        {
            return(++n);
        }
    
    }

    return(-1);
}

/* ************************************************* */
/* loadTSStn_List(): Load list of available stations */
/* from database station table                       */
/* ************************************************* */
int    loadTSStn_List(const char *where, int pos)
{
    extern PAGE_MGR *PageMgr;

    XmStringTable    xmStr;
    LocView        *lvPtr;
    StnClass    *classPtr;
    
    char        buf[100],
            search_str[10];
    
    int         i,
            n,
            class_on_cnt,
            num_search,
            actual_cnt=0,
            valid_class,
            ctr1=0, ctr2=0;

    static    int    cnt = 0;
    static    int    first_time = 1;
    
    /* ******************************************* */
        /* Get the list of valid locations from the    */
    /* hydrologic database.                        */
    /* ******************************************* */

    if ( locviewHead == NULL || first_time )
    {
        locviewHead  = GetLocView( (char *) where);
        StnClassHead = GetStnClass((char *) where);
        cnt     = ListCount(&locviewHead->list);
        LlidPtr = malloc(sizeof(LISTLID) * cnt);
        set_stnclass_allon();
        first_time  =  0;

    }

    /* ********************************************** */
    /* Check for filter options if they are selected  */
    /* (River/Precip/Temperature/Snow/Other/All) from */
    /* Time Series Control Dialog.                    */
    /* ********************************************** */
    if(class_options[5])
            num_search = 0;
    else
    {

        class_on_cnt = 0;
        for ( n = 0; n< 5; n++)
            if( class_options[n] == 1)class_on_cnt++;


        if( class_on_cnt == 0)
        {
            XmListDeleteAllItems(TSC_StationLI);
            XmListDeleteAllItems(TSC_PCodeLI);
            PageMgr->error_report = 1;
            return(actual_cnt);
        }

        memset(search_str, '\0', 10);

        if(class_options[0])
            strcat(search_str,"FDR");
        if(class_options[1])
            strcat(search_str,"T");
        if(class_options[2])
            strcat(search_str,"P");
        if(class_options[3])
            strcat(search_str,"S");
        if(class_options[4])
            strcat(search_str,"O");

        num_search = strlen(search_str);
    }



    /* ********************************************** */
        /* Only show stations that not being filtered out */
    /* from the filter options on the Time Series     */
    /* Control Dialog.                                */
    /* ********************************************** */
    if (locviewHead != NULL && StnClassHead != NULL)
    {
        ctr1 = ListCount(&locviewHead->list);
        ctr2 = ListCount(&StnClassHead->list);
        if (ctr1 != ctr2)
        {
            printf("Error: Number of records in LocView '%d' does not equal"
                " number of records in StnClass '%d'.\n", ctr1, ctr2);
            printf(" Note: Run the set station class program to correct this problem.\n");
            return(0);
        }
        
        xmStr = (XmStringTable) XtMalloc(ctr1 * sizeof(XmString *));

        lvPtr = (LocView *) ListFirst(&locviewHead->list);
        classPtr = (StnClass *) ListFirst(&StnClassHead->list);

        actual_cnt      = 0;
        num_active_lids = 0;

        for (i = 0; i<ctr1 ; i++)
        {           
            valid_class = 0;
            if( num_search == 0 || strlen(search_str) == 0 )
                valid_class = 1;
            else
            {
                for(n=0; n< num_search; n++)
                {
                    if ( (classPtr != NULL) && (strchr(classPtr->disp_class,search_str[n])) )
                    {
                        valid_class = 1;
                        break;
                    }
                 }
            }
        
            if( valid_class)
            {
                /*by gzhou 04-2004
                sprintf(buf, "%-10s %-25s", lvPtr->lid, lvPtr->name);
                   xmStr[actual_cnt++] = XmStringCreateSimple(buf);
                */

                strcpy((LlidPtr+num_active_lids)->lid,lvPtr->lid);
                strcpy((LlidPtr+num_active_lids)->name,lvPtr->name);
                num_active_lids++;
            }

            if (lvPtr != NULL)
                lvPtr    = (LocView *)  ListNext(&lvPtr->node);
            if (classPtr != NULL)
                classPtr = (StnClass *) ListNext(&classPtr->node);
        }
        
        /* Reorder the time series according to the search option */
        
        reorder_ts();
        
        /*
            Delete old items
        */

        XmListDeleteAllItems(TSC_StationLI);
        
        
        /*
                    Load the list box with the selected items.
            */

        
        for (i = 0; i<num_active_lids; i++)
        {
            sprintf(buf, "%-10s %-25s", (LlidPtr+i)->lid, (LlidPtr+i)->name);
            xmStr[i] = XmStringCreateSimple(buf);
        }

          XmListAddItems(TSC_StationLI, xmStr, num_active_lids, 1);
           XmListSelectPos(TSC_StationLI, pos, True);

            /*
                    cleanup and return.
            */
        for (i = 0; i < num_active_lids; i++)
                XmStringFree(xmStr[i]);

        XtFree((char *) xmStr);    
    }

    
    class_options[5] = 0;
    return(num_active_lids);
}

/* ************************************************* */
/* setTSStn_byLid(): Hightlight the selected Lid     */
/* from station list but only load the PeTSed only   */
/* when Lid entered matched                          */
/* ------------------------------------------------- */
void    setTSStn_byLid( const char *lid)
{

    int    pos;

    pos = search_StnList(lid);
    if(pos <= 0) pos = 1;
    setTSStn_byPos ( pos );

       XmListSetPos(TSC_StationLI, pos);
       XmListSelectPos(TSC_StationLI, pos, True);
    XmListSetPos(TSC_StationLI, pos);


          XtManageChild(TSC_stnclassFO);
          XtUnmanageChild(TSC_GroupFO);
          XtManageChild(TSC_StationFO);
    SetMenuPos ( TSC_GroupStationOM, STATION);
    GroupInfo->GroupSelected = STATION;

    return;
}

/* ************************************************** */
/* setTSStn_byPos(): Load Time Series Station list by */
/* Postision in the list.                             */
/* ************************************************** */
void    setTSStn_byPos( int pos )
{
    char    where[255];
    int    cnt = 0;

    memset(where, '\0', sizeof(where));
    sprintf(where,"WHERE lid in (select distinct(lid) from ingestfilter where ingest='T') ORDER BY lid");
    cnt = loadTSStn_List(where, pos);
    if (cnt <= 0)
    {
        DeSensitize(TSC_GraphPB);
        DeSensitize(TSC_TabPB);
    }
    else
    {
        Sensitize(TSC_GraphPB);
        Sensitize(TSC_TabPB);
    }
        
       XmListSetPos(TSC_StationLI, pos);
       XmListSelectPos(TSC_StationLI, pos, True);
    XmListSetPos(TSC_StationLI, pos);

          XtUnmanageChild(TSC_GroupFO);
          XtManageChild(TSC_StationFO);
    SetMenuPos ( TSC_GroupStationOM, STATION);
    GroupInfo->GroupSelected = STATION;
    return;

}

/* ************************************************** */
/* Search for and return the postion that matches the */
/* requested Lid.                                     */
/* ************************************************** */
int    search_StnList(const char *str)
{
    int        len,
            i;
    
    
    memset(current_lid,'\0',strlen(current_lid));

    len    = strlen(str);

    for (i = 0; i<num_active_lids; i++)
    {
        
        if (strncmp((LlidPtr+i)->lid, str, len) == 0)
        {
            strcpy(current_lid,(LlidPtr+i)->lid);
            return(++i);
        }
    
    
    }

    return(-1);
}


/* ************************************************** */
/* Search for Lid by position from the station list   */
/* from Time Series Control Dialog                    */
/* ************************************************** */
char     *getLidFromStnList(const int pos)
{
    int        i;
    static         char  lid[20];
    
    for (i = 0; num_active_lids; i++)
    {
        if (i == (pos - 1))
        {
            strcpy(lid,(LlidPtr+i)->lid);
            return(lid);
        }
            
    }
    
    
    return(NULL);
}  

/* ***************************************************** */
/* Load PeTSED data from database in Ingestfilter tabel  */
/* that matches lid.                                     */
/* ***************************************************** */
void load_Ingestfilter ( char *lid )
{
    IngestFilter    *iHead = NULL ,
            *iPtr = NULL ;

    char            where[255],
                buf[BUFSIZ],
            *durtext;

    int        cnt, 
            select_pos; 
            
    char        *shefpe_name,
            *shefts_name;
    
    PedTSep.num_items = 0;

    sprintf(where, " WHERE lid= '%s' and ingest= 'T' "
                   " ORDER BY pe, ts, extremum, dur", lid);
    
    if ((iHead = GetIngestFilter(where)) != NULL)
    {

        cnt  = ListCount(&iHead->list);
        if ( cnt <= 0) 
        {
            printf("There is no time series available for this station %s\n",lid); 
            if ( iHead )
                FreeIngestFilter(iHead); 
            return;
        }

        if ( PedTSep.display_name != NULL)
            free ( PedTSep.display_name );

        if ( PedTSep.name != NULL)
            free ( PedTSep.name );

        PedTSep.display_name = (RussText *)malloc(sizeof(RussText)*(cnt+2));
        PedTSep.name         = (RussText *)malloc(sizeof(RussText)*(cnt+2));

        iPtr = (IngestFilter *) ListFirst(&iHead->list);

        while (iPtr)
        {
            /*    find description in plain language
                for Physical Element and Type Source
            */
            
            shefpe_name = load_PEdesc(iPtr->pe);
            shefts_name = load_TSdesc(iPtr->ts);
            
            /* 
                convert duration to text corresponding 
                with Yrs/Mos/Hrs/Min/Season/PerRec 
            */
            
            durtext = conv_dur2text ( iPtr->dur );
            
            sprintf(buf,"%-5s%-6s%-3s%-9s%s=%-22s%s=%s", 
                iPtr->pe, iPtr->ts, iPtr->extremum, durtext, 
                iPtr->pe, shefpe_name, iPtr->ts, shefts_name);
            
            
            strcpy((char *)&PedTSep.display_name[PedTSep.num_items],
                   (char *)&buf[0]);

            /* 
                keep original durations for/when loading data 
            */

            sprintf(buf,"%s  %d  %s  %s", iPtr->pe,  iPtr->dur , iPtr->ts, 
                iPtr->extremum);
            strcpy((char *)&PedTSep.name[PedTSep.num_items],(char *)&buf[0]);

            PedTSep.num_items++;

            iPtr = (IngestFilter *) ListNext(&iPtr->node);
         }


    }

    XmListDeleteAllItems(TSC_PCodeLI);
    select_pos = sort_display_name (  PedTSep.display_name, PedTSep.name, PedTSep.num_items);
    loadXmList100(TSC_PCodeLI, PedTSep.display_name, PedTSep.num_items);
    XmListSelectPos(TSC_PCodeLI, select_pos, True);

    if ( iPtr )
        FreeIngestFilter(iPtr); 
    if ( iHead )
        FreeIngestFilter(iHead); 
    return;
}

/************************************ */
/* Sort number items of PeTSED data   */
/* By pe then ts order ( double sort )*/
/************************************ */
int  sort_display_name (RussText *durAsText, RussText *durAsCode, int nitems)
{

    /* durAsText is a list of PETSEDs which have the duration field spelled */
    /* out while durAsCode is a list of PETSEDs which have the duration as */
    /* a SHEF code.  nitems is the number of items in the list */

    char     pe1[10],
        ts1[10],
        pe2[10],
        ts2[10];

    char    peSortByChar[] = {'H', 'Q', 'P', 'S', 'T'};

    int    ctr1=0, ctr2=0, numSorts=0, numSorted=0;

    RussText *tempBuf1 = NULL, *tempBuf2 = NULL;
    RussText oneTempRecord;    

    /* ********************************** */
    /* Sort by PEs in peSortByChar order  */
    /* ********************************** */

    /* allocate memory for temporary storage of PETSED entries */
    tempBuf1 = (RussText *)malloc(sizeof(RussText)*(nitems));
    tempBuf2 = (RussText *)malloc(sizeof(RussText)*(nitems));
    
    /* calculate number of PE sort by criteria
       HardCode to 5 */
    numSorts = 5;
    numSorted = 0;

    /* loop through all entries and pull out those whose first charater */
    /* in the PE match the peSortByChar and copy this entry to a temp list */
    for (ctr1 = 0; ctr1 < numSorts; ctr1++)
    {
        for(ctr2 = 0; ctr2 < nitems; ctr2++)
        {
            sscanf(durAsText[ctr2],"%s %s",pe1,ts1);
            if (pe1[0] == peSortByChar[ctr1])
            {
                strcpy(tempBuf1[numSorted], durAsText[ctr2]);
                strcpy(tempBuf2[numSorted], durAsCode[ctr2]);
                numSorted++;
            }
        }
    }

    /* loop through all entries and pull out those whose first charater */
    /* in the PE do NOT match the peSortByChar and add to the temp list */
    for (ctr2 = 0; ctr2 < nitems; ctr2++)
    {
        sscanf(durAsText[ctr2],"%s %s",pe1,ts1);
        if ( (pe1[0] != peSortByChar[0]) &&
             (pe1[0] != peSortByChar[1]) &&
             (pe1[0] != peSortByChar[2]) &&
             (pe1[0] != peSortByChar[3]) &&
             (pe1[0] != peSortByChar[4]) )
        {
            strcpy(tempBuf1[numSorted], durAsText[ctr2]);
            strcpy(tempBuf2[numSorted], durAsCode[ctr2]);
            numSorted++;
        }
    }


    /* ************************* */
    /* Sort by ts in R P F order */
    /* ************************* */

    /* now bubble up the TS within the same PE */
    /* outer loop is from 1st array position through the next to last */
    /* inner loop is from outer loop position plus one through the last */
    for (ctr1 = 0; ctr1 < nitems-1; ctr1++)
    {
        for (ctr2 = 0; ctr2 < nitems-1; ctr2++)
        {
        sscanf(tempBuf1[ctr2], "%s %s",pe1,ts1);
        sscanf(tempBuf1[ctr2+1], "%s %s",pe2,ts2);
        if (strncmp(pe1, pe2, 2) == 0)
        {
            if ( ((ts1[0] == 'P') && (ts2[0] == 'R')) ||
                 ((ts1[0] == 'F') && (ts2[0] == 'R')) ||
                 ((ts1[0] == 'F') && (ts2[0] == 'P')) )
            {
                strcpy(oneTempRecord, tempBuf1[ctr2]);
                strcpy(tempBuf1[ctr2], tempBuf1[ctr2+1]);
                strcpy(tempBuf1[ctr2+1], oneTempRecord);

                strcpy(oneTempRecord, tempBuf2[ctr2]);
                strcpy(tempBuf2[ctr2], tempBuf2[ctr2+1]);
                strcpy(tempBuf2[ctr2+1], oneTempRecord);

            } /* if there is a need to swap */

        } /* if PEs are the same */

        } /* inner for loop */

    } /* outer for loop */

    /* copy the temporary sorted lists to the original list passed in */
    for (ctr2 = 0; ctr2 < nitems; ctr2++)
    {
        strcpy(durAsText[ctr2], tempBuf1[ctr2]);
        strcpy(durAsCode[ctr2], tempBuf2[ctr2]);
    }

    /* free memory used for the temporary lists */
    if ( tempBuf1 )
        free(tempBuf1);
    if ( tempBuf2 )
        free(tempBuf2);

    return ( 1 );
}

/* **************************************** */
/* Initialize the start time and endtime    */
/* for the Time Series Control Dialog       */
/* **************************************** */
void init_TStime(time_t starttime, time_t endtime)
{
    extern    PAGE_MGR     *PageMgr;
    extern    GROUP_INFO        *GroupInfo;

    time_t     BeginTime, 
        EndTime, 
        Curtime;
    int    days_back,
        days_forward;

    XmString    xmstr[5];
    char        buf[10];

    if ( starttime == 0 && endtime == 0)
    {
        time(&Curtime);

        Curtime /= SECONDS_PER_HOUR;
        Curtime *= SECONDS_PER_HOUR;
        days_back    = GroupInfo->days_back;
        days_forward = GroupInfo->days_forward;
        BeginTime = Curtime - days_back  * HOURS_PER_DAY * SECONDS_PER_HOUR;
        EndTime   = Curtime + days_forward * HOURS_PER_DAY * SECONDS_PER_HOUR;
        
        /* Need check if BeginTime <= EndTime,
         * if not, set EndTime = BeginTime + 1 hour.
         * Added by guoxian zhou 
         */
         
        if(BeginTime > EndTime) 
        {
            printf("ERROR: The \"EndTime\" is less than \"BeginTime\"!\n");
            EndTime = BeginTime + SECONDS_PER_HOUR;        
        }
    }
    else
    {
        BeginTime = starttime;
        EndTime   = endtime;

    }
    conv_mdyh(BeginTime, &TimeData.BegmValue,&TimeData.BegdValue,&TimeData.BegyValue,&TimeData.BeghValue);
    conv_mdyh(EndTime,   &TimeData.EndmValue,&TimeData.EnddValue,&TimeData.EndyValue,&TimeData.EndhValue);

    sprintf(buf,"%.2d",TimeData.BegmValue);
    xmstr[0] = XmStringCreateLtoR ( buf, (XmStringCharSet)XmFONTLIST_DEFAULT_TAG );
    sprintf(buf,"%.2d",TimeData.BegdValue);
    xmstr[1] = XmStringCreateLtoR ( buf, (XmStringCharSet)XmFONTLIST_DEFAULT_TAG );
    sprintf(buf,"%.4d",TimeData.BegyValue);
    xmstr[2] = XmStringCreateLtoR ( buf, (XmStringCharSet)XmFONTLIST_DEFAULT_TAG );
    sprintf(buf,"%.2d",TimeData.BeghValue);
    xmstr[3] = XmStringCreateLtoR ( buf, (XmStringCharSet)XmFONTLIST_DEFAULT_TAG );

    XtVaSetValues(TSC_BeginmonthPB, XmNlabelString, xmstr[0], NULL),
    XtVaSetValues(TSC_BegindayPB,   XmNlabelString, xmstr[1], NULL),
    XtVaSetValues(TSC_BeginyearPB,  XmNlabelString, xmstr[2], NULL),
    XtVaSetValues(TSC_BeginhourPB,  XmNlabelString, xmstr[3], NULL),

    sprintf(buf,"%.2d",TimeData.EndmValue);
    xmstr[0] = XmStringCreateLtoR ( buf, (XmStringCharSet)XmFONTLIST_DEFAULT_TAG );
    sprintf(buf,"%.2d",TimeData.EnddValue);
    xmstr[1] = XmStringCreateLtoR ( buf, (XmStringCharSet)XmFONTLIST_DEFAULT_TAG );
    sprintf(buf,"%.4d",TimeData.EndyValue);
    xmstr[2] = XmStringCreateLtoR ( buf, (XmStringCharSet)XmFONTLIST_DEFAULT_TAG );
    sprintf(buf,"%.2d",TimeData.EndhValue);
    xmstr[3] = XmStringCreateLtoR ( buf, (XmStringCharSet)XmFONTLIST_DEFAULT_TAG );

    XtVaSetValues(TSC_EndmonthPB, XmNlabelString, xmstr[0], NULL),
    XtVaSetValues(TSC_EnddayPB,   XmNlabelString, xmstr[1], NULL),
    XtVaSetValues(TSC_EndyearPB,  XmNlabelString, xmstr[2], NULL),
    XtVaSetValues(TSC_EndhourPB,  XmNlabelString, xmstr[3], NULL),

    XmStringFree ( xmstr [ 0 ] );
    XmStringFree ( xmstr [ 1 ] );
    XmStringFree ( xmstr [ 2 ] );
    XmStringFree ( xmstr [ 3 ] );

    PageMgr->BeginTime = BeginTime;
    PageMgr->EndTime   = EndTime;
    memset(Group.prev_nameselected,'\0',strlen(Group.prev_nameselected));

}

/* ******************************************** */
/* Convert months/days/years/hours from seconds */
/* ******************************************** */
void conv_mdyh(time_t secs, int *m, int *d, int *y, int *h)
{
    char buf[ANSI_TIME_LEN];

    struct    tm     *tm_ptr;

    tm_ptr = gmtime(&secs);
    strftime(buf, sizeof(buf), "%m %d %Y %H", tm_ptr);
    sscanf(buf,"%d %d %d %d",m,d,y,h);
}

/* ***************************************************** */
/* callbacks() to handle the up arrow for Beginning time */
/* ***************************************************** */
void TSC_ArrowBeginUp_CB ( Widget w, XtPointer ptr, XtPointer cbs)
{
    XmString    xmstr = NULL ;
    char        buf[10];

    int        daysinmonth;

     if ( Bmonth )
    {
        if (  TimeData.BegmValue++ >= 12) TimeData.BegmValue = 1;    
        sprintf(buf,"%.2d",TimeData.BegmValue);
        xmstr = XmStringCreateLtoR ( buf, (XmStringCharSet)XmFONTLIST_DEFAULT_TAG );
        XtVaSetValues(TSC_BeginmonthPB, XmNlabelString, xmstr, NULL);

        daysinmonth = day_in_month (TimeData.BegyValue, TimeData.BegmValue);
        if ( TimeData.BegdValue > daysinmonth)
        {
            TimeData.BegdValue = daysinmonth;
            sprintf(buf,"%.2d",TimeData.BegdValue);
            xmstr = XmStringCreateLtoR ( buf, (XmStringCharSet)XmFONTLIST_DEFAULT_TAG );
            XtVaSetValues(TSC_BegindayPB, XmNlabelString, xmstr, NULL);
        }




    }

     if ( Bday   )
    {
        daysinmonth = day_in_month (TimeData.BegyValue, TimeData.BegmValue);

        if (  TimeData.BegdValue++ >= daysinmonth) TimeData.BegdValue = 1;    
        sprintf(buf,"%.2d",TimeData.BegdValue);
        xmstr = XmStringCreateLtoR ( buf, (XmStringCharSet)XmFONTLIST_DEFAULT_TAG );
        XtVaSetValues(TSC_BegindayPB, XmNlabelString, xmstr, NULL);

    }

     if ( Byear   )
    {
        TimeData.BegyValue++; 
        sprintf(buf,"%.4d",TimeData.BegyValue);
        xmstr = XmStringCreateLtoR ( buf, (XmStringCharSet)XmFONTLIST_DEFAULT_TAG );
        XtVaSetValues(TSC_BeginyearPB, XmNlabelString, xmstr, NULL);

    }
     if ( Bhour   )
    {
        if ( TimeData.BeghValue++ >= 23) TimeData.BeghValue = 0;    
        sprintf(buf,"%.2d",TimeData.BeghValue);
        xmstr = XmStringCreateLtoR ( buf, (XmStringCharSet)XmFONTLIST_DEFAULT_TAG );
        XtVaSetValues(TSC_BeginhourPB, XmNlabelString, xmstr, NULL);

    }

    XmStringFree ( xmstr );
    store_TStime (&TimeData );
}

/* ******************************************************* */
/* callbacks() to handle the down arrow for Beginning time */
/* ******************************************************* */
void TSC_ArrowBeginDown_CB ( Widget w, XtPointer ptr, XtPointer cbs)
{

    XmString    xmstr = NULL ;
    char        buf[10];
    int        daysinmonth;

     if ( Bmonth )
    {
        if (  --TimeData.BegmValue <= 0) TimeData.BegmValue = 12;    
        sprintf(buf,"%.2d",TimeData.BegmValue);
        xmstr = XmStringCreateLtoR ( buf, (XmStringCharSet)XmFONTLIST_DEFAULT_TAG );
        XtVaSetValues(TSC_BeginmonthPB, XmNlabelString, xmstr, NULL);

        daysinmonth = day_in_month (TimeData.BegyValue, TimeData.BegmValue);
        if ( TimeData.BegdValue > daysinmonth)
        {
            TimeData.BegdValue = daysinmonth;
            sprintf(buf,"%.2d",TimeData.BegdValue);
            xmstr = XmStringCreateLtoR ( buf, (XmStringCharSet)XmFONTLIST_DEFAULT_TAG );
            XtVaSetValues(TSC_BegindayPB, XmNlabelString, xmstr, NULL);
        }


    }

     if ( Bday   )
    {
        daysinmonth = day_in_month (TimeData.BegyValue, TimeData.BegmValue);

        if (  --TimeData.BegdValue <= 0) TimeData.BegdValue = daysinmonth;    
        sprintf(buf,"%.2d",TimeData.BegdValue);
        xmstr = XmStringCreateLtoR ( buf, (XmStringCharSet)XmFONTLIST_DEFAULT_TAG );
        XtVaSetValues(TSC_BegindayPB, XmNlabelString, xmstr, NULL);

    }

     if ( Byear   )
    {
        TimeData.BegyValue--; 
        sprintf(buf,"%.4d",TimeData.BegyValue);
        xmstr = XmStringCreateLtoR ( buf, (XmStringCharSet)XmFONTLIST_DEFAULT_TAG );
        XtVaSetValues(TSC_BeginyearPB, XmNlabelString, xmstr, NULL);

    }
     if ( Bhour   )
    {
        if ( --TimeData.BeghValue <= 0) TimeData.BeghValue = 23;    
        sprintf(buf,"%.2d",TimeData.BeghValue);
        xmstr = XmStringCreateLtoR ( buf, (XmStringCharSet)XmFONTLIST_DEFAULT_TAG );
        XtVaSetValues(TSC_BeginhourPB, XmNlabelString, xmstr, NULL);

    }

    XmStringFree ( xmstr );
    store_TStime (&TimeData );

}

/* ************************************************** */
/* callbacks() to handle the up arrow for Ending time */
/* ************************************************** */
void TSC_ArrowEndUp_CB ( Widget w, XtPointer ptr, XtPointer cbs)
{

    XmString    xmstr = NULL ;
    char        buf[10];
    int         daysinmonth;

     if ( Emonth )
    {
        if (  TimeData.EndmValue++ >= 12) TimeData.EndmValue = 1;    
        sprintf(buf,"%.2d",TimeData.EndmValue);
        xmstr = XmStringCreateLtoR ( buf, (XmStringCharSet)XmFONTLIST_DEFAULT_TAG );
        XtVaSetValues(TSC_EndmonthPB, XmNlabelString, xmstr, NULL);

        daysinmonth = day_in_month (TimeData.EndyValue, TimeData.EndmValue);
        if ( TimeData.EnddValue > daysinmonth)
        {
            TimeData.EnddValue = daysinmonth;
            sprintf(buf,"%.2d",TimeData.EnddValue);
            xmstr = XmStringCreateLtoR ( buf, (XmStringCharSet)XmFONTLIST_DEFAULT_TAG );
            XtVaSetValues(TSC_EnddayPB, XmNlabelString, xmstr, NULL);
        }


    }

     if ( Eday   )
    {
        daysinmonth = day_in_month (TimeData.EndyValue, TimeData.EndmValue);

        if (  TimeData.EnddValue++ >= daysinmonth ) TimeData.EnddValue = 1;    
        sprintf(buf,"%.2d",TimeData.EnddValue);
        xmstr = XmStringCreateLtoR ( buf, (XmStringCharSet)XmFONTLIST_DEFAULT_TAG );
        XtVaSetValues(TSC_EnddayPB, XmNlabelString, xmstr, NULL);

    }

     if ( Eyear   )
    {
        TimeData.EndyValue++; 
        sprintf(buf,"%.4d",TimeData.EndyValue);
        xmstr = XmStringCreateLtoR ( buf, (XmStringCharSet)XmFONTLIST_DEFAULT_TAG );
        XtVaSetValues(TSC_EndyearPB, XmNlabelString, xmstr, NULL);

    }
     if ( Ehour   )
    {
        if ( TimeData.EndhValue++ >= 23) TimeData.EndhValue = 0;    
        sprintf(buf,"%.2d",TimeData.EndhValue);
        xmstr = XmStringCreateLtoR ( buf, (XmStringCharSet)XmFONTLIST_DEFAULT_TAG );
        XtVaSetValues(TSC_EndhourPB, XmNlabelString, xmstr, NULL);

    }

    XmStringFree ( xmstr );
    store_TStime (&TimeData  );

}

/* **************************************************** */
/* callbacks() to handle the down arrow for Ending time */
/* **************************************************** */
void TSC_ArrowEndDown_CB ( Widget w, XtPointer ptr, XtPointer cbs)
{

    XmString    xmstr = NULL ;
    char        buf[10];
    int        daysinmonth;

     if ( Emonth )
    {
        if ( --TimeData.EndmValue <= 0) TimeData.EndmValue = 12;    
        sprintf(buf,"%.2d",TimeData.EndmValue);
        xmstr = XmStringCreateLtoR ( buf, (XmStringCharSet)XmFONTLIST_DEFAULT_TAG );
        XtVaSetValues(TSC_EndmonthPB, XmNlabelString, xmstr, NULL);

        daysinmonth = day_in_month (TimeData.EndyValue, TimeData.EndmValue);
        if ( TimeData.EnddValue > daysinmonth)
        {
            TimeData.EnddValue = daysinmonth;
            sprintf(buf,"%.2d",TimeData.EnddValue);
            xmstr = XmStringCreateLtoR ( buf, (XmStringCharSet)XmFONTLIST_DEFAULT_TAG );
            XtVaSetValues(TSC_EnddayPB, XmNlabelString, xmstr, NULL);
        }


    }

     if ( Eday   )
    {
        daysinmonth = day_in_month (TimeData.EndyValue, TimeData.EndmValue);

        if (  --TimeData.EnddValue <= 0) TimeData.EnddValue = daysinmonth;    
        sprintf(buf,"%.2d",TimeData.EnddValue);
        xmstr = XmStringCreateLtoR ( buf, (XmStringCharSet)XmFONTLIST_DEFAULT_TAG );
        XtVaSetValues(TSC_EnddayPB, XmNlabelString, xmstr, NULL);

    }

     if ( Eyear   )
    {
        TimeData.EndyValue--; 
        sprintf(buf,"%.4d",TimeData.EndyValue);
        xmstr = XmStringCreateLtoR ( buf, (XmStringCharSet)XmFONTLIST_DEFAULT_TAG );
        XtVaSetValues(TSC_EndyearPB, XmNlabelString, xmstr, NULL);

    }
     if ( Ehour   )
    {
        if ( --TimeData.EndhValue <= 0) TimeData.EndhValue = 23;    
        sprintf(buf,"%.2d",TimeData.EndhValue);
        xmstr = XmStringCreateLtoR ( buf, (XmStringCharSet)XmFONTLIST_DEFAULT_TAG );
        XtVaSetValues(TSC_EndhourPB, XmNlabelString, xmstr, NULL);

    }

    XmStringFree ( xmstr );
    store_TStime (&TimeData  );


}

/* **************************************************** */
/* callbacks() to handle Beginning Month when selected  */
/* **************************************************** */
void TSC_Beginmonth_CB ( Widget w, XtPointer ptr, XtPointer cbs)
{
     Bmonth = 1; 
     Bday   = 0; 
     Byear  = 0; 
     Bhour  = 0; 
}

/* ************************************************** */
/* callbacks() to handle Beginning Day when selected  */
/* ************************************************** */
void TSC_Beginday_CB ( Widget w, XtPointer ptr, XtPointer cbs)
{
     Bmonth = 0; 
     Bday   = 1; 
     Byear  = 0; 
     Bhour  = 0; 
}
/* *************************************************** */
/* callbacks() to handle Beginning Year when selected  */
/* *************************************************** */
void TSC_Beginyear_CB ( Widget w, XtPointer ptr, XtPointer cbs)
{
     Bmonth = 0; 
     Bday   = 0; 
     Byear  = 1; 
     Bhour  = 0; 

}
/* *************************************************** */
/* callbacks() to handle Beginning Hour when selected  */
/* *************************************************** */
void TSC_Beginhour_CB ( Widget w, XtPointer ptr, XtPointer cbs)
{
     Bmonth = 0; 
     Bday   = 0; 
     Byear  = 0; 
     Bhour  = 1; 

}

/* *************************************************** */
/* callbacks() to handle Ending Month   when selected  */
/* *************************************************** */
void TSC_Endmonth_CB ( Widget w, XtPointer ptr, XtPointer cbs)
{
     Emonth = 1; 
     Eday   = 0; 
     Eyear  = 0; 
     Ehour  = 0; 
}

/* *************************************************** */
/* callbacks() to handle Ending Day     when selected  */
/* *************************************************** */
void TSC_Endday_CB ( Widget w, XtPointer ptr, XtPointer cbs)
{
     Emonth = 0; 
     Eday   = 1; 
     Eyear  = 0; 
     Ehour  = 0; 
}

/* *************************************************** */
/* callbacks() to handle Ending Year    when selected  */
/* *************************************************** */
void TSC_Endyear_CB ( Widget w, XtPointer ptr, XtPointer cbs)
{
     Emonth = 0; 
     Eday   = 0; 
     Eyear  = 1; 
     Ehour  = 0; 

}
/* *************************************************** */
/* callbacks() to handle Ending Hour    when selected  */
/* *************************************************** */
void TSC_Endhour_CB ( Widget w, XtPointer ptr, XtPointer cbs)
{
     Emonth = 0; 
     Eday   = 0; 
     Eyear  = 0; 
     Ehour  = 1; 

}

/* ************************************************** */
/* callbacks   to handle selection for group event    */
/* ************************************************** */
void TSC_Group_CB ( Widget w, XtPointer ptr, XtPointer cbs)
{

    extern    GROUP_INFO    *GroupInfo;

    GroupInfo->GroupSelected = GROUP; 

    /* ******************************************* */
    /* Swap group selection with station selection */
    /* ******************************************* */

    /* ****************************************** */
    /* stvo commented out 02/16/2000              */
    /* Added to load Group Information every time */
    /* loadTSGrp_List();                          */
    /* ****************************************** */


    if (Group.ngroups <= 0)
    {
        DeSensitize(TSC_GraphPB);
        DeSensitize(TSC_TabPB);
    }
    else
    {
        Sensitize(TSC_GraphPB);
        Sensitize(TSC_TabPB);
    }

          XtUnmanageChild(TSC_stnclassFO);
          XtUnmanageChild(TSC_StationFO);
          XtManageChild(TSC_GroupFO);

}

/* ************************************************** */
/* callbacks   to handle selection for station event  */
/* ************************************************** */
void TSC_Station_CB ( Widget w, XtPointer ptr, XtPointer cbs)
{
    extern    GROUP_INFO    *GroupInfo;

    GroupInfo->GroupSelected = STATION;

    /* **************************************************** */
    /* Swap station selection group with group selection    */
    /* **************************************************** */

    strcpy(last_lid,current_lid);
    setTSStn_byLid( last_lid);

}

/* **************************************************** */
/* callbacks to handle selection for "All" option       */
/* on filter options of the Time Series Control Dialog  */
/* **************************************************** */
void TSC_Class_allCB ( Widget w, XtPointer ptr, XtPointer cbs)
{
    char    where[255];
    int    cnt = 0;

    set_stnclass_allon();

    class_options[0]=class_options[1]=class_options[2]=class_options[3]=class_options[4]=1;
    class_options[5]=1;
    memset(where, '\0', sizeof(where));
    sprintf(where,"WHERE lid in (select distinct(lid) from ingestfilter where ingest='T') ORDER BY lid");
    cnt = loadTSStn_List(where, 1);
    if (cnt <= 0)
    {
        DeSensitize(TSC_GraphPB);
        DeSensitize(TSC_TabPB);
    }
    else
    {
        Sensitize(TSC_GraphPB);
        Sensitize(TSC_TabPB);
    }

}

/* **************************************************** */
/* callbacks to handle selection for other options      */
/* on filter options of the Time Series Control Dialog  */
/* **************************************************** */
void TSC_Class_CB ( Widget w, XtPointer ptr, XtPointer cbs)
{

    char    where[255];
    int    current_class,
        cnt = 0;

    XmToggleButtonCallbackStruct *tb_state = (XmToggleButtonCallbackStruct *)cbs;

    /* *********************************************************************** */
    /* current_class = 1/2/3/4 or 5 depends on the check box on filter options */
    /* *********************************************************************** */

    current_class = (int) ptr;

    class_options[current_class] = tb_state->set;

    memset(where, '\0', sizeof(where));
    sprintf(where,"WHERE lid in (select distinct(lid) from ingestfilter where ingest='T') ORDER BY lid");
    cnt = loadTSStn_List(where, 1);
    if (cnt <= 0)
    {
        DeSensitize(TSC_GraphPB);
        DeSensitize(TSC_TabPB);
    }
    else
    {
        Sensitize(TSC_GraphPB);
        Sensitize(TSC_TabPB);
    }
    
}

/* *********************************************************** */
/* Handle the "Graph" option on the Time Series Control Dialog */
/* *********************************************************** */
void TSC_Graph_CB ( Widget w, XtPointer ptr, XtPointer cbs)
{

    extern GROUP_INFO     *GroupInfo;
    extern PAGE_MGR        *PageMgr;
    extern GROUP_INFO     *GroupInfo;
    extern PAGE_MGR       *PageMgr;

      int manage_timeseries_gui = 1;

    long             daysmax;
   
   char   errorMessage[30];
   

    if ( PageMgr->BeginTime >=  PageMgr->EndTime )
    {
        TSErrorDialog(w,"Ending Time is prior to Beginning Time");
        return;

    }

    daysmax = (PageMgr->EndTime - PageMgr->BeginTime)/(24*3600);

    /* ********************************************************* */ 
    /* Check for maximum days allowable for data to be displayed */ 
    /* ********************************************************* */ 
    if ( daysmax > DAYS_MAX )
    {
      sprintf(errorMessage, "Time Period exceeds %d days", DAYS_MAX);
        TSErrorDialog(w, errorMessage);
        return;

    }

    if ( (PageMgr->error_report) && (GroupInfo->GroupSelected == STATION) )
    {
        TSErrorDialog(w,"No PEDTSEPs has been selected");
        return;
    }

    if (TimeSeriesDS == NULL)
        show_TimeSeries( w, (XtPointer) & manage_timeseries_gui, cbs);
    else if (! XtIsManaged(TimeSeriesDS))
    {
        show_TimeSeries( w, (XtPointer) & manage_timeseries_gui, cbs);
    }


    /* ****************************************** */ 
    /* Check if current mode is GROUP or STATION  */ 
    /* ****************************************** */ 
    switch ( GroupInfo->GroupSelected )
    {
        case STATION:
            
            get_TSstation_info();
            GroupInfo->current_page = 0;
            get_TSpage_data( GroupInfo->current_page );
            display_TSpage_data(GroupInfo->current_page);

            /* ******************************** */
                /*    Clear previous group name   */
            /* ******************************** */
            memset(Group.prev_nameselected,'\0',strlen(Group.prev_nameselected));

        break;

        case GROUP:

            if( strcmp(Group.prev_nameselected,Group.nameselected)==0)
            {
                GroupInfo->current_page = 0;
                get_TSpage_data( GroupInfo->current_page );
                display_TSpage_data(GroupInfo->current_page);

            }
            else
            {
                GroupInfo->current_page = 0;
                if ( get_TSpage_info(Group.nameselected) == 0)
                {
                    TSErrorDialog(w,"There is no data Available");
                }
                else
                {
                    get_TSpage_data( GroupInfo->current_page );
                    display_TSpage_data(GroupInfo->current_page);

                    memset(Group.prev_nameselected,'\0',strlen(Group.prev_nameselected));
                    strcpy(Group.prev_nameselected,Group.nameselected);
                }
            }
        break;
    }

    /* ********************************************* */
    /* Reset edit mode when GRAPH option is selected */
    /* ********************************************* */

    PageMgr->Edit_active = EDIT_RESET;
    highlight_select_options();

    return;
}

/* ****************************************************** */
/* Handle Table option when selected on the Time Series   */
/* Control Dialog.                                        */
/* ****************************************************** */
void TSC_Tab_CB ( Widget w, XtPointer ptr, XtPointer cbs)
{
    extern PAGE_INFO        TSpageinfo[MAX_PAGE_INFO];
    extern GROUP_INFO     *GroupInfo;
    extern PAGE_MGR         *PageMgr;

    GRAPH_INFO     *Ginfo;
    TRACE_INFO      *Tinfo;
    TAB_INFO     Tabinfo;

    char         tbuf[40];
   char   errorMessage[30];

    int     npage, 
        ngraph, 
        ntrace,
        trace_count = 0 ,
        n;

    long     daysmax;

    if ( PageMgr->BeginTime >=  PageMgr->EndTime )
    {
        TSErrorDialog(w,"Ending Time is prior to Beginning Time");
        return;

    }

    daysmax = (PageMgr->EndTime - PageMgr->BeginTime)/(24*3600);

    /* ********************************************************* */ 
    /* Check for maximum days allowable for data to be displayed */ 
    /* ********************************************************* */ 
    if ( daysmax > DAYS_MAX )
    {
      sprintf(errorMessage, "Time Period exceeds %d days", DAYS_MAX);
        TSErrorDialog(w, errorMessage);
        return;

    }

    if ( (PageMgr->error_report)  && (GroupInfo->GroupSelected == STATION) )
    {
        TSErrorDialog(w,"No PEDTSEPs has been selected");
        PageMgr->error_report = 0;
        return;
    }

    Tabinfo.Begin_time = PageMgr->BeginTime;
    Tabinfo.End_time   = PageMgr->EndTime;

    /* ****************************************** */ 
    /* Check if current mode is GROUP or STATION  */ 
    /* ****************************************** */ 
    switch ( GroupInfo->GroupSelected )
    {
        case STATION:
            
            GroupInfo->current_page = 0;
            trace_count = PedTSep.num_items;
            Tabinfo.buf = (RussText *)malloc(sizeof(RussText)*(trace_count+2));
            if ( Tabinfo.buf == NULL)
            {
                printf("Error in malloc RussText....\n");
                return;
            }

            for ( n = 0; n < trace_count; n++)
            {
                sprintf(tbuf,"%s %s",current_lid, PedTSep.name[n]);
                memset(Tabinfo.buf[n],'\0',sizeof(Tabinfo.buf[n]));
                strcat(Tabinfo.buf[n], tbuf);
            }


            /* ******************************** */
            /*    Clear previous group name     */
            /* ******************************** */

            memset(Group.prev_nameselected,'\0',strlen(Group.prev_nameselected));

        break;

        case GROUP:

            if( strcmp(Group.prev_nameselected,Group.nameselected)!=0)
            {
                get_TSpage_info(Group.nameselected);
                GroupInfo->current_page = 0;
                memset(Group.prev_nameselected,'\0',strlen(Group.prev_nameselected));
                strcpy(Group.prev_nameselected,Group.nameselected);

            }


        break;
    }
    

    /* *********************************************** */
    /* Pack all the information in tabular information */
    /* structure and pass the to tabular_show()        */
    /* *********************************************** */
    if( GroupInfo->GroupSelected == GROUP)
    {

        trace_count = 0;
        tabinfo_selected_pos = 1;
        for ( npage = 0; npage <  GroupInfo->page_count; npage++)
        {
            PAGE_INFO  *Pinfo = (PAGE_INFO *) &TSpageinfo[npage];
            for ( ngraph = 0; ngraph <  Pinfo->num_graphs; ngraph++)
            {
                Ginfo = (GRAPH_INFO *) &Pinfo->graph[ngraph];
                trace_count+=Ginfo->num_traces;
            }
        }

        Tabinfo.buf = (RussText *)malloc(sizeof(RussText)*(trace_count+2));

        if ( Tabinfo.buf == NULL)
        {
            printf("Error in malloc RussText....\n");
            return;
        }

        n = 0;
        for ( npage = 0; npage <  GroupInfo->page_count; npage++)
        {
            PAGE_INFO  *Pinfo = (PAGE_INFO *) &TSpageinfo[npage];
            for ( ngraph = 0; ngraph <  Pinfo->num_graphs; ngraph++)
            {
                Ginfo = (GRAPH_INFO *) &Pinfo->graph[ngraph];
                for ( ntrace = 0; ntrace <  Ginfo->num_traces; ntrace++)
                {
                     Tinfo  = (TRACE_INFO *) &Ginfo->traces[ntrace];
                    sprintf(tbuf, "%s %s %d %s %s",
                    Tinfo->lid,Tinfo->pe,Tinfo->dur, Tinfo->ts,Tinfo->extremum);
                    memset(Tabinfo.buf[n],'\0',sizeof(Tabinfo.buf[n]));
                    strcat(Tabinfo.buf[n], tbuf);
                    n++;
                }
            }
        }
    }


    Tabinfo.selected_pos = tabinfo_selected_pos;

    Tabinfo.nitems = trace_count;
    tabular_show(w,  Tabinfo);  

    if ( Tabinfo.buf != NULL)
         free ( Tabinfo.buf );
}

/* *************************************************************** */
/*    Get location and river name from LocView               */    
/* *************************************************************** */
void getStnRiverName(const char *lid, char *stn_name, char *river_name)    /* --added -- */
{
    LocView    *locviewPtr = NULL;
    static char prev_lid[LOC_ID_LEN + 1] = "NO_LID";
    static char prev_stn_name[LOC_NAME_LEN +1] = "UNDEFINED";
    static char prev_river_name[STREAM_NAME_LEN +1] = "UNDEFINED";
    
    
    /* initialize */
    
    strcpy(stn_name,   " ");
    strcpy(river_name, " ");
    
    
    /* don't need to re-search if the lid has not changed */
    
    if (strcmp(lid, prev_lid) == 0)
    {
        strcpy(stn_name,   prev_stn_name);
        strcpy(river_name, prev_river_name);
        return;
    }
    
    
    /* loop until a match is found */
            
    if (locviewHead != NULL)
       locviewPtr = (LocView *) ListFirst(&locviewHead -> list);
            
    while(locviewPtr)
    {
        if(strcmp(lid, locviewPtr -> lid) == 0)
        {
            strcpy(stn_name, locviewPtr -> name);
            
            if ( ! IsNull(CHAR, &locviewPtr -> stream) )
            {
                strcpy(river_name, locviewPtr -> stream);
            }
            else
                strcpy(river_name, "UNDEFINED");
                
            break;
        }
            
        locviewPtr = (LocView *)ListNext(&locviewPtr-> node);
    }
    
    
    /* load up the prev variables for the next pass */
    
    strcpy(prev_lid, lid);
    strcpy(prev_stn_name, stn_name);
    strcpy(prev_river_name, river_name);
    
    return;
}

/* *************************************************** */
/* Get station information from the station list and   */
/* Add all trace information which are highlighted by  */
/* user in the PeTSed List widget.                     */
/* *************************************************** */
void get_TSstation_info ( )
{
    extern     GROUP_INFO     *GroupInfo;
    extern     PAGE_MGR     *PageMgr;
    
    extern     GRAPH_INFO    *GraphInfo;
    extern     PAGE_INFO    *PageInfo;

    extern TRACE_INFO    *TraceInfo;
    TRACE_INFO *FirstTraceInfo, *SecondTraceInfo = NULL;

    int    n,
        ntraces,
        number_graph = 1;
        
    LIDDATA FirstLidData, TempLidData;

    PageInfo->num_graphs    = 0;
    GroupInfo->page_count   = 0;
    GroupInfo->current_page = 0;
    GraphInfo->num_traces   = 0;

    GraphInfo->graph_pos    = 1;
    GraphInfo->xsize        = 6; 
    GraphInfo->ysize        = 2;

    ntraces = PageMgr->ntraces_fromstn;

    FirstTraceInfo = (TRACE_INFO *) &Tinfo[0];
    strcpy(FirstLidData.pe, FirstTraceInfo->pe);
    FirstLidData.dur = FirstTraceInfo->dur;

    /* check the graph number currently for 1 or 2 */
    for ( n = 0; n < ntraces; n++)
    {

        TraceInfo = (TRACE_INFO *) &Tinfo[n];

        strcpy(TempLidData.pe, TraceInfo->pe);
        TempLidData.dur = TraceInfo->dur;

        if ( lid_check(FirstLidData, TempLidData) != 0)
            number_graph ++;
    }
    
    /*For only one kind of PE and can display in one graph */
    if(number_graph == 1)
    {
        for ( n = 0; n < ntraces; n++)
        {

            TraceInfo = (TRACE_INFO *) &Tinfo[n];
            TraceInfo->isForecast = 0;

            if ( TraceInfo->ts[0] == 'F' || TraceInfo->ts[0] == 'C' )
                TraceInfo->isForecast = 1;
            add_TStrace_info (  GraphInfo, TraceInfo );

        }

        /* ****************************************** */
        /* sort_TStraces( GraphInfo ); no longer used */
        /* ****************************************** */

        add_TSgraph_info (  PageInfo, GraphInfo);
        add_TSpage_info( PageInfo );
    }
    else
    /* Need two graphs to display the traces */
    {
        /* For first graph setup */
        GraphInfo->graph_pos    = 1;
        GraphInfo->xsize        = 6; 
        GraphInfo->ysize        = 1;

        for ( n = 0; n < ntraces; n++)
        {

            TraceInfo = (TRACE_INFO *) &Tinfo[n];
            TraceInfo->isForecast = 0;

            if ( TraceInfo->ts[0] == 'F' || TraceInfo->ts[0] == 'C' )
                TraceInfo->isForecast = 1;

            strcpy(TempLidData.pe, TraceInfo->pe);
            TempLidData.dur = TraceInfo->dur;

            if ( lid_check(FirstLidData, TempLidData) == 0)
                add_TStrace_info (  GraphInfo, TraceInfo );
            else
                SecondTraceInfo = TraceInfo;

        }

        add_TSgraph_info (  PageInfo, GraphInfo);

        if (SecondTraceInfo != NULL)
        {        
            /* For second graph setup */
            GraphInfo->num_traces   = 0;
            GraphInfo->graph_pos    = 7;

            strcpy(FirstLidData.pe, SecondTraceInfo->pe);
            FirstLidData.dur = SecondTraceInfo->dur;

            for ( n = 0; n < ntraces; n++)
            {

                TraceInfo = (TRACE_INFO *) &Tinfo[n];
                TraceInfo->isForecast = 0;

                if ( TraceInfo->ts[0] == 'F' || TraceInfo->ts[0] == 'C' )
                    TraceInfo->isForecast = 1;

                strcpy(TempLidData.pe, TraceInfo->pe);
                TempLidData.dur = TraceInfo->dur;

                if ( lid_check(FirstLidData, TempLidData) == 0)
                    add_TStrace_info (  GraphInfo, TraceInfo );
            }
        }
    

        add_TSgraph_info (  PageInfo, GraphInfo);

        add_TSpage_info( PageInfo );
    }
}


/* ********************************************************** */
/* UnManage the Time Series Control Dialog when Close option  */
/* is selected.                                               */
/* ********************************************************** */
void TSC_Close_CB ( Widget w, XtPointer ptr, XtPointer cbs)
{
    extern  PAGE_MGR        *PageMgr;
    if ( PageMgr->standalone)
    {
        exit (1);
    }
    else
    {
              XtUnmanageChild(TSControlDS);
        return;
    }

}

/* ********************************************************* */
/* check which search mode was choosen: by lid or by station name */
/* ********************************************************* */

void TSC_SearchMode(Widget w, XtPointer ptr,  XtPointer call_data)
{
    char        buf [ 50 ] ,
            *str_search = NULL ;    
    int        i;
            
    XmTextVerifyCallbackStruct *cbs = (XmTextVerifyCallbackStruct *) call_data;

    if (cbs->text->length == 0)
        return;
    if (XmToggleButtonGetState(TSC_lidTB))
    {    
        for (i = 0; i < cbs->text->length; i++)
        {
        
        /* *********************************************** */
                /* Verify input text is alphanumeric, and convert  */
        /* to uppercase, if necessary.                     */
        /* *********************************************** */
            if (! isAlphaNum(cbs->text->ptr[i]))
            {
                cbs->doit = False;
                break;
            }
        
            if (islower(cbs->text->ptr[i]))
                cbs->text->ptr[i] = toupper(cbs->text->ptr[i]);
        }
    
        /* ****************************** */
        /*     Build search pattern.      */
        /* ****************************** */

        if ( ( str_search = XmTextGetString(w) ) )
        {
            strcpy(buf, str_search);
            strcat(buf, cbs->text->ptr);
            XtFree(str_search);
        }
    
                TSC_Lookuplid(buf, call_data);    
    } 
      
    else
    {    
        /* ****************************** */
        /*     Build search pattern.      */
        /* ****************************** */

        if ( ( str_search = XmTextGetString(w) ) )
        {
            strcpy(buf, str_search);
            strcat(buf, cbs->text->ptr);
            XtFree(str_search);
        }

                TSC_Lookupname(buf, call_data);
    }

}
     



/* ******************************************** */
/* Save the selected group's name when selected  */
/* ******************************************** */
void TSC_GroupLI_CB(Widget w, XtPointer ptr, XtPointer cbs)
{
    extern    GROUP_NAME Group;

    int     *poslist = NULL;
    int     cnt      = 0;
    int     pos;

    XmListGetSelectedPos(w, &poslist, &cnt) ;

        if ( poslist != NULL )
        {
        pos = *poslist ;

        if (cnt != 0)
        {
           strcpy(Group.nameselected,Group.name[pos-1]) ;
        }
        
        XtFree((char *) poslist);
        poslist = NULL;
        }
}

/* ************************************************************ */
/* Update the PeTSed widget list based on current selected  LID */
/* ************************************************************ */
void TSC_StationLI_CB ( Widget w, XtPointer ptr, XtPointer cbs)
{
    extern TRACE_INFO TinfoPtr;

    int     *poslist = NULL;
    int     pos, cnt = 0;
    char     *lid,
        buf[80];
    
    XmListGetSelectedPos(w, &poslist, &cnt);
    if(cnt) 
    {
        pos = *poslist;
        lid = getLidFromStnList( pos );
        if ( ! keyhit ) 
        {
            strcpy (TinfoPtr.lid, lid); 
            sprintf(buf,"PE TypSrc Ext Dur %s",TinfoPtr.lid);
            SetLabel(TSC_PeTs,buf);

            memset(current_lid, '\0', strlen(current_lid));
            strcpy(current_lid, lid);
            load_Ingestfilter ( lid );
        }
        keyhit = 0;
        
        XtFree((char *) poslist);
        poslist = NULL;
    }

}


/* ****************************************************** */
/* Hightlight PEs that are matching with first character  */
/*                                                        */
/* Modified by guoxian zhou 07-2004                       */
/* Hightlight up to two kinds of record(s) which has      */
/* different PEs, or, if both records are from "PP",      */
/* the dur values are different.                          */ 
/* ****************************************************** */

void TSC_PCodeLI_CB( Widget w, XtPointer ptr, XtPointer cbs)
{
    int     *poslist = NULL;
    int     n,
        pos,
        ntraces,
        cnt = 0;

     static int    graph_index = 0;
    static LIDDATA prev_LidData, curr_LidData;
    LIDDATA temp_LidData;
    char pe[3], ts[3], extremum[2];
    int dur;

    char        buf[80];

    extern PAGE_MGR *PageMgr;

    memset(buf,'\0',strlen(buf));

    XmListGetSelectedPos(w, &poslist, &cnt);

    PageMgr->error_report = 0;

    if ( cnt <= 0 ) 
    {
        PageMgr->error_report = 1;
        return;
    }

    if ( cnt == 1)
    {
        strcpy(buf,PedTSep.name[*poslist - 1]);
        sscanf(buf,"%s %d %s %s" , pe , &dur, ts , extremum ) ;
        
        strcpy(prev_LidData.pe, pe);
        prev_LidData.dur = dur;
        
        strcpy(curr_LidData.pe, pe);
        curr_LidData.dur = dur;
        graph_index = 1;
    }

    /* ******************************************** */ 
    /*    Check for valid combination of PEDTSEPs   */ 
    /*    when there are two different data types   */ 
    /* ******************************************** */ 

    else if ( cnt > 1 )    
    {
        n = 0; 
        int flag = 0, check1 = 0, check2 = 0;

        /* search for more than 2 different PEs */
        while ( n < cnt && flag == 0)
        {
            pos = *(poslist + n);
            strcpy(buf,PedTSep.name[pos - 1]);
            sscanf(buf,"%s %d %s %s" , pe, &dur, ts , extremum ) ;
            
            strcpy(temp_LidData.pe,pe);
            temp_LidData.dur = dur;

            check1 = lid_check(prev_LidData, temp_LidData);
            check2 = lid_check(curr_LidData, temp_LidData);
            
            if(check1 != 0 && check2 != 0 )
            {
                /* Save the current PE to curr_LidData
                 * when prev_LidData is the same as 
                 * curr_LidData.
                 */
                 
                if (strcmp(prev_LidData.pe, curr_LidData.pe) == 0 &&
                    prev_LidData.dur == curr_LidData.dur)
                {
                    strcpy(curr_LidData.pe, pe);
                    curr_LidData.dur = dur;
                }
                else
                {
                    flag = 1;
                }
            }
            n ++ ;
        }
        
        if(flag == 1)
        {
            /* Both of the prev_LidData and curr_LidData has filled 
             * with different data, need deselect the PE items equal
             * to prev_LidData,
             */
             
            for ( n = 0; n < cnt; n++)
            {
                pos = *(poslist + n);

                strcpy(buf,PedTSep.name[pos - 1]);
                sscanf(buf,"%s %d %s %s" , pe, &dur, ts , extremum ) ;

                strcpy(temp_LidData.pe,pe);
                temp_LidData.dur = dur;

                check1 = lid_check(prev_LidData, temp_LidData);

                if(check1 == 0)
                    XmListDeselectPos(w,pos);

            }

            if (poslist != NULL)
            {
                XtFree((char *) poslist);
                poslist = NULL;
            }

            XmListGetSelectedPos(w, &poslist, &cnt);

            for ( n = 0; n < cnt; n++)
            {
                pos = *(poslist + n);
                strcpy(buf,PedTSep.name[pos - 1]);
                sscanf(buf,"%s %d %s %s" , pe, &dur, ts , extremum ) ;

                strcpy(temp_LidData.pe,pe);
                temp_LidData.dur = dur;

                check1 = lid_check(prev_LidData, temp_LidData);
                check2 = lid_check(curr_LidData, temp_LidData);
            
                if(check1 != 0 && check2 != 0 )
                {

                    /* Both of the prev_LidData and curr_LidData has filled 
                     * with different data, need:
                     * 1) move data from curr_LidData to prev_LidData, and
                     * 2) save the new data to curr_LidData.
                     */
                    strcpy(prev_LidData.pe, curr_LidData.pe);
                    prev_LidData.dur = curr_LidData.dur;
                
                    strcpy(curr_LidData.pe, temp_LidData.pe);
                    curr_LidData.dur = temp_LidData.dur;
                }
            }
        }
    }


    if (poslist != NULL)
    {
        XtFree((char *) poslist);
        poslist = NULL;
    }

    ntraces = 0;
    XmListGetSelectedPos(w, &poslist, &cnt);

    /* ************************************************************ */ 
    /*   Get all selected traces from Parameter Code list widget    */
    /* ************************************************************ */ 
    for ( n = 0; n < cnt; n++)
    {
        pos = *(poslist + n) - 1;
        strcpy(buf,PedTSep.name[pos]);
        sscanf(buf,"%s %d %s %s" , tmpinfo.pe , & ( tmpinfo.dur ) , 
                                          tmpinfo.ts , tmpinfo.extremum ) ;
        Tinfo[ntraces].isForecast = 0;
        sscanf(buf,"%s %d %s %s" , Tinfo[ntraces].pe ,
                                           & ( Tinfo[ntraces].dur ) ,
                                           Tinfo[ntraces].ts ,
                                           Tinfo[ntraces].extremum ) ;
        sprintf(Tinfo[ntraces].name , "%s %d %s %s" ,
                                             Tinfo[ntraces].pe ,
                                             Tinfo[ntraces].dur ,
                                     Tinfo[ntraces].ts , 
                                             Tinfo[ntraces].extremum ) ;
        strcpy(Tinfo[ntraces].lid,TinfoPtr.lid); 
        ntraces++;
    }

    PageMgr->ntraces_fromstn = cnt;
    tabinfo_selected_pos   = *poslist; /* Save first selected position in tab info struct */

    if (poslist != NULL)
    {
        XtFree((char *) poslist);
        poslist = NULL;
    }
}

/* ************************************ */
/* Time Series Error Dialog in general  */
/* ************************************ */
void TSErrorDialog( Widget widget, char *msg )
{

    static Widget        msgBox;
    Arg            arg[4];
    int            ac;
    
    
    if ( !  msgBox )
    {
        ac = 0;
        XtSetArg(arg[ac], XmNtitle, "Error Dialog"); ac++;
        XtSetArg(arg[ac], XmNdialogType, XmDIALOG_ERROR); ac++;
        XtSetArg(arg[ac], XmNdialogStyle, XmDIALOG_PRIMARY_APPLICATION_MODAL); ac++; 
    
        
        msgBox = XmCreateMessageDialog(widget, "msgBox", arg, ac);
        XtUnmanageChild(XmMessageBoxGetChild(msgBox, XmDIALOG_HELP_BUTTON));
        XtUnmanageChild(XmMessageBoxGetChild(msgBox, XmDIALOG_CANCEL_BUTTON));
    

    }
    /* ***************************** */
    /* Set the dialog message label. */
    /* ***************************** */
    ac = 0;
    XtSetArg(arg[ac], XmNmessageString,
         XmStringCreateLtoR(msg, (XmStringCharSet)XmFONTLIST_DEFAULT_TAG)); ac++;
    XtSetValues(msgBox, arg, ac);
    
    
    /* ******************************* */
        /* Manage the dialog box.          */
    /* ******************************* */

    XtManageChild(msgBox);
    XtPopup(XtParent(msgBox), XtGrabNone);

    return;    
}

/* ********************* */
/* Check for key events  */
/* ********************* */

void TSCkey_CB (Widget w, XtPointer ptr, XEvent *event)
{
    keyhit = 1;

}


static int mtbl [] = { 0,31,59,90,120,151,181,212,243,273,304,334,365 };

/* ***************************************************** */
/* check for valid day in month when increment/decrement */
/* ***************************************************** */
int day_in_month ( int year, int month )
{

    int days;
    days = mtbl[month] - mtbl[month-1];
    if ( month == 2 && year % 4 == 0 && ( year % 100 != 0 || year % 400 == 0))
        days++;
    return ( days );
}

/* *************************************************** */
/* check for valid day number when increment/decrement */
/* *************************************************** */
int day_number(int year, int month, int day )
{
    long days_ctr;

    year-=1900;
    days_ctr = ((long)year * 365L) + (( year + 3) / 4);
    days_ctr+=mtbl[month-1] + day + 6;
    if ( month > 2 && (year % 4 == 0 ))
        days_ctr++;

    return ( (int) (days_ctr % 7L));
}

/* *************************************************** */
/* convert duration to text display in PeTSed list     */
/* *************************************************** */
char *conv_dur2text ( int dur )
{
    
    static char durtext[10];

    int     ivalue,
        jvalue;


    ivalue  = dur/1000;

    switch ( ivalue )
    {
        case 0:
            if ( dur == 0)
            {
                strcpy( durtext, "");
                return (  durtext ); 
            }
            else
                sprintf( durtext,"%dmin",dur);

        break;

        case 1:
            jvalue = dur - (dur/1000)*1000;
            sprintf( durtext,"%dhr ",jvalue);
        break;

        case 2:
            jvalue = dur - (dur/1000)*1000;
            sprintf( durtext,"%dday ",jvalue);
        break;

        case 3:
            jvalue = dur - (dur/1000)*1000;
            sprintf( durtext,"%dMos",jvalue);
        break;

        case 4:
            jvalue = dur - (dur/1000)*1000;
            sprintf( durtext,"%dYr",jvalue);
        break;

        case 5:
            jvalue  = dur - (dur/1000)*1000;
            if ( jvalue == 4)
                sprintf( durtext,"Since7AM ");
            else if ( jvalue == 1)
                sprintf( durtext,"Season");
            else if ( jvalue == 2)
                sprintf( durtext,"PerRec");
             else
                sprintf( durtext,"Unk");
        break;

        default:
            sprintf( durtext,"Unk");

    }

    return ( durtext );
}

/* **************************************************** */
/* set all (check box) in station class options to ON   */
/* **************************************************** */
void set_stnclass_allon()
{

    int    n;

    XmToggleButtonSetState(TSC_heightTB, True, False);
    XmToggleButtonSetState(TSC_tempTB,   True, False);
    XmToggleButtonSetState(TSC_snowTB,   True, False);
    XmToggleButtonSetState(TSC_precipTB, True, False);
    XmToggleButtonSetState(TSC_otherTB,  True, False);

    for(n=0; n<6; n++)
        class_options[n] = 1; 

    return;
}

/* ******************************************** */
/*  Disable Close funtion by window manager     */
/* ******************************************** */
void disable_close_func( Widget w )
{
     Arg    args[2];
     XtSetArg (args[0], XmNdeleteResponse, XmDO_NOTHING);
     XtSetValues ( w ,args, 1);
     return;
}

/* ******************************************** */
/*  Initialize memory for PageMgr/GroupInfo/    */
/*  PageInfo/GraphInfo/TraceInfo pointers       */
/*  These pointers are re-usable  pointers      */
/* Throughout the entired Time Series           */
/* ******************************************** */
void TSinit_memory()
{
    static int first = 1;

    if ( first )
    {
        PageMgr   = malloc(sizeof(PAGE_MGR));
        GroupInfo = malloc(sizeof(GROUP_INFO));
        PageInfo  = malloc(sizeof(PAGE_INFO));
        GraphInfo = malloc(sizeof(GRAPH_INFO));
        TraceInfo = malloc(sizeof(TRACE_INFO));
        first = 0;
    }
    return;

}


/* ************************************************** */
/*  load the PeTSed from the Ingestfilter table  from */
/*  database and lightlight items according to the    */
/*  Time Series's requirements                        */
/* ************************************************** */
int    load_hightlight( TSGEN_INFO tsgen )
{

    struct PEDTSE tmpinfo;


    int     i,
        j,
        pos = 0,
        nitems;

    nitems = PedTSep.num_items;

    strcpy(current_lid,tsgen.lid);
    load_Ingestfilter ( current_lid );


    XmListDeselectAllItems(TSC_PCodeLI);
    for ( i = 0; i < tsgen.nitems; i ++)
    for ( j = 0; j < nitems; j ++)
    {
        sscanf ( PedTSep.name[j] , "%s %d %s %s" , tmpinfo.pe ,
                                                           & ( tmpinfo.dur ) , 
                                                           tmpinfo.ts ,
                                                           tmpinfo.extremum ) ;

        if ( strcmp(tmpinfo.pe,tsgen.pedtse[i].pe) == 0 && strcmp(tmpinfo.ts,tsgen.pedtse[i].ts) == 0 &&
                  strcmp(tmpinfo.extremum,tsgen.pedtse[i].extremum) == 0 && tmpinfo.dur == tsgen.pedtse[i].dur)
        {
            pos = j+1;
            XmListSelectPos(TSC_PCodeLI, pos, True);
        }
         
    }

    return ( pos );
    
}

/* ************************************************ */
/*     load the description of Physical Elements   */
/*      from database                               */
/* ************************************************ */

char *load_PEdesc(char *pe)
{
    static int     first = 1;
    static ShefPe    *shefpeHead = NULL;
    static char     pe_desc[SHEF_PE_NAME_LEN + 1]; 
    
    ShefPe        *shefpePtr = NULL;

    char where[] = " ";
    
    /* initialize the returned variable */
    
    sprintf(pe_desc, "Not Defined");
    
    /*memset(where, '\0', sizeof(where));
    sprintf(where,"WHERE pe = '%s' ", pe);*/
    
    if ( shefpeHead == NULL || first )
    {
        shefpeHead = GetShefPe(where);
        first = 0;
    }
    
    if (shefpeHead != NULL)
        shefpePtr = (ShefPe *)ListFirst(&shefpeHead -> list);
        
    
    while(shefpePtr)
    {
        if(strcmp(pe, shefpePtr -> pe) == 0)
        {    
            strcpy(pe_desc, shefpePtr -> name);
            break;
        }
    
        shefpePtr = (ShefPe *)ListNext(&shefpePtr-> node);    
    }
    
    return(pe_desc);
}

/* ************************************************ */
/*     load the description of Type Sources        */
/*      from database                               */
/* ************************************************ */

char *load_TSdesc(char *ts)
{    
    static int     first = 1;
    static ShefTs    *sheftsHead = NULL;
    
    static char     ts_desc[SHEF_TS_NAME_LEN + 1];
    
    ShefTs    *sheftsPtr = NULL; 
    char    where[] = " ";
    
    
    /* initialize the returned variable */
    
    sprintf(ts_desc, "Not Defined");
    
    
    /* get the entire list the first time through */
    
    if ( sheftsHead == NULL || first )
    {
        sheftsHead = GetShefTs(where);
        first = 0;
    }
    
    /* if list loaded, find the match */
    
    if (sheftsHead != NULL)
        sheftsPtr = (ShefTs *)ListFirst(&sheftsHead -> list);
    
    while(sheftsPtr)
    {
        if(strcmp(ts, sheftsPtr -> ts) == 0)
        {    
            strcpy(ts_desc, sheftsPtr -> name);
            break;
        }
        
        sheftsPtr = (ShefTs *)ListNext(&sheftsPtr-> node);
    }
    
        
    return(ts_desc);
}

/* ***************************************************************** */
/* callbacks to handle reorder of time series by either lid or name  */
/* ***************************************************************** */
void reorder_ts_CB ( Widget w, XtPointer ptr, XtPointer cbs)
{
    int i;
    char buf[LOC_ID_LEN+LOC_NAME_LEN+10];

    XmStringTable    xmStr;

    reorder_mode = (int) ptr;
    
    reorder_ts();

    /* Delete old items */
    XmListDeleteAllItems(TSC_StationLI);

    xmStr = (XmStringTable) XtMalloc(num_active_lids * sizeof(XmString *));

    for (i = 0; i<num_active_lids; i++)
    {
        sprintf(buf, "%-10s %-25s", (LlidPtr+i)->lid, (LlidPtr+i)->name);
        xmStr[i] = XmStringCreateSimple(buf);
    }

    /* Load the list box with the reordered items.*/
      XmListAddItems(TSC_StationLI, xmStr, num_active_lids, 1);
    
    XmListSelectPos(TSC_StationLI, 1, True);

    /* cleanup. */
    for (i = 0; i < num_active_lids; i++)
            XmStringFree(xmStr[i]);

    XtFree((char *) xmStr);    

}

/* ***************************************************************** */
/* function to handle reorder of time series by either lid or name  */
/* ***************************************************************** */
void reorder_ts()
{
    int        result, i, j;
    char tmp_lid[LOC_ID_LEN+1];
    char tmp_name[LOC_NAME_LEN+1];

    /*If the list is null or only one record, no need for reorder */
    if (num_active_lids <= 1) return;

    for (i = 0; i < num_active_lids-1; i++)
    {        
        for (j = i+1; j < num_active_lids; j++)
        {
            if(reorder_mode == 0)  /*reorder by lid*/
                result = strcmp((LlidPtr+i)->lid, (LlidPtr+j)->lid);
            else        /*reorder by name */
                result = strcmp((LlidPtr+i)->name, (LlidPtr+j)->name);
            
            /*For the first reorder level */    
            if (result > 0)
            {
                strcpy(tmp_lid,(LlidPtr+i)->lid);
                strcpy(tmp_name,(LlidPtr+i)->name);

                strcpy((LlidPtr+i)->lid,(LlidPtr+j)->lid);
                strcpy((LlidPtr+i)->name,(LlidPtr+j)->name);

                strcpy((LlidPtr+j)->lid,tmp_lid);
                strcpy((LlidPtr+j)->name,tmp_name);

            }
            /* The second reorder level is by another item
             * if the first comparison equals zero.
             * If reorder_mode == 0, the first reorder is by lid
             * and the second reorder is by name.
             */
            else if (result == 0)
            {
                if(reorder_mode == 0)  /* second reorder by name*/
                    result = strcmp((LlidPtr+i)->name, (LlidPtr+j)->name);
                else        /* second reorder by lid */
                    result = strcmp((LlidPtr+i)->lid, (LlidPtr+j)->lid);

                if (result > 0)
                {
                    strcpy(tmp_lid,(LlidPtr+i)->lid);
                    strcpy(tmp_name,(LlidPtr+i)->name);

                    strcpy((LlidPtr+i)->lid,(LlidPtr+j)->lid);
                    strcpy((LlidPtr+i)->name,(LlidPtr+j)->name);

                    strcpy((LlidPtr+j)->lid,tmp_lid);
                    strcpy((LlidPtr+j)->name,tmp_name);

                }
            }
        }
    }
}


/* Check if two LIDs' data belong to the same group.
 * The rules are:
 *  1) if the PEs are different; or
 *  2) if the PEs of both LID are "PP", but the durations are different
 *  Added by guoxian zhou 06-2004
 */
int lid_check(LIDDATA LidData1, LIDDATA LidData2)
{
    int result = 0;
    
    if (strcmp(LidData1.pe, LidData2.pe) != 0 )
    {
        result = 1;
    }
    else if (strncmp(LidData1.pe,"PP",2) == 0 && strncmp(LidData2.pe,"PP",2) == 0)
    {
        if (LidData1.dur != LidData2.dur)
            result = 1;
    }
    
    return result;
}


/* ************************************ */
/* End of File:  TSControl_show.c       */
/* ************************************ */

