
/* ******************************************** */
/*	File:           getTSinfo.c             */
/*	Date:           April 1995              */
/*	Author:         Sung Vo                 */
/*	Purpose:        Provide support for the */
/*      Time Series Control Dialog              */
/* ******************************************** */


#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

/* ****************************** */
/* header files                   */
/* ****************************** */

#include "TSGRAPH.h"
#include "GeneralUtil.h"
#include "TSgetinfo.h"
#include "TSControl_show.h"

/* ***************************************** */
/* external structures used in the Time      */
/* Series that defined in  TimeSeries_show.c */
/* ***************************************** */

extern GROUP_INFO      	*GroupInfo;
extern PAGE_INFO	*PageInfo;
extern PAGE_MGR     	*PageMgr;
extern GRAPH_INFO	*GraphInfo;
extern TRACE_INFO	*TraceInfo;

extern PAGE_DATA TSpagedata[MAX_PAGE_DATA];
extern PAGE_INFO TSpageinfo[MAX_PAGE_INFO];

/* ************************************************* */
/* Get page informtion from the group_definition.cfg */
/* file based on the selected group's name           */
/* ************************************************* */
int get_TSpage_info( char *group_name )
{

	char	gad_value[128];

	char 	buf[200],
		buf_org[200];

	int  	ok,
		status,
		done;

	int  	gad_token_len=0, gad_value_len=0, rcfad=0;


	FILE	*fp;

	ShefDur	*durHead=NULL, *durPtr=NULL;

	GroupInfo->page_count   = 0;
	GroupInfo->current_page = 0;
	PageInfo->num_graphs    = 0;     
	GraphInfo->num_traces   = 0;


	/* ******************************************** */
	/* Get the application data directory path name */
	/* ******************************************** */

	gad_token_len  =  strlen ("whfs_config_dir");
	rcfad = get_apps_defaults("whfs_config_dir", &gad_token_len, gad_value, &gad_value_len);
	if (rcfad == 0)
		sprintf(buf,"%s/timeseries/group_definition.cfg",gad_value);
	else
		sprintf(buf,"%s/timeseries/group_definition.cfg",".");

	if( (fp = fopen(buf,"r")) == NULL)
	{
		printf("Error: open %s Failed.\n",buf);
		return ( 0 );

	}

	/*
        Get ALL of the SHEF Duration Code records and assign the pointer
        of the first one to DurPtr to be passed to the getDurCode function
	*/
	durHead = GetShefDur("");
	if (durHead)
		durPtr = (ShefDur *) ListFirst(&durHead->list);

ok = done = 0;

while ( ! done )
{


	status = 1;

	if ( fgets(buf,200,fp)  == NULL)
	{
		status = 0;

	}

	if ( status )
	{

		/* ******************************* */
		/* ****** Ignore comments ******** */
		/* ******************************* */
		if(strstr (buf,"#")) continue;

		memset(buf_org,'\0',strlen(buf_org));
		strcpy(buf_org,buf);

		if( strstr(buf_org,"Group:")  && strstr (buf_org,group_name))
		{
			ok = 1;
			get_optTSgroup(GroupInfo, buf );
			continue;
		}

		if ( ! ok ) continue; 

 		if(strstr (buf_org,"Page:"))
		{
			if ( GraphInfo->num_traces > 0 )
			{
				sort_TStraces( GraphInfo );
				add_TSgraph_info ( PageInfo, GraphInfo); 
				add_TSpage_info( PageInfo );
				GraphInfo->num_traces = 0;	/* Reset num_traces for next graph */
	   			PageInfo->num_graphs  = 0;      /* Reset num_graphs for next page  */
			}
			get_optTSpage ( PageInfo, buf );
		}

 		if(strstr (buf_org,"Graph:"))
		{
			if( GraphInfo->num_traces > 0)
			{
				sort_TStraces( GraphInfo );
				add_TSgraph_info ( PageInfo, GraphInfo); 
				GraphInfo->num_traces = 0;	/* Reset num_traces for next graph */
			}
	
			get_optTSgraph( GraphInfo, buf );

		}

 		if(strstr (buf_org,"Trace:"))
		{
			get_optTStrace ( TraceInfo, buf );
			decode_pedtse (  TraceInfo, durPtr );
			add_TStrace_info (  GraphInfo, TraceInfo ); 
			continue;
		}
	}

	if( strstr(buf_org,"Group:") || status == 0) 
	{
		sort_TStraces( GraphInfo );
		add_TSgraph_info ( PageInfo, GraphInfo); 
		add_TSpage_info( PageInfo );
		GraphInfo->num_traces = 0;	/* Reset num_traces for next graph */
	   	PageInfo->num_graphs  = 0;      /* Reset num_graphs for next page  */
		done = 1;
		break;
	}

} 

	/* free ShefDur linked list if it exists */
	if (durHead)
	{
		FreeShefDur(durHead);
		durHead = NULL;
	}
     
	fclose ( fp );
	return ( GroupInfo->page_count );

}

/* *********************************** */
/* Sort traces within graph            */
/* *********************************** */
void sort_TStraces( GRAPH_INFO *ginfo )
{
	GRAPH_INFO 	*ginfo_out,
			 tmp_ginfo;

	TRACE_INFO 	*tinfo;

	int		n,
			m;


	ginfo_out = (GRAPH_INFO *)&tmp_ginfo;

	memcpy((char *)&tmp_ginfo,(char *)ginfo,sizeof(*ginfo));

	ginfo_out->num_traces = 0;

	/* ***************************************************** */ 
	/* Check for illegal multiple PC or PP traces in graph   */
	/* ***************************************************** */ 

	for ( m = 0; m < ginfo->num_traces; m++)
	{
		 tinfo = (TRACE_INFO *) &ginfo->traces[m];
		 if ( strncmp("PP",tinfo->pe,2) == 0 || strncmp("PC",tinfo->pe,2) == 0)
		 {
			memcpy((char *)&ginfo_out->traces[ginfo_out->num_traces],(char *)tinfo,sizeof(TRACE_INFO));
			ginfo_out->num_traces++;
			*ginfo = *ginfo_out;
			return;

		 }
	}

	/* ****************************************************** */ 
	/* Sort traces in graph by Observe/Process/Forecast order */
	/* ****************************************************** */ 

	ginfo_out->num_traces = 0;

	for ( n = 0; n < 3; n++)
	{
		for ( m = 0; m < ginfo->num_traces; m++)
		{
			tinfo = (TRACE_INFO *) &ginfo->traces[m];

			if(n == 0 && (tinfo->ts[0] == 'F' || tinfo->ts[0] == 'P')) continue;
			if(n == 1 && tinfo->ts[0] != 'P') continue;
			if(n == 2 && tinfo->ts[0] != 'F') continue;

			memcpy((char *)&ginfo_out->traces[ginfo_out->num_traces],(char *)tinfo,sizeof(TRACE_INFO));
			ginfo_out->num_traces++;

		}
	}

	*ginfo = *ginfo_out;


}

/* *********************************************************** */ 
/* Decode group information group's information structure      */
/* *********************************************************** */ 
void get_optTSgroup ( GROUP_INFO *grp, char *buf)
{
	 char    *cptr = NULL ;
	 char	 *tmp = NULL ;

	 time_t   starttime, endtime;

	 cptr = strtok ( buf , ":," ) ;

	 while (  cptr != NULL )
	 {
		if ( ( tmp = strstr (cptr,"name=") ) )
		{
			strcpy(grp->group_name, cptr+5);
		}

		if ( ( tmp = strstr (cptr,"descript=") ) )
		{
			strcpy(grp->descript, cptr+9);
		}

		if ( ( tmp = strstr (cptr,"grid=") ) )
		{
			strcpy(grp->gridmode, cptr+5);
			grp->grid_lines = 0;
			if ( strcmp(grp->gridmode,"T") == 0) grp->grid_lines = 1;
		}

		if ( ( tmp = strstr (cptr,"tracemode=") ) )
		{
			strcpy(grp->tracemode, cptr+10);
			grp->trace_mode = 1; /* Line only as default */
			if ( strcmp(grp->tracemode,"P") == 0) grp->trace_mode = 0;
			if ( strcmp(grp->tracemode,"B") == 0) grp->trace_mode = 2;
			
		}

		if ( ( tmp = strstr (cptr,"pasthours=") ) )
		{
	   		grp->past_hours   = atoi(cptr+10);
			grp->days_back    = grp->past_hours/24;
		}

		if ( ( tmp = strstr (cptr,"futurehours=") ) )
		{
	   		grp->future_hours = atoi(cptr+12);
			grp->days_forward = grp->future_hours/24;
			starttime = 0; endtime = 0;
			init_TStime(starttime, endtime);  /* initialize the 
                        times for Time Series Dialog */
		}

		cptr = strtok(NULL, ":,\t");

	 }

}

/* ************************************************************** */ 
/* Decode page information from page's information structure      */
/* ************************************************************** */ 
void get_optTSpage  ( PAGE_INFO *page , char *buf)
{
	 char    *cptr = NULL ;
	 char	 *tmp = NULL ;

	 cptr = strtok(buf, ":,");

	 while (  cptr != NULL )
	 {
 		if ( ( tmp = strstr (cptr,"title=") ) )
		{
			strcpy(page->title, cptr+6);
		}

		cptr = strtok(NULL, ":,\t");

	 }

	return;
}

/* *************************************** */
/* return a group's name from a info       */
/* read from the timeseries_definition.cfg */
/* file                                    */
/* *************************************** */
char *get_TSgroupName  ( char *buf)
{
	 char    *cptr = NULL ;
	 char	 *tmp = NULL ;
	 static	 char	 rbuf[200];

	 cptr = strtok(buf, ":,");
	 while (  cptr != NULL )
	 {
 		if ( ( tmp = strstr (cptr,"name=") ) )
		{
			strcpy(rbuf, cptr+5);
			break;
		}

		cptr = strtok(NULL, ":,\t");

	 }

	return( rbuf );
}

/* **************************************************************** */ 
/* Decode graph information from graph's information structure      */
/* **************************************************************** */ 
void get_optTSgraph ( GRAPH_INFO *graph, char *buf)
{
	 char    *cptr = NULL ;
	 char	 *tmp = NULL ;
	 int	  tmpcat ;

	 cptr = strtok(buf, ":,");

         /* Default settings of precipitation elements in case they
            are not specified in the configuration file. */
	 graph->derivepp = INTERPOLATE;
	 graph->showpp = 1;

	 while (  cptr != NULL )
	 {
		if ( ( tmp = strstr (cptr,"pos=") ) )
		{
	   		graph->graph_pos = atoi(cptr+4);
		}

		if ( ( tmp = strstr (cptr,"xsize=") ) )
		{
	   		graph->xsize = atoi(cptr+6);
		}

		if ( ( tmp = strstr (cptr,"ysize=") ) )
		{
	   		graph->ysize = atoi(cptr+6);
		}

 		if ( ( tmp = strstr (cptr,"ylinear=") ) )
		{
			strncpy(graph->ylinearc, cptr+8,1);
		}

 		if ( ( tmp = strstr (cptr,"yscale=") ) )
		{
			strncpy(graph->yscalec, cptr+7,1);
			graph->yscale = 0;
			if (strncmp(graph->yscalec,"D",1)==0)graph->yscale = 1;
		}

 		if ( ( tmp = strstr (cptr,"showcat=") ) )
		{
			strncpy(graph->showcatc, cptr+8,1);
			graph->showcat = 0; tmpcat = 1;
			if (strncmp(graph->showcatc,"T",1)==0)
                           graph->showcat = 1; 
			if ( graph->yscale == 1 && graph->showcat == 0)
                           tmpcat = 0;
			if ( graph->yscale == 1 && graph->showcat == 1)
                           tmpcat = 1;
			if ( graph->yscale == 0 && graph->showcat == 1)
                           tmpcat = 2;
			if ( graph->yscale == 0 && graph->showcat == 0)
                           tmpcat = 2;
			graph->showcat = tmpcat;
			
			/*Initialize menu so that the corresponding item is selected.
			 * Added by guoxian zhou 06-2004
			 */

			Widget widget;
			char buttonIndex[10];
			int i;

			/* update all radio buttons' mode */
			for(i = 0; i < 3; i++ )
			{
				sprintf(buttonIndex, "button_%d", i);
				
				if(i == graph->showcat)
				{
					if((widget = XtNameToWidget(TSscaleMO, buttonIndex)) != NULL)
						XtVaSetValues(widget, XmNset, TRUE, NULL);

					if((widget = XtNameToWidget(TSBatchScaleMO, buttonIndex)) != NULL)
						XtVaSetValues(widget, XmNset, TRUE, NULL);
				}
				else
				{
					if((widget = XtNameToWidget(TSscaleMO, buttonIndex)) != NULL)
						XtVaSetValues(widget, XmNset, FALSE, NULL);

					if((widget = XtNameToWidget(TSBatchScaleMO, buttonIndex)) != NULL)
						XtVaSetValues(widget, XmNset, FALSE, NULL);
				}

			}
		}

 		if ( ( tmp = strstr (cptr,"derivepp=") ) )
		{
                        /* Fixed logic to look for 'I' for interpolation or
                           'A' for assignment.  These describe how PP data
                           are arrived at from PC data. */
			strncpy(graph->deriveppc, cptr+9,1);

                        if (strncmp(graph->deriveppc,"I",1)==0)
                        {
                           graph->derivepp = INTERPOLATE;
                        }
                        else if ( strncmp (graph->deriveppc, "A",1)==0)
                        {
                           graph->derivepp = ASSIGN;
                        }
                        else
                        {
                           graph->derivepp = NO_PC_TO_PP;
                        }
		}

 		if ( ( tmp = strstr (cptr,"showpp=") ) )
		{
			strncpy(graph->showppc, cptr+7,1);
			if (strncmp(graph->showppc,"F",1)==0)
                        {
                           graph->showpp = 0;
                        }
		}

 		if ( ( tmp = strstr (cptr,"latestfcstonly=") ) )
		{
			strncpy(graph->latestfcstonlyc, cptr+15, 1);
			graph->latestfcstonly = 0;
			if (strncmp(graph->latestfcstonlyc,"T",1)==0)
				graph->latestfcstonly = 1;

			/*Initialize menu so that the corresponding item is selected.
			 * Added by guoxian zhou 05-2004
			 */

			if(graph->latestfcstonly == 1)
				XtVaSetValues(TSShowFcstTB, XmNset, TRUE, NULL);
			else
				XtVaSetValues(TSShowFcstTB, XmNset, FALSE, NULL);
				
		}

		cptr = strtok(NULL, ":,\t");
	 }

         if ( graph->showpp == 0 )
         {
            graph->derivepp = NO_PC_TO_PP;
         }

	return;
}

/* **************************************************************** */ 
/* Decode trace information from trace's information structure      */
/* **************************************************************** */ 
void get_optTStrace ( TRACE_INFO *trace, char *buf)
{
	 char   * cptr = NULL;
	 char	* tmp = NULL;
	 int	n;

	 cptr = strtok(buf, ":,");

	 while (  cptr != NULL )
	 {

		if ( ( tmp = strstr (cptr,"stn=") ) )
		{
			strcpy(trace->lid, cptr+4);
			for(n=0;n<strlen(TraceInfo->lid);n++)
				TraceInfo->lid[n] = toupper(TraceInfo->lid[n]);
		}

		if ( ( tmp = strstr (cptr,"pc=") ) )
		{
			strcpy(trace->name, cptr+3);
			for(n=0;n<strlen(TraceInfo->name);n++)
				TraceInfo->name[n] = toupper(TraceInfo->name[n]);
		}

		if ( ( tmp = strstr (cptr,"color=") ) )
		{
			sscanf ( cptr+6 , "%s" , trace->color_name ) ;
		}

		cptr = strtok(NULL, ":,\t");
	}

	return;
}

/* ************************************************************ */
/* Add trace information data to graph information data         */
/* ************************************************************ */
void add_TStrace_info( GRAPH_INFO *ginfo, TRACE_INFO *tinfo)
{
	if (  ginfo->num_traces >= MAX_TRACES)
	{
		printf("Number of traces (%d) >= limit (%d).\n", ginfo->num_traces, MAX_TRACES);
		return;

	}

	memcpy((char *)&ginfo->traces[ginfo->num_traces],(char *)tinfo,sizeof(TRACE_INFO));

	ginfo->num_traces++;

	return;
}

/* ************************************************************ */
/* Add graph information data to page  information data         */
/* ************************************************************ */
void add_TSgraph_info( PAGE_INFO *pinfo , GRAPH_INFO *ginfo)
{

	if ( pinfo->num_graphs >= MAX_GRAPHS) {
	   fprintf(stderr, "Number of graphs (%d) >= limit (%d).\n", pinfo->num_graphs, MAX_GRAPHS);
	   return;
	}
	memcpy((char *)&pinfo->graph[pinfo->num_graphs],(char *)ginfo,sizeof(GRAPH_INFO));
	pinfo->num_graphs++;

	return;

}

/* ************************************************************ */
/* Add page  information data to group information data         */
/* ************************************************************ */
void add_TSpage_info( PAGE_INFO *TPinfoPtr)
{
	extern  GROUP_INFO *GroupInfo;

	if ( GroupInfo->page_count >=  MAX_PAGE_INFO)
	{

	   fprintf(stderr, "Number of groups (%d) >= limit (%d).\n", GroupInfo->page_count, MAX_PAGE_INFO);
	   return;

	}

	memcpy((char *)&TSpageinfo[GroupInfo->page_count],(char *)TPinfoPtr,sizeof(PAGE_INFO));

	GroupInfo->page_count++;

	return;

}


/* ************************************ */ 
/* Add trace data to group data         */
/* ************************************ */ 
void add_TStrace_data( GRAPH_DATA *gptr, TRACE_DATA *tptr)
{

	if (  gptr->num_traces >= MAX_TRACES)
	{
		fprintf(stderr, "Number of traces (%d) >= limit (%d).\n", gptr->num_traces, MAX_TRACES);
		return;

	}
	memcpy((char *)&gptr->traces[gptr->num_traces],(char *)tptr,sizeof(TRACE_DATA));
	gptr->trace_on[gptr->num_traces]    = 1;
	gptr->trace_valid[gptr->num_traces] = 1;
	gptr->num_traces++;
	return;

}

/* ************************************* */ 
/* Print pages info for debug purpose    */
/* ************************************* */ 
void print_TSpages_info ()
{

	int n;

	PAGE_INFO  *pinfo;

	printf("Group Name: %s number of pages %d\n",GroupInfo->group_name,GroupInfo->page_count);
	printf("Group Info: %s %s %s %d %d\n",
		GroupInfo->descript,GroupInfo->gridmode,
		GroupInfo->tracemode,GroupInfo->past_hours,GroupInfo->future_hours);


	for ( n = 0; n < GroupInfo->page_count; n++)
	{
		pinfo = (PAGE_INFO *) &TSpageinfo[n];
		printf("Page  # %d num_graphs = %d title = %s\n", n+1 ,  pinfo->num_graphs,pinfo->title);
		print_TSpage_info( pinfo );

	}


}

/* ************************************ */ 
/* Print page info for debug purpose    */
/* ************************************ */ 
void print_TSpage_info ( PAGE_INFO *pinfo)
{
	int n;
	GRAPH_INFO *ginfo;

	for ( n = 0; n < pinfo->num_graphs; n++)
	{

		printf("Graph # %d\n", n+1 );
		ginfo = (GRAPH_INFO *) &pinfo->graph[n];
		print_TSgraph_info ( ginfo );

	}

}

/* ************************************* */ 
/* Print graph info for debug purpose    */
/* ************************************* */ 
void print_TSgraph_info ( GRAPH_INFO *ginfo)
{
	int n;

	TRACE_INFO *tinfo;
	
	printf("options:%d %d %d %s %s %s %s %s %s\n",
		ginfo->graph_pos,ginfo->xsize, ginfo->ysize,
	    	ginfo->ylinearc,ginfo->yscalec,ginfo->showcatc,
	       	ginfo->deriveppc,ginfo->showppc,ginfo->latestfcstonlyc);

	for ( n = 0; n < ginfo->num_traces; n++)
	{
		
		tinfo = (TRACE_INFO *) &ginfo->traces[n];
		print_TStrace_info ( tinfo );
	}


}

/* ************************************* */ 
/* Print trace info for debug purpose    */
/* ************************************* */ 
void print_TStrace_info ( TRACE_INFO *tinfo)
{
	printf("TRACE INFO = %s %s %d %s %s %s\n", 
	        tinfo->lid, tinfo->name,tinfo->dur,
		tinfo->pe,tinfo->ts,tinfo->extremum); 
}


/* ************************************* */ 
/* Print pages data for debug purpose    */
/* ************************************* */ 
void print_TSpages_data( int page )
{


	PAGE_DATA *p =  (PAGE_DATA *) &TSpagedata[0];
	print_TSpage_data ( p );


}

/* ************************************* */ 
/* Print page  data for debug purpose    */
/* ************************************* */ 
void print_TSpage_data( PAGE_DATA *p)
{

	int n;

	GRAPH_DATA *g;

	for(n =0; n < p->num_graphs; n++) 
	{
		g = (GRAPH_DATA *) &p->graph[n];
		print_TSgraph_data( g );
	}


}

/* ************************************* */ 
/* Print graph data for debug purpose    */
/* ************************************* */ 
void print_TSgraph_data( GRAPH_DATA *g)
{

	int n;

	TRACE_DATA *t;

	for(n =0; n < g->num_traces; n++) 
	{

		t = (TRACE_DATA *) &g->traces[n];
		print_TStrace_data( t );
	}

}

/* ************************************* */ 
/* Print trace data for debug purpose    */
/* ************************************* */ 
void print_TStrace_data( TRACE_DATA *t)
{

printf("DATA TRACE = %s %s %s %s\n",
			t->trace_info.name,t->trace_info.lid,t->trace_info.pe,t->trace_info.ts);

}


/* ************************************* */ 
/* Get abd return page number that match */
/* selected lid.                         */
/* ************************************* */ 
int get_page_fromlid ( char *lid )
{

	int npage, ngraph, ntrace;

	PAGE_INFO   *pinfo;
	GRAPH_INFO  *ginfo;
	TRACE_INFO  *tinfo;

	printf("Pages: %d %s\n",GroupInfo->page_count, lid);
	for ( npage = 0; npage < GroupInfo->page_count; npage++)
	{
		pinfo = (PAGE_INFO *) &TSpageinfo[npage];
		for ( ngraph = 0; ngraph < pinfo->num_graphs; ngraph++)
		{
			ginfo = (GRAPH_INFO *) &pinfo->graph[ngraph];
			for ( ntrace = 0; ntrace < ginfo->num_traces; ntrace++)
			{
				tinfo = (TRACE_INFO *) &ginfo->traces[ntrace];
				/* printf("TRACE NAME = %s %s %s %s\n", tinfo->name, tinfo->lid, tinfo->pe, tinfo->ts); */
				if ( strcmp ( lid, tinfo->lid ) == 0)
							return ( npage );
			}
		}


	}

	return (-1);

}

/* ************************************************************ */
/* return the duration number based on duration character code  */
/* ************************************************************ */
int  lookup_dur( ShefDur *durPtr, char *cdur)
{

	/*
	   loop through linked list of SHEF duration codes and if the durcode
	   (single character) passed in matches one in the table then return
	   duration number (ie. the integer 1006) else move on down the list.
	*/
	while(durPtr)
	{
		if (strncmp(durPtr->durcode, cdur, 1) == 0)
		   return(durPtr->dur);
		else
                   durPtr = (ShefDur *) ListNext(&durPtr->node);
	}

	/* if NO match found then return 0 (Instantaneous) */
	return (0);
}


/* ************************************************************ */
/* Decode Physical Element, Type Source, Extremnum, Duration    */
/* from the trace name from traceInfo structure                 */
/* ************************************************************ */
void decode_pedtse ( TRACE_INFO *tptr, ShefDur *durPtr )
{
	strncpy((char *)&tptr->pe,  (char *)&tptr->name[0],2); 
	strncpy((char *)&tptr->cdur,(char *)&tptr->name[2],1); 
	strncpy((char *)&tptr->ts,  (char *)&tptr->name[3],2); 
	strncpy((char *)&tptr->extremum,(char *)&tptr->name[5],1); 
	tptr->pe[2]=tptr->cdur[1]=tptr->ts[2]=tptr->extremum[1] = '\0';
	tptr->dur = lookup_dur( durPtr, tptr->cdur );
	tptr->isForecast = 0;
	if (tptr->ts[0] == 'F' || tptr->ts[0] == 'C') 
	{
		tptr->isForecast = 1;
	}
	return;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source$";
 static char rcs_id2[] = "$Id$";}
/*  ===================================================  */

}

/* ************************************* */
/* End  of File:           getTSinfo.c   */ 
/* ************************************* */

