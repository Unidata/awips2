#include <string.h>
#include <time.h>

#include "TSCallbacks.h"
#include "TSWidget.h"
#include "SaveTSMod.h"
#include "ifp_atoms.h"
#include "Mods_info.h"
#include "TSPlots.h"
#include "c_call_f/mdyh1.h"
#include "c_call_f/julda.h"

extern Widget          global_toplevel;
extern PLOTMGR  *PlotMgr;

void Save_TSChange(int GraphIndex,int TraceIndex,ModKeywords TSMods[],int *NumTS)
{

  int i,j,Found,HasTSChngFlag;
  float Values[MAXPOINTS] ;
  TRACE *TracePtr;

  TracePtr = (TRACE *) &PlotMgr->Graph[GraphIndex].traces[TraceIndex];
  Found = 0 ; /*False*/
  TSMods[*NumTS].ModIndex=0;
  HasTSChngFlag=0;/*false*/
  for (i=0;i<TracePtr->npts;i++)
  {
    if (TSMods[*NumTS].ModIndex >= MAX_MODS) break ;

    if (TracePtr->TSData[i].status != 0)
    {
      if (!Found) /*The first point is found*/
      {
        strcpy(TSMods[*NumTS].TSID,TracePtr->OperationID);
	strcpy(TSMods[*NumTS].TSType,TracePtr->TraceType);
	TSMods[*NumTS].TimeInterval=TracePtr->TimeInterval;
	
        TSMods[*NumTS].ModArray[TSMods[*NumTS].ModIndex] = (ModInfo *) malloc (sizeof(ModInfo)) ;
        TSMods[*NumTS].ModArray[TSMods[*NumTS].ModIndex]->a2.type = Mod_format_A2 ;
	strcpy(TSMods[*NumTS].ModArray[TSMods[*NumTS].ModIndex]->a2.command,"TSCHNG") ;
        TSMods[*NumTS].ModArray[TSMods[*NumTS].ModIndex]->a2.start_date=TracePtr->TSData[i].t;
	TSMods[*NumTS].ModArray[TSMods[*NumTS].ModIndex]->a2.end_date= 0; 
        TSMods[*NumTS].ModArray[TSMods[*NumTS].ModIndex]->a2.valid_date=TracePtr->TSData[TracePtr->npts-1].t;
	strcpy(TSMods[*NumTS].ModArray[TSMods[*NumTS].ModIndex]->a2.segment_id,PlotMgr->SegName);
	TSMods[*NumTS].ModArray[TSMods[*NumTS].ModIndex]->a2.info = (TS_INFO *) malloc(sizeof(TS_INFO)) ;
	TSMods[*NumTS].ModArray[TSMods[*NumTS].ModIndex]->a2.info->ts_type= 18; /********NEED TO FIND WHAT IS THIS ONE*****************/
	TSMods[*NumTS].ModArray[TSMods[*NumTS].ModIndex]->a2.info->delta_t= TracePtr->TimeInterval;
	strcpy(TSMods[*NumTS].ModArray[TSMods[*NumTS].ModIndex]->a2.info->ts_id,TracePtr->OperationID) ;
	strcpy(TSMods[*NumTS].ModArray[TSMods[*NumTS].ModIndex]->a2.info->data_type,TracePtr->TraceType) ;
	TSMods[*NumTS].ModArray[TSMods[*NumTS].ModIndex]->a2.num_values= 1;
        Values[0] = TracePtr->TSData[i].y ;
	strcpy(TSMods[*NumTS].ModArray[TSMods[*NumTS].ModIndex]->a2.keyword," ") ;
	strcpy(TSMods[*NumTS].ModArray[TSMods[*NumTS].ModIndex]->a2.optype,"PLOT-TS") ;
	strcpy(TSMods[*NumTS].ModArray[TSMods[*NumTS].ModIndex]->a2.opname," ") ;
/*	TSMods[*NumTS].ModArray[TSMods[*NumTS].ModIndex]->a2.opname[8]='\0' ;*/
	
	HasTSChngFlag=1; /*true*/
      }
      else
      {
	Values[TSMods[*NumTS].ModArray[TSMods[*NumTS].ModIndex]->a2.num_values] = TracePtr->TSData[i].y ;
        TSMods[*NumTS].ModArray[TSMods[*NumTS].ModIndex]->a2.num_values++ ;
      }

      TracePtr->TSData[i].status = 0 ;
      Found=1 ; /*Found is true*/
    }
    else
    {
      if (Found) /*One series of tschng found in here*/
      {
	TSMods[*NumTS].ModArray[TSMods[*NumTS].ModIndex]->a2.values = (float*)malloc(sizeof(float)*TSMods[*NumTS].ModArray[TSMods[*NumTS].ModIndex]->a2.num_values) ;

        for (j=0;j<TSMods[*NumTS].ModArray[TSMods[*NumTS].ModIndex]->a2.num_values;j++)
          if (PlotMgr->General_Units==PlotMgr->NWSRFS_Units)
	    TSMods[*NumTS].ModArray[TSMods[*NumTS].ModIndex]->a2.values[j]=Values[j] ;
          else
            if (PlotMgr->General_Units==0)/*0 is english unit*/
	      TSMods[*NumTS].ModArray[TSMods[*NumTS].ModIndex]->a2.values[j]= (Values[j] * PlotMgr->Graph[GraphIndex].CFact) +
                                              PlotMgr->Graph[GraphIndex].Const;
	    else
              TSMods[*NumTS].ModArray[TSMods[*NumTS].ModIndex]->a2.values[j]= (Values[j] - PlotMgr->Graph[GraphIndex].Const) /
                                              PlotMgr->Graph[GraphIndex].CFact;
        TSMods[*NumTS].ModIndex++;
      }
      
      Found=0 ; /*Not found*/
    }
  }/***end of for loop*****/
  if (Found) /*Take of the last tschng*/
  {
    TSMods[*NumTS].ModArray[TSMods[*NumTS].ModIndex]->a2.values = (float*)malloc (sizeof(float)*TSMods[*NumTS].ModArray[TSMods[*NumTS].ModIndex]->a2.num_values) ;
    for (j=0;j<TSMods[*NumTS].ModArray[TSMods[*NumTS].ModIndex]->a2.num_values;j++)
      if (PlotMgr->General_Units==PlotMgr->NWSRFS_Units)
        TSMods[*NumTS].ModArray[TSMods[*NumTS].ModIndex]->a2.values[j]=Values[j] ;
      else
        if (PlotMgr->General_Units==0) /*0 is english unit*/
	  TSMods[*NumTS].ModArray[TSMods[*NumTS].ModIndex]->a2.values[j]= (Values[j] * PlotMgr->Graph[GraphIndex].CFact) +
                                          PlotMgr->Graph[GraphIndex].Const;
	else
          TSMods[*NumTS].ModArray[TSMods[*NumTS].ModIndex]->a2.values[j]= (Values[j] - PlotMgr->Graph[GraphIndex].Const) /
                                          PlotMgr->Graph[GraphIndex].CFact;
    TSMods[*NumTS].ModIndex++;
  }

*NumTS+=HasTSChngFlag ;
}
/*********************initialize the TSMods array **************/
void InitTSMods (ModKeywords TSMods[],int *NumTS)
{ int i,j ;
  
  *NumTS=0 ;
  
  for( i= 0; i<PlotMgr->nPlots; i++)
  {
    for (j=0; j<PlotMgr->Graph[i].ntraces; j++)
      Save_TSChange(i,j,TSMods,NumTS);
  }

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ifp/src/PlotTS/RCS/InitTSChngMod.c,v $";
 static char rcs_id2[] = "$Id: InitTSChngMod.c,v 1.2 2002/05/16 12:40:01 dws Exp $";}
/*  ===================================================  */

}/*end of InitTSMods () */
