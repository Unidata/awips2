#include "temp_item_struct.h"    /* for template item structure */
#include "rpf_general_defs.h"

/************************************************************************
   load_var.c
   (load_template_variables.c)
   
   PURPOSE
   Load the information into a table for template variables.
		
   NOTES
   All variable names are defined here.  The same names are
   used in various functions to check for matches with
   requested variables.
   
   If a variable hsa a null access list (i.e. ""), then it is 
   currently not supported. Access list values are:
    H = header; S = summary; L = Headline; T = tabular; R = data roundup;
    I = impact statement; C = historical comparison
   
  ***********************************************************************/

/* define the allowable items and their attributes;
   the fields contain the following info:
   (1) variable name; 
   (2) basic variable type/format;
   (3) structure that contains the data, or some index to the data as
       in the case of the impact statements;
   (4) access list for controlling which variables are allowed in which
       product sections (needed for general integrity purposes and also
       to avoid long strings such as impact statements being included
       as a variable in the condition).  note that if this string is 
       blank, then the variable is currently not available.
   (5) switch indicating whether to variable is allowed as a
       variable in a conditional expression 
   (6) regular_factor used in eng to metric unit coversion 
   (7) flow_factor used in eng to metric unit conversion when the
       variable represents flow based on the primary pe for the forecast
       point*/

/* declare prototype for dummy() */

void dummy();

const template_variable_struct TEMPLATE_VARIABLES_TABLE[] = 
   {   
   /* Product wide type variables.  These are variables which are completely
      independent of a given forecast point, forecast group, or VTEC action. */
   
   { "<ProdCateg>"     , VAR_STR, MISC, "HTSRL", TRUE, 1.0, 1.0 }, 
   { "<ProdId>"        , VAR_STR, MISC, "HTSRL", TRUE, 1.0, 1.0 },    
   
   { "<CurDate>"       , VAR_TIM, MISC, "HTSRL", TRUE,  1.0, 1.0 },
   { "<IssuanceNumber>", VAR_INT, MISC, "HTSRL", FALSE, 1.0, 1.0 },
   
   { "<UGCListZ>"      , VAR_STR, MISC, "HLT",  FALSE, 1.0, 1.0 },
   { "<UGCListC>"      , VAR_STR, MISC, "HLT",  FALSE, 1.0, 1.0 },
   
   { "<GrpList>"       , VAR_STR, MISC, "HSL", FALSE, 1.0, 1.0 },
   { "<GrpsFPList>"    , VAR_STR, MISC, "HSL", FALSE, 1.0, 1.0 },
   { "<CountyList>"    , VAR_STR, MISC, "HSL", FALSE, 1.0, 1.0 },
   { "<RiverList>"     , VAR_STR, MISC, "HSL", FALSE, 1.0, 1.0 },
   
   { "<HSA>"           , VAR_STR, MISC, "HSTRICL",  FALSE, 1.0, 1.0 }, 
   { "<OfficeName>"    , VAR_STR, MISC, "HSTRICL",  FALSE, 1.0, 1.0 },
   
   { "<Day0>"          , VAR_TIM, MISC, "HSTRICL",  FALSE, 1.0, 1.0 },
   { "<Day1>"          , VAR_TIM, MISC, "HSTRICL",  FALSE, 1.0, 1.0 },
   { "<Day2>"          , VAR_TIM, MISC, "HSTRICL",  FALSE, 1.0, 1.0 },
   { "<Day3>"          , VAR_TIM, MISC, "HSTRICL",  FALSE, 1.0, 1.0 },
   { "<Day4>"          , VAR_TIM, MISC, "HSTRICL",  FALSE, 1.0, 1.0 },
   { "<Day5>"          , VAR_TIM, MISC, "HSTRICL",  FALSE, 1.0, 1.0 },
   { "<Day6>"          , VAR_TIM, MISC, "HSTRICL",  FALSE, 1.0, 1.0 },
   { "<Day7>"          , VAR_TIM, MISC, "HSTRICL",  FALSE, 1.0, 1.0 },
         
   
   /* Forecast group variables */

   { "<GrpId>"            , VAR_STR, GRP,        "S", TRUE,  1.0, 1.0 },
   { "<GrpIdName>"        , VAR_STR, GRP,        "S", FALSE, 1.0, 1.0 },
   
   { "<GrpMaxCurCat>"     , VAR_INT, STAGES_GRP, "S", TRUE,  1.0, 1.0 },
   { "<GrpMaxCurCatName>" , VAR_STR, STAGES_GRP, "S", FALSE, 1.0, 1.0 },
   
   { "<GrpMaxFcstCat>"    , VAR_INT, STAGES_GRP, "S", TRUE,  1.0, 1.0 },
   { "<GrpMaxFcstCatName>", VAR_STR, STAGES_GRP, "S", FALSE, 1.0, 1.0 },
   
   { "<GrpOMFCat>"        , VAR_INT, STAGES_GRP, "S", TRUE,  1.0, 1.0 },
   { "<GrpOMFCatName>"    , VAR_STR, STAGES_GRP, "S", FALSE, 1.0, 1.0 },
   
   { "<GrpObsFound>"      , VAR_INT, STAGES_GRP, "S", TRUE, 1.0, 1.0 },
   { "<GrpFcstFound>"     , VAR_INT, STAGES_GRP, "S", TRUE, 1.0, 1.0 },
   
   { "<NumGrps>"          , VAR_INT, MISC,       "S", TRUE,  1.0, 1.0 },
   { "<GrpFPList>"        , VAR_STR, MISC,       "S", FALSE, 1.0, 1.0 },
  
   
   /* Generic variable for use with PE-based dynamic data for data locations */
   
   { "<PETime>"           , VAR_TIM, MISC,       "TR", FALSE, 1.0, 1.0 },
   { "<PEVal>"            , VAR_FLT, MISC,       "TR", TRUE,  1.0, 1.0 },
   
     
   /* Location static variables  */
   
   { "<Id>"           , VAR_STR, LOCINFO,  "TRICL", TRUE,  1.0, 1.0 },
   { "<IdName>"       , VAR_STR, LOCINFO,  "TRICL", FALSE, 1.0, 1.0 }, 
   { "<County>"       , VAR_STR, LOCINFO,  "TRICL", FALSE, 1.0, 1.0 }, 
   { "<StateId>"      , VAR_STR, LOCINFO,  "TRICL", FALSE, 1.0, 1.0 }, 
   { "<StateName>"    , VAR_STR, LOCINFO,  "TRICL", FALSE, 1.0, 1.0 },
   
   { "<Reach>"        , VAR_STR, LOCINFO,  "TRICL", FALSE, 1.0, 1.0 },
   { "<Proximity>"    , VAR_STR, LOCINFO,  "TRICL", FALSE, 1.0, 1.0 },
   
   { "<River>"        , VAR_STR, LOCINFO,  "TRICL", FALSE, 1.0, 1.0 }, 
   { "<BankStg>"      , VAR_FLT, LOCINFO,  "TRICL", TRUE, 0.3048, 1.0 }, 
   { "<WStg>"         , VAR_FLT, LOCINFO,  "TRICL", TRUE, 0.3048, 1.0 }, 
   { "<ZDatum>"       , VAR_FLT, LOCINFO,  "TRICL", TRUE, 0.3048, 1.0 }, 
   { "<FldStg>"       , VAR_FLT, LOCINFO,  "TRICL", TRUE, 0.3048, 1.0 }, 
   { "<FldFlow>"      , VAR_FLT, LOCINFO,  "TRICL", TRUE, 0.0283, 1.0 },
   
   { "<StgFlowName>"  , VAR_STR, LOCINFO, "TRICL", TRUE, 1.0, 1.0 },
   { "<StgFlowUnits>" , VAR_STR, LOCINFO, "TRICL", TRUE, 1.0, 1.0 },
   
   { "<LocGeoArea>"   , VAR_STR, LOCINFO, "TRICL", FALSE, 1.0, 1.0 },
   { "<LocCntyList>"  , VAR_STR, LOCINFO, "TRICL", FALSE, 1.0, 1.0 },
   
   { "<LocLat>"       , VAR_FLT, LOCINFO, "TRIC", TRUE, 1.0, 1.0 }, 
   { "<LocLon>"       , VAR_FLT, LOCINFO, "TRIC", TRUE, 1.0, 1.0 },
   
  

   /* Forecast point's static variable  */
   
   { "<MinCatVal>"    , VAR_FLT, FP,  "TRICL", TRUE, 0.3048, 0.028317 }, 
   { "<ModCatVal>"    , VAR_FLT, FP,  "TRICL", TRUE, 0.3048, 0.028317 }, 
   { "<MajCatVal>"    , VAR_FLT, FP,  "TRICL", TRUE, 0.3048, 0.028317 },
   { "<RecCatVal>"    , VAR_FLT, FP,  "TRICL", TRUE, 0.3048, 0.028317 }, 
           
   { "<ImpactStg>"    , VAR_FLT, MISC, "IL", FALSE, 0.3048, 1.0 },
   { "<ImpactDescr>"  , VAR_STR, MISC, "IL", FALSE, 1.0,    1.0 },
   
   { "<HistCrestDate>", VAR_DAT, PCC,  "CL", FALSE, 1.0,    1.0 },
   { "<HistCrestStg>" , VAR_FLT, PCC,  "CL", FALSE, 0.3048, 1.0 },
   
   { "<ImpCompUnits>"   , VAR_STR,  FP,  "TRICL", FALSE, 1.0, 1.0 },       
  
    
   /* Forecast point's stage data */
   
   { "<ObsStg>"        , VAR_FLT, STAGES_FP_OBS, "TRICL", TRUE, 0.3048, 0.028317 }, 
   { "<ObsCat>"        , VAR_INT, STAGES_FP_OBS, "TRICL", TRUE,  1.0, 1.0 },
   { "<ObsCatName>"    , VAR_STR, STAGES_FP_OBS, "TRICL", FALSE, 1.0, 1.0 },
   { "<ObsTime>"       , VAR_TIM, STAGES_FP_OBS, "TRICL", TRUE,  1.0, 1.0 },
      
   { "<MaxFcstStg>"    , VAR_FLT, STAGES_FP_FCST, "TRICL", TRUE, 0.3048, 0.028317 },
   { "<MaxFcstCat>"    , VAR_INT, STAGES_FP_FCST, "TRICL", TRUE,  1.0, 1.0 },
   { "<MaxFcstCatName>", VAR_STR, STAGES_FP_FCST, "TRICL", FALSE, 1.0, 1.0 },
   { "<MaxFcstTime>"   , VAR_TIM, STAGES_FP_FCST, "TRICL", TRUE,  1.0, 1.0 },

   { "<OMFVal>"        , VAR_FLT, STAGES_FP_OTHER, "TRICL", TRUE, 0.3048, 0.028317 },
   { "<OMFCat>"        , VAR_INT, STAGES_FP_OTHER, "TRICL", TRUE,  1.0, 1.0 },
   { "<OMFCatName>"    , VAR_STR, STAGES_FP_OTHER, "TRICL", FALSE, 1.0, 1.0 },
   
   { "<ObsStgTrend>"   , VAR_STR, STAGES_FP_OTHER, "TRICL", TRUE, 1.0, 1.0 },
   { "<StgTrend>"      , VAR_STR, STAGES_FP_OTHER, "TRICL", TRUE, 1.0, 1.0 },
   
   
   /* Special forecast point stage data variable */

   
   { "<FcstTrend>", VAR_STR, STAGES_FP_TREND, "TRICL", FALSE, 1.0, 1.0 },

      
   /* More forecast point stage values */
   
   { "<NumObsStg>"      , VAR_INT, STAGES_FP_OBS,  "RL", TRUE, 1.0, 1.0 }, 
   { "<NumFcstStg>"     , VAR_INT, STAGES_FP_FCST, "RL", TRUE, 1.0, 1.0 },

   { "<NumObsFlow>"     , VAR_INT, STAGES_FP_OBS,  "RL", TRUE, 1.0, 1.0 }, 
   { "<NumFcstFlow>"    , VAR_INT, STAGES_FP_FCST, "RL", TRUE, 1.0, 1.0 },
   
   { "<SpecObsStg>"     , VAR_FLT, STAGES_SPEC, "TRL", FALSE, 0.3048, 0.028317 },
   { "<SpecObsStgTime>" , VAR_TIM, STAGES_SPEC, "TRL", FALSE, 1.0, 1.0 },
   { "<SpecFcstStg>"    , VAR_FLT, STAGES_SPEC, "TRL", FALSE, 0.3048, 0.028317 },
   { "<SpecFcstStgTime>", VAR_TIM, STAGES_SPEC, "TRL", FALSE, 1.0, 1.0 },

   { "<ObsCrestStg>"   , VAR_FLT, STAGES_FP_OBS,  "TRICL", TRUE, 0.3048, 0.028317 },
   { "<ObsCrestTime>"  , VAR_TIM, STAGES_FP_OBS,  "TRICL", TRUE, 1.0, 1.0 },
   
   { "<FcstCrestStg>"  , VAR_FLT, STAGES_FP_FCST, "TRICL", TRUE, 0.3048, 0.028317 },
   { "<FcstCrestTime>" , VAR_TIM, STAGES_FP_FCST, "TRICL", TRUE, 1.0, 1.0 },
   
   { "<MaxObsStg24>"   , VAR_FLT, STAGES_FP_OBS,  "TRICL", TRUE, 0.3048, 0.028317 }, 
   { "<MaxObsStg06>"   , VAR_FLT, STAGES_FP_OBS,  "TRICL", TRUE, 0.3048, 0.028317 }, 

   { "<ObsRiseFSTime>" , VAR_TIM, STAGES_FP_OBS,  "TRL", TRUE, 1.0, 1.0 }, 
   { "<ObsFallFSTime>" , VAR_TIM, STAGES_FP_OBS,  "TRL", TRUE, 1.0, 1.0 }, 

   { "<FcstRiseFSTime>", VAR_TIM, STAGES_FP_FCST, "TRL", TRUE, 1.0, 1.0 },
   { "<FcstFallFSTime>", VAR_TIM, STAGES_FP_FCST, "TRL", TRUE, 1.0, 1.0 }, 

   { "<ObsFSDeparture>"  , VAR_FLT, STAGES_FP_OBS,   "TRL", TRUE, 0.3048, 0.028317 }, 
   { "<FcstFSDeparture>" , VAR_FLT, STAGES_FP_FCST,  "TRL", TRUE, 0.3048, 0.028317 },  
   { "<ObsFSDepartureA>" , VAR_FLT, STAGES_FP_OBS,   "TRL", TRUE, 0.3048, 0.028317 }, 
   { "<FcstFSDepartureA>", VAR_FLT, STAGES_FP_FCST,  "TRL", TRUE, 0.3048, 0.028317 },  
   
   { "<XCrestStg>"  ,      VAR_FLT, STAGES_FP_FCST, "TRICL", TRUE, 0.3048, 0.028317 },
   { "<XCrestTime>" ,      VAR_TIM, STAGES_FP_FCST, "TRICL", TRUE, 1.0, 1.0 },
  
   { "<XRCrestStg>"  ,     VAR_FLT, STAGES_FP_FCST, "TRICL", TRUE, 0.3048, 0.028317 },
   { "<XRCrestTime>" ,     VAR_TIM, STAGES_FP_FCST, "TRICL", TRUE, 1.0, 1.0 },
   
   { "<XRMaxStg>"  ,       VAR_FLT, STAGES_FP_FCST, "TRICL", TRUE, 0.3048, 0.028317 },
   { "<XRMaxTime>" ,       VAR_TIM, STAGES_FP_FCST, "TRICL", TRUE, 1.0, 1.0 },
   
   { "<MaxCrestStg>"  ,    VAR_FLT, STAGES_FP_FCST, "TRICL", TRUE, 0.3048, 0.028317 },
   { "<MaxCrestTime>" ,    VAR_TIM, STAGES_FP_FCST, "TRICL", TRUE, 1.0, 1.0 },
   
   { "<XMaxCrestStg>"  ,   VAR_FLT, STAGES_FP_FCST, "TRICL", TRUE, 0.3048, 0.028317 },
   { "<XMaxCrestTime>" ,   VAR_TIM, STAGES_FP_FCST, "TRICL", TRUE, 1.0, 1.0 },
   
   { "<FcstHrs>"      ,    VAR_INT, STAGES_FP_FCST, "TRICL",  TRUE,  1.0, 1.0 }, 
   { "<FcstHrsTime>"  ,    VAR_TIM, STAGES_FP_FCST, "TRICL",  TRUE,  1.0, 1.0 },
   
   /* Forecast point previous issuance info */
   
   { "<PrevCatName>"           ,  VAR_STR, FP, "TRICL", FALSE, 1.0, 1.0 },   
   { "<PrevCat>"               ,  VAR_INT, FP, "TRICL", TRUE,  1.0, 1.0 },
      
   { "<PrevObsCat>"            ,  VAR_INT, FP, "TRICL", TRUE,  1.0, 1.0 },
   { "<PrevObsCatName>"        ,  VAR_STR, FP, "TRICL", FALSE, 1.0, 1.0 },
   { "<PrevMaxFcstCat>"        ,  VAR_INT, FP, "TRICL", TRUE,  1.0, 1.0 },
   { "<PrevMaxFcstCatName>"    ,  VAR_STR, FP, "TRICL", FALSE, 1.0, 1.0 },
   
   /* For VTEC action-based info */
   
   { "<ActionStateCntyList>"   ,  VAR_STR, EVENT, "L",     FALSE, 1.0, 1.0 },
   { "<ActionStateList>"       ,  VAR_STR, EVENT, "L",     FALSE, 1.0, 1.0 },
   { "<ActionRiverList>"       ,  VAR_STR, EVENT, "L",     FALSE, 1.0, 1.0 },
   
   
   /* For VTEC events based info.  Generally the events are expected to
      be forecast point specific - i.e. not forecast group or county specific */
   
   { "<PrevEventBeginTime>"    ,  VAR_TIM, EVENT, "TRICL", TRUE, 1.0, 1.0 },
   { "<PrevEventEndTime>"      ,  VAR_TIM, EVENT, "TRICL", TRUE, 1.0, 1.0 },         
   { "<Action>"                ,  VAR_STR, EVENT, "TRICL", TRUE, 1.0, 1.0 },          
   { "<EventBeginTime>"        ,  VAR_TIM, EVENT, "TRICL", TRUE, 1.0, 1.0 },
   { "<EventEndTime>"          ,  VAR_TIM, EVENT, "TRICL", TRUE, 1.0, 1.0 },
   { "<EventTime>"             ,  VAR_STR, EVENT, "TRICL", FALSE,1.0, 1.0 },
   { "<VRTime>"                ,  VAR_TIM, EVENT, "TRICL", TRUE, 1.0, 1.0 }, 
   { "<VCTime>"                ,  VAR_TIM, EVENT, "TRICL", TRUE, 1.0, 1.0 },
   { "<VFTime>"                ,  VAR_TIM, EVENT, "TRICL", TRUE, 1.0, 1.0 },
   { "<VCValue>"               ,  VAR_FLT, EVENT, "TRICL", TRUE, 1.0, 1.0 }

   
   };

/* set the number of items in the template items table */

int NUM_OF_TEMPLATE_VARIABLES = 
   (sizeof(TEMPLATE_VARIABLES_TABLE) / sizeof(template_variable_struct));
   
/* Consider then check in to CDA, it will add RCS keywords at the end
of file, but if it is not standard function, current CM will add RCS
in the middle of the file. So make a dummy function */

void dummy()
{

return;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob81/ohd/whfs_lib/src/RPFEngine/RCS/load_var.c,v $";
 static char rcs_id2[] = "$Id: load_var.c,v 1.8 2007/02/28 16:05:14 deng Exp $";}
/*  ===================================================  */

}   
