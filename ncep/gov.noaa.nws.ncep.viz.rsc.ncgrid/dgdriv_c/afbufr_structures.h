#ifndef _AFBUFRCOMMON_H
#define _AFBUFRCOMMON_H

#include <mel_bufr.h>

/*************************************************************/
/* Header File for G-AIRMET BUFR Encoding/Decoding Software  */
/* See the README file for the latest revisions              */
/* Author: Larry J. Hinson, AWC January 2006                 */
/*************************************************************/

/* Purpose: Define structures containing sequence info for the
   following G-AIRMET Hazards, by Designator and Types:
   1. SIERRA
      a. IFR Ceiling and Visibility
      b. Mountain Obscuration
   2. TANGO
      a. Turbulence
      b. Strong Surface Winds
      c. Low Level Wind Shear
   3. ZULU
      a. Icing
      b. Freezing Levels
*/
/* Note: Primary Structures defined above, build upon baseline structures:
Including the following:
  TimePeriod                        3-01-014
  Location                          3-01-023
  GFA Id, Obs or Fcst Location      3-16-054
  Description of a Feature          3-01-027
  Description of Horizontal Section 3-01-028
*/

/* Define any constants */

/***********************/

/***************************************************************/
/* Define Structures for Date and Time:  YYMMDD and HHMM       */
/***************************************************************/

/* BUFR 3-01-011 YYMMDD */
typedef struct {
  int year;  /* 0-04-001 */
  int month; /* 0-04-002 */
  int day;   /* 0-04-003 */
} YYYYMMDD1; /* YYYYMMDD is reserved as enum type in mel_bufr_types.h */

/* BUFR 3-01-012 HHMM */
typedef struct {
  int hour;    /* 0-04-004 */
  int minute;  /* 0-04-005 */
} HHMM;

/* -- BUFR 3-01-014 TimePeriod */
typedef struct {
  YYYYMMDD1 bd;  /* 3-01-011 */
  HHMM bt;       /* 3-01-012 */
  YYYYMMDD1 ed;  /* 3-01-011 */
  HHMM et;      /* 3-01-012 */
} TimePeriod;

/* BUFR 3-01-023 Location */
typedef struct {
  float lat;   /* 0-05-002 */
  float lon;   /* 0-06-002 */
} Location;

/*-- BUFR 3-01-028 Desc_HorSect */
typedef struct {
  int flightLevelSig;              /* 0-08-040 */
  int typeLimit;                   /* 0-33-042 */
  int flightLvl;                   /* 0-07-010 */
  int repCountOnCoords;            /* 0-31-002 */
  Location *location;              /* 3-01-023 */
  int radOfFeature;                /* 0-19-007 */
  int flightLevelSigCnl;           /* 0-08-040 */
} DESC_HorSect;


/* BUFR 3-01-027 Description of Feature*/
typedef struct {
  int dimSig;                       /* 0-08-007 */
  int repCount;                     /* 0-31-001 */
  DESC_HorSect *hs;                 /* 3-31-028 */
  int dimSigCnl;                    /* 0-08-007 */
} DESC_Feature;


/* BUFR 3-16-054 GFA Id, Obs or Fcst Location */
typedef struct {
  char GFASeqId[6];                /* 0-01-038 */
  int timeSig;                     /* 0-08-021 */
  TimePeriod tp;                   /* 3-01-014 */
  DESC_Feature df;                 /* 3-01-027 */
  int timeSigCnl;                  /* 0-08-021 */
} GFAIdObsOrFcstLoc;


/* BUFR 3-16-055 IFR CIG & VIS */
typedef struct {
  int prodStat;                    /* 0-08-079 */
  int dataSig;                     /* 0-08-041 */
  GFAIdObsOrFcstLoc giof;          /* 3-16-054 */
  int flightRules;                 /* 0-20-006 */
  int typeLimitCig;                /* 0-33-042 */
  int cloudBase;                   /* 0-20-013 */
  int typeLimitVis;                /* 0-33-042 */
  int horVis;                      /* 0-20-001 */
  int obsc;                        /* 0-20-025 */
  int charObsc;                    /* 0-20-026 */
  int dataSigCnl;                  /* 0-08-041 */
  int prodStatCnl;                 /* 0-08-079 */
} GFA_IFRCigAndVis_t;

/* BUFR 3-16-056 Mountain Obscuration */
typedef struct {
  int prodStat;                    /* 0-08-079 */
  int dataSig;                     /* 0-08-041 */
  GFAIdObsOrFcstLoc giof;          /* 3-16-054 */
  int flightRules;                 /* 0-20-006 */
  int obsc;                        /* 0-20-025 */
  int charObsc;                    /* 0-20-026 */
  int dataSigCnl;                  /* 0-08-041 */
  int prodStatCnl;                 /* 0-08-079 */
} GFA_MtnObsc_t;

/* BUFR 3-16-057  Turbulence */
typedef struct {
  int prodStat;                    /* 0-08-079 */
  int metFeature;                  /* 0-08-011 */
  GFAIdObsOrFcstLoc giof;          /* 3-16-054 */
  int degOfTurb;                   /* 0-11-031 */
  int metFeatureCnl;               /* 0-08-011 */
  int prodStatCnl;                 /* 0-08-079 */
} GFA_Turbulence_t;

/* BUFR 3-16-058  Sustained Strong Surface Winds */
typedef struct {
  int prodStat;                    /* 0-08-079 */
  int dataSig;                     /* 0-08-041 */
  GFAIdObsOrFcstLoc giof;          /* 3-16-054 */
  int typeLimitWndSpd;             /* 0-33-042 */
  int windSpeed10m;                /* 0-11-012 */
  int dataSigCnl;                  /* 0-08-041 */
  int prodStatCnl;                 /* 0-08-079 */
} GFA_SSW_t;

/* BUFR 3-16-059 Low Level Wind Shear */
typedef struct {
  int prodStat;                    /* 0-08-079 */
  int metFeature;                  /* 0-08-011 */
  GFAIdObsOrFcstLoc giof;          /* 3-16-054 */
  int othWxPhen;                   /* 0-20-023 */
  int intOthWxPhen;                /* 0-20-024 */
  int metFeatureCnl;               /* 0-08-011 */
  int prodStatCnl;                 /* 0-08-079 */
} GFA_LLWS_t;

/* BUFR 3-16-060 Moderate Icing */
typedef struct {
  int prodStat;                    /* 0-08-079 */
  int metFeature;                  /* 0-08-011 */
  GFAIdObsOrFcstLoc giof;          /* 3-16-054 */
  int airframeICG;                 /* 0-20-041 */
  int metFeatureCnl;               /* 0-08-011 */
  int prodStatCnl;                 /* 0-08-079 */
} GFA_Icing_t;

/* BUFR 3-16-061 GFA Freezing Level */
typedef struct {
  int prodStat;                    /* 0-08-079 */
  int dataSig;                     /* 0-08-041 */
  GFAIdObsOrFcstLoc giof;          /* 3-16-054 */
  int prodStatCnl;                 /* 0-08-079 */
  int dataSigCnl;                  /* 0-08-041 */
} GFA_FreezingLvl_t;

/* BUFR 3-16-052 G-AIRMET SIERRA */
typedef struct {
  TimePeriod tp;                   /* 3-01-014 Time Period*/
                                   /* 1-01-000 Delayed Replication */
  int repFactorIFR;                /* 0-31-002 Replication Factor */
  GFA_IFRCigAndVis_t *IFR;         /* 3-16-055 GFA IFR Cig&Vis */
                                   /* 1-01-000 Delayed Replication */
  int repFactorMtnObsc;            /* 0-31-002 Replication Factor */
  GFA_MtnObsc_t *MtnObsc;          /* 3-16-056 GFA Mountain Obsc */
} SIERRA_t;

/* BUFR 3-16-053 G-AIRMET TANGO */
typedef struct {
  TimePeriod tp;                   /* 3-01-014 Time Period*/
                                   /* 1-01-000 Delayed Replication */
  int repFactorTurb;               /* 0-31-002 Replication Factor */
  GFA_Turbulence_t *Turb;          /* 3-16-057 */
                                   /* 1-01-000 Delayed Replication */
  int repFactorSSW;                /* 0-31-002 Replication Factor */
  GFA_SSW_t *SSW;                  /* 3-16-058 */
                                   /* 1-01-000 Delayed Replication */
  int repFactorLLWS;               /* 0-31-002 Replication Factor */
  GFA_LLWS_t *LLWS;                /* 3-16-059 */
} TANGO_t;

/* BUFR 3-16-054 G-AIRMET ZULU */
typedef struct {
  TimePeriod tp;                   /* 3-01-014 Time Period */
                                   /* 1-01-000 Delayed Replication */
  int repFactorIcing;              /* 0-31-002 Replication Factor */
  GFA_Icing_t *Icing;              /* 3-16-060 Icing */
                                   /* 1-01-000 Delayed Replication */
  int repFactorFrzLvl;             /* 0-31-002 Replication Factor */
  GFA_FreezingLvl_t *FrzLvl;       /* 3-16-061 Freezing Level */
} ZULU_t;

enum GFADesignator {SIERRA, TANGO, ZULU};
enum GFATypes {IFR, IFR_CIG, IFR_VIS,
               MT_OBSC, TURB, TURB_HI, TURB_LO, SFC_WND, LLWS,
               ICE, FZLVL, M_FZLVL};
#define numGFADesig 3
#define numGFATypes 12

#ifdef _AFBUFR_GBL
  char *GFADesignatorNames[] = {"SIERRA","TANGO","ZULU"};
  char *GFATypeNames[] = {"IFR", "IFR_CIG", "IFR_VIS", "MT_OBSC",
                          "TURB", "TURB-HI", "TURB-LO","SFC_WND", "LLWS",
                          "ICE", "FZLVL", "M_FZLVL" };
#else
  extern char *GFADesignatorNames[];
  extern char *GFATypeNames[];
#endif

typedef struct {
  TimePeriod tp;
} BULLETIN_t;

typedef struct {
  enum GFADesignator td;
  union {
    SIERRA_t *sierra;
    TANGO_t *tango;
    ZULU_t *zulu;
    BULLETIN_t *bulletin;
  } b;
} GFAByDesignatorInfo;

void setCycleAndType(char *outputFormatIn, char *cycle, char *airmetType,
                    char *outputFormatStr);

void buildFilenameAndDateStamp(BULLETIN_t *bulletin, char *outputformat,
                               char *bufrfilename, char *dateStamp);
                               
void buildFilenameAndDateStampFmIssTime(char *issTimeStr, char *outputformat,
                                        char *bufrfilename, char *dateStamp);
                                        
#endif /* #ifndef _AFBUFRCOMMON_H */
