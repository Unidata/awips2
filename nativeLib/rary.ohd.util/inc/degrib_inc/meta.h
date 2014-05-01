/*****************************************************************************
 * meta.h
 *
 * DESCRIPTION
 *    This file contains the code necessary to handle the meta data that
 * comes out of the GRIB2 decoder.  This includes parsing the integer arrays
 * to create a meta structure, and providing routines to write the meta
 * structure to disk.
 *
 * HISTORY
 *    9/2002 Arthur Taylor (MDL / RSIS): Created.
 *
 * NOTES
 *****************************************************************************
 */
#ifndef META_H
#define META_H

#include <time.h>
/* Include type.h for uChar and sChar */
#include "degrib_inc/type.h"
#ifdef MEMWATCH
  #include "degrib_inc/memwatch.h"
#endif

#ifndef GRIB2BIT_ENUM
#define GRIB2BIT_ENUM
/* See rule (8) bit 1 is most significant, bit 8 least significant. */
enum {GRIB2BIT_1=128, GRIB2BIT_2=64, GRIB2BIT_3=32, GRIB2BIT_4=16,
      GRIB2BIT_5=8, GRIB2BIT_6=4, GRIB2BIT_7=2, GRIB2BIT_8=1};
#endif

#ifndef UNITCONVERT_ENUM
#define UNITCONVERT_ENUM
typedef enum { UC_NONE, UC_K2F, UC_InchWater, UC_M2Feet, UC_M2Inch,
   UC_MS2Knots
} unit_convert;
#endif

/* For GRIB1 GDS Types. */
enum { GB1S2_LATLON = 0, GB1S2_MERCATOR = 1, GB1S2_LAMBERT = 3,
      GB1S2_POLAR = 5
};

#define GRIB2MISSING_1 (int) (0xff)
#define GRIB2MISSING_2 (int) (0xffff)
#define GRIB2MISSING_4 (long int) (0xffffffff)

#define NUM_UGLY_WORD 5
#define NUM_UGLY_ATTRIB 5

typedef struct {
   uChar numValid;         /* (0..5) How many valid "types" */
   uChar wx[NUM_UGLY_WORD]; /* (see WxCode) */
   uChar cover[NUM_UGLY_WORD]; /* (see WxCover) */
   uChar intens[NUM_UGLY_WORD]; /* (see WxIntens) */
   uChar vis[NUM_UGLY_WORD]; /* 255 no vis, otherwise in units of 1/32 SM
                            * so 6SM -> 192. P6SM -> 224 */
   uChar f_or[NUM_UGLY_WORD]; /* true if OR, or MX was a hazzard. */
   uChar attrib[NUM_UGLY_WORD][NUM_UGLY_ATTRIB]; /* (see WxAttrib) */
   uChar minVis;           /* vis[] should be constant, but just in case
                            * we use the minimum value. */
   uChar f_valid;          /* 1 valid may be not-used, 2 valid and used,
                            * 0 invalid may be not-used,
                            * temporarily 3 (invalid and used). */
   long int validIndex;    /* Which index this is, counting only used
                            * valid indexes.  If it is not used it is -1 */
   char *english[NUM_UGLY_WORD]; /* The english translation of ugly string. */
   uChar wx_inten[NUM_UGLY_WORD]; /* A code to represent the wx and
                              intensity for an "ugly word". */
   long int HazCode[NUM_UGLY_WORD]; /* A code to represent all the attributes. */
   int SimpleCode;         /* Simple weather code for this ugly string. */
   char *errors;           /* if STORE_ERRORS, then it contains any error
                            * messages found while parsing this string. */ 
} UglyStringType;

typedef struct {
   char **data;              /* Array of text strings (aka "ugly strings) */
   long int dataLen;         /* number of text strings in data. */
   int maxLen;               /* Max Length of all of the "ugly strings"
                              * It includes 1 for the \0 character. */
   UglyStringType *ugly;     /* The parsed Ugly string. */
   int maxEng[NUM_UGLY_WORD]; /* Max length of english phrases for all ugly
                               * word number X. */
} sect2_WxType;

typedef struct {
   double *data;             /* Array of actual values (cast from int or
                                float to double). */
   long int dataLen;         /* Number of elements in data. */
} sect2_UnknownType;

enum { GS2_NONE, GS2_WXTYPE, GS2_UNKNOWN };

typedef struct {
/*   void *ptr;  */              /* Pointer to section 2 data. */
   sect2_WxType wx;          /* wx data. */
   sect2_UnknownType unknown; /* unknown type of section 2 data. */
   uChar ptrType;            /* Which structure is valid.
                                GS2_WXTYPE => wx
                                GS2_UNKNOWN => unknown */
} sect2_type;

enum { CAT_TEMP, CAT_MOIST, CAT_MOMENT, CAT_MASS, CAT_SW_RAD,
       CAT_LW_RAD, CAT_CLOUD, CAT_THERMO_INDEX, CAT_KINEMATIC_INDEX,
       CAT_TEMP_PROB, CAT_MOISTURE_PROB, CAT_MOMENT_PROB, CAT_MASS_PROB,
       CAT_AEROSOL, CAT_TRACE, CAT_RADAR, CAT_RADAR_IMAGERY, CAT_ELECTRO,
       CAT_NUCLEAR, CAT_PHYS_ATMOS };
enum { TEMP_TEMP, TEMP_VIRT, TEMP_POTENTIAL, TEMP_PSEUDO_POTENTIAL,
       TEMP_MAXT, TEMP_MINT, TEMP_DEW_TEMP, TEMP_DEW_DEPRESS,
       TEMP_LAPSE, TEMP_ANOMALY, TEMP_LATENT_FLUX, TEMP_SENSIBLE_FLUX,
       TEMP_HEAT, TEMP_WINDCHILL, TEMP_MIN_DEW_DEPRESS, TEMP_VIRT_POTENTIAL };
enum { CLOUD_ICE, CLOUD_COVER, CLOUD_CONVECT_COVER, CLOUD_LOW,
       CLOUD_MEDIUM, CLOUD_HIGH, CLOUD_WATER, CLOUD_AMNT, CLOUD_TYPE,
       CLOUD_THUDER_MAX, CLOUD_THUNDER_COVER, CLOUD_BASE, CLOUD_TOP,
       CLOUD_CEIL };
enum { MOMENT_WINDDIR, MOMENT_WINDSPD, MOMENT_U_WIND, MOMENT_V_WIND,
       MOMENT_STREAM, MOMENT_VEL_POTENT, MOMENT_MONT_STREAM,
       MOMENT_SIGMA_VERTVEL, MOMENT_VERTVEL_PRESS, MOMENT_VERTVEL_GEOMETRIC,
       MOMENT_ABS_VORT, MOMENT_ABS_DIV, MOMENT_REL_VORT, MOMENT_REL_DIV,
       MOMENT_POT_VORT, MOMENT_VERT_U_SHEAR, MOMENT_VERT_V_SHEAR,
       MOMENT_U_FLUX, MOMENT_V_FLUX, MOMENT_MIX_ENERGY,
       MOMENT_BOUNDARY_DISSPATE, MOMENT_MAX_WINDSPD, MOMENT_GUSTSPD,
       MOMENT_U_GUSTSPD, MOMENT_V_GUSTSPD };
enum { MOIST_SPEC_HUMID, MOIST_REL_HUMID, MOIST_HUMID_MIX, MOIST_PRECIP_WATER,
       MOIST_VAPOR_PRESS, MOIST_SAT_DEFICIT, MOIST_EVAP, MOIST_PRECIP_RATE,
       MOIST_PRECIP_TOT, MOIST_LARGE_SCALE, MOIST_CONVECT_PRECIP,
       MOIST_SNOWAMT, MOIST_SNOWRATE_WATER, MOIST_SNOWAMT_WATER,
       MOIST_CONVECT_SNOW, MOIST_LARGE_SCALE_SNOW, MOIST_SNOWMELT,
       MOIST_SNOWAGE, MOIST_ABS_HUMID, MOIST_PRECIP_TYPE,
       MOIST_INTEGRATE_WATER, MOIST_CONDENSATE, MOIST_CLOUDMIX_RATIO,
       MOIST_ICEMIX_RATIO, MOIST_RAINMIX_RATIO, MOIST_SNOWMIX_RATIO,
       MOIST_HORIZ_CONVERGE, MOIST_MAXREL_HUMID, MOIST_MAXABS_HUMID,
       MOIST_TOT_SNOW, MOIST_PRECIP_WATER_CAT, MOIST_HAIL, MOIST_GRAUPEL };
enum { OCEAN_CAT_WAVES, OCEAN_CAT_CURRENT, OCEAN_CAT_ICE,
       OCEAN_CAT_SURF, OCEAN_CAT_SUBSURF };
enum { OCEAN_WAVE_SPECTRA1, OCEAN_WAVE_SPECTRA2, OCEAN_WAVE_SPECTRA3,
       OCEAN_WAVE_SIG_HT_WV_SWELL, OCEAN_WAVE_DIR_WV,
       OCEAN_WAVE_SIG_HT_WV, OCEAN_WAVE_PD_WV, OCEAN_WAVE_DIR_SWELL,
       OCEAN_WAVE_SIG_HT_SWELL, OCEAN_WAVE_PD_SWELL, OCEAN_WAVE_PRIM_DIR,
       OCEAN_WAVE_PRIM_PD, OCEAN_WAVE_SEC_DIR, OCEAN_WAVE_SEC_PD };

typedef struct {
   uChar processID;          /* Statistical process method used. */
   uChar incrType;           /* Type of time increment between intervals */
   uChar timeRangeUnit;      /* Time range unit. */
   long int lenTime;         /* Range or length of time interval. */
   uChar incrUnit;           /* Unit of time increment. */
   long int timeIncr;        /* Time increment between intervals. */
} sect4_IntervalType;

typedef struct {             /* Used to store data where unpacked value =
                                value / 10**factor */
   long int value;           /* scale value in equation */
   sChar factor;             /* scale factor in equation. */
} ScaleType;

typedef struct {  /* See Template 4.30. */
   unsigned short int series; /* Satellite series of band. */
   unsigned short int numbers; /* Satellite numbers of band. */
   uChar instType;           /* Instrument type of band */
   ScaleType centWaveNum;    /* scaled value for central wave number of
                                band. */
} sect4_BandType;

enum { GS4_ANALYSIS, GS4_ENSEMBLE, GS4_DERIVED, GS4_STATISTIC = 8,
   GS4_PROBABILITY = 9, GS4_RADAR = 20, GS4_SATELLITE = 30
};

typedef struct {
   unsigned short int templat; /* The section 4 template number. */
   uChar cat;                /* General category of Meteo Product. */
   uChar subcat;             /* Specific subcategory of Meteo Product. */
   uChar genProcess;         /* What type of generate process (Analysis,
                                Forecast, Probability Forecast, etc). */
   uChar bgGenID;            /* Background generating process id. */
   uChar genID;              /* Analysis/Forecast generating process id. */
   uChar f_validCutOff;      /* 1 if cutOff is valid, 0 if cutoff missing. */
   long int cutOff;          /* Data Cutoff in seconds after reference time
                                (see sect 1 for reference time). */
   double foreSec;           /* forecast "projection time" in seconds. */
   uChar fstSurfType;        /* Type of the first fixed surface. */
   double fstSurfValue;      /* Value of first fixed surface. */
   uChar fstSurfScale;       /* Scale factor used when storing value. */
   uChar sndSurfType;        /* Type of the second fixed surface. */
   double sndSurfValue;      /* Value of second fixed surface. */
   uChar sndSurfScale;       /* Scale factor used when storing value. */
   time_t validTime;         /* The ending time, or valid time, in seconds
                                UTC.  This is specified in template 4.8, 4.9,
                                for the others it is refTime + foreSec. */
        /* The following are somewhat template specific. */
   uChar typeEnsemble;       /* The type of Ensemble forecast
                                Template 4.1, (Code Table 4.5) */
   uChar perturbNum;         /* Perturbation number, Template 4.1 */
   uChar numberFcsts;        /* Number of forecasts in Ensemble.
                                Template, 4.1, 4.2 */
   uChar derivedFcst;        /* Derived Forecast (from Ensemble).
                                Template 4.2 */
   uChar numInterval;        /* Number of time intervals. Template 4.8,4.9 */
   long int numMissing;      /* Number of missing values. Template 4.8,4.9 */
   sect4_IntervalType *Interval; /* Stores the array of time intervals.
                                Template 4.8,4.9 */
   uChar numBands;           /* Number of Spectral Bands. Template 4.30 */
   sect4_BandType *bands;    /* Holds info about each Band Template 4.30 */
   uChar foreProbNum;        /* Forecast Probability number (Template 4.9) */
   uChar numForeProbs;       /* Total # of forecast probabilities (4.9) */
   uChar probType;           /* Type of probability range. (Template 4.9)
                                0 below lower limit. 1 above upper limit.
                                2 between lower (inclusive) and
                                  upper limit (exclusive)
                                3 above lower limit. 4 below upper limit. */
   ScaleType lowerLimit;     /* Lower Limit probability field. Template 4.9*/
   ScaleType upperLimit;     /* Upper Limit probability field. Template 4.9*/
} sect4_type;

/* This is the structure needed for the PDS (Product Definition Section)
 * for GRIB2.  I hope to combine stuff, but there is a lot more to GRIB2
 * than GRIB1
 */
typedef struct {
   /* Section 0 Data */
   uChar prodType;           /* 0 is meteo product, 1 is hydro, 2 is land
                                3 is space, 10 is oceanographic. */
   /* Section 1 Data */
   unsigned short int center, subcenter; /* Who produced it. */
   uChar mstrVersion;        /* Master table version. */
   uChar lclVersion;         /* Local table version. */
   uChar sigTime;            /* Significance of reference time */
   time_t refTime;           /* Reference time in seconds UTC */
   uChar operStatus;         /* What is operational status of data. */
   uChar dataType;           /* What type of data. (analysis, forecast,
                                analysis & forecast, ...) */

   /* Rest of PDS. */
   uChar f_sect2;            /* 1 if sect2 exists, 0 otherwise. */
   long int sect2NumGroups;  /* total number of groups in section 2 data. */
   sect2_type sect2;         /* sect 2 info */
   sect4_type sect4;         /* sect 4 info */
} pdsG2Type;

/* This is the structure needed for the PDS (Product Definition Section)
 * for GRIB1
 */
typedef struct {
   uChar mstrVersion;        /* Parameter Table Version #. */
   uChar center, subcenter;  /* Who produced it. */
   uChar genProcess;         /* Generating Process ID. ?Sect 4? */
   uChar cat;                /* General category of Meteo Product. */
   uChar gridID;             /* The Grid Defin ID number (GRIB1 specific) */ 
   uChar levelType;          /* Type of level. ?sect 4 fstSurf? */
   int levelVal;             /* Value of level. */
   time_t refTime;           /* Reference time in seconds UTC */
   time_t P1;                /* Period of time1 ?sect 4 valid time? */
   time_t P2;                /* Period of time2 */
   time_t validTime;         /* Valid time in seconds UTC */
   uChar timeRange;          /* Time Range Indicator. */
   /* Specific for averages or accumulations. (determined by timeRange) */
   int Average;              /* number included in average. */
   uChar numberMissing;      /* number missing from averages. */
} pdsG1Type;

/* This is the structure and enumerated types needed for the GDS (Grid
 * Definition Section. In GRIB2 the GDS was in section 3, in GRIB1 it was in
 * section 2.
 */
enum { GS3_LATLON = 0, GS3_MERCATOR = 10, GS3_POLAR = 20,
       GS3_LAMBERT = 30, GS3_ORTHOGRAPHIC = 90,
       GS3_EQUATOR_EQUIDIST = 110, GS3_AZIMUTH_RANGE = 120};

/* Note: It appears that compilers break up a struct based on the largest
   element type.  In this case a double.  So to avoid wasted memory, we need
   things to stop at 8 byte borders. long int, double, long int is larger than
   long int, long int, double. */
typedef struct {
   sInt4 numPts;             /* Number of data points. Typically Nx * Ny, but
                                for some exotic grids they don't have Nx,Ny */
   uChar projType;           /* Projection type / template type. Valid choices
                                0(lat/lon), 10(mercator), 20(Polar Stereo),
                                30(Lambert Conformal),
                                in future maybe 90, 110, 120. */
/* Shape of Earth */
   uChar f_sphere;           /* 1 is a sphere, => majEarth == minEarth */
   double majEarth, minEarth; /* semi major and minor axis of earth in km. */
/* Projection info. */
   sInt4 Nx, Ny;             /* Dimensions of grid. */
   double lat1, lon1;        /* lat,lon position of first grid point. */
   /* resFlag: moved to save memory. */
   double orientLon;         /* Where up is North. (0 for lat/lon grids) */
   double Dx, Dy;            /* Mesh delta x,y in degrees or meters. */
   double meshLat;           /* Where the mesh size is defined
                                (0 for lat/lon grids.) */
   uChar resFlag;            /* Table 7 GRIB1 : Section 2 */
   uChar center;             /* For lambert and polar stereographic, answers:
                                (south/north?) and (bi-polar?) */
   uChar scan;               /* describes how the grid was traversed when it
                                was stored. (ie top/down left/right etc.)
                                Internally we use 0100. (start lower left) */
   double lat2, lon2;        /* lat,lon position of last grid point.
                                (0 if unused) */
/* Specific to Lambert Conformal grids. */
   double scaleLat1, scaleLat2; /* The tangent latitude.  If different:
                                then the latitude where the scale should be
                                equal, which allows one to compute the correct
                                tangent latitude.  (0 for lat/lon and mercator,
                                90 for north polar stereographic). */
   double southLat, southLon; /* Not needed. 0 except lambert.
                                 (and rotated lat/lon) */
   /* Following is for stretched Lat/Lon grids. */
   double poleLat, poleLon;   /* Pole of stretching. */
   double stretchFactor;      /* Factor of stretching. */
   int f_typeLatLon;          /* 0 regular, 1 stretch, 2 stretch / rotate. */
   double angleRotate;        /* Rotation angle. */  
} gdsType;

typedef struct {
   uChar projType;           /* Projection type / template type. Valid choices
                                0(lat/lon), 10(mercator), 20(Polar Stereo),
                                30(Lambert Conformal),
                                in future maybe 90, 110, 120. */
   double majEarth, minEarth; /* semi major and minor axis of earth in km. */
} gdsType2;

enum { GS5_SIMPLE = 0, GS5_CMPLX = 2, GS5_CMPLXSEC = 3, GS5_SPECTRAL = 50,
   GS5_HARMONIC = 41, GS5_JPEG2000 = 40000, GS5_PNG = 40010
};
typedef struct {
   sInt4 packType;           /* What kind of packing was used. */
   float refVal;             /* The refrence value for the grid, also the
                              * minimum value? */
   short int ESF;            /* Power of 2 scaling factor. */
   short int DSF;            /* Decimal Scale Factor */
   uChar fieldType;          /* 0 data was float, 1 if int. */
   uChar f_maxmin;           /* boolean, if min/max are valid*/
   double min, max;          /* Min / Max values in data. */
   uChar missManage;         /* Missing value management
                                0 none, 1 primary, 2 primary & secondary. */
   double missPrim, missSec; /* primary and secondary missing values. */
   long int numMissing;      /* Number of missing values detected,
                                both primary and secondary. */
} gridAttribType;

typedef struct {
   uChar GribVersion;        /* 1 if GRIB1, 2 if GRIB2 */
   char wfo[5];              /* Name of wfo that generated the data. */
   pdsG1Type pds1;           /* Product Definition section for GRIB1. */
   pdsG2Type pds2;           /* Product Definition section for GRIB2. */
   gdsType gds;              /* Grid Definition section (sect3 for GRIB2) */
   gridAttribType gridAttrib; /* Attributes of the Grid (min/max values etc) */
   char *element;            /* A short char look up of variable type. */
   char *comment;            /* A more descriptive look up of variable type. */
   char *unitName;           /* Unit if not the one specified in the GRIB2
                              * document, otherwise NULL. */
   int convert;              /* Enum type of unit conversions (meta.h),
                                Conversion method for this variable's unit. */
   char *shortFstLevel;      /* Short description of the level of this data
                                (above ground) (500 mb), etc */
   char *longFstLevel;       /* Long description of the level of this data
                                (above ground) (500 mb), etc */
   char refTime[20];         /* When forecast was issued. */
   char validTime[20];       /* When forecast is valid. */
   long int deltTime;        /* validTime - refTime in seconds. */

/*  int *size_wx; */         /* (idat[0] + 2) * sizeof (int) */
/*  char **release_datetime; *//* pds2.refTime + pds2.cutOffHour */
} grib_MetaData;

void MetaInit (grib_MetaData *meta);

void MetaSect2Free (grib_MetaData * meta);

void MetaFree (grib_MetaData *meta);

int ParseTime (time_t * AnsTime, int year, uChar mon, uChar day, uChar hour,
               uChar min, uChar sec);

int ParseSect4Time2sec (long int time, int unit, double *ans);

/* Possible error messages left in errSprintf() */
int MetaParse (grib_MetaData * meta, long int *is0, long int ns0,
               long int *is1, long int ns1, long int *is2, long int ns2,
               float *rdat, long int nrdat, long int *idat, long int nidat,
               long int *is3, long int ns3, long int *is4, long int ns4,
               long int *is5, long int ns5, long int grib_len,
               float xmissp, float xmisss, int simpVer);

void ParseGrid (gridAttribType * attrib, double **Grib_Data,
                long int *grib_DataLen, long int Nx, long int Ny, int scan,
                long int *iain, long int ibitmap, long int *ib, double unitM,
                double unitB, uChar f_wxType, sect2_WxType * WxType,
                uChar f_subGrid, int startX, int startY, int stopX, int stopY);

/* Possible error messages left in errSprintf() */
/* int MetaPrintGDS (gdsType * gds, char **ans); */

/* Possible error messages left in errSprintf() */
int MetaPrint (grib_MetaData *meta, char **ans, sChar decimal, sChar f_unit);

#endif
