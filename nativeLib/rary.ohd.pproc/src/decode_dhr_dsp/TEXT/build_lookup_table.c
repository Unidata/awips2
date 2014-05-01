/***********************************************************************
* Filename: build_lookup_table.c
*
* Original Author: Feng Ding
*
* File Creation Date: July 2006
*
* Development Group: OHD / HSMB
*
* Description:
*  Build lookup table for coversion between quarterly HRAP and 
*  radar polar coordinate systems. 
* 
* This file is converted from FORTRAN file a31341.f in ORPG system. 
*
* Modules:
* build_lookup_table
*
***********************************************************************/

/* Include files, definitions, globals go here. */

#include "decode_radar_product.h"

/***********************************************************************
* Module Name: build_lookup_table
*
* Original Author: Feng Ding
*
* Module Creation Date: 
* 
* Description:
*   This function for building lookup table for coversion between quarterly 
*   HRAP and radar polar coordinate systems.
*
*   calling function: write_decoded_dhr, write_decoded_dsp 
*   functions called: find_holes
*
* Calling Arguments:
* Name         Input/Output Type          Description
* 
* rad_lat      Input        const int     radar latitude
* rad_lon      Input        const int     radar longitude
*
* Required
* None
*
* Required Files/Databases:
* None
*
* Non System Routines Called:
* find_holes
*
* Return Value:
* Type          Description
* None
*
* Error Codes/Exceptions:
* 
*
* OS Specific Assumptions:
* None
*
* Local Variables:
* Name     Type       Description
*
* Modification History:
* Date        Developer     Action
*             Feng Ding     Build research version
* 7/19/2006   Guoxian Zhou  Build operational version 
*
***********************************************************************/

 
/* Below is the head part of FORTRAN code to be converted from. It's kept for 
*  understanding all variables in the equations. 
*
       SUBROUTINE A31341__BUILD_LFM_LOOKUP(SIRDALAT, SIRDALON)
*.********************************************************************
*.                    M O D U L E  P R O L O G U E
*.
*.  MODULE NAME: A31341__BUILD_LFM_LOOKUP
*.
*.  MODULE VERSION: 0006
*.
*.  MODULE LANGUAGE: FORTRAN
*.
*.  CALLING SEQUENCE: CALL A31341__BUILD_LFM_LOOKUP(  )
*.
*.  MODULE FUNCTION:
*.
*.     INITIALIZE POLAR TO LFM CONVERSION TABLES
*.
*.  MODULES CALLED: A31342__FIND_HOLES
*.
*.  PARAMETERS:          (*:  G = GLOBAL, C = COMMON, P = PASSED)
*.
*.    *   INPUT         TYPE        DESCRIPTION
*.    -   -----         ----        -----------
*.    C  SIRDALAT      I*4         Site Adaptation - RDA Latitude.
*.    C  SIRDALON      I*4         Site Adaptation - RDA Longitude.
*.    G  BEYOND_GRID   I*4         (CONSTANT)FLAG INDICATING THAT THE AZIMUTH
*.                                  AND RANGE OF A GIVEN BIN IS OUTSIDE THE
*.                                  LOCAL LFM GRID DOMAIN
*.    G  BEYOND_RANGE  I*4         (CONSTANT)Flag value for points beyond 230
*.                                  km. in the 1/4 LFM Rate scan grid.
*.    G  CONST         R*8         (CONSTANT)CONSTANT (135.0) AS USED IN B5
*.                                  SECTION 30.6 - 3.4.1 (SIN_S) AND 4.1
*.                                  (RG_IJ)
*.    G  FLAG_RNG      I*4         (CONSTANT)OFFSET IN THE LFMxxFLAG ARRAY TO
*.                                  A RANGE INDEX USED TO: MARK THE COVERAGE
*.                                  AREA DOMAIN, IDENTIFY MAPPING HOLES, AND
*.                                  TO FILL MAPPING HOLES
*.    G  HYZ_LFM4      I*4         (CONSTANT)Dimension of the 1/4 LFM rate
*.                                  scan grid.
*.    G  HYZ_LFM40     I*4         (CONSTANT)Sizing parameter. Number of
*.                                  boxes along 1/40 LFM grid side.
*.    G  GRID_COORD_I  R*8         (CONSTANT)GLOBAL GRID I COORDINATE OF
*.                                  NORTH POLE IN UNITS OF 1/4 LFM GRID BOXES
*.    G  GRID_COORD_J  R*8         (CONSTANT)GLOBAL GRID J COORDINATE OF
*.                                  NORTH POLE IN UNITS OF 1/4 LFM GRID BOXES
*.    G  LFM16_IDX     I*4         (CONSTANT)INDEX TO 1/16 LFM CONVERSION
*.                                  TABLE
*.    G  LFMMX_IDX     I*4         (CONSTANT)MAXIMUM NUMBER OF LFM GRID SIZES
*.                                  (1/4, 1/16 AND 1/40) THAT THE LFM
*.                                  CONVERSION MODULES ARE DESIGNED FOR
*.    G  MAX_AZIMUTH    I*4         (CONSTANT)Maximum number of azimuths in a
*.                                  scan (index into output buffer of adjusted
*.                                  values).
*.    G  MAX_RANGE    I*4         (CONSTANT)Maximum number of bins along a
*.                                  radial in the rate scan.
*.    G  NUM_LFM16     I*4         (CONSTANT)TOTAL NUMBER OF GRID BOXES IN
*.                                  THE 1/16 LFM GRID
*.    G  NUM_LFM4      I*4         (CONSTANT)TOTAL NUMBER OF GRID BOXES IN
*.                                  THE 1/4 LFM GRID
*.    G  NUM_LFM40     I*4         (CONSTANT)TOTAL NUMBER OF GRID BOXES IN
*.                                  THE 1/40 LFM GRID
*.    G  PRIME         R*8         (CONSTANT)PRIME MERIDIAN LONGITUDE
*.                                  (NEGATIVE)
*.    G  R2KO          R*8         (CONSTANT)CONSTANT 2RKo AS USED IN B5
*.                                  SECTION 30.6 - 3.2
*.    G  EARTH_RADIUS       R*8    (CONSTANT)EARTH RADIUS CONSTANT USED TO
*.                                  CONVERT X/Y TO LAT/LONG (DIFFERENT FROM
*.                                  6371 TO ACCOUNT FOR REFRACTION)
*.    G  EARTH_RADIUS_SQ    R*8    (CONSTANT)SQUARE OF THE EARTH RADIUS
*.                                  CONSTANT
*.    G  RNG_LFM16     I*4         (CONSTANT)THE NUMBER OF RANGE BINS PER
*.                                  AZIMUTH CONTAINED IN THE INPUT DATA FOR
*.                                  THE 1/16 LFM CONVERSION PROCESS
*.    G  WITHIN_RANGE  I*4         (CONSTANT)Parameter defining "within
*.                                  range" flag value (0) for flag table
*.                                  LFMFLAG.
*.    G  HYZ_LFM16     I*4         (CONSTANT)NUMBER OF 1/16 LFM BOXES ALONG A
*.                                  SIDE OF THE LFM GRID (100)
*.    G  LFM40_IDX     I*4         (CONSTANT)INDEX TO 1/40 LFM CONVERSION
*.                                  TABLE
*.    G  LFM4_IDX      I*4         (CONSTANT)INDEX TO 1/4 LFM CONVERSION
*.                                  TABLE
*.
*.    *   OUTPUT        TYPE        DESCRIPTION
*.    -   ------        ----        -----------
*.    C  LFM16FLAG     I*2         FLAG ARRAY FOR 1/16 LFM GRID. FLAG ARRAY
*.                                  USED TO MARK COVERAGE AREA DOMAIN,
*.                                  IDENTIFY MAPPING HOLES, IDENTIFY DATA USED
*.                                  TO FILL MAPPING HOLES
*.    C  LFM16GRID     I*2         LOOKUP TABLE TO CONVERT COMP. REFL. POLAR
*.                                  GRID DATA TO 1/16 LFM COORDS. EACH ENTRY
*.                                  CONTAINS 1/16 LFM I & J COORDS. FOR GIVEN
*.                                  RANGE & AZIMUTH INDEX
*.    C  LFM40FLAG     I*2         FLAG ARRAY FOR 1/40 LFM GRID, USED TO MARK
*.                                  COVERAGE AREA DOMAIN, ID MAPPING HOLES AND
*.                                  ID DATA USED TO FILL MAPPING HOLES
*.    C  LFM40GRID     I*2         LOOKUP TABLE CONVERTS HYDROMET POLAR GRID
*.                                  DATA TO 1/40 LFM COORDS. EACH ENTRY
*.                                  CONTAINS 1/40 LFM I & J COORD FOR GIVEN
*.                                  RANGE & AZIMUTH INDEX
*.    C  LFM4FLAG      I*2         FLAG ARRAY FOR 1/4 LFM GRID, USED TO MARK
*.                                  COVERAGE AREA DOMAIN, ID MAPPING HOLES AND
*.                                  ID DATA USED TO FILL MAPPING HOLES
*.    C  LFM4GRID      I*2         LOOKUP TABLE TO CONVERT HYDROMET POLAR
*.                                  GRID DATA TO 1/4 LFM I & J COORD. FOR A
*.                                  GIVEN RANGE & AZIMUTH INDEX
*.
*.  DATABASE/FILE REFERENCE:  None
*.
*.  INTERNAL TABLES/WORK AREA:
*.
*.    NAME             TYPE        DESCRIPTION
*.    ----             ----        -----------
*.    bear_angle       R*8         BEARING (I.E., AZIMUTH) ANGLE
*.    cos_bearing      R*8         COSINE OF A GIVEN BEARING ANGLE
*.    cos_delta_lamda  R*8         COSINE OF DELTA LAMDA (DELTA LONGITUDE)
*.    cos_lat          R*8         COSINE OF LATITUDE (L) FOR A GIVEN
*.                                 RANGE/BEARING CELL
*.    cos_lamdas_prime R*8         COSINE OF THE ANGLE (SITE LAMDA MINUS THE
*.                                 PRIME MERIDIAN)
*.    COS_LS           R*8         COSINE OF THE SITE LATITUDE (L)
*.    cos_angle_s      R*8         COSINE OF ANGLE S AS DEFINED IN B5 SECTION
*.                                 30.6 - 3.4.1
*.    DEV_NUM          I*4         PROGRAM DEVICE NUMBER
*.    DEGREE_TO_RADIAN R*8         DEGREES TO RADIANS CONVERSION FACTOR
*.    FILLER           I*4         DUMMY VARIABLE USED IN CALL TO A3CM08
*.    FULL_NAME        CHAR*24     COMPLETE IMAGE FILE NAME
*.    GI               R*8         GLOBAL GRID I COORDINATE OF A GIVEN RANGE
*.                                 AND BEARING CELL IN UNITS OF 1/4 LFM BOXES
*.                                 (POSITIVE TO RIGHT)
*.    GIS              R*8         GLOBAL GRID I COORDINATE OF THE RADAR SITE
*.                                 IN UNITS OF 1/4 LFM BOXES (POSITIVE TO THE
*.                                 RIGHT)
*.    GJ               R*8         GLOBAL GRID J COORDINATE OF THE RADAR SITE
*.                                 IN UNITS OF 1/4 LFM BOXES (POSITIVE DOWN)
*.    GJS              R*8         GRID J COORDINATE OF THE RADAR SITE IN
*.                                 UNITS OF 1/4 LFM BOXES (POSITIVE TO THE
*.                                 RIGHT)
*.    HALF             R*8         PARAMETER EQUAL TO 0.5
*.    IAZ              I*4         AZIMUTH INDEX OF DATA TO BE USED TO FILL A
*.                                 MAPPING HOLE
*.    IBOX             I*4         LOOP CONTROL FOR ABSOLUTE LFM BOX NUMBER
*.    IKA              I*4         INTEGER VERSION OF CONSTANT KA(m) AS
*.                                 DEFINED IN B5 SECTION 30.6 - 3.4.1.2
*.    IMAGE_NAME       CHAR*12     BASE NAME OF IMAGE FILE
*.    INITIALIZED      L*4         GOT PROGRAM DEVICE NUMBER FLAG
*.    IRG              I*4         RANGE INDEX OF DATA TO BE USED TO FILL A
*.                                 MAPPING HOLE
*.    site_i           I*4         GLOBAL GRID BOX I COORDINATE FOR BOX 0,0 OF
*.                                 1/4, 1/16, AND 1/40 LOCAL LFM GRIDS.  THIS
*.                                 IS VARIABLE 'IS' AS DEFINED IN B5 SECTION
*.                                 30.6 - 3.2.1
*.    ISTAT            I*4         I/O STATUS
*.    site_j           I*4         GLOBAL GRID BOX I COORDINATE FOR BOX 0.0 OF
*.                                 1/4, 1/16, AND 1/40 LOCAL LFM GRIDS.  THIS
*.                                 IS VARIABLE 'IS' AS DEFINED IN B5 SECTION
*.                                 30.6 - 3.2.1
*.    KA               R*8         FLOATING POINT VERSION OF CONSTANT KA(m) AS
*.                                 DEFINED IN B5 SECTION 30.6 - 3.4.1.2
*.    LAMDAS           R*8         FLOATING POINT SITE LONGITUDE VALUE
*.    LFMBOX           I*4         ABSOLUTE LFM BOX NUMBER (COMBINED I AND J
*.                                 COORDINATES)
*.    lfm_i            I*4         LFM I COORDINATE (I INCREASES TO THE RIGHT)
*.    lfm_j            I*4         LFM J COORDINATE (J INCREASES DOWNWARD)
*.    LS               R*8         FLOATING POINT SITE LATITUDE VALUE
*.    LU               I*4         I/O LOGICAL UNIT
*.    OFFSET           I*4         OFFSET NUMBER OF LFM xxx GRID BOXES USED TO
*.                                 COMPUTE GRID BOX NUMBER FOR BOX 0,0 OF
*.                                 LOCAL GRID.
*.    ONE              R*8         CONSTANT REPRESENTING THE VALUE 1
*.    PBLK             I*4         PARAMETER BLOCK
*.    PRE_GXS          R*8         PART OF THE EQUATION COMMON TO THE
*.                                 CALCULATIONS FOR GIS AND GJS AS DEFINED IN
*.                                 B5 SECTION 30.6 - 3.2
*.    range            R*8         RANGE TO A GIVEN BEARING ANGLE
*.    RL               R*8         VARIABLE AS DEFINED IN B5 SECTION
*.                                 30.6-3.4.1
*.    RNG_INC          R*8         THE SIZE IN KM OF THE RANGE RESOLUTION OF
*.                                 THE HYDROMET POLAR GRID INPUT DATA
*.    SCALE_FACTOR     R*8         CONSTANT USED TO CONVERT ADAPTATION DATA
*.                                 VALUES OF RADAR SITE LATITUDE/LONGITUDE TO
*.                                 FLOATING POINT
*.    sin_bearing      R*8         SINE OF A GIVEN BEARING ANGLE
*.    sin_delta_lamda  R*8         SINE OF DELTA LAMDA (I.E., DELTA LONGITUDE)
*.    sin_lat          R*8         SINE OF LATITUDE (L) FOR A GIVEN
*.                                 RANGE/BEARING CELL
*.    sin_lamdas_prime R*8         SINE OF THE ANGLE (SITE LAMDA MINUS THE
*.                                 PRIME MERIDIAN)
*.    SIN_LS           R*8         SINE OF THE SITE LATITUDE (L)
*.    sin_angle_s      R*8         SINE OF ANGLE S AS DEFINED IN B5 SECTION
*.                                 30.6 - 3.4.1
*.    START_SEC        I*4         START POINT OF I/O TRANSFER
*.    TABYTES          I*4         NUMBER OF BYTES TO TRANSFER WHEN WRITING
*.                                 THE NEW POLAR TO LFM CONVERSION TABLE TO
*.                                 DISK
*.    WW               I*4         WRITE-WAIT I/0 SVC7 INSTRUCTION
*.    YES              L*4         Logical parameter indicating "true".
*.
*.  ERROR CONDITIONS:  NONE
*.
*.  ASSUMPTIONS/RESTRICTIONS:  None
*.
*.  DEVIATION FROM STANDARDS:  None
*.
*.  MISC:  None
*.
*.******************************************************************* 
*/

int radar_to_quarter_hrap_i[MAX_RANGE][MAX_AZIMUTH];
int radar_to_quarter_hrap_j[MAX_RANGE][MAX_AZIMUTH];

int quarter_hrap_to_radar_azimuth[MAX_IHRAP][MAX_JHRAP];
int quarter_hrap_to_radar_range[MAX_IHRAP][MAX_JHRAP];

void build_lookup_table(const int rad_lat, const int rad_lon)
{

    const double ONE = 1.0;
    const double HALF = 0.5;

    const double DEGREE_TO_RADIAN = 0.01745329;
    const double SCALE_FACTOR = 1000.0;
    const double EARTH_RADIUS_SQ = EARTH_RADIUS * EARTH_RADIUS;

    double  dbl_radar_lat, dbl_radar_lon;
    double  pre_gxs, site_grid_i, site_grid_j;
    double  grid_i, grid_j, rl, cos_delta_lamda, sin_delta_lamda, sin_angle_s, cos_angle_s;
    double  sin_site_lat, cos_site_lat, sin_lat, cos_lat, sin_bearing, cos_bearing;
    double  bear_angle, range, cos_lambas_prime, sin_lamdas_prime;
    int     site_i, site_j, iaz, irg;
    long    lfm_i, lfm_j;
    int     ihrap,  jhrap;

    /*
     * convert adaptation data lat/long to double precision data
     */

    dbl_radar_lat = rad_lat / SCALE_FACTOR;
    dbl_radar_lon = rad_lon / SCALE_FACTOR;

    /*
     * re-initialize tables before starting lookup table generation
     */

    for(iaz = 0; iaz < MAX_AZIMUTH; iaz++)
    {
        for (irg = 0; irg < MAX_RANGE; irg++)
        {
            radar_to_quarter_hrap_i[irg][iaz] = BEYOND_GRID;
            radar_to_quarter_hrap_j[irg][iaz] = BEYOND_GRID;
        }
    }   
            
    for(ihrap = 0; ihrap < MAX_IHRAP; ihrap++)
    {
        for (jhrap = 0; jhrap < MAX_JHRAP; jhrap++)
        {
            quarter_hrap_to_radar_azimuth[ihrap][jhrap] = BEYOND_GRID;
            quarter_hrap_to_radar_range[ihrap][jhrap] = BEYOND_GRID;
        }
    }   
  
    /*
     * compute parts of equations used multiple times later
     */

    cos_lambas_prime = cos((dbl_radar_lon + PRIME) * DEGREE_TO_RADIAN);
    sin_lamdas_prime = sin((dbl_radar_lon + PRIME) * DEGREE_TO_RADIAN);
    sin_site_lat = sin(dbl_radar_lat * DEGREE_TO_RADIAN);
    cos_site_lat = cos(dbl_radar_lat * DEGREE_TO_RADIAN);

    /*
     * compute common part of the gis and gjs equations
     */

    pre_gxs = R2KO * cos_site_lat / (ONE + sin_site_lat);

    /*
     * compute reference grid box coordinates
     */

    site_grid_i = pre_gxs * sin_lamdas_prime + GRID_COORD_I;
    site_grid_j = pre_gxs * cos_lambas_prime + GRID_COORD_J;

    /*
     * compute grid box numbers for box 0,0 of local grids
     */

    site_i = (int)(KA * site_grid_i) - OFFSET;
    site_j = (int)(KA * site_grid_j) - OFFSET;

    /*
     * initialize bearing
     */

    bear_angle = -HALF;

    /*
     * DO FOR ALL BEARINGS
     */

    for(iaz = 1; iaz <= MAX_AZIMUTH; iaz++) 
    {
        bear_angle += ONE;
        sin_bearing = sin(bear_angle * DEGREE_TO_RADIAN);
        cos_bearing = cos(bear_angle * DEGREE_TO_RADIAN);

        /*
         * initialize range
         */

        range = -HALF;
       
        /*
         * do for each input data range values (hydro application next)
         */

        for (irg = 1; irg <= MAX_RANGE; irg++) 
        {
            range = range + ONE;          
            sin_angle_s = (range / EARTH_RADIUS) * 
                          (ONE - (CONST * range / EARTH_RADIUS_SQ));
            cos_angle_s = sqrt(ONE - sin_angle_s * sin_angle_s);
            sin_lat = sin_site_lat * cos_angle_s+cos_site_lat
                      * sin_angle_s * cos_bearing;
            cos_lat = sqrt(ONE - sin_lat * sin_lat);
            sin_delta_lamda = sin_angle_s * sin_bearing/cos_lat;
            cos_delta_lamda = sqrt(ONE - sin_delta_lamda * sin_delta_lamda);
            rl = R2KO * cos_lat/(ONE + sin_lat);
            grid_i = rl * (sin_delta_lamda * cos_lambas_prime +
                 cos_delta_lamda * sin_lamdas_prime)+GRID_COORD_I;
            grid_j = rl * (cos_delta_lamda * cos_lambas_prime -
                 sin_delta_lamda * sin_lamdas_prime)+GRID_COORD_J;

            /*
             * compute 1/160 lfm i and j coordinates of the range/azimuth bin
             */

            lfm_i = (int)(grid_i * KA) - site_i;
            lfm_j = (int)(grid_j * KA) - site_j;

            /*
             * if lfm coordinates are within local grid save it into the lookup
             * table and set the 1/160 lfm box range number table to within range
             */

            if ((lfm_i > 0) && (lfm_i <= MAX_IHRAP) &&
                (lfm_j > 0) && (lfm_j <= MAX_JHRAP) )
            {
                radar_to_quarter_hrap_i[irg-1][iaz-1] = lfm_i;
                radar_to_quarter_hrap_j[irg-1][iaz-1] = lfm_j;

                quarter_hrap_to_radar_azimuth[lfm_i-1][lfm_j-1] = iaz;
                quarter_hrap_to_radar_range[lfm_i-1][lfm_j-1] = irg;

            }
        }
    }

    /*
     * call find_holes() to find holes and determine hole filling data
     */
 
     find_holes(dbl_radar_lat, dbl_radar_lon);

}
