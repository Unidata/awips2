/***********************************************************************
* Filename: quarterhrap_to_az_range.c
*
* Original Author: Feng Ding
*
* File Creation Date: July 2006
*
* Development Group: OHD / HSMB
*
* Description:
*  Compute the radar azimuth angle and range from a quarterly HRPA grid 
*  for coversion between quarterly HRAP and radar polar coordinate systems. 
* 
* This file is converted from FORTRAN file a31343.f in ORPG system. 
*
* Modules:
* quarterhrap_to_az_range
*
***********************************************************************/

/* Include files, definitions, globals go here. */

#include "decode_radar_product.h"


/***********************************************************************
* Module Name: quarterhrap_to_az_range
*
* Original Author: Feng Ding
*
* Module Creation Date: 
* 
* Description:
*   This function for computing the radar azimuth angle and range from 
*   a quarterly HRPA grid for coversion between quarterly HRAP and radar
*   polar coordinate systems.
*
*   calling function: find_holes 
*   functions called: 
*
* Calling Arguments:
* Name         Input/Output Type          Description
* 
* LS           Input        const double  radar latitude
* LAMDA        Input        const double  radar longitude
* LFM_I        Input        const int     quarter HRAP_I
* LFM_J        Input        const int     quarter HRAP_J
* THETA_CIJ    Input        double *      azimuth
* RG_IJ        Input        double *      range
*
* Required
* None
*
* Required Files/Databases:
* None
*
* Non System Routines Called:
* 
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


/*
      SUBROUTINE A31343__LFM_TO_AZRAN(LS,LAMDAS,LFMSIZE,LFM_I,LFM_J,
     $ THETA_CIJ,RG_IJ)
*.********************************************************************
*.                    M O D U L E  P R O L O G U E
*.
*.  MODULE NAME: A31343__LFM_TO_AZRAN
*.
*.  MODULE VERSION: 0006
*.
*.  MODULE LANGUAGE: FORTRAN
*.
*.  CHANGE HISTORY:
*.
*.       DATE          VERSION   PROGRAMMER           NOTES
*.       ----------    -------   ----------------     ---------------
*.       10/13/92      0000       BRADLEY SUTKER      CCR# NA92-28001
*.       03/25/93      0001      Toolset              SPR NA93-06801
*.       01/28/94      0002      Toolset              SPR NA94-01101
*.       03/03/94      0003      Toolset              SPR NA94-05501
*.       04/11/96      0004      Toolset              CCR NA95-11802
*.       12/23/96      0005      Toolset              CCR NA95-11807
*.       03/16/99      0006      Toolset              CCR NA98-23803
*.
*.  CALLING SEQUENCE:  CALL A31343__LFM_TO_AZRAN(LS,LAMDAS,LFMSIZE,
*.     LFM_I,LFM_J,THETA_CIJ,RG_IJ)
*.
*.  MODULE FUNCTION:
*.
*.     INITIALIZE POLAR TO LFM CONVERSION TABLES
*.
*.  MODULES CALLED: NONE.
*.
*.  PARAMETERS:          (*:  G = GLOBAL, C = COMMON, P = PASSED)
*.
*.    *   INPUT     TYPE        DESCRIPTION
*.    -   -----     ----        -----------
*.    G   CONST     R*8         (CONSTANT)CONSTANT (135.0) AS USED IN B5
*.                              SECTION 30.6 - 3.4.1 (SIN_S) AND 4.1 (RG_IJ)
*.    G   GRID_COORD_I        R*8         (CONSTANT)GLOBAL GRID I COORDINATE OF NORTH
*.                              POLE IN UNITS OF 1/4 LFM GRID BOXES
*.    G   GRID_COORD_J        R*8         (CONSTANT)GLOBAL GRID J COORDINATE OF NORTH
*.                              POLE IN UNITS OF 1/4 LFM GRID BOXES
*.    G   LFM16_IDX I*4         (CONSTANT)INDEX TO 1/16 LFM CONVERSION TABLE
*.    G   LFMMX_IDX I*4         (CONSTANT)MAXIMUM NUMBER OF LFM GRID SIZES
*.                              (1/4, 1/16 AND 1/40) THAT THE LFM CONVERSION
*.                              MODULES ARE DESIGNED FOR
*.    G   PRIME     R*8         (CONSTANT)PRIME MERIDIAN LONGITUDE (NEGATIVE)
*.    G   R2KO      R*8         (CONSTANT)CONSTANT 2RKo AS USED IN B5 SECTION
*.                              30.6 - 3.2
*.    G   EARTH_RADIUS   R*8         (CONSTANT)EARTH RADIUS CONSTANT USED TO
*.                              CONVERT X/Y TO LAT/LONG (DIFFERENT FROM 6371
*.                              TO ACCOUNT FOR REFRACTION)
*.    C   LFM40_IDX I*4         (CONSTANT)INDEX TO 1/40 LFM CONVERSION TABLE
*.    C   LFM4_IDX  I*4         (CONSTANT)INDEX TO 1/4 LFM CONVERSION TABLE
*.    P   LAMDAS    R*8         FLOATING POINT SITE LONGITUDE VALUE
*.    P   LFMSIZE   I*4         VARIABLE INDICATING THE LFM GRID SIZE (1=1/4,
*.                              2=1/16, 3=1/40)
*.    P   LFM_I     I*4         LFM I COORDINATE (I INCREASES TO THE RIGHT)
*.    P   LFM_J     I*4         LFM J COORDINATE (J INCREASES DOWNWARD)
*.    P   LS        R*8         FLOATING POINT SITE LATITUDE VALUE
*.    P   THETA_CIJ R*8         RANGE TO THE CENTER OF AN LFM BOX
*.
*.    *   OUTPUT    TYPE        DESCRIPTION
*.    -   ------    ----        -----------
*.    P   RG_IJ     R*8         RANGE TO THE CENTER OF AN LFM BOX
*.    P   THETA_CIJ R*8         RANGE TO THE CENTER OF AN LFM BOX
*.
*.    *   ACTUAL ARGUMENTS  TYPE        DESCRIPTION
*.    -   ----------------  ----        -----------
*.
*.  DATABASE/FILE REFERENCE:  None
*.
*.  INTERNAL TABLES/WORK AREA:
*.
*.    NAME             TYPE        DESCRIPTION
*.    ----             ----        -----------
*.    AA               R*8         VARIABLE A AS DEFINED IN B5 SECTION 30.6 -
*.                                 4.1
*.    AI               R*8         VARIABLE AI AS DEFINED IN B5 SECTION 30.6 -
*.                                 4.1
*.    AJ               R*8         VARIABLE AJ AS DEFINED IN B5 SECTION 30.6 -
*.                                 4.1
*.    ANGLE_THRESH     R*8         SIN_SS THRESHOLD (AS USED IN B5 SECTION
*.                                 30.6 - 4.1) BELOW WHICH THETA_CIJ IS SET TO
*.                                 ZERO
*.    BB               R*8         VARIABLE B AS DEFINED IN B5 SECTION 30.6 -
*.                                 4.1
*.    B_CON            R*8         CONSTANT B(m) AS DEFINED IN B5 SECTION 30.6
*.                                 - 4.1
*.    CII              R*8         LFM BOX I COORDINATE (RELATIVE TO POLE) AS
*.                                 DEFINED IN B5 SECTION 30.6 - 4.1
*.    CJJ              R*8         LFM BOX I COORDINATE (RELATIVE TO POLE) AS
*.                                 DEFINED IN B5 SECTION 30.6 - 4.1
*.    COS_LAMDAS_PRIME R*8         COSINE OF THE ANGLE (SITE LAMDA MINUS THE
*.                                 PRIME MERIDIAN)
*.    COS_LIJ          R*8         COSINE OF THE LATITUDE OF AN LFM BOX CENTER
*.    COS_LS           R*8         COSINE OF THE SITE LATITUDE (L)
*.    COS_SS           R*8         COSINE OF ANGLE S AS DEFINED IN B5 SECTION
*.                                 30.6 - 4.1
*.    DTR              R*8         DEGREES TO RADIANS CONVERSION FACTOR
*.    FIRST_TIME       L*4         VARIABLE INDICATING (IF TRUE) THAT THE
*.                                 MODULE IS CALLED FOR THE FIRST TIME SINCE
*.                                 TASK LOADING
*.    GIS              R*8         GLOBAL GRID I COORDINATE OF THE RADAR SITE
*.                                 IN UNITS OF 1/4 LFM BOXES (POSITIVE TO THE
*.                                 RIGHT)
*.    GJS              R*8         GRID J COORDINATE OF THE RADAR SITE IN
*.                                 UNITS OF 1/4 LFM BOXES (POSITIVE TO THE
*.                                 RIGHT)
*.    HALF             R*8         PARAMETER EQUAL TO 0.5
*.    I                I*4         LOOP CONTROL FOR ABSOLUTE LFM BOX NUMBER
*.    IKA              I*4         INTEGER VERSION OF CONSTANT KA(m) AS
*.                                 DEFINED IN B5 SECTION 30.6 - 3.4.1.2
*.    IS               I*4         GLOBAL GRID BOX I COORDINATE FOR BOX 0,0 OF
*.                                 1/4, 1/16, AND 1/40 LOCAL LFM GRIDS.  THIS
*.                                 IS VARIABLE 'IS' AS DEFINED IN B5 SECTION
*.                                 30.6 - 3.2.1
*.    JS               I*4         GLOBAL GRID BOX I COORDINATE FOR BOX 0.0 OF
*.                                 1/4, 1/16, AND 1/40 LOCAL LFM GRIDS.  THIS
*.                                 IS VARIABLE 'IS' AS DEFINED IN B5 SECTION
*.                                 30.6 - 3.2.1
*.    KA               R*8         FLOATING POINT VERSION OF CONSTANT KA(m) AS
*.                                 DEFINED IN B5 SECTION 30.6 - 3.4.1.2
*.    LAMDA_IJ         R*8         LONGITUDE OF THE CENTER OF A GIVEN LFM BOX
*.    L_IJ             R*8         LATITUDE OF THE CENTER OF A GIVEN LFM BOX
*.    NINTY            R*8         CONSTANT REPRESENTING THE VALUE 90.0
*.    OFFSET           I*4         OFFSET NUMBER OF LFM xxx GRID BOXES USED TO
*.                                 COMPUTE GRID BOX NUMBER FOR BOX 0,0 OF
*.                                 LOCAL GRID.
*.    ONE              R*8         CONSTANT REPRESENTING THE VALUE 1
*.    PRE_GXS          R*8         PART OF THE EQUATION COMMON TO THE
*.                                 CALCULATIONS FOR GIS AND GJS AS DEFINED IN
*.                                 B5 SECTION 30.6 - 3.2
*.    R_360            R*8         CONSTANT REPRESENTING THE VALUE 360.0
*.    SIN_DLAMDA       R*8         SINE OF DELTA LAMDA (I.E., DELTA LONGITUDE)
*.    SIN_LAMDAS_PRIME R*8         SINE OF THE ANGLE (SITE LAMDA MINUS THE
*.                                 PRIME MERIDIAN)
*.    SIN_LIJ          R*8         SINE OF THE LATITUDE OF AN LFM BOX CENTER
*.    SIN_LS           R*8         SINE OF THE SITE LATITUDE (L)
*.    SIN_SS           R*8         SINE OF ANGLE S AS DEFINED IN B5 SECTION
*.                                 30.6 - 4.1
*.    TWO              R*8         CONSTANT REPRESENTING THE VALUE 2.0
*.    ZERO             R*8         CONSTANT REPRESENTING THE VALUE 0.0
*.
*.  GLOBAL BLOCKS REFERENCED:
*.
*.
*.  COMMON BLOCKS REFERENCED:
*.
*.
*.  ERROR CONDITIONS:  NONE
*.
*.  ASSUMPTIONS/RESTRICTIONS:  None
*.
*.  DEVIATION FROM STANDARDS:  None
*.
*.  COMPILATION INSTRUCTIONS:
*.
*.     THIS MODULE IS COMPILED USING THE COMP13.CSS
*.
*.  LINKAGE INSTRUCTIONS:
*.
*.     THIS MODULE IS LINKED USING THE LINK13.CSS
*.
*.  MISC:  None
*.
*.*******************************************************************
*/

void quarterhrap_to_az_range(const double LS, const double LAMDAS, 
                             const int LFM_I, const int LFM_J,
                             double *THETA_CIJ, double *RG_IJ)
{
    double PRE_GXS, GIS, GJS, DTR, ONE;
    double COS_LAMDAS_PRIME, SIN_LAMDAS_PRIME, SIN_DLAMDA;
    double HALF, AI, AJ;
    double CII, CJJ, SIN_LS, COS_LS;
    double AA, BB, COS_SS, SIN_SS, COS_LIJ, SIN_LIJ, R_360;
    double LAMDA_IJ, L_IJ, NINTY, TWO, ZERO;
    int   IS, JS;

    R_360 = 360.0;
    NINTY = 90.0;
    ZERO = 0.0;
    TWO = 2.0;
    HALF = 0.5;
    ONE = 1.0;
    DTR = 0.01745329;
    COS_LS = 0.0;
    SIN_LS = 0.0;

    COS_LAMDAS_PRIME = cos((LAMDAS + PRIME) * DTR);
    SIN_LAMDAS_PRIME = sin((LAMDAS + PRIME) * DTR);
    SIN_LS = sin(LS * DTR);
    COS_LS = cos(LS * DTR);

    /*
     * COMPUTE COMMON PART OF THE GIS AND GJS EQUATIONS
     */

    PRE_GXS = R2KO * COS_LS/(ONE + SIN_LS);

    /*
     * COMPUTE REGERENCE GRID BOX COORDINATES
     */

    GIS = PRE_GXS * SIN_LAMDAS_PRIME + GRID_COORD_I;
    GJS = PRE_GXS * COS_LAMDAS_PRIME + GRID_COORD_J;

    /*
     * COMPUTE GRID BOX NUMBERS FOR BOX 0,0 OF LOCAL GRIDS
     */
     
    IS = (int)(KA * GIS) - OFFSET;
    JS = (int)(KA * GJS) - OFFSET;
     
    /*
     * COMPUTE AI AND AJ CONSTANTS
     */

    AI = (IS - GRID_COORD_I * KA + HALF)/KA;
    AJ = (JS - GRID_COORD_J * KA + HALF)/KA;

    CII = AI + B_CON * LFM_I;
    CJJ = AJ + B_CON * LFM_J;
    L_IJ = DTR * NINTY - TWO * atan(sqrt(CII * CII + CJJ * CJJ) / R2KO);

    /*
     * IF BOTH INPUTS TO DATAN2 ARE 0, DONT CALL FUNCTION
     */

    if ( (CII == ZERO) && (CJJ == ZERO) ) 
    {
         LAMDA_IJ = ZERO;
    } 

    /*
     * OTHERWISE COMPUTE LAMDA_IJ
     */

    else
    {
         LAMDA_IJ = -PRIME * DTR + atan2(CII, CJJ);
    } 

    /*
     * COMPUTE INTERMEDIATE VALUES
     */

    COS_LIJ = cos(L_IJ);
    SIN_LIJ = sin(L_IJ);
    SIN_DLAMDA = sin(LAMDA_IJ - DTR * LAMDAS);
    AA = COS_LIJ * SIN_DLAMDA;
    BB = COS_LS * SIN_LIJ - SIN_LS * COS_LIJ * cos(LAMDA_IJ - DTR * LAMDAS);
    SIN_SS = sqrt(AA * AA + BB * BB);
    COS_SS = sqrt(ONE - SIN_SS * SIN_SS);

    /*
     * COMPUTE RANGE
     */

    *RG_IJ = (CONST * SIN_SS + EARTH_RADIUS) * SIN_SS;

    /*
     * IF SIN_SS IS GREATER THAN A SMALL POSITIVE NUMBER
     * COMPUTE THETA_CIJ
     */

    if (SIN_SS >= ANGLE_THRESH) 
    {
       *THETA_CIJ = atan2(COS_LIJ * COS_LS * SIN_DLAMDA,
                         (SIN_LIJ - SIN_LS * COS_SS));
    } 

    /*
     * OTHERWISE, SET THETA_CIJ TO 0
     */

    else
    {
       *THETA_CIJ = ZERO;
    } 

    *THETA_CIJ = *THETA_CIJ / DTR;

    /*
     * IF ANGLE IS LESS THAN 0 ... ADD 360 DEGREES
     */

    if (*THETA_CIJ < ZERO) 
    {
        *THETA_CIJ = *THETA_CIJ + R_360;
    }

}
