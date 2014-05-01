/***********************************************************************
* Filename: find_holes.c
*
* Original Author: Feng Ding
*
* File Creation Date: July 2006
*
* Development Group: OHD / HSMB
*
* Description:
*  Find the holes and determine the filling data during the coversion between 
*  quarterly HRAP and radar polar coordinate systems. 
* 
* This file is converted from FORTRAN file a31342.f in ORPG system. 
*
* Modules:
* find_holes
*
***********************************************************************/

/* Include files, definitions, globals go here. */

#include "decode_radar_product.h"


/***********************************************************************
* Module Name: find_holes
*
* Original Author: Feng Ding
*
* Module Creation Date: 
* 
* Description:
*   This function for finding the holes and determine the filling data 
*   during the coversion between quarterly HRAP and radar polar coordinate systems.
*
*   calling function: build_lookup_table 
*   functions called: quarterhrap_to_az_range
*
* Calling Arguments:
* Name         Input/Output Type          Description
* 
* LS           Input        const double  radar latitude
* LAMDA        Input        const double  radar longitude
*
* Required
* None
*
* Required Files/Databases:
* None
*
* Non System Routines Called:
* quarterhrap_to_az_range
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
 *  Below is the head part of FORTRAN code to be converted from. It's kept for 
*  understanding all variables in the equations. 
*
       SUBROUTINE A31342__FIND_HOLES(LS,LAMDAS)
*.********************************************************************
*.                    M O D U L E  P R O L O G U E
*.
*.  MODULE NAME: A31342__FIND_HOLES
*.
*.  MODULE VERSION: 0006
*.
*.  MODULE LANGUAGE: FORTRAN
*.
*.  CALLING SEQUENCE: CALL A31342__FIND_HOLES(LS,LAMDA)
*.
*.  MODULE FUNCTION:
*.
*.     INITIALIZE POLAR TO LFM CONVERSION TABLES
*.
*.  MODULES CALLED: A31343__LFM_TO_AZRAN 
*.
*.  PARAMETERS:          (*:  G = GLOBAL, C = COMMON, P = PASSED)
*.
*.    *   INPUT        TYPE        DESCRIPTION
*.    -   -----        ----        -----------
*.    G   LFM16FLAG    I*2         FLAG ARRAY FOR 1/16 LFM GRID. FLAG ARRAY
*.                                 USED TO
C ; MARK COVERAGE AREA DOMAIN,
*.                                 IDENTIFY MAPPING HOLES, IDENTIFY DATA USED
*.                                 TO FILL MAPPING HOLES
*.    G   LFM40FLAG    I*2         FLAG ARRAY FOR 1/40 LFM GRID, USED TO MARK
*.                                 COVERAGE AREA DOMAIN, ID MAPPING HOLES AND
*.                                 ID DATA USED TO FILL MAPPING HOLES
*.    G   LFM4FLAG     I*2         FLAG ARRAY FOR 1/4 LFM GRID, USED TO MARK
*.                                 COVERAGE AREA DOMAIN, ID MAPPING HOLES AND
*.                                 ID DATA USED TO FILL MAPPING HOLES
*.    G   BEYOND_RANGE I*4         (CONSTANT)Flag value for points beyond 230
*.                                 km. in the 1/4 LFM Rate scan grid.
*.    G   FLAG_AZ      I*4         (CONSTANT)OFFSET IN THE LFMxxFLAG ARRAY TO
*.                                 AN AZIMUTH INDEX USED TO FILL MAPPING HOLES
*.    G   FLAG_RNG     I*4         (CONSTANT)OFFSET IN THE LFMxxFLAG ARRAY TO
*.                                 A RANGE INDEX USED TO: MARK THE COVERAGE
*.                                 AREA DOMAIN, IDENTIFY MAPPING HOLES, AND TO
*.                                 FILL MAPPING HOLES
*.    G   HYZ_LFM4     I*4         (CONSTANT)Dimension of the 1/4 LFM rate
*.                                 scan grid.
*.    G   HYZ_LFM40    I*4         (CONSTANT)Sizing parameter. Number of boxes
*.                                 along 1/40 LFM grid side.
*.    G   LFM16_IDX    I*4         (CONSTANT)INDEX TO 1/16 LFM CONVERSION
*.                                 TABLE
*.    G   NUM_LFM16    I*4         (CONSTANT)TOTAL NUMBER OF GRID BOXES IN THE
*.                                 1/16 LFM GRID
*.    G   NUM_LFM4     I*4         (CONSTANT)TOTAL NUMBER OF GRID BOXES IN THE
*.                                 1/4 LFM GRID
*.    G   NUM_LFM40    I*4         (CONSTANT)TOTAL NUMBER OF GRID BOXES IN THE
*.                                 1/40 LFM GRID
*.    C   HYZ_LFM16    I*4         (CONSTANT)NUMBER OF 1/16 LFM BOXES ALONG A
*.                                 SIDE OF THE LFM GRID (100)
*.    C   LFM40_IDX    I*4         (CONSTANT)INDEX TO 1/40 LFM CONVERSION
*.                                 TABLE
*.    C   LFM4_IDX     I*4         (CONSTANT)INDEX TO 1/4 LFM CONVERSION TABLE
*.    P   LAMDAS       R*8         FLOATING POINT SITE LONGITUDE VALUE
*.    P   LS           R*8         FLOATING POINT SITE LATITUDE VALUE
*.
*.    *   OUTPUT    TYPE        DESCRIPTION
*.    -   ------    ----        -----------
*.    G   LFM16FLAG I*2         FLAG ARRAY FOR 1/16 LFM GRID. FLAG ARRAY USED
*.                              TO
C ; MARK COVERAGE AREA DOMAIN, IDENTIFY
*.                              MAPPING HOLES, IDENTIFY DATA USED TO FILL
*.                              MAPPING HOLES
*.    G   LFM40FLAG I*2         FLAG ARRAY FOR 1/40 LFM GRID, USED TO MARK
*.                              COVERAGE AREA DOMAIN, ID MAPPING HOLES AND ID
*.                              DATA USED TO FILL MAPPING HOLES
*.    G   LFM4FLAG  I*2         FLAG ARRAY FOR 1/4 LFM GRID, USED TO MARK
*.                              COVERAGE AREA DOMAIN, ID MAPPING HOLES AND ID
*.                              DATA USED TO FILL MAPPING HOLES
*.
*.    *   ACTUAL ARGUMENTS  TYPE        DESCRIPTION
*.    -   ----------------  ----        -----------
*.    P   LAMDAS            R*8         FLOATING POINT SITE LONGITUDE VALUE
*.    P   LS                R*8         FLOATING POINT SITE LATITUDE VALUE
*.
*.  DATABASE/FILE REFERENCE:  None
*.
*.  INTERNAL TABLES/WORK AREA:
*.
*.    NAME      TYPE        DESCRIPTION
*.    ----      ----        -----------
*.    AZ_IJ     R*8         AZIMUTH ANGLE TO THE CENTER OF AN LFM BOX
*.    AZ_RND    R*8         FACTOR USED TO CONVERT AZIMUTH ANGLE TO AZIMUTH
*.                          INDEX OF BIN CLOSEST TO GRID HOLE
*.    I         I*4         LOOP CONTROL FOR ABSOLUTE LFM BOX NUMBER
*.    LFM16_RNG R*8         (CONSTANT) LFM16 RANGE VALUE OF 460.0
*.    LFM40_RNG R*8         (CONSTANT) LFM40 RANGE VALUE OF 230.0
*.    LFM4_RNG  R*8         (CONSTANT) LFM4 RANGE VALUE OF 230.0
*.    lfm_i     I*4         LFM I COORDINATE (I INCREASES TO THE RIGHT)
*.    lfm_j     I*4         LFM J COORDINATE (J INCREASES DOWNWARD)
*.    ONE       I*4         CONSTANT REPRESENTING THE VALUE 1
*.    RG_IJ     R*8         RANGE TO THE CENTER OF AN LFM BOX
*.    RG_RND    R*8         FACTOR USED TO CONVERT RANGE TO RANGE BIN INDEX OF
*.                          BIN CLOSEST TO GRID HOLE
*.
*.  GLOBAL BLOCKS REFERENCED:
*.
*.    A314C1
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

void find_holes(const double LS, const double LAMDA)
{
    double RG_IJ, AZ_IJ, AZ_RND, RG_RND;
    int    lfm_i, lfm_j, ONE, count;
    int    ihrap, jhrap;

    AZ_RND = 1.05;
    RG_RND = 2.0;
    ONE = 1;

    count = 0;

    /*
     * search for holes in the 1/160 lfm lookup table and identify the
     * az/ran of the data to be used to fill in the hole
     */

    for(ihrap = 0; ihrap < MAX_IHRAP; ihrap ++)
    {
        for (jhrap = 0; jhrap < MAX_JHRAP; jhrap ++)
        {
            if( (quarter_hrap_to_radar_azimuth[ihrap][jhrap] == BEYOND_GRID) &&
                (quarter_hrap_to_radar_range[ihrap][jhrap] == BEYOND_GRID) )
            {
                lfm_i = ihrap + 1;
                lfm_j = jhrap + 1;
                quarterhrap_to_az_range(LS, LAMDA, lfm_i, lfm_j,
                                &AZ_IJ, &RG_IJ);            
       
                /*
                *  if the range is within the product coverage area requirement then
                *  store azimuth and range index into the lfm40flag lookup table
                */ 
                   
                if(RG_IJ < MAX_RANGE) 
                {
                    count ++;
                    quarter_hrap_to_radar_azimuth[ihrap][jhrap]
                        = (int)(AZ_IJ + AZ_RND);
                    quarter_hrap_to_radar_range[ihrap][jhrap]
                        = (int)(RG_IJ) + ONE;
                }
                else
                {
                    quarter_hrap_to_radar_azimuth[ihrap][jhrap]
                        = BEYOND_RANGE;
                    quarter_hrap_to_radar_range[ihrap][jhrap]
                        = BEYOND_RANGE;            
                }
            }
        }
    }

}
