C MEMBER UXPDBLOC
C-----------------------------------------------------------------------
C
C                             LAST UPDATE: 11/16/93.14:49:46 BY $WC20SV
C
C @PROCESS LVL(77)
C
      BLOCK DATA PDIBLK
C
C
C  INITIALIZATION ROUTINE TO INITIALIZE PREPROCESSOR DATA BASE
C  COMMON BLOCKS
C
C
      INCLUDE 'pdbcommon/pdsifc'
      INCLUDE 'pdbcommon/pddtdr'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68                RCSKW1,RCSKW2
      COMMON / RCSUXPDBLOC      / RCSKW1,RCSKW2
      DATA                        RCSKW1,RCSKW2 /                      '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/block/RCS/uxpdbloc.f,v $
     . $',                                                             '
     .$Id: uxpdbloc.f,v 1.1 1995/09/17 18:41:29 dws Exp $
     . $' /
C    ===================================================================
C
C
C
      DATA IDDTDR/24,2HPP,2H24,1,5,1,18*0,24,2HPP,2HVR,2,4,-1,18*0,
     *            24,2HTA,2HVR,2,3,-1,18*0,24,2HTM,2H24,3,5,2,18*0,
     *            24,2HMD,2HR6,3,0,4,18*0,24,2HTF,2H24,4,1,2,18*0,
     *            24,2HEA,2H24,4,1,6,18*0,24,2HPP,2HSR,4,28,3,18*0,
     *            24,2HPP,2HST,3,0,1,18*0,24,2HAP,2HIG,4,0,1,18*0,
     *            24,2HPG,2H24,1,0,1,18*0,
     *            24,2HTX,2H24,-4,0,1,18*0,24,2HTN,2H24,-4,1,1,18*0,
     *            24,2HTF,2HMN,-6,1,1,18*0,
     *            24,2HTF,2HMX,-6,0,1,18*0,
     *            24,2HPP,2H01,-2,1,24,18*0,
     *            24,2HPP,2H03,-2,3,8,18*0,
     *            24,2HPP,2H06,-2,6,4,18*0,
     *            24,2HTA,2H01,-3,1,24,18*0,
     *            24,2HTA,2H03,-3,3,8,18*0,
     *            24,2HTA,2H06,-3,6,4,18*0,
     *            24,2HTA,2H24,-7,0,1,18*0,24,2HTD,2H24,-7,1,1,18*0,
     *            24,2HUS,2H24,-7,2,1,18*0,24,2HRC,2H24,-7,3,1,18*0,
     *            24,2HRP,2H24,-7,4,1,18*0,24,2HRI,2H24,-7,5,1,18*0,
     *            72*0/
C
      DATA MXDTYP/30/
      DATA NMDTYP/27/
      DATA MAXDDF/5/
C
      END
