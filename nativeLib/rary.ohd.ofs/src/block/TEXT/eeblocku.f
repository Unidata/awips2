C MODULE EEBLOCKU
C-----------------------------------------------------------------------
C
C                             LAST UPDATE: 02/24/94.12:12:54 BY $WC20SV
C
C @PROCESS LVL(77)
C
      BLOCK DATA EUNBLK
C
C  INITIALIZE EXTENDED STREAMFLOW PREDICTION SYSTEM COMMON BLOCKS
C
C      INCLUDE 'common/eunit'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68                RCSKW1,RCSKW2
      COMMON / RCSEEBLOCKU      / RCSKW1,RCSKW2
      DATA                        RCSKW1,RCSKW2 /                      '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/block/RCS/eeblocku.f,v $
     . $',                                                             '
     .$Id: eeblocku.f,v 1.1 1995/09/17 18:41:09 dws Exp $
     . $' /
C    ===================================================================
C
C
C      DATA KEPARM/96/
C      DATA KEPERM/91,92,93,94,95/
C      DATA KESCRA/90/
C      DATA KESTBL/97/
C
      END
