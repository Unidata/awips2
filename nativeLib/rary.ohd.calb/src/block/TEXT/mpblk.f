C MEMBER MPBLOCK
C-----------------------------------------------------------------------
C
C                             LAST UPDATE: 11/24/93.08:38:57 BY $WC20SV
C
C @PROCESS LVL(77)

C  INITIALIZATION ROUTINE FOR COMMON BLOCKS USED BY NWSRFS CALIBRATION
C  SYSTEM PROGRAM MAP3
C
      BLOCK DATA MPBLK
C
      COMMON /MAP/ DEBUG,DEBUGA
      COMMON /MAP1/ BASNID(5),GP(80)
      COMMON /MAP2/ LASTDA(2,12)
C
      INTEGER DEBUG/0/,DEBUGA/0/
      INTEGER GP
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68                RCSKW1,RCSKW2
      COMMON / RCSMPBLK         / RCSKW1,RCSKW2
      DATA                        RCSKW1,RCSKW2 /                      '
     .$Source: /fs/hseb/ob72/rfc/calb/src/block/RCS/mpblk.f,v $
     . $',                                                             '
     .$Id: mpblk.f,v 1.1 1997/01/23 15:14:26 dws Exp $
     . $' /
C    ===================================================================
C
C
      DATA LASTDA/31,31,28,29,31,31,30,30,31,31,30,30,31,31,31,
     *            31,30,30,31,31,30,30,31,31/
      DATA GP/80*0/
C
      END
