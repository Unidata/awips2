C     MODULE ROUT31
C
      SUBROUTINE ROUT31(IT,EXCESS,WE,AESC,STORGE,NEXLAG,EXLAG,PACKRO)
C.......................................
C     THIS ROUTINE ROUTES EXCESS WATER THROUGH THE SNOW COVER FOR
C        THE 'SNOW-43 ' OPERATION.
C.......................................
C     ROUTINE INITIALLY WRITTEN BY...
C        ERIC ANDERSON - HRL   MAY 1980 for SNOW-17
C        Modified by Mark Woodbury and Nalneesh Gaur 09/95
C.......................................
C    Arguments:
C
C    Argument       I/O         Description
C        IT          I          Precipitation interval.
C    EXCESS          I          Excess water in the pack
C        WE          I          Frozen water equivalent
C      aesc          I          Areal extent of snow-cover
C    storge         I/O         Amount of excess liquid water in storage
C    nexlag          I          number of values in exlag
C     exlag         I/O         Lagged excess liquid water (array)
C    PACKRO          O          Runoff from the pack.
C
C.......................................
C
      implicit none
C----- D E C L A R A T I O N S --------------------------
C     --- F U N C T I O N  A R G U M E N T S ---
      integer nexlag, it
      real    excess, we, aesc, storge, exlag, packro
C     --- L O C A L ---
      real    fit, cl, fn, fi, flag, term
      real    endl1, por2, por1
      real    el, els, wes, r1, os
      integer n, t1, t2, i, l1, l2
C----- D I M E N S I O N --------------------------------
      dimension exlag(7)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_snow43/RCS/rout31.f,v $
     . $',                                                             '
     .$Id: rout31.f,v 1.1 1996/05/07 11:09:38 page Exp $
     . $' /
C    ===================================================================
C
C
C.......................................
C     INITIAL VALUES
      FIT=IT
      PACKRO=0.0
      CL=0.03*FIT/6.0
C.......................................
C     LAG EXCESS WATER FIRST - FUNCTION OF EXCESS AND WE.
      IF(EXCESS.EQ.0.0) GO TO 150
      IF(EXCESS.LT.0.1) GO TO 120
      IF(WE.LT.1.0) GO TO 120
C
C     COMPUTE LAG IN HOURS AND PRORATE EXCESS.
      N=((EXCESS*4.0)**0.3)+0.5
      IF(N.EQ.0) N=1
      FN=N
      DO 110 I=1,N
      FI=I
      TERM=CL*WE*FN/(EXCESS*(FI-0.5))
      IF(TERM.GT.150.0) TERM=150.0
      FLAG=5.33*(1.0-EXP(-TERM))
      L2=(FLAG+FIT)/FIT+1.0
      L1=L2-1
      ENDL1=L1*IT
      POR2=(FLAG+FIT-ENDL1)/FIT
      POR1=1.0-POR2
      EXLAG(L2)=EXLAG(L2)+POR2*EXCESS/FN
      EXLAG(L1)=EXLAG(L1)+POR1*EXCESS/FN
  110 CONTINUE
      GO TO 150
C
C     EXCESS OR WE SMALL, THUS NO LAG.
  120 EXLAG(1)=EXLAG(1)+EXCESS
C.......................................
C     ATTENUATE LAGGED EXCESS WATER - FUNCTION OF STORGE AND WE.
  150 IF((STORGE+EXLAG(1)).EQ.0.0) GO TO 190
      IF((STORGE+EXLAG(1)).GE.0.1) GO TO 160
C
C     NO ATTENUATION
      PACKRO=STORGE+EXLAG(1)
      STORGE=0.0
      GO TO 190
C
C     EFFECT OF ATTENUATION COMPUTED USING A ONE-HOUR TIME STEP.
  160 EL=EXLAG(1)/FIT
      ELS=EL/(25.4*AESC)
      WES=WE/(25.4*AESC)
      TERM=500.0*ELS/(WES**1.3)
      IF(TERM.GT.150.0) TERM=150.0
      R1=1.0/(5.0*EXP(-TERM)+1.0)
      DO 170 I=1,IT
      OS=(STORGE+EL)*R1
      PACKRO=PACKRO+OS
      STORGE=STORGE+EL-OS
  170 CONTINUE
      IF(STORGE.GT.0.001) GO TO 190
      PACKRO=PACKRO+STORGE
      STORGE=0.0
C
C     DOWNSHIFT WATER IN EXLAG().
  190 DO 195 I=2,NEXLAG
      EXLAG(I-1)=EXLAG(I)
  195 CONTINUE
      EXLAG(NEXLAG)=0.0
C.......................................
      RETURN
      END
