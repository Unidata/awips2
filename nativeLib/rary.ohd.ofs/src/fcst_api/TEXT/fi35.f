C MEMBER FI35
C  (from old member FCEX35)
C-----------------------------------------------------------------------
C
C                             LAST UPDATE: 04/12/95.09:42:44 BY $WC20SV
C
      FUNCTION FI35(CV,SAPI,SAEI)
C
C#######################################################################
C
C  THIS FUNCTION SUBPROGRAM DETERMINES THE FINAL INDEX (FI)
C  FROM THE GIVEN VALUES OF SAPI AND SAEI.
C
C#######################################################################
C
      COMMON /FDBUG/ IODBUG,ITRACE,IDBALL,NDEBUG,IDEBUG(20)
      COMMON /IONUM/ IN,IPR,IPU
      DIMENSION CV(72)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_api/RCS/fi35.f,v $
     . $',                                                             '
     .$Id: fi35.f,v 1.1 1995/09/17 18:57:50 dws Exp $
     . $' /
C    ===================================================================
C
C
C  CHECK AEI OFF LOW END OF CURVE (HIGH END IS DO-LOOP EXIT)
C
      J=1
      X=CV(1)
      IF(SAEI.LE.X)GO TO 40
C
C  FIND AEI IN CURVES (AEI IS IN PSNS 1-12 OF CV)
C
      DO 20 J=2,12
        X=CV(J)
        IF(SAEI.EQ.X)GO TO 40
        IF(SAEI.LT.X)GO TO 60
20    CONTINUE
      J=12
C
C  AEI IS EQUAL TO (OR BEING SET TO) VALUE "J" ON "KURV"
C
40    IF(SAPI.GT.1.95)GO TO 50
      FI35=CV(J+12)-SQRT(CV(J+36)**2.0-(CV(J+24)-SAPI)**2.0)
      GO TO 90
50    FI35=((SAPI-CV(J+48))/CV(J+60))*(-1.0)
      GO TO 90
60    RATIO=(SAEI-CV(J-1))/(X-CV(J-1))
      IF(SAPI.GT.1.95)GO TO 80
      XC=RATIO*(CV(J+12)-CV(J+11))+CV(J+11)
      YC=RATIO*(CV(J+24)-CV(J+23))+CV(J+23)
      R=RATIO*(CV(J+36)-CV(J+35))+CV(J+35)
      FI35=XC-SQRT(R**2.0-(YC-SAPI)**2.0)
      GO TO 90
80    CINT=RATIO*(CV(J+48)-CV(J+47))+CV(J+47)
      CDEV=RATIO*(CV(J+60)-CV(J+59))+CV(J+59)
      FI35=((SAPI-CINT)/CDEV)*(-1.0)
90    IF(FI35.LT.0.50)FI35=0.50
      RETURN
      END
