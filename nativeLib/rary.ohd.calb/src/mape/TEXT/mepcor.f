C MODULE MEPCOR
C-----------------------------------------------------------------------
C
      SUBROUTINE MEPCOR (NSTA,IFMON,ICORR,MXPCOR,IERROR)
C
C  ROUTINE TO READ POTENTIAL EVAPORATION CORRECTIONS
C
      INCLUDE 'uiox'
      COMMON /MESTAX/ NWSST(25),TIMEOB(25,11),TPEC(25,11),
     *   IOBCGE(25,10),ITCGE(25,10),SCORF(25,11)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/calb/src/mape/RCS/mepcor.f,v $
     . $',                                                             '
     .$Id: mepcor.f,v 1.2 2001/06/13 08:48:38 mgm Exp $
     . $' /
C    ===================================================================
C
      DATA BLANK/4H    /
C
C
      DO 10 IRG=1,NSTA
         TPEC(IRG,1)=1.0
         DO 10 I=1,MXPCOR
            ITCGE(IRG,I)=9999
            SCORF(IRG,I)=BLANK
10       CONTINUE
C
      IF (ICORR.EQ.0) GO TO 50
C
      NMPCOR=0
C
20    READ (ICD,60) JSTA,JMO,JYR,PEC,SEAS
C
      IF (JSTA.GT.98) GO TO 50
C
      IF (NMPCOR+1.GT.MXPCOR) THEN
         CALL UEROR (LP,0,-1)
         WRITE (LP,70) MXPCOR
         IERROR=1
         GO TO 50
         ENDIF
C
      NMPCOR=NMPCOR+1
C
      ICMON=JYR*12+JMO
      ICMON=ICMON-IFMON+1
C
      DO 30 I=1,MXPCOR
         IF (ITCGE(JSTA,I).LT.9999) GO TO 30
C        SET ITCGE EQUAL TO THE MONTH THE EVAPORATION ADJUSTMENT BEGINS
            ITCGE(JSTA,I)=ICMON
            J=I
            GO TO 40
30       CONTINUE
C
C   SET TPEC EQUAL TO THE ADJUSTMENT TO BE APPLIED,
C   AND SET SCORF EQUAL TO THE INDICATOR OF THE
C   SEASON TO WHICH THE ADJUSTMENT APPLIES
40    TPEC(JSTA,(J+1))=PEC
      SCORF(JSTA,(J+1))=SEAS
      GO TO 20
C
50    RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
60    FORMAT (3I5,F5.0,1X,A4)
70    FORMAT ('0*** ERROR - MAXIMUM NUMBER OF ',
     *   'CORRECTION FACTORS (',I3,') EXCEEDED.')
C
      END
