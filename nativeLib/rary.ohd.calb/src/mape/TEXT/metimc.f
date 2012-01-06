C MODULE METIMC
C-----------------------------------------------------------------------
C
      SUBROUTINE METIMC (NSTA,IFMON,IMO,IYR,MXTIMC,IERROR)
C
C  ROUTINE TO READ OBSERVATION TIME CHANGES IN ORDER BY TIME
C
      INCLUDE 'uiox'
      COMMON /MESTAX/ NWSST(25),TIMEOB(25,11),TPEC(25,11),
     *   IOBCGE(25,10),ITCGE(25,10),SCORF(25,11)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/calb/src/mape/RCS/metimc.f,v $
     . $',                                                             '
     .$Id: metimc.f,v 1.2 2001/06/13 08:49:40 mgm Exp $
     . $' /
C    ===================================================================
C
C
      DO 10 IRG=1,NSTA
         DO 10 I=1,MXTIMC
            IOBCGE(IRG,I)=9999
10       CONTINUE
C
      NMTIMC=0
C
20    READ (ICD,60) JSTA,JMO,JYR,OBNEW
C
      IF (JSTA.GT.98) GO TO 50
C
      IF (NMTIMC+1.GT.MXTIMC) THEN
         CALL UEROR (LP,0,-1)
         WRITE (LP,70) MXTIMC
         IERROR=1
         GO TO 50
         ENDIF
C
      NMTIMC=NMTIMC+1
C
      ICMON=JYR*12+JMO
      ICMON=ICMON-IFMON+1
C
      DO 30 I=1,MXTIMC
         IF (IOBCGE(JSTA,I).LT.9999) GO TO 30
C        SET IOBCGE EQUAL TO THE MONTH IN WHICH THE OBSERVATION
C        TIME CHANGE OCCURRED
            IOBCGE(JSTA,I)=ICMON
            J=I
            GO TO 40
30       CONTINUE
C
C  SET TIMEOB EQUAL TO THE NEW OBSERVATION TIME
40    TIMEOB(JSTA,(J+1))=OBNEW
      GO TO 20
C
50    RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
60    FORMAT (3I5,2F5.0)
70    FORMAT ('0*** ERROR - MAXIMUM NUMBER OF ',
     *   'OBSERVATION TIME CHANGES (',I3,') EXCEEDED.')
C
      END
