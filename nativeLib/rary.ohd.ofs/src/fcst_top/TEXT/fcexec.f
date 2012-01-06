C$PRAGMA C (DEL_ALL_FILES)
C MODULE FCEXEC
C-----------------------------------------------------------------------
C
C  MAIN ROUTINE FOR EXECUTION OF FORECAST FUNCTION.
C
      SUBROUTINE FCEXEC
C
C   ROUTINE ORIGINALLY WRITTEN BY - GEORGE SMITH - HRL - 3/1980
C
      CHARACTER*8 RTNNAM,OLDOPN
C
      INCLUDE 'common/fc'
      INCLUDE 'common/fd'
      INCLUDE 'common/fp'
      INCLUDE 'common/ft'
      INCLUDE 'common/fts'
      INCLUDE 'common/fdbug'
      INCLUDE 'common/where'
      INCLUDE 'common/killcd'
      INCLUDE 'common/fcpuck'
      COMMON /FUN5CB/ IFIRST,ICPUF5
      COMMON /OUTCTL/ IOUTYP
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_top/RCS/fcexec.f,v $
     . $',                                                             '
     .$Id: fcexec.f,v 1.5 2002/02/11 20:29:36 dws Exp $
     . $' /
C    ===================================================================
C
C
      RTNNAM='FCEXEC'
C
      IOPNUM=0
      CALL FSTWHR (RTNNAM,IOPNUM,OLDOPN,IOLDOP)
C
      IF (ITRACE.GE.1) WRITE (IODBUG,*) 'ENTER ',RTNNAM
C
      ICKCPU=0
      IF (IFBUG('ICKCPU').EQ.1) ICKCPU=1
C
      IF (ICKCPU.EQ.1) THEN
         CALL URTIMR (LAPSE,ICPUT)  
         ELAPSE=LAPSE/100.
         XICPUT=ICPUT/100.
         WRITE (IODBUG,10) 'BEFORE CALL TO FAZE0',ELAPSE,XICPUT
10    FORMAT (' ',A,
     *   ' : ',
     *   ' ELAPSED CPU TIME = ',F6.2,3X,
     *   ' TOTAL CPU TIME = ',F6.2)
         ENDIF
C
C  FAZE0 INITIALIZES COMMON BLOCKS
      CALL FAZE0
C
      IF (ICKCPU.EQ.1) THEN
         CALL URTIMR (LAPSE,ICPUT)
         ELAPSE=LAPSE/100.
         XICPUT=ICPUT/100.
         WRITE (IODBUG,10) 'BEFORE CALL TO FAZE1',ELAPSE,XICPUT
	 ENDIF
C
C  FAZE1 SETS UP VALUES FOR RUN
      CALL FAZE1 (MD,D)
      IF (KLCODE.GT.4) GO TO 20
C
      IF (IOUTYP.EQ.0) THEN
C     DELETE OLD SAC AND SNOW MODEL STATE FILES
        CALL DEL_ALL_FILES
        ENDIF
C
      IF (ICKCPU.EQ.1) THEN
         CALL URTIMR (LAPSE,ICPUT)
         ELAPSE=LAPSE/100.
         XICPUT=ICPUT/100.
         WRITE (IODBUG,10) 'BEFORE CALL TO FAZE2',ELAPSE,XICPUT
         ENDIF
C
C  FAZE2 EXECUTES ONE SEGMENT AT A TIME
      CALL FAZE2 (MC,C,MD,D,MP,P,MT,T,MTS,TS)
      IF (KLCODE.GT.4) GO TO 20
C
      IF (ICKCPU.EQ.1) THEN
         CALL URTIMR (LAPSE,ICPUT)
         ELAPSE=LAPSE/100.
         XICPUT=ICPUT/100.
         WRITE (IODBUG,10) 'BEFORE CALL TO FAZE3',ELAPSE,XICPUT
	 ENDIF
C
C  FAZE3 DOES CLEANUP
      CALL FAZE3
C
      IF (ICKCPU.EQ.1) THEN
         CALL URTIMR (LAPSE,ICPUT)
         ELAPSE=LAPSE/100.
         XICPUT=ICPUT/100.
         WRITE (IODBUG,10) 'BEFORE EXIT',ELAPSE,XICPUT
         ENDIF
C
20    IF (ICKCPU.EQ.0) ICPUF5=ICPUT
C
      CALL FSTWHR (OLDOPN,IOLDOP,OLDOPN,IOLDOP)
C
      IF (ITRACE.GE.1) WRITE (IODBUG,*) 'EXIT ',RTNNAM
C
      RETURN
C
      END
