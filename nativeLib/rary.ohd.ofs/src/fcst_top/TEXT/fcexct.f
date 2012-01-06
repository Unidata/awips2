C MODULE FCEXCT
C-----------------------------------------------------------------------
C
C  ROUTINE TO CHECK THE MINIMUM TIME INTERVAL FOR A SEGMENT.
C
      SUBROUTINE FCEXCT (MINDT,IER)
C
      CHARACTER*8 OLDOPN
C
      INCLUDE 'common/ionum'
      INCLUDE 'common/where'
      INCLUDE 'common/fdbug'
      INCLUDE 'common/fctime'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_top/RCS/fcexct.f,v $
     . $',                                                             '
     .$Id: fcexct.f,v 1.3 2002/02/11 19:20:58 dws Exp $
     . $' /
C    ===================================================================
C
C
      IF (ITRACE.GE.1) WRITE (IODBUG,*) 'ENTER FCEXCT'
C
      IOPNUM=0
      CALL FSTWHR ('FCEXCT  ',IOPNUM,OLDOPN,IOLDOP)
C
      IER=0
C
      IF (MOD(IHRRUN,MINDT).EQ.0) GO TO 80
      WRITE (IPR,20) 'START'
20    FORMAT ('0**WARNING** MINDT NOT COMPATIBLE WITH ',A,' HOUR.')
      CALL WARN
C
30    IF (IHRRUN.GT.24) GO TO 60
C
      IF (MOD(IHRRUN,MINDT).EQ.0) GO TO 40
         IHRRUN=IHRRUN+1
         IF (IHRRUN.LE.24) GO TO 30
         IHRRUN=1
         IDARUN=IDARUN+1
         GO TO 30
40    WRITE (IPR,50) IHRRUN
50    FORMAT ('0START HOUR CHANGED TO ',I2)
      IER=2
      GO TO 80
C
60    WRITE (IPR,70) MINDT,'START'
70    FORMAT ('0**ERROR** CANNOT FIND A ',A,' HOUR COMPATIBLE ',
     *   'WITH MINDT OF ',I2,' HOURS.')
      CALL ERROR
      IER=1
      GO TO 130
C
80    IF (MOD(LHRRUN,MINDT).EQ.0) GO TO 130
      WRITE (IPR,20) 'STOP'
      CALL WARN
C
90    IF (LHRRUN.LE.0) GO TO 120
      IF (MOD(LHRRUN,MINDT).EQ.0) GO TO 100
         LHRRUN=LHRRUN-1
         IF (LHRRUN.GT.0) GO TO 90
         LHRRUN=24
         LDARUN=LDARUN-1
         GO TO 90
100   WRITE (IPR,110) LHRRUN
110   FORMAT ('0STOP HOUR CHANGED TO ',I2)
      IER=2
      GO TO 130
C
120   WRITE (IPR,70) MINDT,'STOP'
      CALL ERROR
      IER=1
      GO TO 170
C
C  CHECK THAT START HOUR IS BEFORE STOP HOUR
130   CALL FDATCK (IDARUN,IHRRUN,LDARUN,LHRRUN,4HLT  ,ICHECK)
      IF (ICHECK.EQ.1) GO TO 170
      CALL FDATCK (IDARUN,IHRRUN,LDARUN,LHRRUN,4HEQ  ,ICHECK)
      IF (ICHECK.EQ.1) WRITE (IPR,140)
140   FORMAT ('0**ERROR** START DATE AND END DATE ARE EQUAL')
      IF (ICHECK.NE.1) WRITE (IPR,150)
150   FORMAT ('0**ERROR** START DATE IS AFTER END DATE.')
      CALL KILLPM
      IER=1
C
170   CALL FSTWHR (OLDOPN,IOLDOP,OLDOPN,IOLDOP)
C
      IF (ITRACE.GE.1) WRITE (IODBUG,*) 'EXIT FCEXCT'
C
      RETURN
C
      END
