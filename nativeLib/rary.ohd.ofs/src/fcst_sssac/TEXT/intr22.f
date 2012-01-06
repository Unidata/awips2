C MODULE FCINT22
C **********************************************************************
C **********************************************************************
       SUBROUTINE INTR22 (IMO,IDAY,VAL,RESULT)
C
C                             LAST UPDATE: 01/19/94.12:20:22 BY $WC20SV
C
C...for debugging:
       COMMON /FDBUG/IODBUG,ITRACE,IDBALL,NDEBUG,IDEBUG(20)
C
       INTEGER IMO, IDAY
       INTEGER NDAYS(12)
       REAL VAL(12), RESULT
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_sssac/RCS/intr22.f,v $
     . $',                                                             '
     .$Id: intr22.f,v 1.2 2002/05/15 13:54:03 hank Exp $
     . $' /
C    ===================================================================
C
       DATA NDAYS/31,28,31,30,31,30,31,31,30,31,30,31/
C
       IF (ITRACE .GT. 1) WRITE (IODBUG,990)
990    FORMAT(/10X,'** INTR22 ENTERED.')
C
       IF (IDAY .NE. 16) GOTO 801
           RESULT = VAL(IMO)
           IF (ITRACE .GT. 1) WRITE (IODBUG,991)
991        FORMAT(/10X,'** EXIT INTR22.')
           RETURN
801    CONTINUE
C
       IF (IDAY .GT. 16) GOTO 802
           IMO2 = IMO
           IMO1 = IMO-1
           IF (IMO1 .EQ. 0) IMO1 = 12
802    CONTINUE
       IF (IDAY .LT. 16) GOTO 803
           IMO2 = IMO+1
           IF (IMO2 .EQ. 13) IMO2 = 1
           IMO1 = IMO
803    CONTINUE
C
       NDAY2 = 16
       NDAY1 = NDAYS(IMO1) - 16
       NDAYT = NDAY1 + NDAY2
       IF (IDAY .LT. 16) NDAYC = IDAY + NDAY1
       IF (IDAY .GE. 16) NDAYC = IDAY -16
C
       PERCNT = FLOAT(NDAYC)/FLOAT(NDAYT)
C
       RESULT = ( (VAL(IMO2)-VAL(IMO1))*PERCNT ) + VAL(IMO1)
C
       IF (ITRACE .GT. 1) WRITE (IODBUG,991)
C
       RETURN
       END
