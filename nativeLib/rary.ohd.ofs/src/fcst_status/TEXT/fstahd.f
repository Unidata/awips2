C$PRAGMA C (DATIM2)
C MODULE FSTAHD
C-----------------------------------------------------------------------
C
C  ROUINTE TO PRINT PAGE HEADER FOR FORECAST COMPONENT STATUS REPORT.
C
      SUBROUTINE FSTAHD (ISTART)
C
C  ISTART=1 RESETS PAGE COUNTER TO 1
C
C  ROUTINE ORIGINALLY WRITTEN BY --
C    ED JOHNSON - HRL - 10 NOV 1979
C
      CHARACTER*28  DATE2
C
      INCLUDE 'common/fdbug'
      INCLUDE 'common/ionum'
      INCLUDE 'common/fcrfc'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_status/RCS/fstahd.f,v $
     . $',                                                             '
     .$Id: fstahd.f,v 1.3 2002/02/11 20:27:13 dws Exp $
     . $' /
C    ===================================================================
C
      DATA IPAGE/0/
C
C
      IF (ITRACE.GE.1) WRITE (IODBUG,*) 'ENTER FSTAHD'
C
      IF (ISTART.EQ.1) IPAGE=0
C
C  GET CURRENT DATE AND TIME
      CALL DATIM2 (DATE2)
C
      IPAGE=IPAGE+1
C
      WRITE (IPR,10) RFCNAM,DATE2(2:24),IPAGE
10    FORMAT ('1NWSRFS FORECAST SYSTEM - ',
     *   'FORECAST COMPONENT STATUS REPORT',
     *   T79,'USER=',2A4,
     *   T94,'DATE=',A,
     *   T125,'PAGE=',I4)
C
      RETURN
C
      END
