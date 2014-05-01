C$PRAGMA C (DATIM2)
C MODULE UPAGE2
C-----------------------------------------------------------------------
C
C  ROUTINE TO PRINT HEADER LINE AT TOP OF PAGE.
C
      SUBROUTINE UPAGE2 (NUNIT)
C
      CHARACTER*2   MONTH,DAY
      CHARACTER*4   YEAR,TIME
      CHARACTER*20  XSTR/' '/
      CHARACTER*12  SYSTM
      CHARACTER*28  DATE2
C
      INCLUDE 'uiox'
      INCLUDE 'ucmdbx'
      INCLUDE 'upagex'
      INCLUDE 'upvrsx'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/wfo_rfc/precip_proc/source/gribit/src/RCS/upage2.f,v $
     . $',                                                             '
     .$Id: upage2.f,v 1.1 2006/05/03 13:44:00 gsood Exp $
     . $' /
C    ===================================================================
C
C
      IUNIT=IABS(NUNIT)
C
      IF (ICMTRC.GT.0) THEN
         NPSNLN(ICMPRU)=NPSNLN(ICMPRU)+1
         WRITE (ICMPRU,60) NUNIT,IPSPAG(IUNIT),PUSRID
         ENDIF
C
C  CHECK IF PAGE HEADER TO BE PRINTED
      IF (IPSPAG(IUNIT).EQ.-1) GO TO 50
C
C  CHECK IF UNIT NUMBER IS SAME AS PUNCH UNIT
      IF (IUNIT.EQ.ICDPUN) THEN
C     CHECK IF PUNCH UNIT IS SAME AS PRINT UNIT
         IF (ICDPUN.NE.LP) GO TO 50
         ENDIF
C
      IPRERR=1
C
      DO 10 I=1,LEN(SYSTM)
         SYSTM(I:I)='?'
10       CONTINUE
C
C  CHECK SYSTEM
      IF (PGMSYS.EQ.'CALB') GO TO 20
      IF (PGMSYS.EQ.'FCST'.OR.PGMSYS.EQ.'RFS5') GO TO 20
      IF (PGMSYS.EQ.'????') GO TO 30
      IF (PGMSYS.EQ.'UTIL') THEN
CCC         XSTR=' O/H UTILITY'
         GO TO 30
         ENDIF
      IF (PGMSYS.EQ.'HADS') THEN
         XSTR(2:5)=PGMSYS(1:4)
         GO TO 30
         ENDIF
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  HEADER FOR NWSRFS
C
20    IF (PGMSYS.EQ.'CALB') SYSTM='CALIBRATION'
      IF (PGMSYS.EQ.'FCST'.OR.PGMSYS.EQ.'RFS5') SYSTM='FORECAST'
      CALL ULENTH (SYSTM,LEN(SYSTM),LSYSTM)
C
C  GET CURRENT DATE AND TIME
      CALL DATIM2 (DATE2)
C
      IF (IPSPAG(IUNIT).EQ.1) NPSPAG(IUNIT)=NPSPAG(IUNIT)+1
      IF (IPSPAG(IUNIT).EQ.0) NPSPAG(IUNIT)=-1
C
C  PRINT PAGE HEADER
      IF (PUSRID.NE.' '.AND.IPSPAG(IUNIT).EQ.1)
     *   WRITE (IUNIT,70) SYSTM(1:LSYSTM),PGMNAM,PGMVRN,PGMVRD,
     *      PUSRID,
     *      DATE2(2:24),
     *      NPSPAG(IUNIT)
      IF (PUSRID.NE.' '.AND.IPSPAG(IUNIT).EQ.0)
     *   WRITE (IUNIT,80) SYSTM(1:LSYSTM),PGMNAM,PGMVRN,PGMVRD,
     *      PUSRID,
     *      DATE2(2:24)
      IF (PUSRID.EQ.' '.AND.IPSPAG(IUNIT).EQ.1)
     *   WRITE (IUNIT,90) SYSTM(1:LSYSTM),PGMNAM,PGMVRN,PGMVRD,
     *      DATE2(2:24),
     *      NPSPAG(IUNIT)
      IF (PUSRID.EQ.' '.AND.IPSPAG(IUNIT).EQ.0)
     *   WRITE (IUNIT,100) SYSTM(1:LSYSTM),PGMNAM,PGMVRN,PGMVRD,
     *       DATE2(2:24)
      WRITE (IUNIT,150)
C
C  UPDATE LINE COUNTERS
      NPSNLN(IUNIT)=3
      NPSNLT(IUNIT)=NPSNLT(IUNIT)+3
      GO TO 40
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  GET CURRENT DATE AND TIME
30    CALL UDATC1 (MONTH,DAY,YEAR,TIME)
C
C  GET LENGTH OF STRING
      CALL ULENTH (XSTR,LEN(XSTR),LXSTR)
      IF (LXSTR.EQ.0) THEN
         LXSTR=1
         ELSE
            LXSTR=LXSTR+1
         ENDIF
C
C  PRINT PAGE HEADER AND UPDATE LINE COUNTERS
C
      IF (IPSPAG(IUNIT).EQ.0) THEN
         NPSPAG(IUNIT)=-1
         IF (PUSRID.EQ.' ')
     *      WRITE (IUNIT,110) XSTR(1:LXSTR),
     *          PGMNAM,PGMVRN,PGMVRD,MONTH,DAY,YEAR,TIME
         IF (PUSRID.NE.' ')
     *      WRITE (IUNIT,120) XSTR(1:LXSTR),
     *          PGMNAM,PGMVRN,PGMVRD,MONTH,DAY,YEAR,TIME,
     *          PUSRID
         NPSNLN(IUNIT)=1
         NPSNLT(IUNIT)=NPSNLT(IUNIT)+1
         ENDIF
C
      IF (IPSPAG(IUNIT).EQ.1) THEN
         NPSPAG(IUNIT)=NPSPAG(IUNIT)+1
         IF (PUSRID.EQ.' ')
     *      WRITE (IUNIT,130) XSTR(1:LXSTR),
     *         PGMNAM,PGMVRN,PGMVRD,MONTH,DAY,YEAR,TIME,
     *         NPSPAG(IUNIT)
         IF (PUSRID.NE.' ')
     *      WRITE (IUNIT,140) XSTR(1:LXSTR),
     *         PGMNAM,PGMVRN,PGMVRD,MONTH,DAY,YEAR,TIME,
     *         PUSRID,
     *         NPSPAG(IUNIT)
         NPSNLN(IUNIT)=1
         NPSNLT(IUNIT)=NPSNLT(IUNIT)+1
         WRITE (IUNIT,150)
         NPSNLN(IUNIT)=NPSNLN(IUNIT)+2
         NPSNLT(IUNIT)=NPSNLT(IUNIT)+2
         ENDIF
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  SET TOP OF PAGE INDICATOR
40    IPSNWP(NUNIT)=1
C
50    IF (ICMTRC.GT.0) THEN
         NPSNLN(ICMPRU)=NPSNLN(ICMPRU)+1
         WRITE (ICMPRU,160) IUNIT,NPSPAG(IUNIT),NPSNLN(IUNIT)
         ENDIF
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
60    FORMAT (' ENTER UPAGE2 - ',
     *   'NUNIT=',I2,3X,
     *   'IPSPAG(IUNIT)=',I2,3X,
     *   'PUSRID=',A)
70    FORMAT ('1NWSRFS ',A,' SYSTEM - PROGRAM ',A,1X,
     *   '(VERSION: ',A,' - ',A,')',
     *   T79,'USER=',A,
     *   T94,'DATE=',A,
     *   T125,'PAGE=',I4)
80    FORMAT ('1NWSRFS ',A,' SYSTEM - PROGRAM ',A,1X,
     *   '(VERSION: ',A,' - ',A,')',
     *   T79,'USER=',A,
     *   T94,'DATE=',A)
90    FORMAT ('1NWSRFS ',A,' SYSTEM - PROGRAM ',A,1X,
     *   '(VERSION: ',A,' - ',A,')',
     *   T93,'DATE=',A,
     *   T125,'PAGE=',I4)
100   FORMAT ('1NWSRFS ',A,' SYSTEM - PROGRAM ',A,1X,
     *   '(VERSION: ',A,' - ',A,')',
     *   T93,'DATE=',A)
110   FORMAT ('1*',A,'PROGRAM ',A,' (VERSION: ',A,' - ',A,') *',
     *     10X,'DATE=',A,'/',A,'/',A,'.',A)
120   FORMAT ('1*',A,'PROGRAM ',A,' (VERSION: ',A,' - ',A,') *',
     *     10X,'DATE=',A,'/',A,'/',A,'.',A,
     *     10X,'USER=',A)
130   FORMAT ('1*',A,'PROGRAM ',A,' (VERSION: ',A,' - ',A,') *',
     *     10X,'DATE=',A,'/',A,'/',A,'.',A,
     *     T125,'PAGE=',I4)
140   FORMAT ('1*',A,'PROGRAM ',A,' (VERSION: ',A,' - ',A,') *',
     *     10X,'DATE=',A,'/',A,'/',A,'.',A,
     *     10X,'USER=',A,
     *     T125,'PAGE=',I4)
150   FORMAT ('0')
160   FORMAT (' EXIT UPAGE2 - ',
     *   'IUNIT=',I2,3X,
     *   'NPSPAG(IUNIT)=',I4,3X,
     *   'NPSNLN(IUNIT)=',I4)
C
      END
