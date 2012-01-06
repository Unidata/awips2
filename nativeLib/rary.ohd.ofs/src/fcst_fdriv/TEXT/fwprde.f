C MODULE FWPRDE
C-----------------------------------------------------------------------
C
C  ROUTINE TO PRINT MESSAGES FOR STATUS CODES FROM ROUTINE WPRDD.
C
      SUBROUTINE FWPRDE (TSID,TSTYPE,JHOUR,ITIME,NUMPD,UNITS,NVALS,
     *  LDATA,ICALL,LWKBUF,ISTAT)
C
      CHARACTER*4 TSTYPE,UNITS
      CHARACTER*8 TSID
      CHARACTER*50 STRNG
C
C          These are the only variables that come from common:
C            IPR    .. comes from 'common/ionum'
C            MINDAY .. comes from 'prdcommon/pmaxdm'
C
      INCLUDE 'common/ionum'
      INCLUDE 'prdcommon/pmaxdm'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_fdriv/RCS/fwprde.f,v $
     . $',                                                             '
     .$Id: fwprde.f,v 1.3 2005/11/28 19:13:31 hsu Exp $
     . $' /
C    ===================================================================
C
C
C
      IF (ISTAT.EQ.0) THEN
         WRITE (IPR,10) TSID,TSTYPE
10    FORMAT ('0**NOTE** IN FWPRDE - WPRDD STATUS CODE IS ZERO ',
     * 'FOR IDENTIFIER ',A,' AND DATA TYPE ',A,'.')
         GO TO 150
         ENDIF
C
      IF (ISTAT.EQ.1) THEN
         WRITE (IPR,20) TSID,TSTYPE
20    FORMAT ('0**ERROR** TIME SERIES NOT FOUND ',
     * 'FOR IDENTIFIER ',A,' AND DATA TYPE ',A,'.')
         CALL ERROR
         GO TO 150
         ENDIF
C
      IF (ISTAT.EQ.2) THEN
         WRITE (IPR,30) TSID,TSTYPE,NVALS
30    FORMAT ('0**WARNING** THE TIME SERIES ',
     *     'FOR IDENTIFIER ',A,' AND DATA TYPE ',A,' ',
     *     'WAS TRUNCATED BEFORE BEING WRITTEN BECAUSE ' /
     * 13X,'NUMBER OF VALUES (',I3,') EXCEEDS MAXIMUM.')
         CALL WARN
         GO TO 150
         ENDIF
C
      IF (ISTAT.EQ.3) THEN
         WRITE (IPR,40) ITIME,TSID,TSTYPE
40    FORMAT ('0**ERROR** TIME INTERVAL (',I3,') DOES NOT MATCH ',
     * 'TIME INTERVAL IN THE PROCESSED DATA BASE ',
     * 'FOR IDENTIFIER ',A,' AND DATA TYPE ',A,'.')
         CALL ERROR
         GO TO 150
         ENDIF
C
      IF (ISTAT.EQ.4) THEN
         STRNG='?'
         IF (ICALL.EQ.0) STRNG='PREPROCESSOR COMPONENT'
         IF (ICALL.EQ.1) STRNG='FORECAST COMPONENT'
         WRITE (IPR,50) TSTYPE,STRNG(1:LENSTR(STRNG)),TSID
50    FORMAT ('0**ERROR** DATA TYPE ',A,' CAN NOT BE WRITTEN BY ',
     * 'THE ',A,' ',
     * 'FOR IDENTIFIER ',A,'.')
         CALL ERROR
         GO TO 150
         ENDIF
C
      IF (ISTAT.EQ.5) THEN
         WRITE (IPR,60)
60    FORMAT ('0**ERROR** FILE READ/WRITE ERROR.')
         CALL ERROR
         GO TO 150
         ENDIF
C
      IF (ISTAT.EQ.6) THEN
         WRITE (IPR,70)
70    FORMAT ('0**ERROR** NUMBER OF VALUES PER TIME STEP ',
     * 'IS DIFFERENT ON FILE.')
         CALL ERROR
         GO TO 150
         ENDIF
C
      IF (ISTAT.EQ.7) THEN
C
C         Old message before Nov 2005
C          WRITE (IPR,80) MINDAY
C80    FORMAT ('0**ERROR** MINIMUM DAYS OF OBSERVED DATA (',I3,
C     * ') CANNOT BE PRESERVED. TIME SERIES NOT WRITTEN.')
C
          WRITE (IPR,80) MINDAY, TSID, TSTYPE
80    FORMAT ('0**ERROR** MINIMUM DAYS OF OBSERVED DATA (',I3,
     * ') CANNOT BE PRESERVED.',
     * /10X,' TIME SERIES= ',A,', DATA TYPE= ',A,' NOT WRITTEN.')
         CALL ERROR
         GO TO 150
         ENDIF
C
      IF (ISTAT.EQ.8) THEN
         WRITE (IPR,90) NUMPD,NVALS
90    FORMAT ('0**ERROR** THE NUMBER OF TIME PERIODS (',I3,
     *   ') AND/OR THE NUMBER OF DATA VALUES (',I3,
     *   ') TO BE WRITTEN IS ZERO.')
         CALL ERROR
         GO TO 150
         ENDIF
C
      IF (ISTAT.EQ.9) THEN
         WRITE (IPR,100)  UNITS
100   FORMAT ('0**ERROR** INVALID UNITS CONVERSION REQUESTED FOR DATA',
     * ' UNIT ',A,'.')
         CALL ERROR
         GO TO 150
         ENDIF
C
      IF (ISTAT.EQ.10) THEN
         WRITE (IPR,110) LWKBUF
110   FORMAT ('0**ERROR** THE SIZE OF THE WORK ARRAY (',I6,') IS ',
     * 'TOO SMALL. NO DATA WRITTEN TO THE PROCESSED DATA BASE.')
         CALL ERROR
         GO TO 150
         ENDIF
C
      IF (ISTAT.EQ.11) THEN
         WRITE (IPR,120) JHOUR
120   FORMAT ('0**ERROR** STARTING HOUR (',I6,') IS NOT ',
     * 'COMPATIBLE WITH THE PROCESSED DATA BASE.')
         CALL ERROR
         GO TO 150
         ENDIF
C
      IF (ISTAT.EQ.12) THEN
         WRITE (IPR,130) LDATA
130   FORMAT ('0**ERROR** THE SIZE OF THE DATA ARRAY (',I5,') IS ',
     * 'TOO SMALL. NO DATA WRITTEN TO THE PROCESSED DATA BASE.')
         CALL ERROR
         GO TO 150
         ENDIF
C
      WRITE (IPR,140) ISTAT,TSID,TSTYPE
140   FORMAT ('0**NOTE** IN FWPRDE - WPRDD STATUS CODE ',I3,
     * ' NOT RECOGNIZED ',
     * 'FOR IDENTIFIER ',A,' AND DATA TYPE ',A,'.')
C
150   RETURN
C
      END
