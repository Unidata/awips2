C MODULE QSRPRD
C-----------------------------------------------------------------------
C
C  ROUTINE QSRPRD PRINT ERROR MESSAGES WHEN THE STATUS CODE RETURNED
C  FROM ROUTINE RPRDD IS GREATER THAN ZERO.
C
C  ORIGINALLY CODED BY DEBBIE VAN DEMARK - 2/8/83
C
C-----------------------------------------------------------------------
C
C  INPUT ARGUMENTS
C        ISTAT - STAUS CODE RETURNED BY RPRDD
C        STAID - STATION IDENTIFIER
C        DTYPE - DATA TYPE CODE
C        JHOUR - BEGINNING HOUR TO EXTRACT DATA
C       INTVAL - TIME STEP DESIRED
C        NUMPD - NUMBER OF TIME PERIODS OF DATA DESIRED
C       UNITOT - UNITS DESIRED
C        RMISS - FILLER VALUE
C       LWKBUF - LENGTH OF ARRAY IWKBUF
C       LTSDAT - LENGTH OF ARRAY TSDAT
C
C-----------------------------------------------------------------------
C
      SUBROUTINE QSRPRD (ISTAT,STAID,DTYPE,JHOUR,INTVAL,NUMPD,UNITOT,
     $   RMISS,LWKBUF,LTSDAT)
C
      CHARACTER*4 DTYPE,UNITOT,TZC
      CHARACTER*8 STAID,OLDOPN
C
      INCLUDE 'common/ionum'
      INCLUDE 'common/fctime'
      INCLUDE 'common/fctim2'
      INCLUDE 'common/errdat'
      INCLUDE 'common/toterz'
      INCLUDE 'common/pudbug'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_rrs/RCS/qsrprd.f,v $
     . $',                                                             '
     .$Id: qsrprd.f,v 1.4 1999/07/06 16:09:01 page Exp $
     . $' /
C    ===================================================================
C
C
      IOPNUM=-3
      CALL FSTWHR('QSRPRD  ',IOPNUM,OLDOPN,IOLDOP)
C
      IF (IPTRCE.GT.2) WRITE (IOPDBG,20)
C
      IF (ISTAT.EQ.1) THEN
         WRITE (IPR,30) STAID,DTYPE
         CALL ERROR
         GO TO 10
         ENDIF
C
      IF (ISTAT.EQ.2) THEN
         IDA=JHOUR/24+1
         IHR=JHOUR-IDA*24+24
         CALL MDYH1 (IDA,IHR,IM,ID,IY,IH,NOUTZ,NOUTDS,TZC)
         WRITE (IPR,40) STAID,DTYPE,IM,ID,IY,IH,TZC,NUMPD,RMISS
         CALL ERROR
         GO TO 10
         ENDIF
C
      IF (ISTAT.EQ.3) THEN
         WRITE (IPR,50) INTVAL,STAID,DTYPE
         CALL ERROR
         GO TO 10
         ENDIF
C
      IF (ISTAT.EQ.4) THEN
         WRITE (IPR,60) STAID,DTYPE,UNITOT
         CALL ERROR
         GO TO 10
         ENDIF
C
      IF (ISTAT.EQ.5) THEN
         WRITE (IPR,70) STAID,DTYPE
         CALL ERROR
         GO TO 10
         ENDIF
C
      IF (ISTAT.EQ.6) THEN
         WRITE (IPR,80) 'WORK',LWKBUF,STAID,DTYPE
         CALL ERROR
         GO TO 10
         ENDIF
C
      IF (ISTAT.EQ.7) THEN
         WRITE (IPR,90) JHOUR,STAID,DTYPE
         CALL ERROR
         GO TO 10
         ENDIF
C
      IF (ISTAT.EQ.8) THEN
         WRITE (IPR,80) 'DATA',LTSDAT,STAID,DTYPE
         CALL ERROR
         GO TO 10
         ENDIF
C
      WRITE(IPR,100) ISTAT,STAID,DTYPE
      CALL ERROR
C
10    CALL FSTWHR (OLDOPN,IOLDOP,OLDOPN,IOLDOP)
C
      IF (IPTRCE.GT.2) WRITE (IOPDBG,110)
C
      RETURN
C
20    FORMAT (' *** ENTER QSRPRD')
30    FORMAT ('0**ERROR** TIME SERIES NOT FOUND ',
     $ 'FOR IDENTIFIER ',A,' AND DATA TYPE ',A,'.')
40    FORMAT ('0**ERROR** TIME PERIOD NOT FOUND OR NOT ENOUGH DATA ',
     $    'FOR IDENTIFIER ',A,' AND DATA TYPE ',A /
     $ 11X,'STARTING AT ',I2.2,'/',I2.2,'/',I4.4,'-',I2.2,A,' ',
     $    'FOR ',I4,' VALUES. ',
     $    'ARRAY WAS FILLED OUT WITH ',F6.0,'.')
50    FORMAT ('0**ERROR** DATA TIME INTERVAL OF ',I3,' IS INVALID ',
     $ 'FOR IDENTIFIER ',A,' AND DATA TYPE ',A,'.')
60    FORMAT ('0**ERROR** INVALID UNITS CONVERSION REQUESTED ',
     $ 'FOR IDENTIFIER ',A,' AND DATA TYPE ',A,' AND UNITS ',A,'.')
70    FORMAT ('0**ERROR** READING DATA ',
     $ 'FOR IDENTIFIER ',A,' AND DATA TYPE ',A,'.')
80    FORMAT ('0**ERROR** THE ',A,' ARRAY DIMENSIONED AT ',I5,' ',
     $ 'IS TOO SMALL. NO DATA READ ',
     $ 'FOR IDENTIFIER ',A,' AND DATA TYPE ',A,'.')
90    FORMAT ('0**ERROR** HOUR OF ',I6,' IS INVALID. NO DATA READ ',
     $ 'FOR IDENTIFIER ',A,' AND DATA TYPE ',A,'.')
100   FORMAT ('0**ERROR** STATUS CODE OF RETURNED FROM RPRDD (',I2, ' ',
     $ 'NOT RECOGNIZED ',
     $ 'FOR IDENTIFIER ',A,' AND DATA TYPE ',A,'.')
110   FORMAT (' *** EXIT QSRPRD')
C
      END
