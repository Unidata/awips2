C MODULE QSPRRS
C-----------------------------------------------------------------------
C
C  ROUTINE QSPRRS PRINT ERROR MESSAGES WHEN THE STATUS CODE RETURNED
C  ROUTINE RPDRRS IS GREATER THAN ZERO.
C
C  ORIGINALLY CODED BY DEBBIE VAN DEMARK - 6/5/8
C
C-----------------------------------------------------------------------
C
C  INPUT ARGUMENTS:
C        ISTAT - STAUS CODE RETURNED BY RPDRRS
C        STAID - STATION IDENTIFIER
C        DTYPE - DATA TYPE CODE
C        IMISS - MISSING INDICATOR
C       IFHOUR - BEGINNING HOUR TO EXTRACT DATA
C       ILHOUR - LAST HOUR TO EXTRACT DATA
C       LWKBUF - LENGTH OF ARRAY IWKBUF
C       LWNEED - LENGTH NEEDED FOR ARRAY IWKBUF
C       IRWARN - INDICATES WHETHER TO PRINT WARNING MESSAGES
C       LOBS   - LENGTH OF ARRAY OBS
C       LMIN   - LENGTH OF ARRAY MIN
C
C-----------------------------------------------------------------------
C
      SUBROUTINE QSPRRS (ISTAT,STAID,DTYPE,IMISS,IFHOUR,ILHOUR,
     $   LWKBUF,LWNEED,IRWARN,LOBS,LMIN)
C
      CHARACTER*4 DTYPE
      CHARACTER*8 STAID
      DIMENSION OLDOPN(2)
C
      INCLUDE 'common/ionum'
      INCLUDE 'common/errdat'
      INCLUDE 'common/toterz'
      INCLUDE 'common/pudbug'
      INCLUDE 'common/fctime'
      INCLUDE 'common/fctim2'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_rrs/RCS/qsprrs.f,v $
     . $',                                                             '
     .$Id: qsprrs.f,v 1.5 1999/07/06 16:08:45 page Exp $
     . $' /
C    ===================================================================
C
C
      IOPNUM=-3
      CALL FSTWHR ('QSPRRS  ',IOPNUM,OLDOPN,IOLDOP)
C
      IF (IPTRCE.GT.2) WRITE (IOPDBG,20)
C
C  CHECK DEBUG CODES
      IBUG=IPBUG('QSPR')
C
      IF (ISTAT.GE.1.AND.ISTAT.LE.7) THEN
         ELSE
            WRITE (IPR,30) ISTAT,STAID,DTYPE
            CALL ERROR
            GO TO 10
         ENDIF
C
      IF (ISTAT.EQ.1) THEN
         WRITE (IPR,40) 'OBS',LOBS,STAID,DTYPE
         CALL ERROR
         GO TO 10
         ENDIF

      IF (ISTAT.EQ.2) THEN
         WRITE (IPR,50) STAID
         CALL ERROR
         GO TO 10
         ENDIF
C
      IF (ISTAT.EQ.3) THEN
         WRITE (IPR,60) DTYPE,STAID
         CALL ERROR
         GO TO 10
         ENDIF
C
      IF (ISTAT.EQ.4.AND.IRWARN.EQ.1) THEN
C     SUBTRACT THE DATA BASE OFFSET
         IFHOUR=IFHOUR-NHOPDB
         ILHOUR=ILHOUR-NHOPDB
         IDA=IFHOUR/24+1
         IHR=IFHOUR-IDA*24+24
         CALL MDYH1 (IDA,IHR,IM,ID,IY,IH,NOUTZ,NOUTDS,TZC)
         LDA=ILHOUR/24+1
         LHR=ILHOUR-LDA*24+24
         CALL MDYH1 (LDA,LHR,LM,LD,LY,LH,NOUTZ,NOUTDS,TZC)
         IF (IMISS.EQ.1) THEN
            WRITE (IPR,70) IM,ID,IY,IH,TZC,
     *        LM,LD,LY,LH,TZC,
     *        STAID,DTYPE
            ENDIF
         IF (IMISS.EQ.0) THEN
            WRITE (IPR,80) IM,ID,IY,IH,TZC,
     *        LM,LD,LY,LH,TZC,
     *        STAID,DTYPE
            ENDIF
         CALL WARN
         GO TO 10
         ENDIF
C
      IF (ISTAT.EQ.5) THEN
         WRITE (IPR,90) 'MIN',LMIN,STAID,DTYPE
         CALL ERROR
         GO TO 10
         ENDIF
C
      IF (ISTAT.EQ.6) THEN
         WRITE (IPR,100) LWKBUF,STAID,DTYPE,LWNEED
         CALL ERROR
         GO TO 10
         ENDIF
C
      IF (ISTAT.EQ.7) THEN
         WRITE (IPR,110) STAID,DTYPE
         CALL ERROR
         GO TO 10
         ENDIF
C
10    CALL FSTWHR (OLDOPN,IOLDOP,OLDOPN,IOLDOP)
C
      IF (IPTRCE.GT.2) WRITE (IOPDBG,120)
C
      RETURN
C
20    FORMAT (' *** ENTER QSPRRS')
30    FORMAT ('0**WARNING** IN QSPRRS - STATUS CODE RETURNED FROM ',
     *       'ROUTINE RPDRRS (',I2,') ',
     $       'FOR STATION ',A,' AND DATA TYPE ',A,' NOT RECOGNIZED.' /
     *   13X,'PROCESSING WILL CONTINUE WITH THE NEXT DATA TYPE.')
40    FORMAT ('0**ERROR** THE SIZE OF ARRAY ',A,' (',I5,') ',
     $       'IS TOO SMALL FOR STATION ',A,' AND DATA TYPE ',A,'.' /
     $   11X,'THE DATA BASE WILL BE READ AGAIN TO GET ALL THE DATA ',
     $       'REQUESTED.')
50    FORMAT ('0**ERROR** STATION ',A,' NOT FOUND IN THE PPDB. ',
     $   'PROCESSING WILL CONTINUE WITH THE NEXT STATION.')
60    FORMAT ('0**ERROR** DATA TYPE ',A,' NOT FOUND FOR STATION ',A,
     $   '. PROCESSING WILL CONTINUE WITH THE NEXT DATA TYPE.')
70    FORMAT ('0**WARNING** NO OBSERVATIONS FOUND WITHIN THE PERIOD ',
     $       I2.2,'/',I2.2,'/',I4,'-',I2.2,A4,
     $       ' TO ',
     $       I2.2,'/',I2.2,'/',I4,'-',I2.2,A4, /
     $   13X,'FOR STATION ',A,' AND DATA TYPE ',A,'. ',
     $       'THE PREVIOUS VALUE WILL BE READ FROM THE PPDB AND ',
     $       'EXTRAPOLATED.')
80    FORMAT ('0**WARNING** NO OBSERVATIONS FOUND WITHIN THE PERIOD ',
     $       I2.2,'/',I2.2,'/',I4,'-',I2.2,A4,
     $       ' TO ',
     $       I2.2,'/',I2.2,'/',I4,'-',I2.2,A4 /
     $   13X,'FOR STATION ',A,' AND DATA TYPE ',A,'. ',
     $       'THE TIME SERIES WILL BE SET TO MISSING.')
90    FORMAT ('0**ERROR** THE SIZE OF ARRAY ',A,' (',I5,') IS ',
     $       'TOO SMALL FOR STATION ',A,' AND DATA TYPE ',A,'.' /
     $   11X,'PROCESSING WILL CONTINUE WITH THE NEXT DATA TYPE.')
100   FORMAT ('0**ERROR** THE SIZE OF THE WORK ARRAY (',I5,') IS ',
     $       'TOO SMALL FOR STATION ',A,' AND DATA TYPE ',A,'. ',
     $       'THE SIZE NEEDED IS ',I5,'.' /
     $   11X,'PROCESSING WILL CONTINUE WITH THE NEXT DATA TYPE.')
110   FORMAT ('0**ERROR** SYSTEM ERROR ACCESSING PPDB FOR ',
     *       'STATION ',A,' AND DATA TYPE ',A,'.' /
     *   11X,'PROCESSING WILL CONTINUE WITH THE NEXT DATA TYPE.')
120   FORMAT (' *** EXIT QSPRRS')
C
      END
