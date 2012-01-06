C MEMBER PSTRDD
C-----------------------------------------------------------------------
C
      SUBROUTINE PSTRDD(IPPDRD,ISTAT,LPNTRS,LDATA,TYPE)
C
C   THIS ROUTINE WRITES ERROR MESSAGES FOR ERRORS WHICH
C   OCCUR IN SUBROUTINE RPDDLY.
C
      INCLUDE 'common/ionum'
      INCLUDE 'common/pudbug'
      INCLUDE 'common/fctime'
      INCLUDE 'common/fctim2'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shared_util/RCS/pstrdd.f,v $
     . $',                                                             '
     .$Id: pstrdd.f,v 1.2 2000/03/14 12:26:15 page Exp $
     . $' /
C    ===================================================================
C
C
      IF (IPTRCE.GT.0) WRITE (IOPDBG,*) 'ENTER PSTRDD'
C
      IF (ISTAT.EQ.0) GO TO 90
C
      IF (ISTAT.EQ.1) THEN
         WRITE (IPR,10) LPNTRS
10    FORMAT ('0**ERROR** IN RPDDLY - POINTER ARRAY IS TOO SMALL. ',
     *   I4,' WORDS FILLED.')
         CALL ERROR
         GO TO 90
         ENDIF
C
      IF (ISTAT.EQ.2) THEN
         WRITE (IPR,20) LDATA
20    FORMAT ('0**ERROR** IN RPDDLY - DATA ARRAY IS TOO SMALL. ',
     1   I4,' WORDS FILLED.')
         CALL ERROR
         GO TO 90
         ENDIF
C
      IF (ISTAT.EQ.3) THEN
         WRITE (IPR,30) LPNTRS,LDATA
30    FORMAT ('0**ERROR** IN RPDDLY - BOTH THE POINTER AND DATA ',
     *       'ARRAYS ARE TOO SMALL. ' /
     *   11X,I4,' WORDS FILLED IN THE POINTER ARRAY. ',
     *       I4,' WORDS FILLED IN THE DATA ARRAY.')
         CALL ERROR
         GO TO 90
         ENDIF
C
      IF (ISTAT.EQ.4) THEN
         INTDA=IPPDRD-1
         INTHR=24
         CALL MDYH1 (INTDA,INTHR,IMON,IDAY,IYEAR,IHOUR,NOUTZ,NOUTDS,TZC)
         WRITE (IPR,40) IMON,IDAY,IYEAR
40    FORMAT ('0**ERROR** IN RPDDLY - DATA DOES NOT EXIST FOR ',
     *   'THE DATE ',I2.2,'/',I2.2,'/',I4,'.')
         CALL ERROR
         GO TO 90
         ENDIF
C
      IF (ISTAT.EQ.5) THEN
         WRITE (IPR,50) TYPE
50    FORMAT ('0**ERROR** IN RPDDLY - ',A4,' IS AN INVALID ',
     *   'DAILY DATA TYPE.')
         CALL ERROR
         GO TO 90
         ENDIF
C
      IF (ISTAT.EQ.6) THEN
         WRITE (IPR,60) TYPE
60    FORMAT ('0**ERROR** IN RPDDLY - ',A4,' IS A VALID DATA TYPE ',
     *   'BUT NO DATA EXISTS.')
         CALL ERROR
         GO TO 90
         ENDIF
C
      IF (ISTAT.EQ.7) THEN
         WRITE (IPR,70)
70    FORMAT ('0**ERROR** IN RPDDLY - ARRAY TOO SMALL TO READ ',
     *   'FUTURE DATA.')
         CALL ERROR
         GO TO 90
         ENDIF
C
      IF (ISTAT.EQ.8) THEN
         WRITE (IPR,80)
80    FORMAT ('0**ERROR** IN RPDDLY - SYSTEM ERROR ACCESSING FILE.')
         CALL ERROR
         GO TO 90
         ENDIF
C
90    RETURN
C
      END
