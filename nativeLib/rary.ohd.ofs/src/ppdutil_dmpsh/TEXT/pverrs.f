C MODULE PVERRS
C-----------------------------------------------------------------------
C
C  PGM: PVERRS(KEY,STAID,DTYPE,ISTAT) .. OUTPUT ERR MESSAGES FOR RPDRRS
C
C   IN: KEY ........ 0 FOR ALL MESSAGES, 1 FOR ERRORS ONLY - INTEGER
C   IN: STAID(2) ... STATION ID - 2A4
C   IN: DTYPE ...... DATA TYPE - A4
C  I/O: ISTAT ...... ERROR CODE - INTEGER
C  I/O:                0 = NO ERROR SO NO MESSAGES
C  I/O:                1 = DATA VALUE ARRAY TOO SMALL
C  I/O:                2 = STATION ID NOT FOUND
C  I/O:                3 = DATA TYPE NOT FOUND (MAY BE SKIPPED)
C  I/O:                4 = DATE NOT FOUND (MAY BE SKIPPED)
C  I/O:                5 = TIME (MIN) ARRAY TOO SMALL
C  I/O:                6 = WORK ARRAY TOO SMALL
C  I/O:                7 = SYSTEM ERROR IN RPDRRS
C   IN: LPE ........ (FROM COMMON) I/O UNIT NUM TO OUTPUT ERRORS - INT
C
C  =====================================================================
C
      SUBROUTINE PVERRS (KEY,STAID,DTYPE,ISTAT)
C
      INCLUDE 'uiox'
C
      CHARACTER*4 DTYPE
      CHARACTER*8 STAID
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/ppdutil_dmpsh/RCS/pverrs.f,v $
     . $',                                                             '
     .$Id: pverrs.f,v 1.2 2002/02/11 20:52:36 dws Exp $
     . $' /
C    ===================================================================
C
C
      IF (ISTAT.GT.0) THEN
         IF (ISTAT.EQ.1) WRITE (LP,20) STAID,DTYPE
         IF (ISTAT.EQ.2) WRITE (LP,30) STAID
         IF (ISTAT.EQ.3) WRITE (LP,40) STAID,DTYPE
         IF (ISTAT.EQ.4.AND.KEY.EQ.0) WRITE (LP,50) STAID,DTYPE
         IF (ISTAT.EQ.5) WRITE (LP,60) STAID,DTYPE
         IF (ISTAT.EQ.6) WRITE (LP,70) STAID,DTYPE
         IF (ISTAT.EQ.7) WRITE (LP,80) STAID,DTYPE
         ENDIF
C
      RETURN
C
20    FORMAT (' **ERROR** OBS DATA ARRAY TOO SMALL FOR FOR STATION ',A,
     *   ' AND DATA TYPE ',A,'.')
30    FORMAT (' **ERROR** STATION ',A,' NOT FOUND.')
40    FORMAT (' **ERROR** RRS TYPE NOT FOUND FOR FOR STATION ',A,
     *   ' AND DATA TYPE ',A,'.')
50    FORMAT (' **WARNING** DATE NOT FOUND FOR FOR STATION ',A,
     *   ' AND DATA TYPE ',A,'.')
60    FORMAT (' **ERROR** TIME ARRAY TOO SMALL FOR FOR STATION ',A,
     *   ' AND DATA TYPE ',A,'.')
70    FORMAT (' **ERROR** WORK ARRAY TOO SMALL FOR FOR STATION ',A,
     *   ' AND DATA TYPE ',A,'.')
80    FORMAT (' **ERROR** SYSTEM ERROR FROM RPDRRS FOR STATION ',A,
     *   ' AND DATA TYPE ',A,'.')
C
      END
