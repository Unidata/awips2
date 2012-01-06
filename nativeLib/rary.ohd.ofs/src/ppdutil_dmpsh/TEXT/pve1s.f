C MODULE PVE1S
C-----------------------------------------------------------------------
C
C  PGM: PVE1S(KEY,STAID,DTYPE,ISTAT) .. OUTPUT ERROR MESSAGES FOR RPD1S
C
C
C   IN: KEY ........ 0 FOR ALL MESSAGES, 1 FOR ERRORS ONLY - INTEGER
C   IN: STAID(2) ... STATION ID - 2A4
C   IN: DTYPE ...... DATA TYPE - A4
C  I/O: ISTAT ...... ERROR CODE - INTEGER
C  I/O:                0 = NO ERROR SO NO MESSAGES
C  I/O:                1 = DATA TYPE ARRAY TOO SMALL
C  I/O:                2 = DATA VALUE ARRAY TOO SMALL
C  I/O:                3 = STATION ID NOT FOUND
C  I/O:                4 = DATA TYPE NOT FOUND (MAY BE SKIPPED)
C  I/O:                5 = DATE NOT FOUND (MAY BE SKIPPED)
C  I/O:                6 = SOME DATES NOT FOUND (MAY BE SKIPPED)
C  I/O:                7 = SYSTEM ERROR IN RPD1S
C   IN: LPE ........ (FROM COMMON) I/O UNIT NUM TO OUTPUT ERRORS - INT
C
C  =====================================================================
C
      SUBROUTINE PVE1S (KEY,STAID,DTYPE,ISTAT)
C
      INCLUDE 'uiox'
C
      CHARACTER*4 DTYPE
      CHARACTER*8 STAID
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/ppdutil_dmpsh/RCS/pve1s.f,v $
     . $',                                                             '
     .$Id: pve1s.f,v 1.2 2002/02/11 20:52:15 dws Exp $
     . $' /
C    ===================================================================
C
C
      IF (ISTAT.GT.0) THEN
         IF (ISTAT.EQ.1) WRITE (LP,20) STAID,DTYPE
         IF (ISTAT.EQ.2) WRITE (LP,30) STAID,DTYPE
         IF (ISTAT.EQ.3) WRITE (LP,40) STAID
         IF (ISTAT.EQ.4) WRITE (LP,50) STAID,DTYPE
         IF (ISTAT.EQ.5.AND.KEY.EQ.0) WRITE (LP,60) STAID,DTYPE
         IF (ISTAT.EQ.6) THEN
            IF (KEY.EQ.0) WRITE (LP,70) STAID,DTYPE
            ISTAT=0
            ENDIF
         IF (ISTAT.EQ.7) WRITE (LP,80) STAID,DTYPE
         ENDIF
C
      RETURN
C
20    FORMAT(' **ERROR** TYPE ARRAY TOO SMALL FOR FOR STATION ',A,
     *   ' AND DATA TYPE ',A,'.')
30    FORMAT(' **ERROR** VALUE ARRAY TOO SMALL FOR FOR STATION ',A,
     *   ' AND DATA TYPE ',A,'.')
40    FORMAT(' **ERROR** STATION NOT FOUND, STA=',2A4)
50    FORMAT(' **ERROR** TYPE NOT FOUND FOR FOR STATION ',A,
     *   ' AND DATA TYPE ',A,'.')
60    FORMAT(' **WARNING** DATE NOT FOUND FOR FOR STATION ',A,
     *   ' AND DATA TYPE ',A,'.')
70    FORMAT(' **WARNING** SOME DATES NOT FOUND FOR FOR STATION ',A,
     *   ' AND DATA TYPE ',A,'.')
80    FORMAT(' **ERROR** SYSTEM ERROR FROM RPD1S FOR FOR STATION ',A,
     *   ' AND DATA TYPE ',A,'.')
C
      END
