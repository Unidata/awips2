C MEMBER PVE1SF
C  (from old member PDBDMPSH)
C-----------------------------------------------------------------------
C
C                             LAST UPDATE: 05/19/95.10:00:36 BY $WC20SV
C
C @PROCESS LVL(77)
C
C  PGM: PVE1SF(KEY,STAID,IDTYP,ISTAT) .. OUTPUT ERR MESSAGES FOR RPD1SF
C
C
C   IN: KEY ........ 0 FOR ALL MESSAGES, 1 FOR ERRORS ONLY - INTEGER
C   IN: STAID(2) ... STATION ID - 2A4
C   IN: IDTYP ...... DATA TYPE - A4
C  I/O: ISTAT ...... ERROR CODE - INTEGER
C  I/O:                0 = NO ERROR SO NO MESSAGES
C  I/O:                1 = STATION ID NOT FOUND
C  I/O:                2 = DATA TYPE NOT FOUND (MAY BE SKIPPED)
C  I/O:                3 = DATE NOT FOUND (MAY BE SKIPPED)
C  I/O:                4 = SOME DATES NOT FOUND (MAY BE SKIPPED)
C  I/O:                5 = DATA VALUE ARRAY TOO SMALL
C  I/O:                6 = SOME DATES MISSING, DATA ARRAY TOO SMALL
C                      7 = SYSTEM ERROR IN RPD1SF
C   IN: LPE ........ (FROM COMMON) I/O UNIT NUM TO OUTPUT ERRORS - INT
C
C
C  RQD: COMMON:  UIO
C
C
C  CMT: ISTAT IS RESET TO 0 FOR ERROR 4.
C
C
C  HIS: WRITTEN BY D. STREET IN APRIL 1988
C  =====================================================================
      SUBROUTINE PVE1SF (KEY,STAID,IDTYP,ISTAT)
C
C
C
      INCLUDE 'uio'
C
      INTEGER    KEY,STAID(2),IDTYP,ISTAT
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/ppdutil_dmpsh/RCS/pve1sf.f,v $
     . $',                                                             '
     .$Id: pve1sf.f,v 1.1 1995/09/17 19:10:10 dws Exp $
     . $' /
C    ===================================================================
C
C
C
C
C                  SKIP THIS ROUTINE IF NO ERRORS NOR WARNINGS
C
        IF ( ISTAT.EQ.0 ) GO TO 90
C
C                  OUTPUT INDICATED ERROR MESSAGES
C
        IF ( ISTAT.EQ.1 ) WRITE (LPE,20) STAID
        IF ( ISTAT.EQ.2 ) WRITE (LPE,30) STAID,IDTYP
        IF ( ISTAT.EQ.5 ) WRITE (LPE,60) STAID,IDTYP
        IF ( ISTAT.EQ.6 ) WRITE (LPE,70) STAID,IDTYP
        IF ( ISTAT.EQ.7 ) WRITE (LPE,80) STAID,IDTYP
C
C                  OUTPUT INDICATED WARNING MESSAGES (IF WANTED)
C
        IF ( KEY.EQ.1 ) GO TO 10
        IF ( ISTAT.EQ.3 ) WRITE (LPE,40) STAID,IDTYP
        IF ( ISTAT.EQ.4 ) WRITE (LPE,50) STAID,IDTYP
10      CONTINUE
C
C                  IF ERROR OR WARNING NOT FATAL, CHANGE STATUS TO 0
C
        IF ( ISTAT.EQ.4 ) ISTAT = 0
C
C                  ERROR AND WARNING MESSAGES
C
20    FORMAT(' **ERROR** STATION NOT FOUND, STA=',2A4)
30    FORMAT(' **ERROR** TYPE NOT FOUND, STA=',2A4,' TYP=',A4)
40    FORMAT(' **WARNING** DATE NOT FOUND, STA=',2A4,' TYP=',A4)
50    FORMAT(' **WARNING** SOME DATES NOT FOUND, STA=',2A4,' TYP=',A4)
60    FORMAT(' **ERROR** VALUE ARRAY TOO SMALL, STA=',2A4,' TYP=',A4)
70    FORMAT(' **ERROR** DATES MISSG, ARRAY SMALL, STA=',2A4,' TYP=',A4)
80    FORMAT(' **ERROR** SYSTEM ERROR FROM RPD1SF, STA=',2A4,' TYP=',A4)
C
C
C
90    RETURN
      END
