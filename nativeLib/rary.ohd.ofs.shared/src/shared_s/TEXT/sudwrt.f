C MEMBER SUDWRT
C-----------------------------------------------------------------------
C
C  DESC ROUTINE TO SET INDICATORS FOR DATA BASES THAT HAVE BEEN WRITTEN
C  DESC TO.
C
      SUBROUTINE SUDWRT (NDB,DBNMS,ISTAT)
C
      REAL PPD/4HPPD /,PPP/4HPPP /,PRD/4HPRD /
      REAL SASM/4HSASM/,GOES/4HGOES/,FMM/4HFMM /
C
      DIMENSION DBNMS(1)
C
      INCLUDE 'uio'
      INCLUDE 'scommon/sudbgx'
      INCLUDE 'scommon/suddsx'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shared_s/RCS/sudwrt.f,v $
     . $',                                                             '
     .$Id: sudwrt.f,v 1.1 1995/09/17 19:21:54 dws Exp $
     . $' /
C    ===================================================================
C
C
C
      IF (ISTRCE.GT.1) WRITE (IOSDBG,90)
      IF (ISTRCE.GT.1) CALL SULINE (IOSDBG,1)
C
C  SET DEBUG CODE
      LDEBUG=ISBUG(4HSYS )
C
      IF (NDB.EQ.0) GO TO 80
C
      DO 70 I=1,NDB
         IF (DBNMS(I).NE.PPD) GO TO 10
            IDBWRT(4)=1
            GO TO 70
10       IF (DBNMS(I).NE.PPP) GO TO 20
            IDBWRT(5)=1
            GO TO 70
20       IF (DBNMS(I).NE.PRD) GO TO 30
            IDBWRT(6)=1
            GO TO 70
30       IF (DBNMS(I).NE.SASM) GO TO 40
            IDBWRT(8)=1
            GO TO 70
40       IF (DBNMS(I).NE.GOES) GO TO 50
            IDBWRT(9)=1
            GO TO 70
50       IF (DBNMS(I).NE.FMM) GO TO 60
            IDBWRT(10)=1
            GO TO 70
60       WRITE (LP,100) DBNMS(I)
         CALL SUERRS (LP,2,-1)
70       CONTINUE
C
80    IF (ISTRCE.GT.1) WRITE (IOSDBG,110)
      IF (ISTRCE.GT.1) CALL SULINE (IOSDBG,2)
C
      RETURN
C
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
90    FORMAT (' *** ENTER SUDWRT')
100   FORMAT ('0*** ERROR - IN SUDWRT - INVALID DATA BASE NAME : ',A4)
110   FORMAT (' *** EXIT SUDWRT')
C
      END
