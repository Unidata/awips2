C MEMBER GPG24U
C  (from old member PPGPG24U)
C
      SUBROUTINE GPG24U(PG24, ISTATC)
C
C.....THIS IS THE SUBROUTINE THAT IS CALLED WHEN THE GRID POINT
C.....PRECIPITATION DATA (PG24 ARRAY) IS TO BE WRITTEN TO THE
C.....PREPROCESSOR DATA BASE.
C
C.....HERE ARE THE ARGUMENTS:
C
C.....PG24   - THE 24-HOUR PRECIPITATION ARRAY.
C.....ISTATC - STATUS CODE.
C.....          = 0   NORMAL RETURN.
C.....            NOT 0  ERROR RETURN.
C
C.....ORIGINALLY WRITTEN BY:
C
C.....JERRY M. NUNN       WGRFC, FT. WORTH, TEXAS       DECEMBER 1986
C
      INCLUDE 'gcommon/explicit'
C
      INTEGER*2 PG24(1)
C
      DIMENSION SNAME(2)
      INCLUDE 'gcommon/gsize'
      INCLUDE 'gcommon/gdate'
      INCLUDE 'common/where'
      INCLUDE 'common/fctime'
      INCLUDE 'common/pudbug'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_maro/RCS/gpg24u.f,v $
     . $',                                                             '
     .$Id: gpg24u.f,v 1.1 1995/09/17 19:02:13 dws Exp $
     . $' /
C    ===================================================================
C
C
C
      DATA GPCPN, SNAME /4hPG24, 4hGPG2, 4h4U  /
C
  900 FORMAT(1H0, '*** ENTER SUBROUTINE GPG24U ***')
  901 FORMAT(1H0, '*** EXIT SUBROUTINE GPG24U -- STATUS CODE = ', I4,
     * ' ***')
  902 FORMAT(5X, 'CALLING SUBROUTINE WPDDLY. JULIAN DAY = ', I6)
      INCLUDE 'gcommon/setwhere'
C
      ISTATC = 0
C
      IF(IPTRCE .GT. 0) WRITE(IOPDBG,900)
C
C.....TEST IF THE DATA FILE CAN ACCOMODATE THE DATA TO BE WRITTEN.
C
      CALL GWPTST(GPCPN, MAXGRD, ISTATC)
      IF(ISTATC .GT. 0) GOTO 999
C
C.....NOW WRITE OUT THE PRECIPITATION ONTO THE PREPROCESSOR DATA BASE.
C
      IF(IPTRCE .GE. 3) WRITE(IOPDBG,902) ICURDA
      CALL WPDDLY(GPCPN, ICURDA, MAXGRD, PG24, ISTATC)
      IF(ISTATC .EQ. 0) GOTO 999
C
C.....IF THERE IS AN ERROR...CALL THE ERROR PROCESSING ROUTINE.
C
      CALL GWPERR(GPCPN, MAXGRD, ISTATC)
C
  999 IF(IPTRCE .GT. 0) WRITE(IOPDBG,901) ISTATC
      RETURN
      END
