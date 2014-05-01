C MEMBER GWPTST
C  (from old member PPGWPTST)
C
      SUBROUTINE GWPTST(TYPE, MAXGRD, ISTATC)
C
C.....THIS IS THE SUBROUTINE THAT MAKES A TEST OF THE PREPROCESSOR
C.....DATA FILES BEFORE WRITING TO THE PREPROCESSOR DATA BASE WITH
C.....SYSTEM PROGRAM WPDDLY.
C
C.....THE ARGUMENT LIST IS:
C
C.....TYPE   - PPDB DATA TYPE.
C.....MAXGRD - THE MAXIMUM NUMBER OF GRID POINTS PERMITTED.
C.....ISTATC - A STATUS CODE RETURNED FROM SUBROUTINE RPDGRD.
C.....              = 0   NORMAL RETURN, NO ERRORS.
C.....          NOT = 0   ERROR RETURN CODE FROM SUBROUTINE RPDGRD.
C
C.....ORIGINALLY WRITTEN BY:
C
C.....JERRY M. NUNN       WGRFC, FT. WORTH, TEXAS       DECEMBER 1986
C
      INCLUDE 'gcommon/gsize'
      INCLUDE 'common/pudbug'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_maro/RCS/gwptst.f,v $
     . $',                                                             '
     .$Id: gwptst.f,v 1.1 1995/09/17 19:02:50 dws Exp $
     . $' /
C    ===================================================================
C
C
  900 FORMAT(1H0, '*** ENTER SUBROUTINE GWPTST ***')
  901 FORMAT(1H0, '*** EXIT SUBROUTINE GWPTST -- STATUS CODE = ', I4,
     * ' ***')
C
      IF(IPTRCE .GT. 0) WRITE(IOPDBG,900)
C
C.....GET THE MAXIMUM NUMBER OF VALUES THAT CAN BE ACCOMODATED.
C
      CALL RPDGRD(TYPE, MAXGRD, ISTATC)
      IF(ISTATC .EQ. 0) GOTO 100
C
      CALL GRDGER(ISTATC, TYPE, MAXGRD, 1)
      GOTO 999
C
  100 IF(MAXGRD .GE. NGRID) GOTO 200
      CALL GRDGER(ISTATC, TYPE, MAXGRD, 2)
C
  200 ISTATC = 0
C
  999 IF(IPTRCE .GT. 0) WRITE(IOPDBG,901) ISTATC
C
      RETURN
      END
