C MEMBER GGMDRD
C  (from old member PPGGMDRD)
C
      SUBROUTINE GGMDRD(GMDR)
C
C.....THIS SUBROUTINE DUMPS OUT THE CONTENTS OF THE GMDR PARAMETRIC
C.....ARRAY.  HERE ARE THE ARGUMENTS:
C
C.....GMDR   - THE GMDR PARAMETRIC ARRAY
C
C.....JERRY M. NUNN       WGRFC FT. WORTH, TEXAS       NOVEMBER 27, 1987
C
      INTEGER*2 GMDR(1)
C
      INCLUDE 'common/pudbug'
      INCLUDE 'gcommon/gmdr'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_maro/RCS/ggmdrd.f,v $
     . $',                                                             '
     .$Id: ggmdrd.f,v 1.1 1995/09/17 19:01:42 dws Exp $
     . $' /
C    ===================================================================
C
C
  900 FORMAT(1H0, '*** ENTER SUBROUTINE GGMDRD ***')
  901 FORMAT(1H0, '*** EXIT SUBROUTINE GGMDRD ***')
  902 FORMAT(1H0, 'GMDR PARAMETRIC ARRAY DUMP', //, 1X, 'WESTERNMOST MDR
     * COLUMN = ', I3, 3X, 'NUMBER OF COLUMNS = ', I3, /, 1X, 'SOUTHERNM
     *OST MDR ROW   = ', I3, 3X, 'NUMBER OF ROWS    = ', I3, //, 1X, 'TA
     *BLE OF CENTROID GRID POINT ADDRESSES FOR MDR BOXES IN THE MARO FUN
     *CTION', //, 1X, 'NATIONAL', /, 'ROW NO.', 19X, 'M D R   B O X   C
     *E N T R O I D   G R I D   P O I N T   A D D R E S S E S')
  903 FORMAT(1H0, I5, 9X, I5, 5X, I5, 5X, I5, 5X, I5, 5X, I5, 5X, I5,
     * 5X, I5, 5X, I5, 5X, I5, 5X, I5, /, 15X, I5, 5X, I5, 5X, I5, 5X,
     * I5, 5X, I5, 5X, I5, 5X, I5, 5X, I5, 5X, I5, 5X, I5, /, 15X, I5,
     * 5X, I5, 5X, I5, 5X, I5, 5X, I5, 5X, I5, 5X, I5, 5X, I5, 5X, I5,
     * 5X, I5, /, 15X, I5, 5X, I5, 5X, I5, 5X, I5)
  905 FORMAT(1H0, 'NUMBER OF MDR BOXES IN SYSTEM = ', I5)
C
C
      IF(IPTRCE .GE. 3) WRITE(IOPDBG,900)
      WRITE(IOPDBG,902) ICMDR, NCMDR, IRMDR, NRMDR
C
      J  = 2
      IP = 1
      JP = NCMDR
C
      DO 100 KP = 1, NRMDR
      WRITE(IOPDBG,903) J, (GMDR(NP), NP = IP, JP)
      J = J + 1
      IP = IP + NCMDR
      JP = JP + NCMDR
  100 CONTINUE
C
C
      WRITE(IOPDBG,905) NMDR
C
      IF(IPTRCE .GE. 3) WRITE(IOPDBG,901)
      RETURN
      END
