C MEMBER GLPTVR
C  (from old member PPGLPTVR)
C
      SUBROUTINE GLPTVR(NP, PTVR)
C
C.....THIS IS A SUBROUTINE TO DUMP THE CONTENTS OF THE PPVR POINTERS
C.....ARRAY.
C
C.....ARGUMENTS:
C
C.....NP     - NUMBER OF ELEMENTS IN THE PPVR POINTERS ARRAY.
C.....PTVR   - THE PPVR POINTERS ARRAY.
C
C.....ORIGINALLY WRITTEN BY:
C
C.....JERRY M. NUNN     WGRFC FT. WORTH, TEXAS     OCTOBER 16, 1989
C
      INTEGER*2 PTVR(1)
C
      INCLUDE 'common/pudbug'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_maro/RCS/glptvr.f,v $
     . $',                                                             '
     .$Id: glptvr.f,v 1.1 1995/09/17 19:01:50 dws Exp $
     . $' /
C    ===================================================================
C
C
  900 FORMAT(1H0, '*** ENTER SUBROUTINE GLPTVR ***')
  901 FORMAT(1H0, '*** EXIT SUBROUTINE GLPTVR ***')
  902 FORMAT(1H0, 'DUMP OF THE PPVR POINTERS ARRAY -- THERE ARE ', I4,
     * ' ELEMENTS IN THE ARRAY.')
  903 FORMAT(1H0, I6, 2X, 20I6)
C
      IF(IPTRCE .GE. 3) WRITE(IOPDBG,900)
C
      WRITE(IOPDBG,902) NP
C
C
      JP =  1
      KP = 20
C
      IF(NP .LE. 0) GOTO 999
      IF(KP .GT. NP) KP = NP
C
      WRITE(IOPDBG,902) NP
C
  100 WRITE(IOPDBG,903) JP, (PTVR(LP), LP = JP, KP)
      JP = JP + 20
      IF(JP .GT. NP) GOTO 999
      KP = KP + 20
      IF(KP .GT. NP) KP = NP
      GOTO 100
C
  999 RETURN
      END
