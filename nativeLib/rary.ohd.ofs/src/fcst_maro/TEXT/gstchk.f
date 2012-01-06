C MEMBER GSTCHK
C  (from old member PPGSTCHK)
C
      SUBROUTINE GSTCHK(NP, IBUF, LENGTH, ISTAT)
C
C.....THIS SUBROUTINE SEARCHES A STRING FOR A PARTICULAR CHARACTER
C.....COMBINATION.
C
C.....ARGUMENT LIST
C
C.....NP     - SEARCH FLAG. THIS DETERMINES WHICH PARTICULAR SEQUENCE OF
C.....         CHARACTERS ARE TO BE SEARCHED FOR.
C.....IBUF   - INPUT CHARACTER STRING.
C.....LENGTH - LENGTH OF STRING.
C.....ISTAT  - STATUS CODE
C.....           = 0   OK
C.....           = 1   IMPROPER LENGTH STRING
C.....           = 2   STRING NOT RECOGNIZABLE
C.....           = 3   INVALID SEARCH CODE.
C
C.....ORIGINALLY WRITTEN BY
C
C.....JERRY M. NUNN       WGRFC FT. WORTH, TEXAS       SEPTEMBER 1986
C
      INTEGER*4 GRIDPX(7)
C
      DIMENSION IBUF(1), SNAME(2)
C
      INCLUDE 'common/pudbug'
      INCLUDE 'common/where'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_maro/RCS/gstchk.f,v $
     . $',                                                             '
     .$Id: gstchk.f,v 1.1 1995/09/17 19:02:42 dws Exp $
     . $' /
C    ===================================================================
C
C
      DATA GRIDPX /4H.   , 4HG   , 4HR   , 4HI   , 4HD   ,
     * 4HP   , 4HX   /
      DATA SNAME /4hGSTC, 4hHK  /
C
  900 FORMAT(1H0, '*** GSTCHK ENTERED ***')
  901 FORMAT(1H0, '*** EXIT GSTCHK -- STATUS CODE = ', I2, ' ***')
      INCLUDE 'gcommon/setwhere'
C
C.....CHECK SEARCH FLAG.
C
      IF(IPTRCE .GE. 3) WRITE(IOPDBG,900)
      ISTAT = 0
C
      IF(NP .EQ. 1) GOTO 100
      GOTO 930
C
  100 IF(LENGTH .NE. 7) GOTO 910
C
      DO 110 KP = 1, 7
      IF(IBUF(KP) .NE. GRIDPX(KP)) GOTO 920
  110 CONTINUE
      GOTO 999
C
  910 ISTAT = 1
      GOTO 999
C
  920 ISTAT = 2
      GOTO 999
C
  930 ISTAT = 3
      GOTO 999
C
  999 IF(IPTRCE .GE. 3) WRITE(IOPDBG,901) ISTAT
      RETURN
      END
