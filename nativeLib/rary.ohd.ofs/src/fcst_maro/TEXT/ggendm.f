C MODULE GGENDM
C
      SUBROUTINE GGENDM(ITSTT)
C
C.....THIS SUBROUTINE LOOKS FOR AN ENDMOD CARD.
C
C.....ARGUMENTS
C
C.....ITSTT  - RETURN CODE.
C.....           = 0   OK.
C.....           =-1   INVALID STRING.
C
C.....ORIGINALLY WRITTEN BY
C
C.....JERRY M. NUNN       WGRFC FT. WORTH, TEXAS       SEPTEMBER 1986
C
      CHARACTER*4 ENDMOD(6)
      DIMENSION SNAME(2)
      INCLUDE 'ufreex'
      INCLUDE 'common/where'
      INCLUDE 'common/pudbug'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_maro/RCS/ggendm.f,v $
     . $',                                                             '
     .$Id: ggendm.f,v 1.2 1998/07/02 20:33:36 page Exp $
     . $' /
C    ===================================================================
C
      DATA SNAME /4hGGEN, 4hDM  /
C
  901 FORMAT(1H0, '*** GGENDM ENTERED ***')
  902 FORMAT(1H0, '*** EXIT GGENDM -- RETURN CODE IS ' , I2, ' ***')
C
      DATA ENDMOD/4HE   , 4HN   , 4HD   , 4HM   , 4HO   , 4HD   /
C
      INCLUDE 'gcommon/setwhere'
      IF(IPTRCE .GE. 3) WRITE(IOPDBG,901)
C
      DO 100 NP = 1, 6
      IF(ICDBUF(NP:NP) .NE. ENDMOD(NP)) GOTO 900
  100 CONTINUE
C
      ITSTT = 0
      GOTO 999
C
  900 ITSTT = -1
C
  999 IF(IPTRCE .GE. 3) WRITE(IOPDBG,902) ITSTT
      RETURN
      END
