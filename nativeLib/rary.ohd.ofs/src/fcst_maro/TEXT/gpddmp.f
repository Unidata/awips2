C MEMBER GPDDMP
C  (from old member PPGPDDMP)
C
      SUBROUTINE GPDDMP(TYPE, NUM, PPDATA)
C
C.....THIS SUBROUTINE DUMPS OUT THE DATA ARRAYS FROM THE PPDB. THE MARO
C.....FUNCTION CURRENTLY USES 3 PPDB DATA TYPES:  (1) PP24; (2) PPVR;
C.....AND (3) MDR6.
C
C.....THE ARGUMENTS TO THE SUBROUTINE FOLLOW:
C
C.....TYPE   - PP24 DATA TYPE.
C.....NUM    - THE NUMBER OF DATA VALUES TO DUMP.
C.....PPDATA - THE PPDB DATA ARRAY ITSELF.
C
C.....ORIGINALLY WRITTEN BY:
C
C.....JERRY M. NUNN     WGRFC FT. WORTH, TEXAS     MARCH 10, 1988
C
      INTEGER*2 PPDATA(1)
C
      INCLUDE 'common/pudbug'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_maro/RCS/gpddmp.f,v $
     . $',                                                             '
     .$Id: gpddmp.f,v 1.1 1995/09/17 19:02:11 dws Exp $
     . $' /
C    ===================================================================
C
C
      DATA PP24 /4hPP24/
C
  900 FORMAT(1H0, '*** GPDDMP ENTERED ***')
  901 FORMAT(1H0, '*** EXIT GPDDMP ***')
  902 FORMAT(2X, A4, '(', I4, ') = ', I7, 2X, A4, '(', I4, ') =', I7,
     * 2X, A4, '(', I4, ') = ', I7, 2X, A4, '(', I4, ') = ', I7,
     * 2X, A4, '(', I4, ') = ', I7, 2X, A4, '(', I4, ') = ', I7)
  903 FORMAT(2X, A4, '(', I4, ') = ', I7, 2X, A4, '(', I4, ') =', I7,
     * 2X, A4, '(', I4, ') = ', I7, 2X, A4, '(', I4, ') = ', I7,
     * 2X, A4, '(', I4, ') = ', I7)
  904 FORMAT(2X, A4, '(', I4, ') = ', I7, 2X, A4, '(', I4, ') =', I7,
     * 2X, A4, '(', I4, ') = ', I7, 2X, A4, '(', I4, ') = ', I7)
  905 FORMAT(2X, A4, '(', I4, ') = ', I7, 2X, A4, '(', I4, ') =', I7,
     * 2X, A4, '(', I4, ') = ', I7)
  906 FORMAT(2X, A4, '(', I4, ') = ', I7, 2X, A4, '(', I4, ') =', I7)
  907 FORMAT(2X, A4, '(', I4, ') = ', I7)
C
      IF(IPTRCE .GE. 3) WRITE(IOPDBG,900)
C
      J = 1
C
  100 IP = J
      JP = J + 1
      KP = J + 2
      LP = J + 3
      MP = J + 4
      NP = J + 5
C
      IF(NP .LE. NUM) GOTO 200
      IF(MP .LE. NUM) GOTO 300
      IF(LP .LE. NUM) GOTO 400
      IF(KP .LE. NUM) GOTO 500
      IF(JP .LE. NUM) GOTO 600
      IF(IP .LE. NUM) GOTO 700
      GOTO 999
C
  200 IF(TYPE .EQ. PP24) GOTO 250
      WRITE(IOPDBG,902) TYPE, IP, PPDATA(IP), TYPE, JP, PPDATA(JP),
     * TYPE, KP, PPDATA(KP), TYPE, LP, PPDATA(LP), TYPE, MP, PPDATA(MP),
     * TYPE, NP, PPDATA(NP)
      GO TO 800
C
  250 IX = PPDATA(IP)/10 + 3000
      JX = PPDATA(IP)/10 + 3000
      KX = PPDATA(KP)/10 + 3000
      LX = PPDATA(LP)/10 + 3000
      MX = PPDATA(MP)/10 + 3000
      NX = PPDATA(NP)/10 + 3000
C
      WRITE(IOPDBG,902) TYPE, IP, IX, TYPE, JP, JX, TYPE, KP, KX,
     * TYPE, LP, LX, TYPE, MP, MX, TYPE, NP, NX
      GOTO 800
C
  300 IF(TYPE .EQ. PP24) GOTO 350
      WRITE(IOPDBG,903) TYPE, IP, PPDATA(IP), TYPE, JP, PPDATA(JP),
     * TYPE, KP, PPDATA(KP), TYPE, LP, PPDATA(LP), TYPE, MP, PPDATA(MP)
      GO TO 800
C
  350 IX = PPDATA(IP)/10 + 3000
      JX = PPDATA(IP)/10 + 3000
      KX = PPDATA(KP)/10 + 3000
      LX = PPDATA(LP)/10 + 3000
      MX = PPDATA(MP)/10 + 3000
C
      WRITE(IOPDBG,903) TYPE, IP, IX, TYPE, JP, JX, TYPE, KP, KX,
     * TYPE, LP, LX, TYPE, MP, MX
      GOTO 800
C
  400 IF(TYPE .EQ. PP24) GOTO 450
      WRITE(IOPDBG,904) TYPE, IP, PPDATA(IP), TYPE, JP, PPDATA(JP),
     * TYPE, KP, PPDATA(KP), TYPE, LP, PPDATA(LP)
      GO TO 800
C
  450 IX = PPDATA(IP)/10 + 3000
      JX = PPDATA(IP)/10 + 3000
      KX = PPDATA(KP)/10 + 3000
      LX = PPDATA(LP)/10 + 3000
C
      WRITE(IOPDBG,904) TYPE, IP, IX, TYPE, JP, JX, TYPE, KP, KX,
     * TYPE, LP, LX
      GOTO 800
C
  500 IF(TYPE .EQ. PP24) GOTO 550
      WRITE(IOPDBG,905) TYPE, IP, PPDATA(IP), TYPE, JP, PPDATA(JP),
     * TYPE, KP, PPDATA(KP)
      GO TO 800
C
  550 IX = PPDATA(IP)/10 + 3000
      JX = PPDATA(IP)/10 + 3000
      KX = PPDATA(KP)/10 + 3000
C
      WRITE(IOPDBG,905) TYPE, IP, IX, TYPE, JP, JX, TYPE, KP, KX
      GOTO 800
C
  600 IF(TYPE .EQ. PP24) GOTO 650
      WRITE(IOPDBG,906) TYPE, IP, PPDATA(IP), TYPE, JP, PPDATA(JP)
      GO TO 800
C
  650 IX = PPDATA(IP)/10 + 3000
      JX = PPDATA(IP)/10 + 3000
C
      WRITE(IOPDBG,906) TYPE, IP, IX, TYPE, JP, JX
      GOTO 800
C
  700 IF(TYPE .EQ. PP24) GOTO 750
      WRITE(IOPDBG,907) TYPE, IP, PPDATA(IP)
      GO TO 800
C
  750 IX = PPDATA(IP)/10 + 3000
C
      WRITE(IOPDBG,907) TYPE, IP, IX
      GOTO 800
C
  800 J = J + 6
      GOTO 100
C
  999 IF(IPTRCE .GE. 3) WRITE(IOPDBG,901)
      RETURN
      END
