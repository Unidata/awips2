C MEMBER GSPFIL
C  (from old member PPGSPFIL)
C
      SUBROUTINE GSPFIL(NUM, NDATA, NSP)
C
C.....THIS SUBROUTINE BUILDS AND INTERGER*2 ARRAY OF SORT POINTERS, BASE
C.....ON VALUES IN AN INTEGER*2 DATA ARRAY.
C
C.....THE SUBROUTINE HAS THE FOLLOWING ARGUMENTS:
C
C.....NUM    - THE NUMBER OF ELEMENTS IN NDATA.
C.....NDATA  - THE I*2 ARRAY OF DATA ELEMENTS.
C.....NSP    - THE I*2 ARRAY OF SORT POINTERS.
C
C.....ORIGINALLY WRITTEN BY:
C
C.....JERRY M. NUNN       WGRFC FT. WORTH, TEXAS       DECEMBER 1986
C
      DIMENSION SNAME(2)
C
      INTEGER*2 NDATA(1), NSP(1)
C
      INCLUDE 'common/pudbug'
      INCLUDE 'common/where'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_maro/RCS/gspfil.f,v $
     . $',                                                             '
     .$Id: gspfil.f,v 1.1 1995/09/17 19:02:40 dws Exp $
     . $' /
C    ===================================================================
C
C
      DATA SPLS, SNAME /4hSPLS, 4hGSPF, 4hIL  /
C
  901 FORMAT(1H0, '*** GSPFIL ENTERED ***')
  902 FORMAT(1X, '*** EXIT GSPFIL ***')
  903 FORMAT(1X, 'DATA ELEMENTS...')
  904 FORMAT(1X, 20I6)
  905 FORMAT(1X, 'SORT POINTERS...')
  906 FORMAT(1X, 'THERE ARE ', I5, ' ELEMENTS TO SORT')
C
      INCLUDE 'gcommon/setwhere'
      IF(IPTRCE .GE. 3) WRITE(IOPDBG,901)
C
C.....INITIALIZE THE ARRAY OF SORT POINTERS.
C
      DO 100 NP = 1, NUM
      NSP(NP) = NP
  100 CONTINUE
C
C.....NOW BUILD THE ARRAY OF SORT POINTERS. BEGINNING WITH THE LARGEST
C.....VALUE IN THE LIST...REARRANGE THE ARRAY OF SORT POINTERS SO IT
C.....POINTS TO THE LARGEST ABSOLUTE VALUE.
C
      LNUM = NUM - 1
C
      DO 300 JP = 1, LNUM
      MP = NSP(JP)
      J  = NDATA(MP)
      IBIG = IABS(J)
      IP = JP + 1
C
      DO 200 KP = IP, NUM
      LP = NSP(KP)
      K  = NDATA(LP)
      KEY = IABS(K)
C
      IF(IBIG .GE. KEY) GOTO 200
C
C.....INSERT THE 'NEW LARGEST VALUE'.
C
      IBIG = KEY
C
C.....SWAP OUT SORT POINTERS.
C
      NSP(JP) = LP
      NSP(KP) = MP
      MP = NSP(JP)
C
  200 CONTINUE
C
  300 CONTINUE
C
      IBUG = IPBUG(SPLS)
      IF(IBUG .EQ. 0) GOTO 999
      WRITE(IOPDBG,906) NUM
      WRITE(IOPDBG,903)
      WRITE(IOPDBG,904) (NDATA(NP), NP = 1, NUM)
      WRITE(IOPDBG,905)
      WRITE(IOPDBG,904) (NSP(NP), NP = 1, NUM)
C
  999 IF(IPTRCE .GE. 3) WRITE(IOPDBG,902)
      RETURN
      END
