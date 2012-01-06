C MEMBER GSO6H
C  (from old member PPGSO6H)
C
      SUBROUTINE GSO6H(W6, GP6, NPGRID, NRAIN, N6P, WP)
C
C.....THIS SUBROUTINE STORES SIX HOUR PRECIPITATION DATA THAT HAS BEEN
C.....SUBMITTED IN A RUNTIME MOD.
C
C.....THIS ROUTINE CAN BE CALLED BY GSODAT WHICH CAN BE CALLED BY GGPXGP
C
C.....HERE ARE THE ARGUMENTS:
C
C.....W6     - ARRAY OF 6-HOUR DISTRIBUTION PERCENTAGES.
C.....GP6    - POINTER ARRAY TO W6 ARRAY.
C.....NPGRID - GRID POINT ADDRESS.
C.....NRAIN  - 24 HR PRECIPITATION.
C.....N6P    - 6 HR PERCENTAGE DISTRIBUTION ARRAYS.
C.....WP     - POINTER TO NEXT AVAILABLE SLOT IN W6.
C
C
C.....ORIGINALLY WRITTEN BY
C
C.....JERRY M. NUNN       WGRFC FT. WORTH, TEXAS       SEPTEMBER 1986
C
      DIMENSION PX(4), SNAME(2)
      INTEGER*2 NRAIN, NFLAG6, N6P(1), ISUM, IP, JP, KP, LP
      INTEGER*2 W6(1), GP6(1)
      INCLUDE 'gcommon/explicit'
C
      INCLUDE 'gcommon/gsize'
      INCLUDE 'common/where'
      INCLUDE 'common/ionum'
      INCLUDE 'common/pudbug'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_maro/RCS/gso6h.f,v $
     . $',                                                             '
     .$Id: gso6h.f,v 1.1 1995/09/17 19:02:38 dws Exp $
     . $' /
C    ===================================================================
C
C
      DATA GPXP, SNAME /4hGPXP, 4hGSO6, 4hH   /
C
  900 FORMAT(1H0, '*** GSO6H ENTERED ***')
  901 FORMAT(1H0, '*** EXIT GSO6H -- NEXT AVAILABLE SLOT IN THE W6 ARRAY
     * IS ', I5, ' ***')
  902 FORMAT(1H0, 4X, 'GP6(', I4, ') = ', I5, 2X, 4(3X, 'W6(', I4,
     * ') = ', I3))
  903 FORMAT(1H0, '*** NOTE ***  THE SUM OF THE SIX-HOURLY PRECIPITATION
     *VALUES ENTERED FOR GRID POINT ', I4, ' WITH THIS RUNTIME MOD IS ',
     * I4, /, 5X, 'THE SIX-HOURLY FLAG AND THE SIX-HOURLY WEIGHTS WILL N
     *OT BE COMPUTED FOR THIS GRID POINT.')
C
      INCLUDE 'gcommon/setwhere'
      IBUG = IPBUG(GPXP)
C
C.....SUM UP THE SIX HOURLY PRECIPITATION AMOUNTS.
C
      ISUM = 0
      IF(IPTRCE .GE. 3) WRITE(IOPDBG,900)
C
      DO 100 NP = 1, 4
      ISUM = ISUM + N6P(NP)
  100 CONTINUE
C
C.....CHECK IF 24 HR PRECIPITATION EQUALS THE SUM OF THE SIX HOUR PRECIP
C
C
      IF(ISUM .EQ. 0) GOTO 300
      SUM = ISUM
      PX(1) = N6P(1)
      PX(2) = N6P(2)
      PX(3) = N6P(3)
      PX(4) = N6P(4)
      IP = (PX(1)*100.)/SUM + 0.5
      JP = (PX(2)*100.)/SUM + 0.5
      KP = (PX(3)*100.)/SUM + 0.5
      LP = (PX(4)*100.)/SUM + 0.5
      GOTO 400
C
  300 IP = 0
      JP = 0
      KP = 0
      LP = 0
      IF(IBUG .EQ. 1) WRITE(IOPDBG,903) NPGRID, ISUM
      GOTO 450
C
C.....STORE THE PERCENTAGES IN THE NEXT 4 AVAILABLE SLOTS IN THE WP
C.....ARRAY.
C
  400 W6(WP)   = IP
      W6(WP+1) = JP
      W6(WP+2) = KP
      W6(WP+3) = LP
C
      J = WP
      K = WP + 1
      L = WP + 2
      N = WP + 3
C
C.....STORE THE POINTER TO THE BEGINNING ADDRESS.
C
      GP6(NPGRID) = WP
      IF(IBUG .EQ. 1) WRITE(IOPDBG,902) NPGRID, GP6(NPGRID), J, W6(J),
     * K, W6(K), L, W6(L), N, W6(N)
C
C.....UPDATE THE NEXT AVAILABLE SLOT IN THE W6 ARRAY.
C
      WP = WP + 4
C
  450 DO 500 NP = 1, 4
      N6P(NP) = 0
  500 CONTINUE
C
      IF(IPTRCE .GE. 3) WRITE(IOPDBG,901) WP
C
      RETURN
      END
