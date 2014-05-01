C MEMBER GTRNSF
C  (from old member PPGTRNSF)
C
      SUBROUTINE GTRNSF(I, J, NARRAY, NUM, MAX, L, M, ISTAT)
C
C.....THIS SUBROUTINE MOVES 'NUM' ELEMENTS OF NARRAY FROM ADDRESS I TO
C.....ADDRESS J. NARRAY IS AN I*2 ARRAY.
C
C.....ARGUMENTS TO THE SUBROUTINE ARE:
C
C.....I      - LOCATION TO MOVE FROM
C.....J      - LOCATION TO MOVE TO
C.....NARRAY - THE ARRAY (I*2)
C.....NUM    - THE NUMBER OF ELEMENTS TO MOVE.
C.....MAX    - MAXIMUM DIMENSION OF NARRAY.
C.....L      - INITIAL LOCATION OF J
C.....M      - FINAL LOCATION OF J
C.....ISTAT  - STATUS CODE
C.....         = 0   NORMAL EXIT
C....          = 1   J EXCEEDS MAX. TRANSFER OF DATA HALTED.
C
C.....ORIGINALLY WRITTEN BY
C
C.....JERRY M. NUNN       WGRFC FT. WORTH       AUGUST 1986
C
      INTEGER*2 NARRAY(1)
C
      INCLUDE 'common/pudbug'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_maro/RCS/gtrnsf.f,v $
     . $',                                                             '
     .$Id: gtrnsf.f,v 1.1 1995/09/17 19:02:48 dws Exp $
     . $' /
C    ===================================================================
C
C
      DATA GTRN /4hGTRN/
C
  900 FORMAT(1H0, '** GTRNSF ENTERED')
  901 FORMAT(1H0, I4, ' ELEMENTS OF THE ARRAY ARE TO BE TRANSFERRED FROM
     * LOCATION BEGINNING AT ', I5, ' TO LOCATION BEGINNING AT ', I5)
  902 FORMAT(1H0, '** FATAL ERROR **  MAXIMUM LIMITS OF THE ARRAY HAVE B
     *EEN EXCEEDED.')
  903 FORMAT(1H0, '** EXIT GTRNSF')
C
      IF(IPTRCE .GE. 3) WRITE(IOPDBG,900)
C
      NTRBUG = IPBUG(GTRN)
C
      ISTAT = 0
      L = I
      M = J
C
      IF(NTRBUG .EQ. 1) WRITE(IOPDBG,901) NUM, I, J
C
      DO 100 K = 1, NUM
      IF(J .GT. MAX) GOTO 999
C
      NARRAY(J) = NARRAY(I)
      I = I + 1
      J = J + 1
  100 CONTINUE
C
      IF(IPTRCE .GE. 3) WRITE(IOPDBG,903)
C
      RETURN
C
  999 ISTAT = 1
C
      IF(IPTRCE .GE. 3) WRITE(IOPDBG,902)
      IF(IPTRCE .GE. 3) WRITE(IOPDBG,903)
C
      RETURN
      END
