C MEMBER GMODGP
C  (from old member PPGMODGP)
C-----------------------------------------------------------------------
C
C                             LAST UPDATE: 06/08/94.08:40:24 BY $WC20SV
C
C @PROCESS LVL(77)
C
      SUBROUTINE GMODGP(GMODNM, MAXCRD, ICARD, NCARD, ISTATC)
C
C.....THIS SUBROUTINE IS THE ROUTINE TO RETRIEVE THE MANUAL GRID POINT
C.....PRECIPITATION DATA OVERRIDE RUNTIME MOD DATA.
C
C.....ARGUMENTS TO THE SUBROUTINE ARE AS FOLLOWS:
C
C.....GMODNM - THE NAME OF THE MOD (8 CHARACTERS MAX)
C.....MAXCRD - THE MAXIMUM NUMBER OF CARD IMAGES THAT CAN BE STORED
C....          IN THIS MOD.
C.....ICARD  - THE ARRAY CONTAINING THE MOD CARD IMAGES.
C.....NCARD  - THE ACTUAL NUMBER OF CARD IMAGES RETURNED.
C.....ISTATC - A STATUS CODE.
C.....         0 = OK
C.....             ANY OTHER NUMBER - ERROR(S) PRESENT.
C
C.....ORIGINALLY WRITTEN BY
C
C.....JERRY M. NUNN       WGRFC FT. WORTH, TEXAS       SEPTEMBER 1986
C
      DIMENSION GMODNM(1), ICARD(20,1), SNAME(2)
C
      INCLUDE 'gcommon/gsize'
      INCLUDE 'common/where'
      INCLUDE 'common/pudbug'
      INCLUDE 'common/ionum'
      INCLUDE 'common/errdat'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_maro/RCS/gmodgp.f,v $
     . $',                                                             '
     .$Id: gmodgp.f,v 1.1 1995/09/17 19:02:05 dws Exp $
     . $' /
C    ===================================================================
C
C
      DATA SNAME /4HGMOD, 4HGP  /
C
  900 FORMAT(1H0, '*** GMODGP ENTERED ***')
  901 FORMAT(1H0, '*** ERROR *** RUNTIME MOD DATA FILE RETRIEVAL ERROR I
     *N SUBROUTINE HMODCK CALLED FROM SUBROUTINE GMODGP', /, 1X, 'STATUS
     * CODE = ', I4, '. THERE IS LIKELY A PROBLEM IN HCL INTERFACE ROUTI
     *NES.'  / 1X, 'PROCESSING WILL CONTINUE ',
     * 'BUT NO RUNTIME MOD DATA WILL BE AVAILABLE.')
  902 FORMAT(1H0, '*** EXIT GMODGP -- STATUS CODE = ', I4, ' ***',/, 5X,
     * 'OF THE ', I3, ' CARD IMAGES AVAILABLE, ', I3, ' WERE USED.')
C
C.....SET WHERE COMMON BLOCK.
C
      IF(IPTRCE .GE. 3) WRITE(IOPDBG,900)
C
      IOPNUM = -1
      OPNAME(1) = SNAME(1)
      OPNAME(2) = SNAME(2)
C
C.....NOW CALL THE ROUTINE TO CALL THE MOD CARD IMAGES.
C
      CALL HMODCK(GMODNM, MAXCRD, ICARD, NCARD, ISTATC)
C
C.....CHECK STATUS CODE.
C
      IF(ISTATC .EQ. 0) GOTO 999
C
      WRITE(IPR,901) ISTATC
      CALL ERROR
C
  999 IF(IPTRCE .GE. 3) WRITE(IOPDBG,902) ISTATC, MAXCRD, NCARD
C
      RETURN
      END
