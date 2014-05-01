C MEMBER GGENLD
C  (from old member PPGGENLD)
C
      SUBROUTINE GGENLD(IBUG, PA, CLTR, DESC, IGRID)
C
C.....THIS SUBROUTINE IS CALLED TO RETRIEVE THE NWS ID, THE STATION
C.....DESCRIPTION, AND THE GRID POINT FIELDS FROM THE GENL PARAMETRIC
C.....ARRAY, FOR USE BY THE WGRFC MARO FUNCTION PRECIPITATION LIST
C.....PRINTOUT.
C
C.....WRITTEN BY:  JERRY M. NUNN   WGRFC FT. WORTH   OCTOBER 6, 1987
C
C.....THE SUBROUTINE HAS THE FOLLOWING ARGUMENTS.
C
C.....IBUG   - A DEBUG CODE.
C.....          = 1  PRINT DEBUG OUTPUT.
C.....          = 0  DO NOT PRINT OUT DEBUG OUTPUT.
C.....PA     - PARAMETRIC ARRAY BUFFER.
C.....CLTR   - NWS COMMS HB 5 IDS.
C.....DESC   - STATION DESCRIPTION (NAME).
C.....IGRID  - GRID POINT ADDRESS.
C
      DIMENSION PA(1), CLTR(1), DESC(1)
C
      INCLUDE 'common/pudbug'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_maro/RCS/ggenld.f,v $
     . $',                                                             '
     .$Id: ggenld.f,v 1.1 1995/09/17 19:01:41 dws Exp $
     . $' /
C    ===================================================================
C
C
  900 FORMAT(3X, 'ID = ', 2A4, 3X, 'GRID PT ADRS = ', I4, 3X, 'DESC = ',
     * 5A4)
C
      CLTR(1) = PA(2)
      CLTR(2) = PA(3)
C
      NP = 5
C
      DO 10 IP = 1, 5
      DESC(IP) = PA(NP)
      NP = NP + 1
   10 CONTINUE
C
      IGRID = PA(17)
C
      IF(IBUG .EQ. 0) GOTO 999
      WRITE(IOPDBG,900) (CLTR(K), K = 1, 2), IGRID, (DESC(K), K = 1, 5)
C
  999 RETURN
      END
