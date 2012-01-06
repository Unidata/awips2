C MEMBER GMDMP2
C  (from old member PPGMDMP2)
C
      SUBROUTINE GMDMP2(NUM)
C
C.....THIS SUBROUTINE DUMPS OUT AN MARO AREA PARAMETRIC RECORD.
C.....SUBROUTINE CHANGED FROM ENTRY POINT AND PLACED IN ITS
C.....SOURCE CODE FILE  10/13/88   JERRY M. NUNN, WGRFC FT. WORTH.
C
C.....HERE IS THE ARGUMENT LIST:
C
C.....NUM    - THE MARO AREA NUMBER.
C
      DIMENSION SNAME(2)
      INCLUDE 'gcommon/gmropa'
      INCLUDE 'common/where'
      INCLUDE 'common/pudbug'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_maro/RCS/gmdmp2.f,v $
     . $',                                                             '
     .$Id: gmdmp2.f,v 1.1 1995/09/17 19:02:01 dws Exp $
     . $' /
C    ===================================================================
C
C
      DATA SNAME /4hGMDM, 4hP2  /
C
  901 FORMAT(1H0, 'MARO AREA NO.', I4, '  ID:  ', 2A4, '  NAME:  ', 5A4,
     * '  LATITUDE ', F5.2, '  LONGITUDE ', F6.2, //, 1X, 'MAPG AND MAPI
     * TIME SERIES RECORD NUMBERS ARE:  ', I4, ' AND ', I4, //, 1X,
     * 'FIRST ORDER GRID POINT AND ID ARE:  ', I4, 2X, 2A4,
     * 3X, 'MDR BOX NUMBERS (GRID POINTS) ARE:  ', 3(2X, I4, '(', I4,
     * ')'), //, 1X, 'THERE ARE ', I3, ' GRID POINTS IN THE MARO AREA WI
     *TH GRID ADDRESSES AND RAINFALL-RUNOFF RELATION NUMBERS AS FOLLOWS:
     *', /, 10(2X, 2I5))
  904 FORMAT(1H0, '*** ENTER GMDMP2 ***')
  905 FORMAT(1H0, '*** EXIT GMDMP2 ***')
      INCLUDE 'gcommon/setwhere'
C
      IF(IPTRCE .GE. 6) WRITE(IOPDBG,904)
      WRITE(IOPDBG,901) NUM, IDMARO, IDESC, XLAT, XLONG, IREC, IFGRID,
     * IFCLTR, (MDRBOX(NP), MDRGRD(NP), NP = 1, 3), NGRIDA,
     * (IGRID(NP), IRELN(NP), NP = 1, NGRIDA)
C
      IF(IPTRCE .GE. 6) WRITE(IOPDBG,905)
      RETURN
      END
