c MEMBER PRC1
C  (from old member FCPRC1)
C
      SUBROUTINE PRC1 (PL,CL)
C.......................................
C     THIS SUBROUTINE PRINTS THE STATE VARIABLES (CARRYOVER VALUES)
C        FOR THE SACRAMENTO SOIL-MOISTURE ACCOUNTING OPERATION.
C.......................................
C     SUBROUTINE INITIALLY WRITTEN BY. . .
C            ERIC ANDERSON - HRL     APRIL 1979     VERSION 1
C.......................................
      DIMENSION PL(1),CL(1)
C
C     COMMON BLOCKS.
      COMMON/FDBUG/IODBUG,ITRACE,IDBALL,NDEBUG,IDEBUG(20)
      COMMON/IONUM/IN,IPR,IPU
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_prpc/RCS/prc1.f,v $
     . $',                                                             '
     .$Id: prc1.f,v 1.1 1995/09/17 18:49:34 dws Exp $
     . $' /
C    ===================================================================
C
C.......................................
C     CHECK TRACE LEVEL -- TRACE LEVEL FOR THIS SUBROUTINE=1.
      IF (ITRACE.GE.1) WRITE (IODBUG,900)
  900 FORMAT (1H0,15H** PRC1 ENTERED)
C     NO DEBUG OUTPUT FOR THIS SUBROUTINE.
C.......................................
C     PRINT TITLE
      WRITE (IPR,901) (PL(I),I=3,7)
  901 FORMAT (1H0,10X,30HSOIL-MOISTURE CONTENTS(MM) FOR,1X,5A4)
C
C     PRINT CARRYOVERS
      WRITE (IPR,902)
  902 FORMAT (1H0,18X,5HUZTWC,3X,5HUZFWC,3X,5HLZTWC,3X,5HLZFSC,3X,
     15HLZFPC,3X,5HADIMC)
      WRITE (IPR,903)(CL(I),I=1,6)
  903 FORMAT (1H ,15X,F8.0,F8.1,F8.0,F8.1,2F8.0)
      IFRZE=PL(24)
      IF (IFRZE.EQ.0) RETURN
      WRITE (IPR,904)
  904 FORMAT (1H0,18X,7HFGCO(1),3X,7HFGCO(2),3X,7HFGCO(3),3X,7HFGCO(4),
     13X,7HFGCO(5),3X,7HFGCO(6))
      NXCO=PL(23)
      IC=NXCO+7
      LC=IC+5
      WRITE(IPR,905) (CL(I),I=IC,LC)
  905 FORMAT (1H ,15X,6F10.3)
C.......................................
      RETURN
      END
