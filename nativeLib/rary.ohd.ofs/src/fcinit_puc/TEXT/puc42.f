C MEMBER PUC42
C  (from old member FCPUC42)
C
      SUBROUTINE PUC42(PO,CO)
C.......................................
C  PUNCH SUBROUTINE FOR THE 'RSNWELEV' OPERATION
C.......................................
C  INITIALLY WRITTEN BY ERIC ANDERSON - HRL   DEC. 1991
C.......................................
      DIMENSION PO(1),CO(1)
C    COMMON BLOCKS
      INCLUDE 'common/fdbug'
      INCLUDE 'common/ionum'
      INCLUDE 'common/pudflt'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_puc/RCS/puc42.f,v $
     . $',                                                             '
     .$Id: puc42.f,v 1.2 2002/05/15 14:50:35 hank Exp $
     . $' /
C    ===================================================================
C
C.......................................
C  TRACE LEVEL=1, NO DEBUG
      IF (ITRACE.GE.1) WRITE(IODBUG,900)
  900 FORMAT(1H0,'** PUC42 ENTERED')
C.......................................
C   CONTROL VARIABLES
      IDT=PO(2)
      IZIN=PO(12)
      IRDCO=1
      IF (IPDFLT.EQ.1) IRDCO=0
      IF (IZIN.EQ.0) IRDCO=0
C.......................................
C   PUNCH CARD 1
      WRITE(IPU,901) (PO(I),I=9,11),IDT,PO(8),PO(7),(PO(I),I=3,6),
     -(PO(I),I=13,15),IRDCO
  901 FORMAT(2X,2A4,1X,A4,3X,I2,F5.2,F5.2,7X,2A4,1X,A4,F5.0,2X,
     -2A4,1X,A4,1X,I1)
C.......................................
C    PUNCH CARD 2
      IF (IRDCO.EQ.0) GO TO 190
      WRITE(IPU,902) CO(1)
  902 FORMAT(F10.0)
C.......................................
  190 IF (ITRACE.GE.1) WRITE(IODBUG,903)
  903 FORMAT(1H0,'** EXIT PUC42')
      RETURN
      END
