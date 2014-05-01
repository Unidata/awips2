C MEMBER PRC9
C  (from old member FCPRC9)
C
      SUBROUTINE PRC9(PO,CO)
C
C     THIS SUBROUTINE PRINTS THE CARRYOVER VALUES
C     FOR THE MUSKINGUM ROUTING OPERATION
C
C     INITIALLY WRITTEN BY
C        DAVID REED - HRL   OCT. 1979
C
C
C
      DIMENSION PO(1),CO(1)
C
C     COMMON BLOCKS
      COMMON/FDBUG/IODBUG,ITRACE,IDBALL,NDEBUG,IDEBUG(20)
      COMMON/FCONIT/IVALUE
      COMMON/IONUM/IN,IPR,IPU
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_prpc/RCS/prc9.f,v $
     . $',                                                             '
     .$Id: prc9.f,v 1.1 1995/09/17 18:49:52 dws Exp $
     . $' /
C    ===================================================================
C
C
C     CHECK TRACE LEVEL- TRACE LEVEL FOR THIS ROUTINE =1
C
      IF(ITRACE.GE.1) WRITE(IODBUG,900)
  900 FORMAT(1H0,15H**PRC9 ENTERED )
C
C     NO DEBUG OUTPUT FOR THIS SUBROUTINE
C
      ICVAL=PO(20)
      IF((ICVAL.EQ.0).AND.(IVALUE.EQ.1)) GO TO 101
C
C     PRINT CARRYOVER
C
      WRITE(IPR,903) (PO(I),I=2,6),CO(1),CO(2)
  903 FORMAT(1H0,10X,20HCARRYOVER VALUES FOR,1X,5A4//
     1  11X,16HPREVIOUS INFLOW=,F11.2,4H CMS//
     2  11X,17HPREVIOUS OUTFLOW=,F10.2,4H CMS)
      GO TO 102
  101 WRITE(IPR,904)(PO(I),I=2,6)
  904 FORMAT(1H0,10X,20HCARRYOVER VALUES FOR,1X,5A4,
     1//,11X,41HINITIAL CARRYOVER VALUES WERE SET TO ZERO)
  102 RETURN
      END
