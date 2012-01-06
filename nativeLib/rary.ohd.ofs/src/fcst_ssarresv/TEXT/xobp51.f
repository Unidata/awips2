C MEMBER XOBP51
C DESC PRINTS GENERATED TIME SERIES
C
C@PROCESS LVL(77)
C
      SUBROUTINE XOBP51(WK,LAOBS)
C---------------------------------------------------------------------
C  ROUTINE TO PRINT GENERATE POOL ELEVATIONS (AND STORAGES) AND
C  DISCHARGES. INPUT FOR THE PROCEDURE CAN BE EITHER AN OBSERVED
C  ELEVATION TS OR OBSERVED DISCHARGE TS OR BOTH. DIFFERENT STRATEGIES
C  ARE USED FOR THE DIFFERENT COMBINATIONS OF TIME SERIES AND THEIR
C  TIME INTERVALS.
C---------------------------------------------------------------------
C  WRITTEN BY KUANG HSU - HRL - OCTOBER 1994
C---------------------------------------------------------------------
C
C  LAOBS -- LAST PERIOD WITH NON-MISSING OBSERVED DATA WITHIN NRUN
C  TIME SERIES SEQUENCE STORED IN PO ARRAY --
C     (QI1),QI2,QO1,QO2,(QOM),(POOL),(STORAGE),(OBSQO),(OBSQOM),(OBSH),
C              (QO1,QO2,(QOM),(POOL),(STORAGE),(OBSQO),(OBSQOM),(OBSH))
C
      INCLUDE 'common/fdbug'
      INCLUDE 'common/fctime'
      INCLUDE 'common/fprog'
      INCLUDE 'common/ionum'
      INCLUDE 'common/sarr51'
      INCLUDE 'common/lts51'
C
      DIMENSION WK(*)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_ssarresv/RCS/xobp51.f,v $
     . $',                                                             '
     .$Id: xobp51.f,v 1.1 1996/03/21 13:46:49 page Exp $
     . $' /
C    ===================================================================
C
C
      IF (IBUG.LT.2) GO TO 9000
C
C  WRITE INST. GEN. DISCHARGES
C
      WRITE(IODBUG,1690)
 1690 FORMAT(/10X,'** INST. GEN. DISCHARGES AT PERIOD START **')
      WRITE(IODBUG,1651) (WK(LWQO1+I-1),I=1,LAOBS)
C
      WRITE(IODBUG,1691)
 1691 FORMAT(/10X,'** INST. GEN. DISCHARGES AT PERIOD END **')
      WRITE(IODBUG,1651) (WK(LWQO2+I-1),I=1,LAOBS)
C
C  WRITE MEAN GEN. DISCHARGES
C
      WRITE(IODBUG,1692)
 1692 FORMAT(/10X,'** MEAN GEN. DISCHARGES **')
      WRITE(IODBUG,1651) (WK(LWQM+I-1),I=1,LAOBS)
C
C  WRITE GEN. POOL ELEVATIONS
C
      WRITE(IODBUG,1694)
 1694 FORMAT(/10X,'** GEN. POOL ELEVATIONS **')
      WRITE(IODBUG,1652) (WK(LWEL+I-1),I=1,LAOBS)
C
C  WRITE GEN. STORAGES
C
      WRITE(IODBUG,1696)
 1696 FORMAT(/10X,'** GEN. STORAGES **')
      WRITE(IODBUG,1650) (WK(LWST+I-1),I=1,LAOBS)
C
 9000 CONTINUE
 1650 FORMAT(5X,8F10.0)
 1651 FORMAT(5X,8F10.1)
 1652 FORMAT(5X,8F10.2)
C
      RETURN
      END
