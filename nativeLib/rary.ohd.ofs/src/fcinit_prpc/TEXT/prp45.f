C MEMBER PRP45
C  (from old member FCPRP45)
C
C @PROCESS LVL(77)
C
C                             LAST UPDATE: 02/03/94.10:58:47 BY $WC30KH
C
      SUBROUTINE PRP45(PO)
C.......................................
C     THIS IS THE PRINT PARAMETER SUBROUTINE FOR
C     THE DELTA-TS OPERATION.
C.......................................
C     SUBROUTINE INITIALLY WRITTEN BY. . .
C        KUANG HSU - HRL   DEC. 1993
C.......................................
      DIMENSION PO(*)
      DIMENSION SNAME(2)
C
C     COMMON BLOCKS.
      COMMON/IONUM/IN,IPR,IPU
      COMMON/FDBUG/IODBUG,ITRACE,IDBALL,NDEBUG,IDEBUG(20)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_prpc/RCS/prp45.f,v $
     . $',                                                             '
     .$Id: prp45.f,v 1.1 1995/09/17 18:50:23 dws Exp $
     . $' /
C    ===================================================================
C
C
C     DATA STATEMENTS
      DATA SNAME/4HPRP4,4H5   /
C.......................................
C     TRACE LEVEL FOR SUBROUTINE=1, DEBUG SWITCH=IBUG
      CALL FPRBUG(SNAME,1,45,IBUG)
C.......................................
C     GET CONTROL VARIABLES.
      ITH=PO(5)
C.......................................
C     PRINT INFORMATION ON INPUT AND DELTA TIME SERIES
      WRITE(IPR,901) (PO(I),I=2,4),ITH,
     1(PO(I),I=6,8),ITH
  901 FORMAT(1H0,10X,'INPUT TIME SERIES (I.D.=',2A4,3X,5HTYPE=,A4,3X,
     114HTIME INTERVAL=,I2,1X,7HHOURS).
     2 /11X,'RATE OF CHANGE TIME SERIES (I.D.=',2A4,
     3 3X,5HTYPE=,A4,3X,14HTIME INTERVAL=,I2,1X,7HHOURS).)
C.......................................
      RETURN
      END
