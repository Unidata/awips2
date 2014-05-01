C MEMBER PRC6
C  (from old member FCPRC6)
C
      SUBROUTINE PRC6(PO,CO)
C.......................................................................
C     THIS SUBROUTINE PRINTS THE CARRYOVER VALUES FOR
C        THE MEAN DISCHARGE OPERATION.
C.......................................................................
C     SUBROUTINE INITIALLY WRITTEN BY
C        LARRY BRAZIL -- HRL   SEPTEMBER 1979  VERSION 1
C.......................................................................
      DIMENSION PO(1), CO(1)
C     COMMON BLOCKS.
      COMMON/FDBUG/IODBUG,ITRACE,IDBALL,NDEBUG,IDEBUG(20)
      COMMON/IONUM/IN,IPR,IPU
      COMMON/FCONIT/IVALUE
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_prpc/RCS/prc6.f,v $
     . $',                                                             '
     .$Id: prc6.f,v 1.1 1995/09/17 18:49:50 dws Exp $
     . $' /
C    ===================================================================
C
C.......................................................................
C     CHECK TRACE LEVEL -- TRACE LEVEL FOR THIS SUBROUTINE=1.
      IF(ITRACE.GE.1) WRITE(IODBUG,900)
  900 FORMAT(1H0,15H** PRC6 ENTERED)
C     NO DEBUG OUTPUT FOR THIS SUBROUTINE.
C.......................................................................
C     CHECK FOR INITIAL OR OLD CARRYOVER VALUES.
      IF(IVALUE.EQ.0) GO TO 300
C     CHECK FOR DEFAULT INITIAL VALUES.
      NCO=PO(10)
      IF(NCO.GT.0) GO TO 400
      WRITE(IPR,902)
  902 FORMAT(1H0,10X,63HINITIAL CARRYOVER VALUES HAVE BEEN SET TO THEIR
     1DEFAULT VALUES.)
      GO TO 350
C.......................................................................
C     DETERMINE NUMBER OF VALID CARRYOVER VALUES.
  300 IDTQ=PO(5)
      IDTQB=PO(9)
      NCOMAX=IDTQB/IDTQ
      NCOUNT=0
      DO 320 I=1,NCOMAX
      NC=NCOMAX-I+1
      IF(IFMSNG(CO(NC)).EQ.0) GO TO 330
      NCOUNT=I
  320 CONTINUE
  330 NCO=NCOMAX-NCOUNT
C.......................................................................
C     PRINT CARRYOVER VALUES.
  400 WRITE(IPR,904)
  904 FORMAT(1H0,10X,50HCARRYOVER VALUES FOR THE MEAN DISCHARGE OPERATIO
     1N:)
      DO 500 I=1,NCO,15
      J=I+14
      IF(J.GT.NCO) J=NCO
      WRITE(IPR,906) (K,K=I,J)
  906 FORMAT(1H0,10X,9HCARRYOVER,I6,14I7)
      WRITE(IPR,908) (CO(K),K=I,J)
  908 FORMAT(1H0,10X,10HQ (CMS)   ,15F7.2)
  500 CONTINUE
  350 CONTINUE
      RETURN
      END
