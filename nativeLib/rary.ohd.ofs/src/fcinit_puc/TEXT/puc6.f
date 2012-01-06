C MEMBER PUC6
C  (from old member FCPUC6)
C
      SUBROUTINE PUC6(PO,CO)
C.......................................................................
C     THIS SUBROUTINE PUNCHES PARAMETERS AND CARRYOVER VALUES FOR
C        THE UNIT HYDROGRAPH OPERATION IN THE FORMAT REQUIRED
C        BY THE INPUT SUBROUTINE PIN6.
C.......................................................................
C     SUBROUTINE INITIALLY WRITTEN BY
C        LARRY BRAZIL -- HRL   SEPTEMBER 1979  VERSION 1
C.......................................................................
      DIMENSION PO(1),CO(1)
C     COMMON BLOCKS.
      COMMON/FDBUG/IODBUG,ITRACE,IDBALL,NDEBUG,IDEBUG(20)
      COMMON/IONUM/IN,IPR,IPU
      COMMON/PUDFLT/IPDFLT
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_puc/RCS/puc6.f,v $
     . $',                                                             '
     .$Id: puc6.f,v 1.1 1995/09/17 18:51:04 dws Exp $
     . $' /
C    ===================================================================
C
C.......................................................................
C     CHECK TRACE LEVEL -- TRACE LEVEL FOR THIS SUBROUTINE=1.
      IF(ITRACE.GE.1) WRITE(IODBUG,900)
  900 FORMAT(1H0,15H** PUC6 ENTERED)
C     NO DEBUG OUTPUT FOR THIS SUBROUTINE.
C.......................................................................
C     PUNCH CARD NO. 1
      IDTQ=PO(5)
      IDTQB=PO(9)
      IF(IPDFLT.EQ.0) GO TO 40
      NCO=0
      GO TO 42
   40 NCO=IDTQB/IDTQ
   42 CONTINUE
      WRITE(IPU,902) PO(2),PO(3),PO(4),IDTQ,PO(6),PO(7),PO(8),IDTQB,NCO
  902 FORMAT(2X,2A4,1X,A4,3X,I2,2X,2A4,1X,A4,3X,I2,3X,I2)
C.......................................................................
C     PUNCH CARD NO.2 IF NEEDED
      IF(IPDFLT.EQ.1) GO TO 100
      WRITE(IPU,904) (CO(I),I=1,NCO)
  904 FORMAT(7F10.4)
  100 CONTINUE
C.......................................................................
      RETURN
      END
