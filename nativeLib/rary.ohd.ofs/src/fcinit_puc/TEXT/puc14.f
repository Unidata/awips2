C MEMBER PUC14
C  (from old member FCPUC14)
C.......................................................................
      SUBROUTINE PUC14(PADJ,CADJ)
C
C     SUBROUTINE PUC14 PUNCHES INPUT CARDS FOR THE 'ADJUST-Q'
C     OPERATION IN THE FORMAT REQUIRED BY THE PIN14 SUBROUTINE.
C
C.......................................................................
C     PROGRAMMED BY KAY KROUSE    JULY 1980
C.......................................................................
      REAL IDQM,IDSQ,IDADJ,IDQI
      DIMENSION PADJ(1),CADJ(1),IDQM(2),IDSQ(2),IDADJ(2),IDQI(2),
     1ANAME(5),PU14(2)
      INCLUDE 'common/fdbug'
      INCLUDE 'common/ionum'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_puc/RCS/puc14.f,v $
     . $',                                                             '
     .$Id: puc14.f,v 1.1 1995/09/17 18:50:37 dws Exp $
     . $' /
C    ===================================================================
C
C
      DATA PU14/4HPUC1,4H4   /
C
C     CHECK TRACE LEVEL
      CALL FPRBUG(PU14,1,14,IBUG)
C.......................................................................
C     RETRIEVE INPUT VALUES FROM THE PADJ ARRAY.
      DO 15 I=2,6
      ANAME(I-1)=PADJ(I)
 15   CONTINUE
C
      IOI=PADJ(7)
      IOM=PADJ(8)
      IDSQ(1)=PADJ(9)
      IDSQ(2)=PADJ(10)
      QSTYPE=PADJ(11)
      ITSQ=PADJ(12)
C
      IDADJ(1)=PADJ(13)
      IDADJ(2)=PADJ(14)
      QATYPE=PADJ(15)
      NSTEPS=PADJ(17)
      ICRY=PADJ(19)
      MM=25
C
      IF(IOM.EQ.0)GO TO 20
      TOL=PADJ(25)
      IDQM(1)=PADJ(26)
      IDQM(2)=PADJ(27)
      QMTYPE=PADJ(28)
      MM=29
C
 20   IF(IOI.EQ.0)GO TO 25
      IDQI(1)=PADJ(MM)
      IDQI(2)=PADJ(MM+1)
      QITYPE=PADJ(MM+2)
      ITQI=PADJ(MM+3)
      IOPT=PADJ(MM+4)
 25   CONTINUE
C.......................................................................
C     PUNCH CARD 1
      WRITE(IPU,900) (ANAME(I),I=1,5),IOI,IOM,ICRY
 900  FORMAT(5A4,3I5)
C.......................................................................
C     PUNCH TIME SERIES INPUT CARDS:
C     OBS INST Q--CARD 2(OPTIONAL)
      IF(IOI.EQ.0)GO TO 30
      WRITE(IPU,910) IDQI(1),IDQI(2),QITYPE,ITQI
 910  FORMAT(2A4,3X,A4,I5)
C.......................................................................
C     MEAN DAILY Q--CARD 3(OPTIONAL)
 30   IF(IOM.EQ.0)GO TO 35
      WRITE(IPU,910) IDQM(1),IDQM(2),QMTYPE
C.......................................................................
C     SIM INST Q--CARD 4
 35   WRITE(IPU,910) IDSQ(1),IDSQ(2),QSTYPE,ITSQ
C.......................................................................
C     ADJ INST Q--CARD 5
      WRITE(IPU,910) IDADJ(1),IDADJ(2),QATYPE
C.......................................................................
C     PUNCH CARD 6
      IF(IOM.EQ.0)GO TO 40
      WRITE(IPU,915) NSTEPS,TOL
 915  FORMAT(I5,F10.3)
      GO TO 45
 40   WRITE(IPU,915) NSTEPS
C.......................................................................
C     PUNCH CARD 7(OPTIONAL)
 45   IF(IOI.EQ.1) WRITE(IPU,915) IOPT
C.......................................................................
C     PUNCH CARRYOVER INPUT, IF REQUIRED--CARDS 8 & 9
      IF(ICRY.EQ.0)GO TO 100
      MM=24/ITSQ+1
C     PUNCH CARD 8
      WRITE(IPU,925) (CADJ(I),I=1,MM)
 925  FORMAT(7F10.1)
      IF(IOI.EQ.0)GO TO 100
C     PUNCH CARD 9
      DQI=CADJ(MM+1)
      NBI=CADJ(MM+2)
      QBI=CADJ(MM+3)
      WRITE(IPU,920) DQI,NBI,QBI
 920  FORMAT(F10.2,I5,F10.2)
C.......................................................................
C.......................................................................
 100  CONTINUE
      RETURN
      END
