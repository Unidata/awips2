C MEMBER PRC24
C  (from old member FCPRC24)
C
      SUBROUTINE PRC24(PO,CO)
C*********************************************************
C   THIS IS THE PRINT CARRYOVER SUBROUTINE FOR THE API-
C   CONT OPERARTION.
C*********************************************************
C   INITIALLY WRITTEN BY   ERIC ANDERSON, HRL,  MARCH 1990
C**********************************************************
C
      DIMENSION PO(1),CO(1)
      DIMENSION SNAME(2)
C
C**********************************************************
C    COMMON BLOCKS
C**********************************************************
      INCLUDE 'common/fdbug'
      INCLUDE 'common/ionum'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_prpc/RCS/prc24.f,v $
     . $',                                                             '
     .$Id: prc24.f,v 1.1 1995/09/17 18:49:41 dws Exp $
     . $' /
C    ===================================================================
C
C
C**********************************************************
C    DATA STATEMENTS
C**********************************************************
C
      DATA SNAME/4HPRC2,4H4   /,BLNK/3H   /
      DATA CAEI,CATI,CFI/3HAEI,3HATI,3HFI /
      DATA CFEI/3HFEI/
C
C**********************************************************
C   CHECK TRACE LEVEL
C**********************************************************
C
      IF (ITRACE.LT.1) GO TO 100
      WRITE(IODBUG,900) SNAME
  900 FORMAT(1H0,'**ENTER',1X,2A4)
C
C**********************************************************
C    GET CONTROL VARIABLES
C**********************************************************
C
  100 IVOPT=PO(14)
      LFRZE=PO(24)
      F5=BLNK
      F6=BLNK
      F7=BLNK
      IF (IVOPT.EQ.0) GO TO 105
      IF (IVOPT.EQ.1) F5=CAEI
      IF (IVOPT.EQ.2) F5=CATI
C
  105 IF (IVOPT.GT.0) GO TO 106
      IF (LFRZE.EQ.0) GO TO 110
      F5=CFI
      F6=CFEI
      GO TO 110
C
  106 IF (LFRZE.EQ.0) GO TO 110
      F6=CFI
      F7=CFEI
C
C**********************************************************
C    PRINT HEADING
C**********************************************************
C
  110 WRITE(IPR,901) F5,F6,F7
  901 FORMAT(1H0,10X,'CARRYOVER',15X,'API',4X,'SMI',3X,'BFSC',
     -4X,'BFI',4X,A3,4X,A3,4X,A3,5X,'ENGLISH UNITS')
C
      API=CO(1)
      SMI=CO(2)
      BFSC=CO(3)
      BFI=CO(4)
      IF (IVOPT.EQ.1) AEI=CO(5)
      IF (IVOPT.EQ.2) ATI=CO(5)
      IF (LFRZE.EQ.0) GO TO 112
      FI=CO(6)
      FEI=CO(7)
  112 WRITE(IPR,902) API,SMI,BFSC,BFI
  902 FORMAT(1H ,30X,4F7.2)
C
      IF ((IVOPT.EQ.0).AND.(LFRZE.EQ.0)) GO TO 190
      IF (IVOPT.GT.0) GO TO 115
      WRITE(IPR,903) FI,FEI
  903 FORMAT(1H+,58X,F7.1,F7.2)
C
      GO TO 190
C
  115 IF (LFRZE.GT.0) GO TO 120
      IF (IVOPT.EQ.1) WRITE(IPR,904) AEI
  904 FORMAT(1H+,58X,F7.2,F7.1,F7.2)
C
      IF (IVOPT.EQ.2) WRITE(IPR,903) ATI
      GO TO 190
C
  120 IF (IVOPT.EQ.1) WRITE(IPR,904) AEI,FI,FEI
      IF (IVOPT.EQ.2) WRITE(IPR,906) ATI,FI,FEI
  906 FORMAT(1H+,58X,2F7.1,F7.2)
C
C***********************************************************
C    CHECK TRACE LEVEL
C**********************************************************
C
  190 IF (ITRACE.LT.1) GO TO 199
      WRITE(IODBUG,905) SNAME
  905 FORMAT(1H0,'**EXIT',1X,2A4)
C
  199 RETURN
      END
