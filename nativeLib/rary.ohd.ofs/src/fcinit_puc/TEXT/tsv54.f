C MEMBER TSV54
C
      SUBROUTINE TSV54(LOC,PO,TSID,TSDT,NODT)
C
C***********************************************************
C     OBTAINS TIME SERIES IDENTIFIERS AND TIME INTERVALS
C     FOR THE SWB-NILE OPERATION PUNCH SUBROUTINE
C***********************************************************
C     WRITTEN BY QINGYUN DUAN SEPTMEBER 1995
C***********************************************************
C
      DIMENSION PO(1),TSID(2),CHAR(8)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_puc/RCS/tsv54.f,v $
     . $',                                                             '
     .$Id: tsv54.f,v 1.1 1997/09/22 17:36:40 page Exp $
     . $' /
C    ===================================================================
C
      DATA BLANK/4H    /,BLANK2/2H  /
      DATA CHAR/2H 1,2H 2,2H 3,2H 4,2H 6,2H 8,2H12,2H24/
C***********************************************************
      IF(LOC.GT.0) GO TO 100
      TSID(1)=BLANK
      TSID(2)=BLANK
      IF(NODT.EQ.0) TSDT=BLANK2
      RETURN
  100 TSID(1)=PO(LOC)
      TSID(2)=PO(LOC+1)
      IF(NODT.EQ.1) RETURN
      IDT=PO(LOC+3)
      I=IDT
      IF(IDT.LT.6) GO TO 105
      I=4+IDT/4
      IF(IDT.EQ.24) I=8
  105 TSDT=CHAR(I)
      RETURN
      END
