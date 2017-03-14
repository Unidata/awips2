C MEMBER TSV24
C  (from old member FCPUC24)
C
      SUBROUTINE TSV24(LOC,PO,TSID,TSDT,NODT)
C***********************************************************
C     OBTAINS TIME SERIES IDENTIFIERS AND TIME INTERVALS
C     FOR THE API-CONT OPERATION PUNCH SUBROUTINE
C***********************************************************
C     WRITTEN BY ERIC ANDERSON,HRL - SEPT 1992
C***********************************************************
      DIMENSION PO(1),TSID(2),CHAR(8)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_puc/RCS/tsv24.f,v $
     . $',                                                             '
     .$Id: tsv24.f,v 1.1 1995/09/17 18:51:10 dws Exp $
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
