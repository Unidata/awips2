C MEMBER TSCV26
C  (from old member FCCOX26)
C
C***********************************************************************
C
      SUBROUTINE TSCV26(TS1,TS2,NT1,NT2,LOC,NCNEW,NCOLD,D)
C
C***********************************************************************
C
C     GENERATE TIME SERIES WITH INTERVAL NT2 FROM INTERVAL NT1
C
      DIMENSION TS1(1),TS2(1),D(1)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_cox/RCS/tscv26.f,v $
     . $',                                                             '
     .$Id: tscv26.f,v 1.1 1995/09/17 18:47:38 dws Exp $
     . $' /
C    ===================================================================
C
      IST=LOC
      IEND=LOC+NCOLD-1
      IF (NT1 .EQ. NT2) GO TO 300
      IF(NCOLD.LE.1) GO TO 300
C  FIRST GENERATE HOURLY TIME SERIES
      NPM1 = NCOLD-1
      IDT1 = 24/NT1
      IDT2 = 24/NT2
      DO 100 I=1,NPM1
      IST = (I-1)*IDT1+1
      IEND = IST+IDT1
      LOCI = LOC+I-1
      JJ = 0
      DO 110 J=IST,IEND
      JJ = JJ+1
      NPD1 = J
  110 D(J) = TS1(LOCI)+(TS1(LOCI+1)-TS1(LOCI))*(JJ-1)/IDT1
  100 CONTINUE
      NCOLD = (NCOLD-1)*NT2/NT1+1
      TS2(1) = D(1)
      IF (NCNEW .LE. 1) GO TO 320
      DO 200 I=2,NCNEW
      J = (I-1)*IDT2+1
      TS2(I) = D(NPD1)
  200 IF (I .LE. NCOLD) TS2(I) = D(J)
      GO TO 320
  300 CONTINUE
      TS2(1) = TS1(IST)
      IF (NCNEW .LE. 1) GO TO 320
      DO 310 I=2,NCNEW
      LOCI=LOC+I-1
      DUMY = TS1(IEND)
      IF (I .LE. NCOLD) DUMY=TS1(LOCI)
  310 TS2(I) = DUMY
  320 CONTINUE
C  REPEAT LAST VALUE FOR CHANGING OPERATION INTERVAL OR
C  CHANGING FROM MEAN Q TO INSTANTANEOUS OR VISE VERSA
      IF(NCNEW.GE.1) TS2(NCNEW+1)=TS2(NCNEW)
      RETURN
      END
