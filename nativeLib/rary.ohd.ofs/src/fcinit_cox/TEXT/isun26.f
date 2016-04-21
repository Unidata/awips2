C MEMBER ISUN26
C  (from old member FCCOX26)
C
C***********************************************************************
C
      SUBROUTINE ISUN26(PO,CO,LP,LT,LC,NP,IFIND,NUMN)
C
C***********************************************************************
C
C     DETERMINE IF SCHEME/UTILITY HAS BEEN USED AND THEIR LOCATION
C   NUMN = SCHEME/UTILITY NUMBER IN NEW ARRAY
C
      DIMENSION PO(1),CO(1)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_cox/RCS/isun26.f,v $
     . $',                                                             '
     .$Id: isun26.f,v 1.1 1995/09/17 18:47:36 dws Exp $
     . $' /
C    ===================================================================
C
      IFIND = 1
      LOC=20
      NSUN=PO(LOC)
      LOC=LOC+1
      IF (NSUN .EQ. 0) GO TO 510
      DO 500 IS=1,NSUN
      NUM=PO(LOC)
      LP=PO(LOC+1)
      LOCD = LOC+1
      IF (IS .EQ. NSUN) GO TO 46
      DO 45 I=IS,NSUN
      LOCD = LOCD+4
      LP1 = PO(LOCD)
      IF (LP1 .NE. 0) GO TO 50
   45 CONTINUE
   46 LP1 = PO(11)
   50 NP = LP1-LP
      LT=PO(LOC+2)
      LC=PO(LOC+3)
      LOC=LOC+4
      IF (NUMN .EQ. NUM) GO TO 510
  500 CONTINUE
      LP = 1
      LT = 1
      LC = 1
      NP = 0
      IFIND = 0
  510 CONTINUE
      RETURN
      END
