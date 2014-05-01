C MEMBER STP126
C  (from old member FCCOX26)
C
C***********************************************************************
C
      SUBROUTINE STP126(PO,LP,ING,NLK)
C
C***********************************************************************
C
C     ACCESS FILE FROM 'GAGE1' TO 'LAG/K' WHILE USING STPOOLQ SCHEME
C
      DIMENSION PO(1)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_cox/RCS/stp126.f,v $
     . $',                                                             '
     .$Id: stp126.f,v 1.1 1995/09/17 18:47:36 dws Exp $
     . $' /
C    ===================================================================
C
      LP=LP+3
      NREL=PO(LP)
      LP=LP+1
      DO 1708 J=1,NREL
      NPAIR=PO(LP)*.5
      LP=LP+NPAIR
      LP=LP+NPAIR+1
 1708 CONTINUE
      NRISE=PO(LP)
      LP=LP+NRISE
      LP=LP+NRISE+1
      NFALL=PO(LP)
      IF(NFALL.LE.0)GO TO 1722
      LP=LP+NFALL
      LP=LP+NFALL
 1722 LP=LP+1
      IF(ING.EQ.2)GO TO 1731
      NRULE=PO(LP)
      IF(NRULE.LE.0) GO TO 1730
      NVAL=PO(LP)
      LP=LP+NVAL
      LP=LP+NVAL+1
 1730 LP=LP+1
 1731 NLK=PO(LP)
      LP=LP+1
      RETURN
      END
