C MEMBER ECNV
C  (FROM OLD MEMBER EEDEX02)
C
C                             LAST UPDATE: 06/07/95.09:10:24 BY $WC30EW
C
      SUBROUTINE ECNV(ITTS,NUM,IBASE,I3,I4,I5,LOC,IL3,IL4,IL5,IOBS)
      DIMENSION ITTS(1),LOC(1)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shared_esp/RCS/ecnv.f,v $
     . $',                                                             '
     .$Id: ecnv.f,v 1.1 1995/09/17 19:18:27 dws Exp $
     . $' /
C    ===================================================================
C
C
      IOBS=1
      DO 30 I=1,NUM
      IF (ITTS(I).EQ.3) GO TO 40
 30   CONTINUE
 40   I3=I
      IL3=LOC(I)
      DO 50 J=1,NUM
      IF (ITTS(J).EQ.4) GO TO 60
 50   CONTINUE
      IOBS=0
      GO TO 65
 60   I4=J
      IL4=LOC(J)
 65   IF (IBASE.GE.1) GO TO 70
      RETURN
 70   DO 80 K=1,NUM
      IF (ITTS(K).EQ.5) GO TO 90
 80   CONTINUE
 90   I5=K
      IL5=LOC(K)
      RETURN
      END
