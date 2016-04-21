C MEMBER EDSPA
C  (FROM OLD MEMBER EEDEX02)
C
C                             LAST UPDATE: 06/07/95.09:10:24 BY $WC30EW
C
      SUBROUTINE EDSPA(Y,A,NYRS,ILOC,J,NVAR,IMIS,INOR)
      DIMENSION Y(1),A(1)
      INCLUDE 'common/evar'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shared_esp/RCS/edspa.f,v $
     . $',                                                             '
     .$Id: edspa.f,v 1.1 1995/09/17 19:18:35 dws Exp $
     . $' /
C    ===================================================================
C
C
C
      IMIS=0
      INOR=0
      IF (NVAR.GE.7) GO TO 400
      IF (NVPV(NVAR).EQ.2) GO TO 200
      DO 20 K=1,NYRS
      KK=ILOC+K-1
      VAL=A(KK)
      IF (ABS(VAL).LT.1.E-5) VAL=.01
      IF (IFMSNG(VAL).EQ.1) IMIS=1
      Y(K)=VAL
 20   CONTINUE
      RETURN
 200  DO 210 K=1,NYRS
      KK=ILOC+2*(K-1)+J-1
      VAL=A(KK)
      IF (ABS(VAL).LT.1.E-5) VAL=.01
      IF (IFMSNG(VAL).EQ.1) IMIS=1
      Y(K)=VAL
 210  CONTINUE
      RETURN
C
C        OUTPUT VARIABLE IS 7 OR 8.
C
 400  CONTINUE
      IF (NVPV(NVAR).EQ.2) GO TO 600
      DO 420 K=1,NYRS
      KK=ILOC+K-1
      VAL=A(KK)
      IF (IFMSNG(VAL).EQ.1) IMIS=1
      IF (EFNORE(VAL).EQ.1) INOR=1
      Y(K)=VAL
 420  CONTINUE
      RETURN
 600  DO 610 K=1,NYRS
      KK=ILOC+2*(K-1)+J-1
      VAL=A(KK)
      IF (IFMSNG(VAL).EQ.1) IMIS=1
      IF (EFNORE(VAL).EQ.1) INOR=1
      Y(K)=VAL
 610  CONTINUE
      RETURN
      END
