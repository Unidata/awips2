C MEMBER AUG50
C  (from old member FCEX50)
C
C                             LAST UPDATE: 07/06/95.14:58:38 BY $WC21DT
C
C  ***  ROUTINE OF NEW SET PARAMETERS DETERMINATION
C @PROCESS LVL(77)
      SUBROUTINE AUG50(H,NB,X,ZB,AB,L,IX,XN,XV,ITSIZE)
      REAL X(1),AB(ITSIZE,ITSIZE),ZB(3,ITSIZE)
C
      DIMENSION IX(*),XN(*),XV(*)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_assim/RCS/aug50.f,v $
     . $',                                                             '
     .$Id: aug50.f,v 1.1 1995/09/17 18:55:31 dws Exp $
     . $' /
C    ===================================================================
C
      DO 1 I=1,NB
      ZB(2,I)=ZB(2,I)+AB(I,L)*H
      X(I)=ZB(2,I)*ZB(3,I)
      K=IX(I)
      IF (X(I).LE.XN(K)) X(I)=XN(K)
      IF (X(I).GE.XV(K)) X(I)=XV(K)
    1 CONTINUE
      RETURN
      END
