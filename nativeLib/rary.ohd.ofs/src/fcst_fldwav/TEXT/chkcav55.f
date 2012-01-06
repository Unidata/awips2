      SUBROUTINE CHKCAV55(I,J,YU,YDIFF,HCAV,IFCV,NCS,K9,K2,K1)

      COMMON/M155/NU,JN,JJ,KIT,G,DT,TT,TIMF,F1
      COMMON/M3255/IOBS,KTERM,KPL,JNK,TEH
      COMMON/IONUM/IN,IPR,IPU
      DIMENSION HCAV(K9,K2,K1),IFCV(K9,K2,K1)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_fldwav/RCS/chkcav55.f,v $
     . $',                                                             '
     .$Id: chkcav55.f,v 1.1 1999/04/23 18:08:23 dws Exp $
     . $' /
C    ===================================================================
C

C   THIS PROGRAMS CHECKS TO SEE IF THE CAVE HAS BEEN FILLED
C        IFCV=0 ==>NO CAVE OR CAVE IS FILLING
C             1 ==>CAVE IS FULL
C             2 ==>RECESSION SIDE OF HDROGRAPH; IGNORE CAVE IF FULL

      IF(TT.LT.DT) GO TO 500
      DO 50 K=1,NCS
        IF(IFCV(K,I,J).GE.1) THEN
          IF(YDIFF.GT.0.) IFCV(K,I,J)=1
          IF(YDIFF.LT.0) IFCV(K,I,J)=2
        ELSE
          IF(HCAV(K,I,J).GT.0.0.AND.YU.GT.HCAV(K,I,J)) THEN
            IFCV(K,I,J)=1
            IF(JNK.GT.4) WRITE(IPR,10) K,I,J,IFCV(K,I,J),HCAV(K,I,J),YU,
     1 		                     TT
   10       FORMAT(5X,'    K    I  J IFCV      HCAV        YU        TT'
     .               /5X,2I5,I3,I5,3F10.2)
          ENDIF
        ENDIF
   50 CONTINUE
  500 RETURN
      END
