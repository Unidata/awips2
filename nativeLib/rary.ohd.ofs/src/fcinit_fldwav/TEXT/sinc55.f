      SUBROUTINE SINC55(Y,J,I,IP,NCS,HS,SNC,SC,DSC,SNM,SM,DSM,K1,K2,K9)

C  COMPUTE SINUOSITY COEFFICIENT AT WATER DEPTH Y

      INCLUDE 'common/fdbug'

      DIMENSION HS(K9,K2,K1),SNC(K9,K2,K1),SNM(K9,K2,K1)

      CHARACTER*8 SNAME
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_fldwav/RCS/sinc55.f,v $
     . $',                                                             '
     .$Id: sinc55.f,v 1.2 2004/02/02 20:43:22 jgofus Exp $
     . $' /
C    ===================================================================
C

      DATA SNAME/'SINC55  '/
C
      CALL FPRBUG(SNAME,1,55,IBUG)

      DO 10 K=2,NCS
      KT=K
      HC=0.5*(HS(K,I,J)+HS(K,IP,J))
      IF(Y.LE.HC) GO TO 20
   10 CONTINUE
      KT=NCS
   20 KL=KT-1
      HCKT=0.5*(HS(KT,I,J)+HS(KT,IP,J))
      HCKL=0.5*(HS(KL,I,J)+HS(KL,IP,J))
      DH=HCKT-HCKL
      DY=Y-HCKL
      DSC=(SNC(KT,I,J)-SNC(KL,I,J))/DH
      SC=SNC(KL,I,J)+DSC*DY
      DSM=(SNM(KT,I,J)-SNM(KL,I,J))/DH
      SM=SNM(KL,I,J)+DSM*DY
      RETURN
      END
