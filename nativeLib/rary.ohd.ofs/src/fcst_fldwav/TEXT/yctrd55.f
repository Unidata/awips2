      SUBROUTINE YCTRD55(J,I,H,YCT,A,B,NCS,BS,HS,K1,K2,K9)

      INCLUDE 'common/fdbug'

      DIMENSION BS(K9,K2,K1),HS(K9,K2,K1),SNAME(2)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_fldwav/RCS/yctrd55.f,v $
     . $',                                                             '
     .$Id: yctrd55.f,v 1.1 1999/04/23 18:08:59 dws Exp $
     . $' /
C    ===================================================================
C


      DATA SNAME/4HYCTR,4HD55 /
C
      CALL FPRBUG(SNAME,1,55,IBUG)

C      FIND LEVEL (KM) BELOW H
      DO 10 K=2,NCS
      IF(H.LE.HS(K,I,J)) GO TO 15
   10 CONTINUE
      KT=NCS
      GO TO 20
   15 KT=K
   20 KM=KT-1
C      FIND A*YB FOR AREA ABOVE LEVEL KM
      DH=H-HS(KM,I,J)
      A1=BS(KM,I,J)*DH
      A2=(B-BS(KM,I,J))*DH/2.
      DA=A1+A2
      YB1=(A1*DH/2.+A2*DH/3.)/DA
      SAY=DA*YB1
      IF(KM.EQ.1) GO TO 50
C      FIND SUM (A*YB) FOR ALL AREAS BELOW H
      DO 30 K=2,KM
      KK=K-1
      DH=HS(K,I,J)-HS(KK,I,J)
      A1=BS(KK,I,J)*DH
      A2=(BS(K,I,J)-BS(KK,I,J))*DH/2.
      DA=A1+A2
      YB1=(A1*DH/2.+A2*DH/3.)/DA
      HC=HS(K,I,J)-YB1
      SAY=SAY+DA*(H-HC)
   30 CONTINUE
   50 YCT=SAY/A
      RETURN
      END
