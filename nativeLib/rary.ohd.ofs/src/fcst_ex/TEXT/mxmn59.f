C*********************************************************
C
C	   FIND HIGHES AND LOWS
C
      SUBROUTINE MXMN59(H,IT,HM,ITM,K1,K2,KM)
      DIMENSION H(1200),IT(1200),HM(1200),ITM(1200)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob81/ohd/ofs/src/fcst_ex/RCS/mxmn59.f,v $
     . $',                                                             '
     .$Id: mxmn59.f,v 1.2 2007/01/05 15:53:49 xfan Exp $
     . $' /
C    ===================================================================
C
C
C          SEARCH FOR TIDAL HIGHS AND LOWS
C
      KM=0
      DO 10 I=K1+5,K2
C
C               COMPUTE HOURLY CHANGES IN STAGE
C
      DH1=H(I)-H(I-1)
      DH2=H(I-1)-H(I-2)
      DH3=H(I-2)-H(I-3)
      DH4=H(I-3)-H(I-4)
C
C               FIRST TEST IS FOR HIGHS (FIRST TWO LINES OF IF
C               STATEMENT); SECOND TEST IS FOR LOWS (LAST TWO
C               LINES OF IF STATEMENT)
C
      IF((DH1.LE.0..AND.DH2.LE.0..AND.
     ?       DH3.GT.0..AND.DH4.GE.0.).OR.
     ?      (DH1.GE.0..AND.DH2.GE.0..AND.
     ?       DH3.LT.0..AND.DH4.LE.0.)) THEN
       KM=KM+1
       HM(KM)=H(I-2)
       ITM(KM)=IT(I-2)
      END IF
   10 CONTINUE
      RETURN
      END
