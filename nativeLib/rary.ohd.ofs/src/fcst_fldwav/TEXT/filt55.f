C-----------------------------------------------------------------------
C      THIS IS ONE OF THE SUBPROGRAMS FOR KALMAN FILTER
C      IT PERFORMES KALMAN FILTER MATRIX OPERATIONS
C      WHEN ENTERS,     Y2=Y(j|j-1)      ---- PREDICTED BY FLDWAV
C      WHEN RETURNS,    Y2=Y(j|j)        ---- UPDATED BY THE FILTER
C      TEMP's ARE  SPARE MATRIXS FOR TEMPRARY STARAGE
C-----------------------------------------------------------------------
         SUBROUTINE FILT55(Y2,C,H,P,Q,R,AK,OBS,NS,NO,TEMP1,TEMP2)
         DIMENSION Y2(2*NS),C(2*NS,2*NS),Q(2*NS,2*NS),
     #   P(2*NS,2*NS),AK(2*NS,NO),H(NO,2*NS),R(NO,NO),OBS(NO),
     #   TEMP1(2*NS,2*NS),TEMP2(NO,NO)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_fldwav/RCS/filt55.f,v $
     . $',                                                             '
     .$Id: filt55.f,v 1.1 1999/04/23 18:08:32 dws Exp $
     . $' /
C    ===================================================================
C
         NS2=2*NS
C-----------------------   STEP NO.2  ----------------------------------
         DO 120 I=1,NS2
         DO 120 J=1,NS2
         TEMP1(I,J)=0.0
         DO 120 K=1,NS2
         TEMP1(I,J)=TEMP1(I,J)+P(I,K)*C(J,K)
120      CONTINUE
         CALL TIME55(C,TEMP1,P,NS2,NS2,NS2)
         DO 150 I=1,NS2
         DO 150 J=1,NS2
         P(I,J)=P(I,J)+Q(I,J)
150      CONTINUE
C-----------------------   STEP NO.3  ----------------------------------
         DO 200 I=1,NS2
         DO 200 J=1,NO
         TEMP1(I,J)=0.0
         DO 200 K=1,NS2
         TEMP1(I,J)=TEMP1(I,J)+P(I,K)*H(J,K)
200      CONTINUE
         DO 220 I=1,NO
         DO 220 J=1,NO
         TEMP2(I,J)=0.0
         DO 220 K=1,NS2
         TEMP2(I,J)=TEMP2(I,J)+H(I,K)*TEMP1(K,J)
220      CONTINUE
         DO 240 I=1,NO
         DO 240 J=1,NO
         R(I,J)=R(I,J)+TEMP2(I,J)
240      CONTINUE
         CALL AVER55(R,TEMP2,NO)
         DO 250 I=1,NS2
         DO 250 J=1,NO
         AK(I,J)=0.0
         DO 250 K=1,NO
         AK(I,J)=AK(I,J)+TEMP1(I,K)*TEMP2(K,J)
250      CONTINUE
C------------------------   STEP NO.4    -------------------------------
         DO 300 I=1,NO
         TEMP2(1,I)=0.0
         DO 300 K=1,NS2
         TEMP2(1,I)=TEMP2(1,I)+H(I,K)*Y2(K)
300      CONTINUE 
         DO 320 I=1,NO
         OBS(I)=OBS(I)-TEMP2(1,I) 
320      CONTINUE 
         DO 350 I=1,NS2
         TEMP1(1,I)=0.0
         DO 350 K=1,NO
         TEMP1(1,I)=TEMP1(1,I)+AK(I,K)*OBS(K)
350      CONTINUE         
         DO 380 I=1,NS2 
         Y2(I)=Y2(I)+TEMP1(1,I)
380      CONTINUE         
C------------------------   STEP NO.5   --------------------------------
         CALL TIME55(AK,H,TEMP1,2*NS,NO,2*NS)
         DO 400 I=1,NS2
         DO 400 J=1,NS2
400      TEMP1(I,J)=-1.0*TEMP1(I,J)
         DO 420 L=1,NS2
420      TEMP1(L,L)=1.0+TEMP1(L,L)
         CALL TIME55(TEMP1,P,Q,2*NS,2*NS,2*NS)
         DO 500 I=1,NS2
         DO 500 J=1,NS2
500      P(I,J)=Q(I,J)         
C-----------------------------------------------------------------------
         DO 520 K=1,NO
520      OBS(K)=OBS(K)+TEMP2(1,K)
         RETURN
         END
