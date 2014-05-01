      SUBROUTINE CRESTL55(HCRESL,CRESL,AREA,TW,DTW,KL,J,YY,K1,K16)
C
C  CRESTL COMPUTES TOP WIDTH AND FLOW AREA AT WATER DEPTH YY
C  FOR VARIABLE DAM CRESTS

      INCLUDE 'common/fdbug'

      DIMENSION HCRESL(8,K16,K1),CRESL(8,K16,K1)
      CHARACTER*8 SNAME
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_fldwav/RCS/crestl55.f,v $
     . $',                                                             '
     .$Id: crestl55.f,v 1.3 2004/10/18 19:05:45 jgofus Exp $
     . $' /
C    ===================================================================
C

      DATA SNAME/'CRESTL55'/
C
      CALL FPRBUG(SNAME,1,55,IBUG)

      NW=8
      AREA=0.0
      TW=0.0
      DTW=0.0
      IF (YY.LE.HCRESL(1,KL,J)) GOTO 999

C   FIND OUT THE NUMBER (NV) OF VALID DATA IN CRESL ARRY AND TOTAL AREA
      DO 100 K=2,NW
        NV=K-1
        IF (CRESL(K,KL,J).LE.0.001) GOTO 120
        NV=K
  100 CONTINUE

  120 IF (YY.GE.HCRESL(NV,KL,J)) THEN
        TW=CRESL(NV,KL,J)
        DTW=0.0
        DO 140 I=2,NV
          AREA=AREA+0.5*(CRESL(I-1,KL,J)+CRESL(I,KL,J))*
     .      (HCRESL(I,KL,J)-HCRESL(I-1,KL,J))
  140   CONTINUE
        AREA=AREA+TW*(YY-HCRESL(NV,KL,J))
        GOTO 999
      ENDIF

C    FIND YY IS AT WHICH LAYER (BETWEEN K1 AND K)
      AJ=0.0
      DO 200 K=2,NV
      AJ=AJ+0.5*(CRESL(K-1,KL,J)+CRESL(K,KL,J))*(HCRESL(K,KL,J)-
     .   HCRESL(K-1,KL,J))
      IF (YY.LT.HCRESL(K,KL,J)) GOTO 220
  200 CONTINUE
C jgg changed following lines to fix OB5 beta bug 10/14/04  
C jgg  220 K1=K-1

C jgg      DH=HCRESL(K,KL,J)-HCRESL(K1,KL,J)
C jgg      DTW=(CRESL(K,KL,J)-CRESL(K1,KL,J))/DH
C jgg      TW=CRESL(K1,KL,J)+DTW*(YY-HCRESL(K1,KL,J))

  220 KK1=K-1

      DH=HCRESL(K,KL,J)-HCRESL(KK1,KL,J)
      DTW=(CRESL(K,KL,J)-CRESL(KK1,KL,J))/DH
      TW=CRESL(KK1,KL,J)+DTW*(YY-HCRESL(KK1,KL,J))
C jgg  end of changes

      AREA=AJ-0.5*(TW+CRESL(K,KL,J))*(HCRESL(K,KL,J)-YY)

  999 RETURN
      END
