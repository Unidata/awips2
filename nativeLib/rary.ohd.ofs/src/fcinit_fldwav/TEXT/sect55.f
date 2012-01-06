      SUBROUTINE SECT55(PR,AS,BS,HS,ASS,BSS,J,I,Y,HCAV,IFCV,K1,K2,K9)
C
C      THIS SUBROUTINE COMPUTES THE CROSS SECTIONAL AREAS AND TOP WIDTHS
C
      COMMON/M3255/IOBS,KTERM,KPL,JNK,TEH
      COMMON/SS55/ NCS,A,B,DB,R,DR,AT,BT,P,DP,ZH
      COMMON/PRES55/KPRES
      COMMON/PRMS55/DPRM,WPRM
      COMMON/IONUM/IN,IPR,IPU

      DIMENSION AS(K9,K2,K1),BS(K9,K2,K1),HS(K9,K2,K1),PR(K9,K2,K1)
      DIMENSION ASS(K9,K2,K1),BSS(K9,K2,K1),IFCV(K9,K2,K1)
      DIMENSION HCAV(K9,K2,K1)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_fldwav/RCS/sect55.f,v $
     . $',                                                             '
     .$Id: sect55.f,v 1.1 1999/04/23 18:09:45 dws Exp $
     . $' /
C    ===================================================================
C

      KSL=0
      NSLI=0
      YSAVE=Y
      ICOND=0
      IF(BS(NCS,I,J).LT.0.1) ICOND=1
      IF(Y.GT.HS(1,I,J)) GO TO 4
      IF(ICOND.EQ.1) Y=HS(NCS-1,I,J)-0.1
    4 IF(ICOND.EQ.0) GO TO 5
      IF(Y.LT.HS(NCS-1,I,J)) GO TO 5
      B=BS(NCS,I,J)
      DB=0.
      DPRM=0.
      A=AS(NCS-1,I,J)
      AT=A
      BT=B
      IF(KPRES.EQ.1) WPRM=PR(NCS-1,I,J)
CC      WPRM=AT/BT
      GO TO 25
C
    5 IF(Y.GT.HS(1,I,J)) GO TO 7
      DB=0.0
      DBS=0.0
      DPRM=0.0
      BSSS=0.0
      HMIN=HS(1,I,J)-10.*(HS(NCS,I,J)-HS(1,I,J))
      IF(Y.LT.HMIN) Y=HMIN
      IF(AS(1,I,J).LE.0.0) THEN
        B=0.01
        A=B*(HS(1,I,J)-Y)+0.01
      ELSE
        B=BS(1,I,J)
        A=AS(1,I,J)
        YDIF=HS(1,I,J)-Y
        YREC=AS(1,I,J)/B
        YTRI=2.*AS(1,I,J)/B
        IF(YDIF.GE.YREC) A=YDIF*B
        IF(YDIF.LT.YREC.AND.YDIF.GE.YTRI) THEN
          B=B*(1.-YDIF/YTRI)
          A=0.5*YDIF*B
        ENDIF
      END IF
    6 WPRM=A/B
      ASSS=0.0
      BT=B+BSSS
      AT=A+ASSS
      Y=YSAVE
      GO TO 25
 7    DO 10 K=2,NCS
      IF(Y.LE.HS(K,I,J)) GO TO 15
 10   CONTINUE
C
      HMAX=HS(NCS,I,J)+2.*(HS(NCS,I,J)-HS(1,I,J))
      IF(Y.GT.HMAX) Y=HMAX
      K=NCS
 15   L=K-1
      HFAC=HS(K,I,J)-HS(L,I,J)
      IF(HFAC.LE.0.00) HFAC=0.01
      DB=(BS(K,I,J)-BS(L,I,J))/HFAC
      DBS=(BSS(K,I,J)-BSS(L,I,J))/HFAC
      IF(KPRES.EQ.1) DPRM=(PR(K,I,J)-PR(L,I,J))/HFAC
 20   B=BS(L,I,J)+DB*(Y-HS(L,I,J))
      A=AS(L,I,J)+0.5*(B+BS(L,I,J))*(Y-HS(L,I,J))
      BSS1=BSS(L,I,J)
      BSS2=BSS(K,I,J)
      ASS1=ASS(L,I,J)
      IF(IFCV(L,I,J).GE.1.AND.Y.LT.HCAV(K,I,J)) THEN
        IF(JNK.GT.4) WRITE(IPR,9999) I,L,K,IFCV(K,I,J),HCAV(K,I,J),
     .  HCAV(K+1,I,J),Y
 9999   FORMAT(3X,'    I    L    K IFCV     HCAV1     HCAV2         Y'/
     .        3X,4I5,3F10.2)
        IF(HCAV(K,I,J).GT.0.) THEN
          BSS1=0.5*(BSS(L,I,J)+BSS2)
          ASS1=ASS(L-1,I,J)+0.5*(BSS(L-1,I,J)+BSS1)*
     .      (HS(L,I,J)-HS(L-1,I,J))
        ELSEIF(HCAV(K+1,I,J).GT.0) THEN
          BSS2=0.5*(BSS(K+1,I,J)+BSS1)
        ENDIF
        IF(JNK.NE.0) WRITE(IPR,9998) BSS(L,I,J),BSS1,BSS(K,I,J),BSS2,
     .      ASS(L-1,I,J),ASS(L,I,J),ASS1
 9998   FORMAT(10X,'OLD BSS1  NEW BSS1  OLD BSS2  NEW BSS2',3X,
     .      '  PRV ASS0  OLD ASS1  NEW ASS1'/10X,4F10.2,3X,3F10.0)
      ENDIF
      DBS=(BSS2-BSS1)/HFAC
      BSSS=BSS1+DBS*Y
      BSSS=BSS1+DBS*(Y-HS(L,I,J))
      ASSS=ASS1+0.5*(BSSS+BSS(L,I,J))*(Y-HS(L,I,J))
      BT=B+BSSS
      AT=A+ASSS
      IF(KPRES.EQ.1) WPRM=PR(L,I,J)+DPRM*(Y-HS(L,I,J))
      Y=YSAVE
 25   RETURN
      END
