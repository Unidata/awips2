C MEMBER SECT21
C  (from old member FCSECT21)
C
      SUBROUTINE SECT21(AS,BS,HS,ASS,BSS,HSS,NCSSS,NCSS1,NB,J,I,Y,K7,K9)
C
C      THIS SUBROUTINE COMPUTES THE CROSS SECTIONAL AREAS AND TOP WIDTHS
C
C           THIS SUBROUTINE WAS WRITTEN ORIGINALLY BY:
C           DR. DANNY FREAD   HRL   APRIL 1978
C
C           THIS SUBROUTINE WAS MODIFIED TO MEET VER. NO. 5 STANDARDS
C           OF THE NWSRFS BY:
C           JANICE LEWIS      HRL   NOVEMBER,1982     VERSION NO. 1
C
      COMMON/FDBUG/IODBUG,ITRACE,IDBALL,NDEBUG,IDEBUG(20)
      COMMON/SS21/ NCS,A,B,DB,R,DR,AT,BT,NCSS,P,DP,ZH,NP,NPEND
C
      DIMENSION AS(K7,1),BS(K7,1),HS(K7,1)
      DIMENSION ASS(K9,1),BSS(K9,1),HSS(K9,1)
      DIMENSION NCSSS(1),NCSS1(1),SNAME(2),NB(1)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_dwoper/RCS/sect21.f,v $
     . $',                                                             '
     .$Id: sect21.f,v 1.1 1995/09/17 18:56:17 dws Exp $
     . $' /
C    ===================================================================
C
C
      DATA SNAME/4HSECT,4H21  /
C
      CALL FPRBUG(SNAME,1,21,IBUG)
C
      LIJ=LCAT21(I,J,NB)
      LLJ=LCAT21(1,J,NCSS1)-1
      DO 20 K=2,NCS
      KT=K
      IF(Y-HS(K,LIJ)) 10,10,20
   10 CONTINUE
      GO TO 30
   20 CONTINUE
   30 KL=KT-1
      DB=(BS(KT,LIJ)-BS(KL,LIJ))/(HS(KT,LIJ)-HS(KL,LIJ))
      B=BS(KL,LIJ)+DB*(Y-HS(KL,LIJ))
      A=AS(KL,LIJ)+0.5*(B+BS(KL,LIJ))*(Y-HS(KL,LIJ))
C     IF(I.EQ.7) WRITE(1,9999) A,AS(KL,LIJ),B,BS(KL,LIJ),Y,HS(KL,LIJ)
C9999 FORMAT(3X,2HA=,F15.4,5X,3HAS=,F15.4/3X,2HB=,F15.4,5X,3HBS=,F15.4/
C    1          2HY=,F15.4,5X,3HHS=,F15.4)
      IF(NCSS) 130,130,60
   60 NCSJ=NCSS1(J)
      IF (NCSJ.EQ.0) GO TO 130
      DO 120 L=1,NCSJ
      IF(I-NCSSS(L+LLJ)) 130,70,120
   70 LKJ=LLJ+L
      DO 100 K=2,NCSS
      KT=K
      IF(Y-HSS(1,LKJ)) 130,130,80
   80 IF(Y-HSS(K,LKJ)) 90,90,100
   90 CONTINUE
      GO TO 110
  100 CONTINUE
  110 KL=KT-1
      DBS=(BSS(KT,LKJ)-BSS(KL,LKJ))/(HSS(KT,LKJ)-HSS(KL,LKJ))
      BSSS=BSS(KL,LKJ)+DBS*(Y-HSS(KL,LKJ))
      ASSS=ASS(KL,LKJ)+0.5*(BSSS+BSS(KL,LKJ))*(Y-HSS(KL,LKJ))
      GO TO 140
  120 CONTINUE
  130 BSSS=0.0
      ASSS=0.0
  140 AT=A+ASSS
      BT=B+BSSS
C
      IF(ITRACE.EQ.1) WRITE(IODBUG,9000) SNAME
 9000 FORMAT(1H0,2H**,1X,2A4,8H EXITED.)
C
      RETURN
      END
