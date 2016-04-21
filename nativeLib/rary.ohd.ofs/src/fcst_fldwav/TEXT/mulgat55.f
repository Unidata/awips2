C    THIS SUB DEALS WITH MULTIPLE MOVEBLE GATE
      SUBROUTINE MULGAT55(KL,J,NG,GSIL,GWID,TGHT,GHT,HU,HD,QG,DQGU,
     . DQGD,K1,K19,K20,K21)

      COMMON/GT55/KCG,NCG
      COMMON/M155/NU,JN,JJ,KIT,G,DT,TT,TIMF,F1
      INCLUDE 'common/fdbug'

      DIMENSION NG(K20,K1),GSIL(K19,K20,K1),GWID(K19,K20,K1)
      DIMENSION TGHT(K21,K19,K20,K1),GHT(K21,K19,K20,K1)
      CHARACTER*8 SNAME
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_fldwav/RCS/mulgat55.f,v $
     . $',                                                             '
     .$Id: mulgat55.f,v 1.2 2004/02/02 21:53:36 jgofus Exp $
     . $' /
C    ===================================================================
C

      DATA SNAME/'MULGAT55'/
C
      CALL FPRBUG(SNAME,1,55,IBUG)

C     HU     ----    WATER ELEVATION UNSTREAM OF THE GATES
C     HD     ----    WATER ELEVATION DOWNSTREAM OF THE GATES
C     QG     ----    TOTAL DISCHARGE THROUGH ALL GATES AT TIME TT
C     DQGU   ----    (DQU/DH) FOR TIME U
C     DQGD   ----    (DQG/DH) FOR TIME D
C     CS     ----    SUBMERGENCE COEFFICIENT

      QG=0.0
      DQGU=0.0
      DQGD=0.0
      CS=1.0
      NUMG=NG(KL,J)
      IF (NUMG.EQ.0) GOTO 999
      DO 200 K=1,NUMG
      QGK=0.0
      DQGUK=0.0
      DQGDK=0.0
      BOTTOM=GSIL(K,KL,J)
      HEAD=HU-BOTTOM
      DOWN=HD-BOTTOM
      IF (HEAD.LE. 0.0) GOTO 200
      IF (DOWN.GE.HEAD) GOTO 200
           RX=DOWN/HEAD
           IF (RX.LE.0.67) THEN
           CS=1.0
           ELSE
           CS=1.0-27.83*(RX-0.67)**3
           ENDIF
C     TIME DEPENDENT OPENNING OF GATE K      
            IF (TT.LE.TGHT(1,K,KL,J)) THEN
            GOPEN=GHT(1,K,KL,J)
            GOTO 160
            ENDIF
            IF (TT.GE.TGHT(KCG,K,KL,J)) THEN
            GOPEN=GHT(KCG,K,KL,J)
            GOTO 160
            ENDIF
      DO 120 I=2,KCG
      IF (TT.LT.TGHT(I,K,KL,J)) GOTO 130
120   CONTINUE  
130   DGOPEN=(GHT(I,K,KL,J)-GHT(I-1,K,KL,J))*(TT-TGHT(I-1,K,KL,J))/
     .         (TGHT(I,K,KL,J)-TGHT(I-1,K,KL,J))      
      GOPEN=GHT(I-1,K,KL,J)+DGOPEN
160   CONTINUE
C-------------------  WEIR FLOW FOR K-TH GATE  ------------------------
      IF (GOPEN.LE.HEAD) GOTO 180
      QGK=3.1*CS*GWID(K,KL,J)*HEAD**1.5
      DQGUK=4.65*CS*GWID(K,KL,J)*HEAD**0.5
      DQGDK=0.0
      GOTO 190
C-------------------  GATE FLOW FOR K-TH GATE  ------------------------
180   QGK=8.02*CS*GOPEN*GWID(K,KL,J)*HEAD**0.5
      DQGUK=4.01*CS*GOPEN*GWID(K,KL,J)/(HEAD**0.5)
      DQGDK=0.0
190   QG=QG+QGK
      DQGU=DQGU+DQGUK
      DQGD=DQGD+DQGDK
200   CONTINUE

999   RETURN
      END
