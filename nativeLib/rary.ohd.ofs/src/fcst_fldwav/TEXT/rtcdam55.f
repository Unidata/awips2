      SUBROUTINE RTCDAM55(KL,J,YY,QRT,DQRT,RHI,RQI,NRCP,K1,K16)

      INCLUDE 'common/fdbug'

      DIMENSION RHI(112,K16,K1),RQI(112,K16,K1),NRCP(K16,K1),SNAME(2)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_fldwav/RCS/rtcdam55.f,v $
     . $',                                                             '
     .$Id: rtcdam55.f,v 1.2 1999/07/06 16:49:54 page Exp $
     . $' /
C    ===================================================================
C

      DATA SNAME/4HRTCD,4HAM55/
C
      CALL FPRBUG(SNAME,1,55,IBUG)

C  THIS SUBROUTINE COMPUTES FLOW (QRT, DQRT) THROUGH A RATING CURVE
C  QRT=F(YY) FROM RHI/RQI TABLES

      NR=NRCP(KL,J)
      QRT=0.0
      DQRT=0.0
      IF(YY.LE.RHI(1,KL,J)) GO TO 15
      DO 5 L=2,NR
      IF(YY.LT.RHI(L,KL,J)) GO TO 10
    5 CONTINUE
      L=NR
   10 LM=L-1
      IF(ABS(RHI(1,KL,J)).LT.0.001 .AND.
     & ABS(RHI(2,KL,J)).LT.0.001) GO TO 13
      DQRT=(RQI(L,KL,J)-RQI(LM,KL,J))/(RHI(L,KL,J)-RHI(LM,KL,J))
      QRT=RQI(LM,KL,J)+DQRT*(YY-RHI(LM,KL,J))
      GO TO 15
   13 DEP=YY-RHI(1,KL,J)
      QRT=RQI(1,KL,J)+RQI(2,KL,J)*DEP+RQI(3,KL,J)*DEP*DEP+RQI(4,KL,J)*
     1  DEP*DEP*DEP
      DQRT=RQI(2,KL,J)+2.*RQI(3,KL,J)*DEP+3.*RQI(4,KL,J)*DEP*DEP
   15 RETURN
      END
