C     THIS SUBROUTINE DETERMINES TURBINE FLOW QT FOR TIME TT
C     THE QT MAY BE REDUCED IN BRECH WHEN BREACHING IS BIG

      SUBROUTINE QTURB55(KL,J,QTD,QTT,TQT,K1,K16,NU,TT,QT)

      INCLUDE 'common/fdbug'

      DIMENSION QTD(K16,K1),QTT(NU,K16,K1),TQT(NU,K16,K1),SNAME(2)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_fldwav/RCS/qturb55.f,v $
     . $',                                                             '
     .$Id: qturb55.f,v 1.2 2004/10/18 19:06:30 jgofus Exp $
     . $' /
C    ===================================================================
C

      DATA SNAME/4HQTUR,4HB55 /
C
      CALL FPRBUG(SNAME,1,55,IBUG)

      QT=QTD(KL,J)
      IF (QT.GE.0.0) GOTO 999
      QT=QTT(1,KL,J)
      DO 150 K=2,NU
      T1=TQT(K,KL,J)
      IF (TT.LE.T1) GOTO 160
  150 CONTINUE
C jgg changed following lines to fix OB5 beta bug 10/14/04
C jgg  160 K2=K
C jgg      K1=K-1
C jgg      DQ=QTT(K2,KL,J)-QTT(K1,KL,J)
C jgg      DT=TQT(K2,KL,J)-TQT(K1,KL,J)
C jgg      QT=QTT(K1,KL,J)+(TT-TQT(K1,KL,J))*DQ/DT

  160 KK2=K
      KK1=K-1
      DQ=QTT(KK2,KL,J)-QTT(KK1,KL,J)
      DT=TQT(KK2,KL,J)-TQT(KK1,KL,J)
      QT=QTT(KK1,KL,J)+(TT-TQT(KK1,KL,J))*DQ/DT
C jgg end of changes      
  999 RETURN
      END

