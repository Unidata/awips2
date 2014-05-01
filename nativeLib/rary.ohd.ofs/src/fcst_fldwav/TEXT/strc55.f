C MEMBER STRC55
C  This subroutine stores dam rating curve infor in rhi,qhi arrays
C    (KRCH=11,21,12,22,13,23,16,17)
C=======================================================================
      SUBROUTINE STRC55(PO,LRAT,RHI,RQI,NRCP,KRCH,LAD,NUMLAD,
     .  JN,K1,K2,K16)
      INCLUDE 'common/fratng'
      INCLUDE 'common/fdbug'
      COMMON/IONUM/IN,IPR,IPU
      DIMENSION KRCH(K2,K1),NUMLAD(K1),LAD(K16,K1),NRCP(K16,K1)
      DIMENSION PO(1),RHI(112,K16,K1),RQI(112,K16,K1)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_fldwav/RCS/strc55.f,v $
     . $',                                                             '
     .$Id: strc55.f,v 1.3 2004/02/02 21:54:02 jgofus Exp $
     . $' /
C    ===================================================================
C
C
C
C     WRITE(IPR,777) IODBUG
C 777 FORMAT(//3X,25(1H=),' IODBUG=',I2,1X,25(1H=))

      LORC=LRAT
      DO 100 J=1,JN
        IF(NUMLAD(J).EQ.0) GO TO 100
        NUM=NUMLAD(J)
        DO 80 I=1,NUM
          NRCP(I,J)=0
          LD=LAD(I,J)
          KR=KRCH(LD,J)
          IF(KR.GT.23) GO TO 80
          IF(KR.GT.17.AND.KR.LT.21) GO TO 80
          IF(KR.LT.11) GO TO 80
          IF(KR.EQ.14.OR.KR.EQ.15) GO TO 80
          WRITE(IODBUG,899) LORC
 899      FORMAT(/5X,'LORC=',I5)
          CALL FGETRC(PO(LORC),ISW)
          NRCP(I,J)=NRCPTS
          WRITE(IODBUG,900) I,J,NRCP(I,J)
 900      FORMAT(/5X,'RATING CURVE AT DAM NO. ',I3,' ON RIVER NO.',I3,
     .            3X,I3,' POINTS')
          IF(GZERO.LE.-999.) GZERO=0.
          DO 10 L=1,NRCPTS
            RHI(L,I,J)=XRC(LOCH+L-1)+GZERO
            RQI(L,I,J)=XRC(LOCQ+L-1)
 10       CONTINUE
          WRITE(IODBUG,902) (RHI(L,I,J),L=1,NRCPTS)
 902      FORMAT(3X,'RHI=',10F10.2)
          WRITE(IODBUG,904) (RQI(L,I,J),L=1,NRCPTS)
 904      FORMAT(3X,'RQI=',10F10.2)
          LORC=LORC+2
 80     CONTINUE
 100  CONTINUE
      RETURN
      END




