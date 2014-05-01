C MEMBER FRCT21
C  (from old member FCFRCT21)
C
      SUBROUTINE FRCT21(NRCM1,NCM,CM,YQCM,J,I1,YQ,CMM,DCM,K6)
C
C      THIS SUBROUTINE SETS UP MANNING'S N VALUES
C
C           THIS SUBROUTINE WAS WRITTEN ORIGINALLY BY:
C           DR. DANNY FREAD   HRL   APRIL 1978
C
C           THIS SUBROUTINE WAS MODIFIED TO MEET VER. NO. 5 STANDARDS
C           OF THE NWSRFS BY:
C           JANICE LEWIS      HRL   NOVEMBER,1982     VERSION NO. 1
C
      COMMON/FDBUG/IODBUG,ITRACE,IDBALL,NDEBUG,IDEBUG(20)
      COMMON/M421/NRCM,NCML
      COMMON/XCED21/NBDXCD,NCMXCD,NFRXCD,NICXCD,NINXCD,NONXCD,MTXDV
C
      DIMENSION NCM(1),NRCM1(1),CM(K6,1),YQCM(K6,1),SNAME(2)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_dwoper/RCS/frct21.f,v $
     . $',                                                             '
     .$Id: frct21.f,v 1.1 1995/09/17 18:56:03 dws Exp $
     . $' /
C    ===================================================================
C
C
      DATA SNAME/4HFRCT,4H21  /
C
C
      CALL FPRBUG(SNAME,1,21,IBUG)
C
      NRCM=NRCM1(J)
      M=NCML
      DCM=0.
      L1J=LCAT21(1,J,NRCM1)-1
      DO 5 KK=1,NRCM
      K=KK
      LLKJ=K+L1J
      IF(I1-NCM(KK+L1J)) 6,5,5
    5 CONTINUE
C
C        IF TABLE HAS BEEN EXCEEDED, SET MANNING'S N TO THE UPPER
C        OR LOWER LIMIT
C
    6 IF(YQ.LT.YQCM(1,LLKJ).OR.YQ.GT.YQCM(M,LLKJ)) NFRXCD=NFRXCD+1
      IF(YQ.LT.YQCM(1,LLKJ))CMM=CM(1,LLKJ)
      IF(YQ.LT.YQCM(1,LLKJ)) GO TO 20
      IF(YQ.GT.YQCM(M,LLKJ))CMM=CM(M,LLKJ)
      IF(YQ.GT.YQCM(M,LLKJ)) GO TO 20
C
C        INTERPOLATE TO GET MANNING'S N
C
      DO 10 L=2,M
      IF(YQ.GT.YQCM(L,LLKJ)) GO TO 10
      GO TO 15
   10 CONTINUE
   15 DCM=(CM(L,LLKJ)-CM(L-1,LLKJ))/(YQCM(L,LLKJ)-YQCM(L-1,LLKJ))
      CMM=CM(L-1,LLKJ)+DCM*(YQ-YQCM(L-1,LLKJ))
   20 CONTINUE
C
      IF(ITRACE.EQ.1) WRITE(IODBUG,9000) SNAME
 9000 FORMAT(1H0,2H**,2A4,8H EXITED.)
      RETURN
      END
