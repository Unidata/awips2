C MODULE FCSA222
C**************************************************************
C**************************************************************
       SUBROUTINE FLWZ22 (X1,RES)
C
C
C  THIS ROUTINE IS PART OF THE TSFP PROGRAM, VERSION 2
C  CREATED BY KONSTANTINE P. GEORGAKAKOS, HRL-NWS, AUGUST-1985
C  MODIFIED BY KONSTANTINE P. GEORGAKAKOS, UI, NOVEMBER-1986
C
C
C
C  COMPUTES THE OBSERVATION EQUATIONS OF THE FLOW MODEL
C
          DIMENSION X1(12),RES(1)
          DIMENSION PROP(6)
C
C***J.C.,890810
C      COMMON/PMOD22/ PARMS(1000)
       COMMON/PMOD22/ PARMS(70)
CJAS..added ZF(50) for uniformity of OBSZ block with CV factors use
CJAS..20020827 changed max from 50 to 744 to accommodate 31 x 24 steps
       COMMON/OBSZ22/ ZP(744),ZE(744),ZH,ZF(744)
       COMMON/LINZ22/ H(1,12)
       COMMON/STAN22/ N
       COMMON/TIMC22/ NHST
       COMMON/VARI22/ VARCHN
       COMMON/VARA22/ CFVMX
       COMMON/PARM22/ XM,ALP(6)
C
C...for debugging:
       COMMON /FDBUG/IODBUG,ITRACE,IDBALL,NDEBUG,IDEBUG(20)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_sssac/RCS/flwz22.f,v $
     . $',                                                             '
     .$Id: flwz22.f,v 1.3 2002/10/10 16:00:02 dws Exp $
     . $' /
C    ===================================================================
C
C
          IF (ITRACE .GT. 1) WRITE (IODBUG,990)
990       FORMAT(/10X,'** FLWZ22 ENTERED.')
C
          LWN=PARMS(29)
          ALP(LWN)=PARMS(29+LWN+LWN)
          XM=PARMS(30+LWN+LWN)
          M=1
C
C  RESIDUAL OUTFLOW RATE
C
          RES(M)=ZH-ALP(LWN)*X1(6+LWN)**XM
C
C  INITIALIZE H TO ZERO
C
C
          DO 18 I=1,M
          DO 18 J=1,N
 18       H(I,J)=0.
          IF(CFVMX.LE.1.E-6)VARCHN=0.
C
C  LINEARIZATION COEFFICIENT H
C
          IN=6+LWN
          Y1=X1(IN)
          YY11=Y1*Y1*CFVMX
          VARNCH=VARCHN
          IF(VARNCH.GT.YY11) VARNCH=YY11
          IF(Y1.GT.0.)H(M,IN)=ALP(LWN)*XM*Y1**(XM-1.)
          IF(Y1.GT.0.)H(M,IN)=H(M,IN)+ALP(LWN)*
     *    XM*(XM-1.)*(XM-2.)/2.*VARNCH*Y1**(XM-3.)
          IF(Y1.GT.0.)RES(M)=RES(M)-ALP(LWN)*XM*(XM-1.)/2.
     *    *VARNCH*Y1**(XM-2.)
C
C
          IF (ITRACE .GT. 1) WRITE (IODBUG,991)
991       FORMAT(/10X,'** EXIT FLWZ22.')
C
          RETURN
          END
