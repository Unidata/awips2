C MODULE FCSA122
C		UPDATED 01/05/01 KP GEORGAKAKOS FOR SS-SAC VERSION 1.6
C***********************************************************************
C***********************************************************************
       SUBROUTINE FLWS22 (T,Y,DY)
C
C
C  THIS ROUTINE IS PART OF THE TSFP PROGRAM, VERSION 2
C  CREATED BY KONSTANTINE P. GEORGAKAKOS, HRL-NWS, AUGUST-1985
C
C  MODIFIED BY KONSTANTINE P. GEORGAKAKOS, UI, NOV-1986
C  FOR INCLUSION IN THE WMO, VANCOUVER INTERCOMPARISON PROJECTS
C
C
C  COMPUTES THE STATE MEAN AND STATE COVARIANCE
C
C  DERIVATIVES FOR THE NWSRFS SOIL- CHANNEL MODEL
C
C
          DIMENSION Y(91),DY(91),TEMP(7),FMF(12,2)
          DIMENSION UCDR(7),QUR(2),DUCX(20),DBTA(20),DXMI(20)
          DIMENSION FM(12,2),QQ(12,12),AP(20,20),ETEM(20,20)
          DIMENSION ETEMA(20,20),ETEMMA(20,20)
C
CKPG 01/05/01 ADDED ZFAC TO COMMON BLOCK OBSF22
       COMMON/OBSF22/ PREC,UE,ZHH,ZFAC
C***J.C.,890810
C      COMMON/PMOD22/ PARMS(1000)
       COMMON/PMOD22/ PARMS(70)
       COMMON/ADSP22/ A(12,12)
       COMMON/SPDN22/ Q(12)
       COMMON/VARI22/ VARCHN
       COMMON/PRM522/ RX1M,RX2M,RX1,RAT1,Y413,Y713
       COMMON/PRM622/ LWN,BT1,BT2,PROP(6)
       COMMON/VARA22/ CFVMX
       COMMON/PARM22/ XM,ALP(6)
       COMMON/SOIL22/ UC
       COMMON/CVPR22/ ALINP,ALPAR,PSTDV(20)
C
C...for debugging:
       COMMON /FDBUG/IODBUG,ITRACE,IDBALL,NDEBUG,IDEBUG(20)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_sssac/RCS/flws22.f,v $
     . $',                                                             '
     .$Id: flws22.f,v 1.2 2002/05/15 13:52:31 hank Exp $
     . $' /
C    ===================================================================
C
C
C  RETRIEVE RELEVANT PARAMETERS FROM PARMS-ARRAY
C
          IF (ITRACE .GT. 1) WRITE (IODBUG,990)
990       FORMAT(/10X,'** FLWS22 ENTERED.')
C
          DT=1.
          IPVARI=1
          QURAT=PARMS(11)
          X10=PARMS(12)
          X20=PARMS(13)
          X30=PARMS(14)
          X40=PARMS(15)
          X50=PARMS(16)
          DU=PARMS(17)
          DLPR=PARMS(18)
          DLDPR=PARMS(19)
          EPS=PARMS(20)
          THSM=PARMS(21)
C
          PF=PARMS(22)
          XMIOU=PARMS(23)
          BT1=PARMS(24)
          BT2=PARMS(25)
          XM1=PARMS(26)
          XM2=PARMS(27)
          XM3=PARMS(28)
          LWN=PARMS(29)
          N = 6+LWN
          M=1
          DO 11 I=1,LWN
 11       PROP(I)=PARMS(29+I)
          DO 12 I=1,LWN
 12       ALP(I)=PARMS(29+LWN+I)
          ICURR=29+LWN+LWN
          XM=PARMS(ICURR+1)
          DO 131 I=1,12
          DO 131 J=1,2
          QUR(J)=0.
 131      FM(I,J)=0.
          IF(QURAT.LE.0.) GO TO 14
C
         
CKPG ADDED FACTOR ZFAC TO MODIFY THE CV OF MAPF
          QUR(1)=ZFAC*PARMS(ICURR+2)*PREC+PARMS(ICURR+3)
C
          QUR(2)=PARMS(ICURR+4)*UE+PARMS(ICURR+5)
 14       CONTINUE
C
C  SOIL STATES DYNAMIC EQUATIONS
C
          RX1M=0.
          RX2M=0.
          RX3M=0.
          C11=DLPR*X40
          C1=C11+DLDPR*X50
C
          RX2=Y(2)/X20
         IF(RX2.LE.0.)RX2=0.
          C2=C11/C1
          RX3=Y(3)/X30
         IF(RX3.LE.0.)RX3=0.
          YLC=1.-(Y(3)+Y(4)+Y(5))/(X30+X40+X50)
          IF(YLC.LE.0.) YLC=0.
          D3=C1*RX2*(1.+EPS*YLC**THSM)
          D4=C1*RX2*EPS*THSM*YLC**(THSM-1.)/(X30+X40+X50)
          IF(Y(3).GE.0.0 .AND. RX3.GE.1.0E-04)RX3M=RX3**XM3
          D5=D4*(1.-PF)*(1.-RX3M)
          D6=1.-(1.-PF)*(1.-RX3M)
          RX5=Y(5)/X50
          RX4=Y(4)/X40
         IF(RX4.LE.0.)RX4=0.
         IF(RX5.LE.0.)RX5=0.
          D7=(C2*RX5-1.)*RX4+1.
          D8=D4*D6
          D9=D8*(1.-D7)
          D8=D8*D7
          D10=0.
          IF(RX2.GT.0.)D10=C1*(1.+EPS*YLC**THSM)/X20
          RX1=Y(1)/X10
          IF(RX1.LE.0.)RX1=0.
C
C***K.G.: to avoid problems due to floating-point underflow
C         IF(Y(2).GE.0.)RX2M=RX2**XM2
          IF(Y(2).GE.0. .AND. RX2.GE.1.0E-04) RX2M=RX2**XM2
C
          IF(Y(1).GE.0.0 .AND. RX1.GE.1.0E-04)RX1M=RX1**XM1
          RAT1=Y(6)/X30
          IF(RAT1.LE.0.)RAT1=0.
          DF1=1.-RAT1*RAT1
          DF5=RX2M*RX1M
          DF9=1.-RAT1*RAT1*RX1M
          DF51=DF5*PREC
          DF91=DF9*PREC
          DY(1)=(1.-RX1M)*PREC-UE*RX1
          DY(2)=RX1M*(1.-RX2M)*PREC-DU*Y(2)-D3
          DY(3)=D3*(1.-D6)-UE*(1.-RX1)*Y(3)/(X10+X30)
          DY(4)=-DLPR*Y(4)+D6*D3*D7
          DY(5)=-DLDPR*Y(5)+D6*D3*(1.-D7)
          DY(6)=DF1*(1.-RX2M)*RX1M*PREC-
     *    UE*(1.-RX1)*Y(6)/(X30+X10)
C
C  CHANNEL INPUT
C
          UC1=(DU*Y(2)+(DLPR*Y(4)+DLDPR*Y(5))/(1.+XMIOU))*
     *    (1.-BT1-BT2)
          UC2=BT2*PREC
          UC3=RAT1*RAT1*PREC*
     *    RX1M*BT1
          UC4=DF51*(1.-BT1-BT2)
          UC5=DF1*DF51*BT1
          UC=UC1+UC2+UC3+UC4+UC5
          IF(UC.LE.0.) UC=0.
C
C
C  CHANNEL STATES DYNAMIC EQUATIONS
C
          DY(7)=PROP(1)*UC
          IF(Y(7).GE.0.)DY(7)=DY(7)-ALP(1)*Y(7)**XM
          IF(LWN.LE.1)GOTO 1551
          DO 155 ICHN=2,LWN
          IN=6+ICHN
          DY(IN)=PROP(ICHN)*UC
          IF(Y(IN-1).GE.0.)DY(IN)=DY(IN)+ALP(ICHN-1)*
     *    Y(IN-1)**XM
          IF(Y(IN).GE.0.)DY(IN)=DY(IN)-ALP(ICHN)*Y(IN)**XM
 155      CONTINUE
 1551     CONTINUE
C
          TEMYPH=PREC
C
C    L I N E A R I Z A T I O N
C
C
C  INITIALIZE A AND CHANNEL INFLOW DERIVATIVES TO ZERO
C
          DO 16 I=1,N
          DO 16 J=1,N
 16       A(I,J)=0.0
          DO 17 I=1,6
 17       UCDR(I)=0.0
C
C  SOIL MODEL
C
         CNTM=0.
C
C **GFS - CHECK FOR Y(1) LT E-04 - TO AVOID FLOATING PT UNDERFLOW
         IF(Y(1).GT.1.0E-04)CNTM=Y(1)**(XM1-1.)
          A(1,1)=-XM1*CNTM/X10**XM1*PREC
         IF(RX1.GT.0.)A(1,1)=A(1,1)-UE/X10
          A(2,1)=XM1*CNTM/X10**XM1*PREC
     *    *(1.-RX2M)
          CNTM=0.
C **GFS - CHECK FOR Y(2) LT E-04 - TO AVOID FLOATING PT UNDERFLOW
          IF(Y(2).GT.1.0E-04)CNTM=Y(2)**(XM2-1.)
          A(2,2)=-RX1M*XM2*CNTM/X20**XM2*PREC
     *    -DU-D10
          A(2,3)=D4
          A(2,4)=D4
          A(2,5)=D4
          IF(RX1.GT.0.)A(3,1)=UE*Y(3)/X10/(X10+X30)
          A(3,2)=D10*(1.-D6)
         CNTM=0.
C **GFS - CHECK FOR Y(3) LT E-04 - TO AVOID FLOATING PT UNDERFLOW
         IF(Y(3).GT.1.0E-04)CNTM=Y(3)**(XM3-1.)
          A(3,3)=-D3*(1.-PF)*XM3*CNTM
     *    /X30**XM3
     *    -UE*(1.-RX1)/(X10+X30)-D5
          A(3,4)=-D5
          A(3,5)=-D5
          A(4,2)=D10*D6*D7
          A(4,3)=D3*(1.-PF)*D7*XM3*CNTM
     *    /X30**XM3-D8
         A(4,4)=-DLPR-D8
         IF(RX4.GT.0.)A(4,4)=A(4,4)+D3*D6*(C2*RX5-1.)/X40
         A(4,5)=-D8
         IF(RX5.GT.0.)A(4,5)=A(4,5)+D3*D6*C2/X50*RX4
          A(5,2)=D10*D6*(1.-D7)
          A(5,3)=D3*(1.-PF)*(1.-D7)*XM3*CNTM/X30**XM3
     *    -D9
         A(5,4)=-D9
         IF(RX4.GT.0.)A(5,4)=A(5,4)+D6*D3*(1.-C2*RX5)/X40
         A(5,5)=-DLDPR-D9
         IF(RX5.GT.0.)A(5,5)=A(5,5)-D3*D6*C2*RX4/X50
          DF2=DF1*RX2M
          DF3=DF1*RX1M
          DF4=DF2*RX1M
         CNTM1=0.
         CNTM=0.
C **GFS - CHECK FOR Y(1), Y(2) LT E-04 - TO AVOID FLOATING PT UNDERFLOW
         IF(Y(1).GT.1.0E-04)CNTM1=Y(1)**(XM1-1.)
         IF(Y(2).GT.1.0E-04)CNTM=Y(2)**(XM2-1.)
          PHIY=PREC
          A(6,1)=DF1*(1.-RX2M)*XM1*CNTM1/X10**XM1
     *    *PHIY
C
          IF(RX1.GT.0.)A(6,1)=A(6,1)+UE*Y(6)/(X30+X10)/X10
          A(6,2)=-DF1*RX1M*PHIY*XM2*CNTM
     *    /X20**XM2
          A(6,6)=-UE*(1.-RX1)/(X30+X10)
          A(6,6)=A(6,6)-2.*RAT1/X30*(1.-RX2M)
     *    *RX1M*PHIY
C
C  CHANNEL ROUTING MODEL
C
          UCDR(1)=XM1*CNTM1/X10**XM1*RAT1
     *    *RAT1*BT1*PHIY
     *    +PHIY*XM1*CNTM1/X10**XM1
     *    *RX2M*(1.-BT1-BT2)+XM1*CNTM1/X10**XM1
     *    *DF1*RX2M*PHIY*BT1
          UCDR(2)=DU*(1.-BT1-BT2)+XM2*CNTM/X20
     *    **XM2*PHIY*RX1M*(1.-BT1-BT2)
     *    +XM2*CNTM/X20**XM2*PHIY*BT1*DF3
          TMPRA=(1.-BT1-BT2)/(1.+XMIOU)
          UCDR(4)=TMPRA*DLPR
          UCDR(5)=TMPRA*DLDPR
          UCDR(6)=2.*RAT1/X30*PHIY*RX1M*BT1*(1.-RX2M)
          N2 = 6+LWN
          DO 160 ICHN=7,N2
          DO 160 INPUT=1,6
          A(ICHN,INPUT)=PROP(ICHN-6)*UCDR(INPUT)
 160      CONTINUE
          IP=0
          K=N
          DO 170 I=1,N
C***910319, J.C. FIX PROBLEM WITH PROCESSING UPPER TRIANGLE OF MATRIX
C         DO 170 J=1,N
          DO 170 J=I,N
          K=K+1
          IF(I.NE.J) GO TO 170
          IF(I.LE.6) GO TO 170
          IP=IP+1
          UCDR(IP)=Y(K)
 170      CONTINUE
          DO 175 ICHN=1,LWN
          IN=6+ICHN
          IF(Y(IN).LE.0.)GO TO 173
          CVART2=ABS(UCDR(ICHN)/Y(IN)/Y(IN))
          IF(CVART2.GT.CFVMX) CVART2=CFVMX
          DY(IN)=DY(IN)-ALP(ICHN)*CVART2*XM*(XM-1.)
     *    /2.*Y(IN)**XM
 173      CONTINUE
          IF(ICHN.LE.1) GO TO 175
          IF(Y(IN-1).LE.0.)GO TO 175
          CVART2=ABS(UCDR(ICHN-1)/Y(IN-1)/Y(IN-1))
          IF(CVART2.GT.CFVMX) CVART2=CFVMX
          DY(IN)=DY(IN)+ALP(ICHN-1)*CVART2
     *    *XM*(XM-1.)/2.*Y(IN-1)**XM
 175      CONTINUE
          DO 180 ICHN=1,LWN
          IN=ICHN+6
          YY1=Y(IN)
          YY2=Y(IN-1)
          YY12=YY1*YY1*CFVMX
          YY22=YY2*YY2*CFVMX
          CDRU=ABS(UCDR(ICHN))
          CDRU1=0.
          IF(ICHN.GT.1) CDRU1=ABS(UCDR(ICHN-1))
          IF(CDRU.GT.YY12) CDRU=YY12
          IF(CDRU1.GT.YY22) CDRU1=YY22
          IF(YY1.GT.0.)A(IN,IN)=-ALP(ICHN)*XM*Y(IN)**(XM-1.)
          IF(YY1.GT.0.)A(IN,IN)=A(IN,IN)-ALP(ICHN)*XM*(XM-1.)
     *    *(XM-2.)/2.*CDRU*Y(IN)**(XM-3.)
          IF(ICHN.EQ.1) GO TO 180
          IF(YY2.GT.0.)A(IN,IN-1)=ALP(ICHN-1)*XM*YY2**(XM-1.)
          IF(YY2.GT.0.)A(IN,IN-1)=A(IN,IN-1)+ALP(ICHN-1)
     *    *XM*(XM-1.)*(XM-2.)/2.*CDRU1*Y(IN-1)
     *    **(XM-3.)
 180      CONTINUE
C
C  SAVE THE VARIANCE OF THE LAST ROUTER STATE FOR OUTPUT
C  LINEARIZATION
C
          VARCHN=ABS(UCDR(LWN))
C
C  COMPUTE DERIVATIVES WITH RESPECT TO INPUT
C
          Y413=Y(3)/(X10+X30)
          Y713=Y(6)/(X10+X30)
C
         IF(QURAT.GT.0.)CALL DERU22 (FM)
C
         DO 185 I=1,N
         DO 185 IU=1,2
 185      FMF(I,IU)=FM(I,IU)*FM(I,IU)*QUR(IU)*QUR(IU)
          DO 46 I=1,N
         U2=0.
         DO 45 IU=1,2
 45       U2=U2+FMF(I,IU)
C
         QQ(I,I)=Q(I)+U2*DT*ALINP
 46       CONTINUE
C
         IF(N.LE.1) GO TO 50
         DO 49 I=2,N
         I1=I-1
         DO 48 J=1,I1
         U2=0.
         DO 47 ICN=1,2
 47      U2=U2+FM(I,ICN)*FM(J,ICN)*QUR(ICN)*QUR(ICN)
         QQ(I,J)=U2*DT*ALINP
         QQ(J,I)=QQ(I,J)
 48      CONTINUE
 49      CONTINUE
 50      CONTINUE
C
C  DEFINE THE STATE-VARIANCE PROPAGATION DIFFERENTIAL EQUATION
C
C
         IF(IPVARI.LE.0)GO TO 1860
C
C  COMPUTE DERIVATIVES OF SOIL STATES DYNAMIC EQNS. W.R.T.
C  PARAMETERS
C
      DF1X10=RX1M*XM1*PREC/X10+UE*RX1/X10
      DF2X10=-XM1*PREC*(1.-RX2M)*RX1M/X10
      DD3Y=C1*RX2*EPS*THSM*YLC**(THSM-1.0)
      DD3X20=-D3/X20
      DD3THS=C1*RX2*EPS*YLC**THSM*ALOG(THSM)
      DD3C1=RX2*(1.+EPS*YLC**THSM)
      DD3EPS=C1*RX2*YLC**THSM
      DYX30=(1.0-YLC)/(X30+X40+X50)
      DYX40=DYX30
      DYX50=DYX30
      DF2X20=RX1M*PREC*RX2M*XM2/X20-DD3X20
      DF2X30=-DD3Y*DYX30
      DC1X40=DLPR
      DC1X50=DLDPR
      DF2X40=-DD3Y*DYX40-DC1X40*DD3C1
      DF2X50=-DD3Y*DYX50-DD3C1*DC1X50
      DCDLPR=X40
      DCDLP=X50
      DF2DU=-Y(2)
      DF2DL1=-DCDLPR*DD3C1
      DF2DL2=-DCDLP*DD3C1
      DF2EPS=-DD3EPS
      DF2THS=-DD3THS
      DF3X10=-UE*Y(3)*(RX1/(X10+X30)/X10-(1.-RX1)/(X30+X10)**2)
      DF3X20=(1.-D6)*DD3X20
      DD6X30=-(1.-PF)*RX3M*XM3/X30
      DD6PF=(1.-RX3M)
C
      DF3X30=-D3*DD6X30+(1.-D6)*DD3Y*DYX30+UE*(1.-RX1)*Y(3)/
     >(X30+X10)**2
      DF3X40=-(1.-D6)*DF2X40
      DF3X50=-(1.-D6)*DF2X50
      DF3DL1=-(1.-D6)*DF2DL1
      DF3DL2=-(1.-D6)*DF2DL2
      DF3EPS=(1.-D6)*DD3EPS
      DF3THS=(1.-D6)*DD3THS
      DF3PF=-DD6PF*D3
      DF4X20=D6*D7*DD3X20
      DF4X30=D3*D7*DD6X30-D6*D7*DF2X30
      DC2X40=DLPR*DLDPR*X50/C1**2
      DC2X50=-C11/C1**2*DLDPR
      C2DLPR=DLDPR*X40*X50/C1**2
      C2DLP=-C11/C1**2*X50
      DD7X40=-(C2*RX5-1.)*RX4/X40+RX4*RX5*DC2X40
      DD7X50=-C2*RX5/X50*RX4+RX4*RX5*DC2X50
      DD7DL1=RX4*RX5*C2DLPR
      DD7DL2=RX4*RX5*C2DLP
      DF4X40=D3*D6*DD7X40-D6*D7*DF2X40
      DF4X50=D3*D6*DD7X50-D6*D7*DF2X50
      DF4DL1=-D6*D7*DF2DL1+D6*D3*DD7DL1-Y(4)
      DF4DL2=-D6*D7*DF2DL2+D6*D3*DD7DL2
      DF4EPS=D6*D7*DD3EPS
C
      DF4THS=D6*D7*DD3THS
      DF4PF=D3*D7*DD6PF
      DF5X20=(1.-D7)*D6*DD3X20
      DF5X30=(1.-D7)/D7*DF4X30
      DF5X40=-D3*D6*DD7X40-D6*(1.-D7)*DF2X40
      DF5X50=-D3*D6*DD7X50-D6*(1.-D7)*DF2X50
      DF5DL1=-D6*(1.-D7)*DF2DL1-D3*D6*DD7DL1
      DF5DL2=-D6*(1.-D7)*DF2DL2-D3*D6*DD7DL2-Y(5)
      DF5EPS=(1.-D7)/D7*DF4EPS
      DF5THS=(1.-D7)/D7*DF4THS
      DF5PF=D3*(1.-D7)*DD6PF
      DF6X10=XM1*RX1M/X10*(-DF1*(1.-RX2M)*PREC)-UE*Y(6)/
     >(X30+X10)*RX1/X10+UE*(1.-RX1)*Y(6)/(X30+X10)**2
      DF6X20=DF1*RX1M*PREC*XM2*RX2M/X20
      DDFX30=2.*RAT1*RAT1/X30
      DF6X30=RX1M*PREC*(1.-RX2M)*DDFX30+UE*(1.-RX1)*Y(6)/
     >(X30+X10)**2
C
C  DERIVATIVES OF CHANNEL STATES DYNAMIC EQUATIONS W.R.T.
C  PARAMETERS
C
      BT12=1.-BT1-BT2
C
 1860    CONTINUE
      DO 1851 INU=1,20
      DUCX(INU)=0.0
      DBTA(INU)=0.0
 1851 DXMI(INU)=0.0
C
         IF(IPVARI.LE.0)GO TO 1862
C
      DUCX(1)=-XM1*PREC*RAT1*RAT1*RX1M*BT1/X10
     >-XM1*BT12*DF51/X10-XM1*BT1*DF1*DF51/X10
      DUCX(2)=-XM2*BT12*DF51/X20-XM2*BT1*DF1*DF51/X20
      DUCX(3)=-2.*PREC*RX1M*BT1*RAT1*RAT1*(1.-RX2M)/X30
      XMU1=1.+XMIOU
      DUCX(6)=Y(2)*BT12
      DUCX(7)=Y(4)*BT12/XMU1
      DUCX(8)=Y(5)*BT12/XMU1
C
      DUCX(12)=-BT12*C1/XMU1/XMU1
      DUCX(13)=-(DU*Y(2)+C1/XMU1)+RAT1*RAT1*PREC*RX1M-DF51+
     >DF51*DF1
      DUCX(14)=-(DU*Y(2)+C1/XMU1)+PREC-DF51
      DBTA(15)=1.0
      DXMI(16)=1.0
C
 1862    CONTINUE
C
C  INITIALIZE AP MATRIX
C
      DO 186 I=1,20
      DO 186 J=1,20
186   AP(I,J)=0.0
C
         IF(IPVARI.LE.0)GO TO 1863
C
C  FILL UP AP MATRIX AP(I,J)=D(F(I))/DA(J)
C
      AP(1,1)=DF1X10
      AP(2,1)=DF2X10
      AP(2,2)=DF2X20
      AP(2,3)=DF2X30
      AP(2,4)=DF2X40
      AP(2,5)=DF2X50
      AP(2,6)=DF2DU
      AP(2,7)=DF2DL1
      AP(2,8)=DF2DL2
      AP(2,9)=DF2EPS
      AP(2,10)=DF2THS
      AP(3,1)=DF3X10
      AP(3,2)=DF3X20
      AP(3,3)=DF3X30
      AP(3,4)=DF3X40
      AP(3,5)=DF3X50
      AP(3,7)=DF3DL1
      AP(3,8)=DF3DL2
      AP(3,9)=DF3EPS
      AP(3,10)=DF3THS
      AP(3,11)=DF3PF
C     AP(4,1)=DF4X10
      AP(4,2)=DF4X20
      AP(4,3)=DF4X30
      AP(4,4)=DF4X40
      AP(4,5)=DF4X50
      AP(4,7)=DF4DL1
      AP(4,8)=DF4DL2
      AP(4,9)=DF4EPS
C
      AP(4,10)=DF4THS
      AP(4,11)=DF4PF
C     AP(5,1)=DF5X10
      AP(5,2)=DF5X20
      AP(5,3)=DF5X30
      AP(5,4)=DF5X40
      AP(5,5)=DF5X50
      AP(5,7)=DF5DL1
      AP(5,8)=DF5DL2
      AP(5,9)=DF5EPS
      AP(5,10)=DF5THS
      AP(5,11)=DF5PF
      AP(6,1)=DF6X10
      AP(6,2)=DF6X20
      AP(6,3)=DF6X30
      IF(LWN.LE.1)GOTO 1855
      DO 1852 JNU=2,LWN
      JJN=JNU+6
      JJN1=JJN-1
C
C***KG
      ALPP = ALP(JNU)
C***KG
C
      DO 1852 INU=1,20
      TEMP01=0.0
      IF(Y(JJN1).GT.0.)TEMP01=Y(JJN1)**XM
      TEMP02=0.0
      IF(Y(JJN).GT.0.)TEMP02=Y(JJN)**XM
      TEMP03=0.0
      TEMP04=0.0
      IF(Y(JJN1).GT.0.)TEMP03=TEMP01*ALOG(Y(JJN1))*ALPP
      IF(Y(JJN).GT.0.)TEMP04=TEMP02*ALOG(Y(JJN))*ALPP
      AP(JJN,INU)=TEMP01*DBTA(INU)+TEMP03*DXMI(INU)-
     >TEMP02*DBTA(INU)-TEMP04*DXMI(INU)
 1852 CONTINUE
 1855 CONTINUE
      TEMP01=0.0
      TEMP02=0.0
      IF(Y(7).GT.0.)TEMP01=Y(7)**XM
      IF(Y(7).GT.0.)TEMP02=TEMP01*ALOG(Y(7))*ALPP
      DO 1853 INU=1,20
 1853 AP(7,INU)=DUCX(INU)-TEMP01*DBTA(INU)-TEMP02*DXMI(INU)
C
         NN=LWN+6
         DO 261 JST=1,NN
         DO 261 INU=1,16
 261     ETEM(JST,INU)=AP(JST,INU)*PSTDV(INU)*PSTDV(INU)
         DO 262 JST=1,NN
         DO 262 INU=1,16
 262     ETEMA(INU,JST)=AP(JST,INU)
         DO 264 IST=1,NN
         DO 264 JST=1,NN
         ETEMMA(IST,JST)=0.
         DO 263 INU=1,16
 263     ETEMMA(IST,JST)=ETEMMA(IST,JST)+ETEM(IST,INU)*ETEMA(INU,JST)
 264     CONTINUE
         DO 265 IST=1,NN
         DO 265 JST=1,NN
 265     QQ(IST,JST)=QQ(IST,JST)+ALPAR*ETEMMA(IST,JST)
C
 1863    CONTINUE
C
C        CALL SYMM22 (20,NN,QQ)
         CALL SYMM22 (NN,QQ)
C        CALL POSD22 (20,NN,QQ)
         CALL POSD22 (NN,QQ)
C
         CALL VRPR22 (A,N,Y,DY,QQ)
C
         IF (ITRACE .GT. 1) WRITE (IODBUG,991)
991      FORMAT(/10X,'** EXIT FLWS22.')
C
          RETURN
          END
