C MODULE FCDV22
C***********************************************************************
C***********************************************************************
       SUBROUTINE DERU22 (FM)
C
C
C  THIS IS PART OF THE TSFP PROGRAM, VERSION 2,
C  CREATED BY KONSTANTINE P. GEORGAKAKOS, HRL-NWS, AUGUST-1985.
C
C
       COMMON/PRM522/ RX1M,RX2M,RX1,RAT1,Y413,Y713
       COMMON/PRM622/ LWN,BT1,BT2,PROP(6)
C
C...for debugging:
       COMMON /FDBUG/IODBUG,ITRACE,IDBALL,NDEBUG,IDEBUG(20)
C
          DIMENSION FM(12,2),DDUC(1)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_sssac/RCS/deru22.f,v $
     . $',                                                             '
     .$Id: deru22.f,v 1.2 2002/05/15 13:54:53 hank Exp $
     . $' /
C    ===================================================================
C
C
C  DERIVATIVES OF THE DYNAMIC EQUATIONS OF THE SOIL-
C  CHANNEL MODELS
C
          IF (ITRACE .GT. 1) WRITE (IODBUG,990)
990       FORMAT(/10X,'** DERU22 ENTERED.')
C
          FM(1,1)=(1.-RX1M)
          FM(2,1)=RX1M*(1.-RX2M)
          FM(6,1)=(1.-RAT1*RAT1)*RX1M
     *    *(1.-RX2M)
          DDUC(1)=(BT2+RAT1*RAT1*RX1M*BT1+
     *    RX1M*RX2M*(1.-BT1-BT2)+(1.-RAT1*RAT1)*RX2M
     *    *RX1M*BT1)
         DO 160 I=1,LWN
         J=I+6
 160      FM(J,1)=PROP(I)*DDUC(1)
C
C  DERIVATIVES WITH RESPECT TO EVAPOTRANSPIRATION
C
         FM(1,2)=-RX1
         FM(3,2)=-(1.-RX1)*Y413
         FM(6,2)=-(1.-RX1)*Y713
 175      CONTINUE
C
         IF (ITRACE .GT. 1) WRITE (IODBUG,991)
991      FORMAT(/10X,'** EXIT DERU22.')
C
         RETURN
         END
