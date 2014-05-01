C-----------------------------------------------------------------------
C            ONE OF SUBPROGRAMS FOR KALMAN FILTER
C            THIS SUB GETS STATE-RELATED CROSS SECTIONAL  DATA        
C            V(10,NS),  1=B,  2=A,  3=dB/dA, 4=n,   5=dn/dQ   
C                       6=Sf, 7=Ke, 8=B0,    9=A0,  10= 
C-----------------------------------------------------------------------
      SUBROUTINE FTMV55(PO,JR,NS,Y,V,K1,K2,K7,K8,K9,NQCM)
      COMMON/SS/ NCS,A,B,DB,R,DR,AT,BT,P,DP,ZH

      INCLUDE 'common/ofs55'

      DIMENSION PO(*),Y(2*NS),V(10,NS),NQCM(K1)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_fldwav/RCS/ftmv55.f,v $
     . $',                                                             '
     .$Id: ftmv55.f,v 1.2 2000/12/19 15:50:09 dws Exp $
     . $' /
C    ===================================================================
C

      DO 200 I=1,NS
      CALL SECT55(PO(LCPR),PO(LOAS),PO(LOBS),PO(LOHS),PO(LOASS),
     $       PO(LOBSS),JR,I,Y(2*I),PO(LCHCAV),PO(LCIFCV),K2,K9)
      V(1,I)=B
      V(2,I)=A
      V(3,I)=DB
      V(8,I)=BT-B
      V(9,I)=AT-A
      IF (I .GE. NS) GOTO 200
      NCML=ABS(NQCM(JR))
      IF (NCML.EQ. 0) NCML=NCS
      IF (NQCM(JR) .GE. 0) YQM=0.5*(Y(2*I)+Y(2*(I+1)))
      IF (NQCM(JR) .LT. 0) YQM=0.5*(Y(2*I-1)+Y(2*I+1))
      CALL FRICT(NCML,PO(LOCM),PO(LOYQCM),JR,I,YQM,
     $        V(4,I),V(5,I),K1,K7,K8)
200   CONTINUE

      DO 250 I=1,NS-1
      YYQ=0.5*(Y(2*I-1)+Y(2*I+1))
      YYA=0.5*(V(2,I)+V(2,I+1))
      YYR=(V(2,I)+V(2,I+1))/(V(1,I)+V(1,I+1))
      V(6,I)=V(4,I)**2*ABS(YYQ)*YYQ/(2.21*YYA*YYA*YYR**(4.0/3.0))
      AKE=PO(LOFKEC+I+K2*(JR-1))
      V(7,I)=AKE
250   CONTINUE
      V(4,NS)=V(4,NS-1)
      V(5,NS)=V(5,NS-1)
      V(6,NS)=V(6,NS-1)
      V(7,NS)=V(7,NS-1)

      RETURN
      END
