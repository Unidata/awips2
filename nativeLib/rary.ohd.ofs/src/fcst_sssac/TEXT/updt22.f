C MEMBER UPDT22
C
       SUBROUTINE UPDT22(IBUG,IDB)
C                             LAST UPDATE: 01/19/94.12:20:35 BY $WC20SV
C
C
C
C  THIS ROUTINE IS PART OF SS-SAC
C  CREATED BY KONSTANTINE P. GEORGAKAKOS, HRL-NWS, FALL-1985
C  MODIFIED BY KONSTANTINE P. GEORGAKAKOS, UI, NOVEMBER-1986
C
C REVISIONS:
C
C   HFS VERSION 1.3:
C 7-26-89  7:59:35 am
C  to make module compatible with IBM OS FORTRAN IV (H-EXTENDED)
C
C
          DIMENSION X(12),CX(12,12),X1(12)
          DIMENSION CX1(12,12),Z(1),R(1,1)
          DIMENSION GAIN(12,1),CINOV(1,1)
          DIMENSION TEMP1(12,12),TEMP11(12,12)
          DIMENSION TEMP2(1,1),CXX(12,12)
C
C***J.C.,890810
C      COMMON/PMOD22/PARMS(1000)
       COMMON/PMOD22/PARMS(70)
       COMMON/STTU22/ Y(91)
       COMMON/STAN22/ N
       COMMON/OBSN22/ RRR(2)
CJAS..added ZF(50) for uniformity of OBSZ block with CV factors use
CJAS..20020827 changed max from 50 to 744 to accommodate 31 x 24 steps
       COMMON/OBSZ22/ ZP(744),ZE(744),ZQ,ZF(744)
       COMMON/TIMC22/ NHST
       COMMON/SQKM22/ AREA
       COMMON/PARM22/ XM,ALP(6)
       COMMON/VARI22/ VARCHN
       COMMON/VARA22/ CFVMX
       COMMON/LINZ22/ H(1,12)
C
C***********************************************************************
C
C...for debugging:
       COMMON /FDBUG/IODBUG,ITRACE,IDBALL,NDEBUG,IDEBUG(20)
C
C...input/output logical unit numbers:
C...  IN  = for card image input
C...  IPR = for printed output
C...  IPU = for punched output
       COMMON /IONUM/IN,IPR,IPU
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_sssac/RCS/updt22.f,v $
     . $',                                                             '
     .$Id: updt22.f,v 1.3 2002/10/10 16:05:13 dws Exp $
     . $' /
C    ===================================================================
C
C
C***********************************************************************
C
       IF (ITRACE .GT. 1) WRITE (IODBUG,990)
990    FORMAT(/10X,'** UPDT22 ENTERED.')
C
C
C  SET ROW DIMENSIONS FOR STATE AND OBSERVATION RELATED MATRICES
C  FROM DIMENSION STATEMENT
C
          IS=12
          IO=1
          M=1
C
C  CONVERT A 1-D VECTOR TO A N-VECTOR AND A NXN-SYMMETRIC MATRIX
C
         CALL CNVR22 (0,N,Y,X1,CX1)
C
         IF(IDB .LE. 0) GO TO 816
         DO 815 I=1,N
 815     WRITE(IPR,120) (CX1(I,J),J=1,N)
 816     CONTINUE
C
C
C  FORM RESIDUAL Z AND COEFFICIENT MATRIX H
C
         CALL FLWZ22 (X1,Z)
C
         NR=N-6
         ZQPRD=ALP(NR)*Y(N)**XM
         IF(IDB .GT. 0) WRITE(IPR,111) ZQ,ZQPRD
 111     FORMAT(/12X,'UPDATE- DISCHARGE OBS IN MM/DT: ',F8.3,
     *   '  PREDICTION: ',F8.3)
 2       CONTINUE
C
C  DEFINE THE COVARIANCE MATRIX OF THE OBSERVATIONS
C
         DO 44 I=1,M
         DO 42 J=1,M
         R(I,J)=0.
 42      CONTINUE
 44      CONTINUE
C
          R(M,M)=RRR(M)
C
         DO 914 IJ=1,M
         IF(IDB .GT. 0) WRITE(IPR,913)(H(IJ,IK),IK=1,N)
 913     FORMAT(/12X,'UPDATE- H: ',6F8.3)
 914     CONTINUE
C
C
         IF(IDB .GT. 0) WRITE(IPR,112)(R(I,I),I=1,M)
 112     FORMAT(/12X,'UPDATE- MATRIX R DIAGONAL: ',5F8.3)
C
C
C
C  COMPUTE INNOVATIONS COVARIANCE MATRIX
C
          CALL VMFP22 (CX1,H,N,N,M,IS,IO,TEMP1,IS)
C
          CALL VMFF22 (H,TEMP1,M,N,M,IO,IS,CINOV,IO)
C
          DO 5 I=1,M
          DO 5 J=1,M
  5          CINOV(I,J)=CINOV(I,J)+R(I,J)
C
C
         IF(IDB .LE. 0) GO TO 115
         DO 114 I=1,M
 114     WRITE(IPR,113) (CINOV(I,J),J=1,M)
 113     FORMAT(/12X,'UPDATE- INNOV. VARIANCE: ',F8.3)
 115     CONTINUE
C
C
C  INVERT INNOVATIONS COVARIANCE MATRIX
C
          TEMP2(1,1)=1./CINOV(1,1)
C
C  COMPUTE THE FILTER GAIN MATRIX
C
C
         CALL VMFF22 (TEMP1,TEMP2,N,M,M,IS,IO,GAIN,IS)
C
         DO 917 IJ=1,N
         IF (IDB .GT. 0) WRITE(IPR,918)(GAIN(IJ,IK),IK=1,M)
 918     FORMAT(/12X,'UPDATE- GAIN: ',5F8.3)
 917     CONTINUE
C
C
C  COMPUTE THE FILTERED STATE-COVARIANCE MATRIX
C
C
          CALL VMFF22 (GAIN,H,N,M,N,IS,IO,TEMP1,IS)
C
          DO 40 I=1,N
          DO 40 J=1,N
          IF(I.NE.J)TEMP1(I,J)=-TEMP1(I,J)
          IF(I.EQ.J)TEMP1(I,J)=1.-TEMP1(I,J)
  40          CONTINUE
C
C
          CALL VMFP22 (CX1,TEMP1,N,N,N,IS,IS,CX,IS)
C
C
          CALL VMFF22 (TEMP1,CX,N,N,N,IS,IS,CXX,IS)
C
C
          CALL VMFP22 (R,GAIN,M,M,N,IO,IS,TEMP11,IS)
C
C
          CALL VMFF22 (GAIN,TEMP11,N,M,N,IS,IS,TEMP1,IS)
C
          DO 45 I=1,N
          DO 45 J=1,N
  45          CX(I,J)=CXX(I,J)+TEMP1(I,J)
C
C  COMPUTE FILTERED STATE-MEAN VECTOR
C
          DO 50 I=1,N
              X(I)=0.
              DO 50 J=1,M
              X(I)=X(I)+GAIN(I,J)*Z(J)
  50      CONTINUE
          DO 60 I=1,N
  60          X(I)=X1(I)+X(I)
C
C  CHECK STATE-COVARIANCE MATRIX SYMMETRY AND POSITIVE
C  DEFINITENESS
C
C***J.C.: 881206
          DO 61 I=1,N
              IF (X(I) .LT. 0.0) X(I) = 0.0
61        CONTINUE
C***
C
          CALL SYMM22 (N,CX)
C
          CALL POSD22 (N,CX)
C
         IF(IDB .LE. 0) GO TO 121
         DO 119 I=1,N
 119     WRITE(IPR,120) (CX(I,J),J=1,N)
 120     FORMAT(/12X,'UPDATE- STATE COV: ',6F8.3)
 121     CONTINUE
C
C
C  CONVERT A N-VECTOR AND A NXN-SYMMETRIC MATRIX TO A 1-D VECTOR
C
         CALL CNVR22 (1,N,Y,X,CX)
C
         N11=N+1
         N22=N+N*(N+1)/2
         IF (IDB .GT. 0) WRITE(IPR,122) (Y(I),I=N11,N22)
 122     FORMAT(/2X,'UPDATE- 1-D VECT. STATE COV: ',6F8.3)
C
C
         IF (ITRACE .GT. 1) WRITE (IODBUG,991)
991      FORMAT(/10X,'** EXIT UPDT22.')

          RETURN
          END
