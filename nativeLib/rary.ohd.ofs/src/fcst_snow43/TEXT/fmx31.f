C MODULE FMX31
C...................................................
C
C     Routine fmxadd
C         This routine adds two matrices.
C...................................................
      SUBROUTINE fmxadd(A,B,C,N,M,IPFMT,FMT,IFMT,IPMTX)
      INCLUDE 'common/ionum'
      DIMENSION A(N,M),B(N,M),C(N,M)
      CHARACTER*4   FMT(IFMT)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_snow43/RCS/fmx31.f,v $
     . $',                                                             '
     .$Id: fmx31.f,v 1.2 2000/12/19 15:05:52 dws Exp $
     . $' /
C    ===================================================================
C
      DO 10 I=1,N
      DO 10 J=1,M
   10 C(I,J)=A(I,J)+B(I,J)
C
      IF(IPFMT.EQ.1)WRITE(IPR,FMT)
      IF(IPMTX.EQ.1)CALL fmxprnt(C,N,M)
C
      RETURN
      END
C...................................................
C
C      Routine fmxsub
C          This routine subtracts one matrix from another.
C...................................................
      SUBROUTINE fmxsub(A,B,C,N,M,IPFMT,FMT,IFMT,IPMTX)
      INCLUDE 'common/ionum'
      DIMENSION A(N,M),B(N,M),C(N,M)
      CHARACTER*4   FMT(IFMT)
      DO 10 I=1,N
      DO 10 J=1,M
   10 C(I,J)=A(I,J)-B(I,J)
C
      IF(IPFMT.EQ.1)WRITE(IPR,FMT)
      IF(IPMTX.EQ.1)CALL fmxprnt(C,N,M)
C
C
      RETURN
      END
C
C...........................................................
C
C      Routine fmxmult
C          This routine multiplies two matrices.
C...........................................................
      SUBROUTINE fmxmult(A,B,C,L,M,N,IPFMT,FMT,IFMT,IPMTX)
      INCLUDE 'common/ionum'
      DIMENSION A(L,M),B(M,N),C(L,N)
      CHARACTER*4   FMT(IFMT)
      DO 20 I=1,L
      DO 20 J=1,N
      TEMP=0.
      DO 10 K=1,M
   10 TEMP=TEMP+A(I,K)*B(K,J)
   20 C(I,J)=TEMP
C
      IF(IPFMT.EQ.1)WRITE(IPR,FMT)
      IF(IPMTX.EQ.1)CALL fmxprnt(C,L,N)
C
      RETURN
      END
C...........................................................
C
C      Routine fmxprnt
C          This routine prints a matrix.
C...........................................................
      SUBROUTINE fmxprnt(X,N,M)
      INCLUDE 'common/ionum'
      DIMENSION X(N,M)
      DO 10 L=1,M,10
      LL=L+9
      IF(LL.GT.M)LL=M
      WRITE(IPR,600)(J,J=L,LL)
  600 FORMAT(/4X,'ROW',2X,'COLUMN:',10(I2,10X))
      DO 10 I=1,N
   10 WRITE(IPR,601)I,(X(I,J),J=L,LL)
  601 FORMAT(3X,I3,4X,10E12.5)
      RETURN
      END
C...........................................................
C
C      Routine fmxread
C          This routine reads a matrix in free format
C          from the IN file descriptor.
C...........................................................
C
C
      SUBROUTINE fmxread(X,N,M)
      INCLUDE 'common/ionum'
      DIMENSION X(N,M)
      DO 10 I=1,N
   10 READ(IN,*)(X(I,J),J=1,M)
      RETURN
      END
C...........................................................
C
C      Routine fmxinv
C          This routine inverts a matrix, but does not
C          overwrite the input.
C...........................................................
C
      SUBROUTINE fmxinv(N,X,XI,T1,T2,IPFMT,FMT,IFMT,IPMTX)
      INCLUDE 'common/ionum'
      DIMENSION X(N,N),XI(N,N),T1(N),T2(N)
      CHARACTER*4   FMT(IFMT)
C
      DO 10 I=1,N
      DO 10 J=1,N
   10 XI(I,J)=X(I,J)
C
      CALL fmxminv(XI,N,D,T1,T2)
C
      IF(D.NE.0.) GO TO 30
      WRITE(IPR,601)
  601 FORMAT(//' ******* MATRIX IS SINGULAR *******')
      STOP
C
   30 IF(IPFMT.EQ.1)WRITE(IPR,FMT)
      IF(IPMTX.EQ.1)CALL fmxprnt(XI,N,N)
C
      RETURN
      END
C...........................................................
C
C      Routine fmxidnt
C          This routine creates an identity matrix.
C...........................................................
      SUBROUTINE fmxidnt(X,N)
      INCLUDE 'common/ionum'
      DIMENSION X(N,N)
      CALL fmxclr(X,N,N)
      DO 10 I=1,N
   10 X(I,I)=1.
      RETURN
      END
C...........................................................
C
C      Routine fmxtmult
C          This routine performs transpose matrix 
C          multiplication.
C...........................................................
      SUBROUTINE fmxtmult(A,B,C,L,M,N,IPFMT,FMT,IFMT,IPMTX)
      INCLUDE 'common/ionum'
      DIMENSION A(L,M),B(N,M),C(L,N)
      CHARACTER*4   FMT(IFMT)
      DO 20 I=1,L
      DO 20 J=1,N
      TEMP=0.
      DO 10 K=1,M
   10 TEMP=TEMP+A(I,K)*B(J,K)
   20 C(I,J)=TEMP
C
      IF(IPFMT.EQ.1)WRITE(IPR,FMT)
      IF(IPMTX.EQ.1)CALL fmxprnt(C,L,N)
C
      RETURN
      END
C...........................................................
C
C      Routine fmxsrd
C          This routine performs symmetric matrix read.
C...........................................................
C
      SUBROUTINE fmxsrd(X,N)
      INCLUDE 'common/ionum'
      DIMENSION X(N,N)
      DO 10 I=1,N
   10 READ(IN,*)(X(I,J),J=1,I)
      CALL fmxsfil(X,N)
      RETURN
      END
C...........................................................
C
C      Routine fmxsfil
C          This routine will fill the other half of the
C          diagonal.
C...........................................................
      SUBROUTINE fmxsfil(X,N)
      INCLUDE 'common/ionum'
      DIMENSION X(N,N)
      M=N-1
      IF(M.LE.0)RETURN
      DO 10 I=1,M
      L=I+1
      DO 10 J=L,N
   10 X(I,J)=X(J,I)
      RETURN
      END
C...........................................................
C
C      Routine fmxminv
C          Inverses a matrix and overwrites the input.
C...........................................................
      SUBROUTINE fmxminv(A,N,D,L,M)
      INCLUDE 'common/ionum'
      DIMENSION A(*),L(*),M(*)
C
C        SEARCH FOR LARGEST ELEMENT
C
      D=1.
      NK=-N
      DO 80 K=1,N
      NK=NK+N
      L(K)=K
      M(K)=K
      KK=NK+K
      BIGA=A(KK)
      DO 20 J=K,N
      IZ=N*(J-1)
      DO 20 I=K,N
      IJ=IZ+I
   10 IF( ABS(BIGA).GE. ABS(A(IJ))) GO TO 20
   15 BIGA=A(IJ)
      L(K)=I
      M(K)=J
   20 CONTINUE
C
C        INTERCHANGE ROWS
C
      J=L(K)
      IF(J.LE.K) GO TO 35
   25 KI=K-N
      DO 30 I=1,N
      KI=KI+N
      HOLD=-A(KI)
      JI=KI-K+J
      A(KI)=A(JI)
   30 A(JI)=HOLD
C
C        INTERCHANGE COLUMNS
C
   35 I=M(K)
      IF(I.LE.K) GO TO 45
   38 JP=N*(I-1)
      DO 40 J=1,N
      JK=NK+J
      JI=JP+J
      HOLD=-A(JK)
      A(JK)=A(JI)
   40 A(JI)=HOLD
C
C        DIVIDE COLUMN BY MINUS PIVOT (VALUE OF PIVOT ELEMENT IS
C        CONTAINED IN BIGA)
C
   45 IF(BIGA) 48,46,48
   46 D=0.
      RETURN
C
   48 DO 55 I=1,N
      IF(I.EQ.K) GO TO 55
   50 IK=NK+I
      A(IK)=A(IK)/(-BIGA)
   55 CONTINUE
C
C        REDUCE MATRIX
C
      DO 65 I=1,N
      IK=NK+I
      HOLD=A(IK)
      IJ=I-N
      DO 65 J=1,N
      IJ=IJ+N
      IF(I.EQ.K) GO TO 65
   60 IF(J.EQ.K) GO TO 65
   62 KJ=IJ-I+K
      A(IJ)=HOLD*A(KJ)+A(IJ)
   65 CONTINUE
C
C        DIVIDE ROW BY PIVOT
C
      KJ=K-N
      DO 75 J=1,N
      KJ=KJ+N
      IF(J.EQ.K) GO TO 75
   70 A(KJ)=A(KJ)/BIGA
   75 CONTINUE
C
C        PRODUCT OF PIVOTS
C
      D=D*BIGA
C
C        REPLACE PIVOT BY RECIPROCAL
C
      A(KK)=1.0/BIGA
   80 CONTINUE
C
C        FINAL ROW AND COLUMN INTERCHANGE
C
      K=N
  100 K=K-1
      IF(K.LE.0) GO TO 150
  105 I=L(K)
      IF(I.LE.K) GO TO 120
  108 JQ=N*(K-1)
      JR=N*(I-1)
      DO 110 J=1,N
      JK=JQ+J
      HOLD=A(JK)
      JI=JR+J
      A(JK)=-A(JI)
  110 A(JI)=HOLD
  120 J=M(K)
      IF(J.LE.K) GO TO 100
  125 KI=K-N
      DO 130 I=1,N
      KI=KI+N
      HOLD=A(KI)
      JI=KI-K+J
      A(KI)=-A(JI)
  130 A(JI)=HOLD
      GO TO 100
  150 RETURN
      END
C...........................................................
C
C      Routine fmxclr
C          This routine fills a given matrix with 0.
C...........................................................
      SUBROUTINE fmxclr(X,N,M)
      INCLUDE 'common/ionum'
      DIMENSION X(N,M)
      DO 10 I=1,N
      DO 10 J=1,M
   10 X(I,J)=0.
      RETURN
      END
