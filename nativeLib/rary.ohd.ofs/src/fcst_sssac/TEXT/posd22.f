C MEMBER POSD22
C***********************************************************************
       SUBROUTINE POSD22 (N,CX)
C
C
C  THIS ROUTINE IS PART OF THE TSFP PROGRAM, VERSION 2
C  CREATED BY KONSTANTINE P. GEORGAKAKOS, MIT, MAY-1982
C
C
C
C  ENFORCES POSITIVE DEFINITENESS ON A COVARIANCE MATRIX
C
C...for debugging:
       COMMON /FDBUG/IODBUG,ITRACE,IDBALL,NDEBUG,IDEBUG(20)
C
          DIMENSION CX(12,12)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_sssac/RCS/posd22.f,v $
     . $',                                                             '
     .$Id: posd22.f,v 1.2 2002/05/15 13:53:11 hank Exp $
     . $' /
C    ===================================================================
C
C
C  CHECK VARIANCES FOR POSITIVE SIGN
C
C  THE NEGATIVE ELEMENTS ARE ASSIGNED THE POSITIVE VALUES EQUAL TO THEIR
C  INITIAL ABSOLUTE VALUE
C
C  THE ELEMENTS ARE NOT ALLOWED TO OBTAIN VALUES LESS THAN A LOWER
C  BOUND OF 1.E-8
C
          IF (ITRACE .GT. 1) WRITE (IODBUG,990)
990       FORMAT(/10X,'** POSD22 ENTERED.')
C
          DO 10 I=1,N
          IF(CX(I,I).LT.0.)CX(I,I)=-CX(I,I)
          IF(CX(I,I).LE.1.E-8)CX(I,I)=1.E-8
  10          CONTINUE
C
          IF(N.NE.1) GOTO 801
              IF (ITRACE .GT. 1) WRITE (IODBUG,991)
991           FORMAT(/10X,'** EXIT POSD22.')
              RETURN
801       CONTINUE
C
C  ENFORCE CORRELATIONS LESS THAN ONE
C
          DO 20 I=2,N
          N2 = I-1
          DO 20 J=1,N2
          TEMP1=CX(I,J)
          TEMP2=CX(I,I)*CX(J,J)
          TEMP2=SQRT(TEMP2)
          IF(TEMP1.GE.0.) GO TO 15
          IF(TEMP1.GE.-TEMP2) GO TO 20
          CX(I,J)=-0.99*TEMP2
          CX(J,I)=CX(I,J)
          GO TO 20
 15       CONTINUE
          IF(TEMP1.LT.TEMP2) GO TO 20
          CX(I,J)=0.99*TEMP2
          CX(J,I)=CX(I,J)
  20          CONTINUE
C
          IF (ITRACE .GT. 1) WRITE (IODBUG,991)
C
          RETURN
          END
