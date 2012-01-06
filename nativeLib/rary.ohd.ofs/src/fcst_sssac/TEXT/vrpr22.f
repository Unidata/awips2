C MEMBER VRPR22
C***********************************************************************
C
      SUBROUTINE VRPR22 (A,N,Y,DY,Q)
C
C
C  THIS ROUTINE IS PART OF THE SS-SAC PROGRAM
C  CREATED BY KONSTANTINE P. GEORGAKAKOS, UI, NOV-1986
C
C
C
C  DEFINES THE STATE COVARIANCE PROPAGATION DIFFERENTIAL
C  EQUATION
C
C...for debugging:
       COMMON /FDBUG/IODBUG,ITRACE,IDBALL,NDEBUG,IDEBUG(20)
C
C
       DIMENSION Y(91),DY(91),X1(12)
       DIMENSION A(12,12),TEMP11(12,12)
          DIMENSION TEMP1(12,12),TEMP2(12,12),Q(12,12)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_sssac/RCS/vrpr22.f,v $
     . $',                                                             '
     .$Id: vrpr22.f,v 1.2 2002/05/15 13:56:31 hank Exp $
     . $' /
C    ===================================================================
C
C
C
C  RETRIEVE STATE-COVARIANCE MATRIX STORED IN Y-ARRAY
C
       IF (ITRACE .GT. 1) WRITE (IODBUG,990)
990    FORMAT(/10X,'** VRPR22 ENTERED.')
C
       CALL CNVR22 (0,N,Y,X1,TEMP1)
C
          IS=12
C
          CALL VMFF22 (A,TEMP1,N,N,N,IS,IS,TEMP2,IS)
C
          CALL VMFP22 (TEMP1,A,N,N,N,IS,IS,TEMP11,IS)
C
          DO 20 I=1,N
          DO 20 J=1,N
          TEMP1(I,J)=TEMP11(I,J)+TEMP2(I,J)
         TEMP1(I,J)=TEMP1(I,J)+Q(I,J)
  20      CONTINUE
C
C  STORE COMPUTED STATE-COVARIANCE DERIVATIVE MATRIX IN DY-ARRAY
C
         DO 10 IJ=1,N
 10      X1(IJ)=DY(IJ)
C
         CALL CNVR22 (1,N,DY,X1,TEMP1)
C
         IF (ITRACE .GT. 1) WRITE (IODBUG,991)
991      FORMAT(/10X,'** EXIT VRPR22.')
C
         RETURN
         END
