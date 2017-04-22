C MEMBER VMFP22
C***********************************************************************
C
      SUBROUTINE VMFP22 (A,B,N,L,M,IA,IB,R,IR)
C
C
C  THIS SUBROUTINE IS PART OF THE TSFP PROGRAM VERSION 2
C  CREATED BY KONSTANTINE P. GEORGAKAKOS, NRC-NOAA, SEPT-1982
C
C
C  IT MULTIPLIES MATRIX A BY THE TRANSPOSE OF MATRIX B TO PRODUCE
C  MATRIX R
C
C...for debugging:
       COMMON /FDBUG/IODBUG,ITRACE,IDBALL,NDEBUG,IDEBUG(20)
C
      DIMENSION A(1),B(1),R(1),BT(12,12)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_sssac/RCS/vmfp22.f,v $
     . $',                                                             '
     .$Id: vmfp22.f,v 1.2 2002/05/15 13:50:58 hank Exp $
     . $' /
C    ===================================================================
C
C
C  CONVERT FROM TWO- TO ONE-DIMENSIONAL MATRICES
C
      IF (ITRACE .GT. 1) WRITE (IODBUG,990)
990   FORMAT(/10X,'** VMPF22 ENTERED.')
C
      MODE=2
      CALL ARAY22 (MODE,N,L,IA,L,A,A)
      CALL ARAY22 (MODE,M,L,IB,L,B,B)
C
C  COMPUTE TRANSPOSE OF B
C
      CALL GMTR22 (B,BT,M,L)
C
C  MULTIPLY A BY B-TRANSPOSE
C
      CALL GMPR22 (A,BT,R,N,L,M)
C
C  CONVERT FROM ONE- TO TWO-DIMENSIONAL MATRICES
C
      MODE=1
      CALL ARAY22 (MODE,N,L,IA,L,A,A)
      CALL ARAY22 (MODE,M,L,IB,L,B,B)
      CALL ARAY22 (MODE,N,M,IR,M,R,R)
C
      IF (ITRACE .GT. 1) WRITE (IODBUG,991)
991   FORMAT(/10X,'** EXIT VMPF22.')
C
      RETURN
      END
