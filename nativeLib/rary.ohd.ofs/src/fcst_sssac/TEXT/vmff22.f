C MEMBER VMFF22
C***********************************************************************
      SUBROUTINE VMFF22 (A,B,N,M,L,IA,IB,R,IR)
C
C
C  THIS SUBROUTINE IS PART OF THE TSFP PROGRAM VERSION 2
C  CREATED BY KONSTANTINE P. GEORGAKAKOS, NRC-NOAA, SEPT-1982
C
C
C  IT MULTIPLIES MATRIX A BY MATRIX B TO PRODUCE MATRIX R
C
C...for debugging:
       COMMON /FDBUG/IODBUG,ITRACE,IDBALL,NDEBUG,IDEBUG(20)
C
      DIMENSION A(1),B(1),R(1)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_sssac/RCS/vmff22.f,v $
     . $',                                                             '
     .$Id: vmff22.f,v 1.2 2002/05/15 13:53:33 hank Exp $
     . $' /
C    ===================================================================
C
C
C  CONVERT FROM TWO- TO ONE-DIMENSIONAL MATRICES
C
      IF (ITRACE .GT. 1) WRITE (IODBUG,990)
990   FORMAT(/10X,'** VMFF22 ENTERED.')
C
      MODE=2
      CALL ARAY22 (MODE,N,M,IA,M,A,A)
      CALL ARAY22 (MODE,M,L,IB,L,B,B)
C
C  MULTIPLY MATRICES
C
      CALL GMPR22 (A,B,R,N,M,L)
C
C  CONVERT FROM ONE- TO TWO-DIMENSIONAL MATRICES
C
      MODE=1
      CALL ARAY22 (MODE,N,M,IA,M,A,A)
      CALL ARAY22 (MODE,M,L,IB,L,B,B)
      CALL ARAY22 (MODE,N,L,IR,L,R,R)
C
      IF (ITRACE .GT. 1) WRITE (IODBUG,991)
991   FORMAT(/10X,'** EXIT VMFF22.')
C
      RETURN
      END
