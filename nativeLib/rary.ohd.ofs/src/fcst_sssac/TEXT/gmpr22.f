C MEMBER GMPR22
C***********************************************************************
C
C  THIS SUBROUTINE IS PART OF THE TSFP PROGRAM, VERSION 2,
C  CREATED BY KONSTANTINE P. GEORGAKAKOS, NRC-NWS, SEPTEMBER 1982
C
C  IT COMPUTES THE PRODUCT OF TWO GENERAL MATRICES
C  IT IS BASED ON SUBROUTINE GMPRD OF THE SSP LIBRARY
C
      SUBROUTINE GMPR22 (A,B,R,N,M,L)
C
C...for debugging:
       COMMON /FDBUG/IODBUG,ITRACE,IDBALL,NDEBUG,IDEBUG(20)
C
      DIMENSION A(1),B(1),R(1)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_sssac/RCS/gmpr22.f,v $
     . $',                                                             '
     .$Id: gmpr22.f,v 1.2 2002/05/15 13:55:10 hank Exp $
     . $' /
C    ===================================================================
C
C
      IF (ITRACE .GT. 1) WRITE (IODBUG,990)
990   FORMAT(/10X,'** GMPR22 ENTERED.')
C
      IR=0
      IK=-M
      DO 10 K=1,L
      IK=IK+M
      DO 10 J=1,N
      IR=IR+1
      JI=J-N
      IB=IK
      R(IR)=0.
      DO 10 I=1,M
      JI=JI+N
      IB=IB+1
 10    R(IR)=R(IR)+A(JI)*B(IB)
C
      IF (ITRACE .GT. 1) WRITE (IODBUG,991)
991   FORMAT(/10X,'** EXIT GMPR22.')
C
      RETURN
      END
