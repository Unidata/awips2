C MEMBER GMTR22
C***********************************************************************
      SUBROUTINE GMTR22 (A,R,N,M)
C
C
C  THIS SUBROUTINE IS PART OF THE TSFP PROGRAM VERSION 2
C  CREATED BY KONSTANTINE P. GEORGAKAKOS, NRC-NOAA, SEPT-1982
C
C
C  IT COMPUTES THE TRANSPOSE OF A MATRIX
C  IT IS BASED ON SUBROUTINE GMTRA OF THE SSP LIBRARY
C
C...for debugging:
       COMMON /FDBUG/IODBUG,ITRACE,IDBALL,NDEBUG,IDEBUG(20)
C
      DIMENSION A(1),R(1)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_sssac/RCS/gmtr22.f,v $
     . $',                                                             '
     .$Id: gmtr22.f,v 1.2 2002/05/15 13:52:44 hank Exp $
     . $' /
C    ===================================================================
C
C
      IF (ITRACE .GT. 1) WRITE (IODBUG,990)
990   FORMAT(/10X,'** GMTR22 ENTERED.')
C
      IR=0
      DO 10 I=1,N
      IJ=I-N
      DO 10 J=1,M
      IJ=IJ+N
      IR=IR+1
 10   R(IR)=A(IJ)
C
      IF (ITRACE .GT. 1) WRITE (IODBUG,991)
991   FORMAT(/10X,'** EXIT GMTR22.')
C
      RETURN
      END
