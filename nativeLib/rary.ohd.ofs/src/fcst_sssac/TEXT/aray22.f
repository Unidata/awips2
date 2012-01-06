C MEMBER ARAY22
C***********************************************************************
C
C  THIS SUBROUTINE IS PART OF THE TSFP PROGRAM, VERSION 2,
C  CREATED BY KONSTANTINE P. GEORGAKAKOS, NRC-NWS, SEPTEMBER 1982
C
C  IT CONVERTS DATA ARRAYS FROM SINGLE TO DOUBLE DIMENSIONS AND
C  VICE-VERSA
C  IT IS BASED ON SUBROUTINE ARRAY OF THE SSP LIBRARY
C
C
      SUBROUTINE ARAY22 (MODE,I,J,N,M,S,D)
C
C...for debugging:
       COMMON /FDBUG/IODBUG,ITRACE,IDBALL,NDEBUG,IDEBUG(20)
C
      DIMENSION S(1),D(1)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_sssac/RCS/aray22.f,v $
     . $',                                                             '
     .$Id: aray22.f,v 1.2 2002/05/15 13:54:44 hank Exp $
     . $' /
C    ===================================================================
C
C
      IF (ITRACE .GT. 1) WRITE (IODBUG,990)
990   FORMAT(/10X,'** ARAY22 ENTERED.')
C
      NI=N-I
C
C  TEST TYPE OF CONVERSION
C
      IF(MODE-1) 100,100,120
C
C  CONVERT FROM SINGLE TO DOUBLE DIMENSION
C
 100  IJ=I*J+1
      NM=N*J+1
      DO 110 K=1,J
      NM=NM-NI
      DO 110 L=1,I
      IJ=IJ-1
C
      NM=NM-1
 110  D(NM)=S(IJ)
      GO TO 140
C
C  CONVERT FROM DOUBLE TO SINGLE DIMENSION
C
 120  IJ=0
      NM=0
      DO 130 K=1,J
      DO 125 L=1,I
      IJ=IJ+1
      NM=NM+1
 125  S(IJ)=D(NM)
 130  NM=NM+NI
C
      IF (ITRACE .GT. 1) WRITE (IODBUG,991)
991   FORMAT(/10X,'** EXIT ARAY22.')
C
 140  RETURN
      END
