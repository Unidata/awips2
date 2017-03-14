C MEMBER DVRK22
C***********************************************************************
C
      SUBROUTINE DVRK22(NDIM,FCT22,PRMT1,Y,PRMT2,TOL,IHLF,PRMT,AUX)
C
C
C  THIS SUBROUTINE IS PART OF THE TSFP PROGRAM VERSION 2
C  CREATED BY KONSTANTINE P. GEORGAKAKOS, NRC-NOAA, SEPT-1982
C
C
C  IT INTEGRATES THE DIFFERENTIAL EQUATIONS OF THE USER'S MODEL.
C  THE FIRST ORDER DIFFERENTIAL EQUATIONS ARE STORED IN SUBROUTINE FCT
C
      EXTERNAL FCT22
      EXTERNAL OUTP22
C
      DIMENSION PRMT(5),Y(91),DERY(91),AUX(16,91)
C
C
C********* NON-FC LABELED COMMON BLOCKS ********************************
C
      COMMON/SUBD22/ SUBS
      COMMON/STAN22/ NC

C***********************************************************************
C
C...for debugging:
       COMMON /FDBUG/IODBUG,ITRACE,IDBALL,NDEBUG,IDEBUG(20)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_sssac/RCS/dvrk22.f,v $
     . $',                                                             '
     .$Id: dvrk22.f,v 1.2 2002/05/15 13:53:47 hank Exp $
     . $' /
C    ===================================================================
C
C
C***********************************************************************
C
      IF (ITRACE .GT. 1) WRITE (IODBUG,990)
990   FORMAT(/10X,'** DVRK22 ENTERED.')
C
      PRMT(1)=PRMT1
      PRMT(2)=PRMT2
      PRMT(3)=(PRMT2-PRMT1)/SUBS
      PRMT(4)=TOL
      XNDM=NDIM
      DO 100 I=1,NDIM
C
 100  DERY(I)=1./XNDM
C
      CALL DVR122 (PRMT,Y,DERY,NDIM,IHLF,FCT22,OUTP22,AUX)
C
      PRMT1=PRMT2
C
      IF (ITRACE .GT. 1) WRITE (IODBUG,991)
991   FORMAT(/10X,'** EXIT DVRK22.')
C
      RETURN
      END
