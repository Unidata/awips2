C MEMBER FFPURL
C  (from old member FCFFPURL)
C
      SUBROUTINE FFPURL(X,N,IBUG)
C.......................................................................
C
C     THIS SUBROUTINE WRITES REAL VALUES TO UNIT 'IPU' IN A
C     FORM THAT CAN BE READ BY A FREE FORMAT READ SUBROUTINE (FFRDRL).
C.......................................................................
C
C     SUBROUTINE ORIGINALLY WRITTEN BY
C                GEORGE F. SMITH - HRL   OCTOBER 1979  VERSION 1
C.......................................................................
C
C          VARIABLES IN ARGUMENT LIST
C
C            1. X    - A REAL ARRAY CONTAINING THE VALUES TO BE
C                      WRITTEN
C            2. N    - THE NUMBER OF VALUES TO BE WRITTEN
C            3. IBUG - A SWITCH WHICH DETERMINES IF DEBUG INFORMATION
C                      IS WRITTEN BY THIS SUBROUTINE
C                         = 0, NO DEBUG INFORMATION WRITTEN
C                         = 1, DEBUG INFORMATION WRITTEN
C.......................................................................
C
C      THE VALUE OF 'IPU' IS PASSED THROUGH THE COMMON BLOCK /IONUM/
C.......................................................................
C
      COMMON/FDBUG/IODBUG,IXX(23)
      COMMON/IONUM/IN,IPR,IPU
      CHARACTER*8 OPTFMT(2),FORMT(3),FMT31,FMT32
      DIMENSION X(N)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_puc/RCS/ffpurl.f,v $
     . $',                                                             '
     .$Id: ffpurl.f,v 1.2 2000/12/18 21:38:18 jgofus Exp $
     . $' /
C    ===================================================================
C
      DATA OPTFMT/ '5.0)','5.3)' /
      DATA FORMT(1) / '(4(1X,F1' /
      DATA FMT31    / ',2H X)  ' /
      DATA FMT32    / ')       ' /
C
      IF(IBUG.EQ.1)WRITE(IODBUG,900)
  900 FORMAT(1H0,10X,16H**FFPURL ENTERED)
C
C         FIND MAXIMUM VALUE
      XMAX=X(1)
      DO 2 I=1,N
      IF(X(I).GT.XMAX) XMAX=X(I)
    2 CONTINUE
C
C         DETERMINE THE PROPER FORMAT FIELD
      FORMT(2)=OPTFMT(2)
      FORMT(3)=FMT31
      IF(XMAX.GE.10**7) FORMT(2)=OPTFMT(1)
C
      DO 50 J=1,N,4
      IF(J+3.EQ.N)GO TO 40
      IF(J+3.LT.N)GO TO 30
      WRITE(IPU,FORMT)(X(K),K=J,N)
      GO TO 50
   30 L=J+3
      WRITE(IPU,FORMT)(X(K),K=J,L)
      GO TO 50
   40 FORMT(3)=FMT32
      WRITE(IPU,FORMT)(X(K),K=J,N)
   50 CONTINUE
      RETURN
      END
