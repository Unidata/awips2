C MEMBER CHECKC
C  (from old member FCCHECKP)
C
      SUBROUTINE CHECKC(I,L,IER)
C.......................................................................
C
C     THIS SUBROUTINE CHECKS TO SEE IF THS NUMBER OF POSITIONS
C     REQUESTED IN THE P ARRAY ARE AVAILABLE.
C     IF THE POSITIONS ARE NOT AVAILABLE A MESSAGE IS PRINTED,
C     SUBROUTINE ERROR IS CALLED, AND AN ERROR FLAG IS RETURNED.
C
C     SUBROUTINE CHECKC PERFORMS THE SAME CHECK FOR THE C ARRAY.
C     THE ARGUMENT LISTS FOR SUBROUTINE CHECKP AND SUBROUTINE
C     CHECKC ARE IDENTICAL.
C.......................................................................
C
C     SUBROUTINE ORIGINALLY WRITTEN BY
C                GEORGE F. SMITH - HRL   OCTOBER 1979   VERSION 1
C.......................................................................
C
C        VARIABLES IN ARGUMENT LIST
C
C           1. I   - NUMBER OF POSITIONS REQUESTED
C           2. L   - NUMBER OF POSITIONS AVAILABLE
C           3. IER - ERROR FLAG
C                      = 0, NO ERROR
C                      = 1, ERROR DETECTED
C.......................................................................
C
      INCLUDE 'common/ionum'
      DIMENSION NAME(2)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_util/RCS/checkc.f,v $
     . $',                                                             '
     .$Id: checkc.f,v 1.1 1995/09/17 18:54:05 dws Exp $
     . $' /
C    ===================================================================
C
      DATA NAME/1HP,1HC/
      INAM=2
      IER=0
      IF(I.LE.L)RETURN
      WRITE(IPR,900) NAME(INAM),I,L
  900 FORMAT(1H0,10X,20H**ERROR**  IN ARRAY ,A1/
     1  11X,I4,32H POSITIONS WERE REQUESTED, ONLY ,I4,
     1  15H ARE AVAILABLE.)
      CALL ERROR
      IER=1
      RETURN
      END
