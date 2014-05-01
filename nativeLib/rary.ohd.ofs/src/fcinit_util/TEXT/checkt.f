C MEMBER CHECKT
C  (from old member FCCHECKT)
C
      SUBROUTINE CHECKT(NEEDT,LEFT,IERR)
C.......................................
C     CHECKS TO SEE IF THE SPACE REQUESTED IS AVAILABLE
C        IN THE T ARRAY.  IF NOT, MESSAGE IS PRINTED,
C        SUBROUTINE ERROR IS CALLED, AND AN ERROR FLAG
C        IS RETURNED.
C.......................................
C     SUBROUTINE INITIALLY WRITTEN BY. . .
C        ERIC ANDERSON - HRL   NOV. 1979
C.......................................
C     COMMON BLOCKS.
      COMMON/FDBUG/IODBUG,ITRACE,IDBALL,NDEBUG,IDEBUG(20)
      COMMON/IONUM/IN,IPR,IPU
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_util/RCS/checkt.f,v $
     . $',                                                             '
     .$Id: checkt.f,v 1.1 1995/09/17 18:54:07 dws Exp $
     . $' /
C    ===================================================================
C
C.......................................
C     TRACE LEVEL = 3 - NO DEBUG OUTPUT
      IF (ITRACE.GE.3) WRITE (IODBUG,900)
  900 FORMAT(1H0,17H** CHECKT ENTERED)
C.......................................
      IERR=0
      IF (LEFT.GE.NEEDT) RETURN
      WRITE(IPR,901) NEEDT,LEFT
  901 FORMAT(1H0,10X,24H**ERROR** IN THE T ARRAY,1X,I4,1X,
     130HPOSITIONS WERE REQUESTED, ONLY,1X,I4,1X,14HARE AVAILABLE.)
      CALL ERROR
      IERR=1
C.......................................
      RETURN
      END
