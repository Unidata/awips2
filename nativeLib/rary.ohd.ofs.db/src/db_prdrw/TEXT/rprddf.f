C MODULE RPRDDF
C-----------------------------------------------------------------------
C
C  ROUTINE TO READ DATA FOR FUTURE TIME SERIES.
C
      SUBROUTINE RPRDDF (ITSID,ITYPE,JHOUR,ITSTEP,NUM,IUNITS,RMISS,
     *   LTSDAT,TSDAT,LIWORK,IWORK,ISTAT)
C
      DIMENSION ITSID(2)
      DIMENSION TSDAT(LTSDAT),IWORK(LIWORK)
C
      INCLUDE 'uio'
      INCLUDE 'udebug'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/db_prdrw/RCS/rprddf.f,v $
     . $',                                                             '
     .$Id: rprddf.f,v 1.1 1999/01/20 13:35:06 page Exp $
     . $' /
C    ===================================================================
C
C
C
      IF (IPRTR.GT.0) WRITE (IOGDB,10)
10    FORMAT (' *** ENTER RPRDDF')
C
      IFUT=1
      CALL RPRDDT (ITSID,ITYPE,JHOUR,ITSTEP,NUM,IUNITS,RMISS,
     *   LTSDAT,TSDAT,JFPTR,LIWORK,IWORK,IFUT,ISTAT)
C
      IF (IPRTR.GT.0) WRITE (IOGDB,20) ISTAT
20    FORMAT (' *** EXIT RPRDDF - ISTAT=',I3)
C
      RETURN
C
      END
