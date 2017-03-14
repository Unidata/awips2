C MODULE RPRDD
C-----------------------------------------------------------------------
C
C  ROUTINE TO READ DATA FOR REGULAR TIME SERIES.
C
      SUBROUTINE RPRDD (ITSID,ITYPE,JHOUR,ITSTEP,NUM,IUNITS,RMISS,
     *   LTSDAT,TSDAT,JFPTR,LIWORK,IIWORK,ISTAT)
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
     .$Source: /fs/hseb/ob72/rfc/ofs/src/db_prdrw/RCS/rprdd.f,v $
     . $',                                                             '
     .$Id: rprdd.f,v 1.1 1999/01/20 13:34:52 page Exp $
     . $' /
C    ===================================================================
C
C
      IF (IPRTR.GT.0) WRITE (IOGDB,10)
10    FORMAT (' *** ENTER RPRDD')
C
      IFUT=0
      CALL RPRDDT (ITSID,ITYPE,JHOUR,ITSTEP,NUM,IUNITS,RMISS,
     *   LTSDAT,TSDAT,JFPTR,LIWORK,IWORK,IFUT,ISTAT)
C
      IF (IPRTR.GT.0) WRITE (IOGDB,20) ISTAT
20    FORMAT (' *** EXIT RPRDD - ISTAT=',I3)
C
      RETURN
C
      END
