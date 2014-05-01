C MODULE RPRDT
C-----------------------------------------------------------------------
C
C  ROUTINE TO READ A TIME SERIES FROM A PROCESSED DATA BASE FILE.
C
      SUBROUTINE RPRDT (ITSID,ITYPE,JHOUR,ITSTEP,NUM,IUNITS,RMISS,
     *   TSDAT,JFPTR,LIWORK,IWORK,IFUT,ISTA)
C
      INCLUDE 'uio'
      INCLUDE 'udebug'
C
      DIMENSION ITSID(2),TSDAT(*),IWORK(LIWORK)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/db_prdrw/RCS/rprdt.f,v $
     . $',                                                             '
     .$Id: rprdt.f,v 1.2 1999/01/20 13:35:47 page Exp $
     . $' /
C    ===================================================================
C
C
      IF (IPRTR.GT.0) WRITE (IOGDB,10)
10    FORMAT (' *** ENTER RPRDT')
C
      LTSDAT=-1
      CALL RPRDDT (ITSID,ITYPE,JHOUR,ITSTEP,NUM,IUNITS,RMISS,
     *   LTSDAT,TSDAT,JFPTR,LIWORK,IWORK,IFUT,ISTA)
C
      IF (IPRTR.GT.0) WRITE (IOGDB,20) ISTA
20    FORMAT (' *** EXIT RPRDT - ISTA=',I2)
C
      RETURN
C
      END
