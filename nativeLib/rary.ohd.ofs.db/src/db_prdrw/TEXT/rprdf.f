C MEMBER RPRDF
C  (from old member PRDRPRD)
C-----------------------------------------------------------------------
C
C                             LAST UPDATE: 03/27/95.12:18:25 BY $WC20SV
C
C @PROCESS LVL(77)
C
      SUBROUTINE RPRDF (ITSID,ITYPE,JHOUR,ITSTEP,NUM,IUNITS,RMISS,
     *   BUF,LWKBUF,IWKBUF,ISTAT)
C
C  READ DATA FOR FUTURE TIME SERIES.
C
      DIMENSION BUF(*),IWKBUF(LWKBUF)
      DIMENSION ITSID(2)
C
      INCLUDE 'uio'
      INCLUDE 'udebug'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/db_prdrw/RCS/rprdf.f,v $
     . $',                                                             '
     .$Id: rprdf.f,v 1.1 1995/09/17 18:45:51 dws Exp $
     . $' /
C    ===================================================================
C
C
C
      IF (IPRTR.GT.0) WRITE (IOGDB,10)
10    FORMAT (' *** ENTER RPRDF')
C
      IFUT=1
      CALL RPRDT (ITSID,ITYPE,JHOUR,ITSTEP,NUM,IUNITS,RMISS,
     *   BUF,JFPTR,LWKBUF,IWKBUF,IFUT,ISTAT)
C
      IF (IPRTR.GT.0) WRITE (IOGDB,20) ISTAT
20    FORMAT (' *** EXIT RPRDF - ISTAT=',I3)
C
      RETURN
C
      END
