C MODULERPRDFH
C-----------------------------------------------------------------------
C
      SUBROUTINE RPRDFH (ITSID,ITYPE,MAXX,IHEAD,NUMX,XBUF,ISTAT)
C
C  ROUTINE TO READ HEADER FOR FUTURE TIME SERIES.
C
      DIMENSION ITSID(2),IFTSID(2),IHEAD(22),XBUF(1)
C
      INCLUDE 'uiox'
      INCLUDE 'udebug'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/db_prdrw/RCS/rprdfh.f,v $
     . $',                                                             '
     .$Id: rprdfh.f,v 1.2 2002/02/11 14:26:37 michaelo Exp $
     . $' /
C    ===================================================================
C
C
      IF (IPRTR.GT.0) WRITE (IOGDB,*) 'ENTER RPRDFH'
C
      IFUT=1
      CALL RPRDHT (ITSID,ITYPE,MAXX,IHEAD,NUMX,XBUF,IFTSID,IFUT,ISTAT)
C
      IF (IPRTR.GT.0) WRITE (IOGDB,*) 'EXIT RPRDFH'
C
      RETURN
C
      END
