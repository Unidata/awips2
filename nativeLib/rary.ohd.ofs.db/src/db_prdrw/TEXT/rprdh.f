C MODULE RPRDH
C-----------------------------------------------------------------------
C
      SUBROUTINE RPRDH (ITSID,ITYPE,MAXX,IHEAD,NUMX,XBUF,IFTSID,ISTAT)
C
C  ROUTINE TO READ HEADER FOR REGULAR TIME SERIES.
C
      DIMENSION ITSID(2),IFTSID(2),IHEAD(22),XBUF(1)
C
      INCLUDE 'uiox'
      INCLUDE 'udebug'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/db_prdrw/RCS/rprdh.f,v $
     . $',                                                             '
     .$Id: rprdh.f,v 1.2 2002/02/11 14:27:01 michaelo Exp $
     . $' /
C    ===================================================================
C
C
      IF (IPRTR.GT.0) WRITE (IOGDB,*) 'ENTER RPRDH'
C
      IFUT=0
      CALL RPRDHT (ITSID,ITYPE,MAXX,IHEAD,NUMX,XBUF,IFTSID,IFUT,ISTAT)
C
      IF (IPRTR.GT.0) WRITE (IOGDB,*) 'EXIT RPRDH'
C
      RETURN
C
      END
