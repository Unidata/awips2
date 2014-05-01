C MEMBER PGTSIL
C  (from old member PRDINCOR)
C-----------------------------------------------------------------------
C
      SUBROUTINE PGTSIL (ITYPE,INDBTS)
C
C GIVEN A TYPE, THIS ROUTINE RETURNS THE INCORE BUFFER POINTER OF
C THE LAST TIME SERIES OF THIS TYPE
C
      INCLUDE 'udebug'
      INCLUDE 'uio'
      INCLUDE 'prdcommon/picdmg'
      INCLUDE 'prdcommon/picptr'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/db_prdrw/RCS/pgtsil.f,v $
     . $',                                                             '
     .$Id: pgtsil.f,v 1.1 1995/09/17 18:45:41 dws Exp $
     . $' /
C    ===================================================================
C
      IF (IPRTR.GT.0) WRITE (IOGDB,2000) ITYPE
2000  FORMAT (' *** ENTER PGTSIL - ITYPE=',A4)
C
      INDBTS=0
      CALL PFDTIC (ITYPE,IDNXD,IDX)
      IF (IDX.EQ.0) GO TO 999
      IPX=TSICDL(IDX)
      INDBTS=TSBUPT(IPX)
      IF (IPRDB.GT.0) WRITE (IOGDB,2001) ITYPE,INDBTS
2001  FORMAT (' LAST TIME SERIES OF TYPE: ',A4,' FOUND AT ',I4)
C
999   RETURN
C
      END
