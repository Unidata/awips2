C MEMBER PGTSIC
C  (from old member PRDINCOR)
C-----------------------------------------------------------------------
C
      SUBROUTINE PGTSIC (ITSID,ITYPE,INDBTS)
C
C THIS ROUTINE LOOKS FOR A TIME SERIES AND RETURNS
C ITS INCORE BUFFER POINTER IF THERE OR 0 IF NOT
C
      DIMENSION ITSID(2)
C
      INCLUDE 'uio'
      INCLUDE 'udebug'
      INCLUDE 'prdcommon/picdmg'
      INCLUDE 'prdcommon/picptr'
      INCLUDE 'prdcommon/ptsicb'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/db_prdrw/RCS/pgtsic.f,v $
     . $',                                                             '
     .$Id: pgtsic.f,v 1.1 1995/09/17 18:45:40 dws Exp $
     . $' /
C    ===================================================================
C
C
      IF (IPRTR.GT.0.AND.ITSID(2).NE.0) WRITE (IOGDB,2001) ITSID,ITYPE
2001  FORMAT (' *** ENTER PGTSIC - ITSID=',2A4,3X,'ITYPE=',A4)
      IF (IPRTR.GT.0.AND.ITSID(2).EQ.0) WRITE (IOGDB,2002) ITSID,ITYPE
2002  FORMAT (' *** ENTER PGTSIC - ITSID=',I7,1X,I7,3X,'ITYPE=',A4)
C
      INDBTS=0
C
C CHECK FOR VALID TYPE AND IF INCORE
C
      CALL PFDTIC (ITYPE,INDXD,IDX)
      IF (IDX.EQ.0) GO TO 999
C
C YES THERE ARE SOME, SEARCH THEM
C
      NUM=TSICDN(IDX)
      IPX=TSICD1(IDX)
      DO 100 I=1,NUM
C
C DOES NAME MATCH
C
      CALL UNAMCP (TSIDIC(1,IPX),ITSID,ISTAT)
      IF (ISTAT.NE.0) GO TO 10
C
C HOW ABOUT TYPE?
C
      IF (ITYPE.EQ.TSTYPE(IPX)) GO TO 200
10    IPX=TSPTNX(IPX)
100   CONTINUE
C
C NO MATCH
C
      GO TO 999
C
C FOUND IT AT IPX
C
200   INDBTS=TSBUPT(IPX)
      IF (IPRDB.GT.0) WRITE (IOGDB,2000) IPX,INDBTS
2000  FORMAT (' TIME SERIES FOUND AT PTR= ',I4,' BUFPTR= ',I4)
999   RETURN
      END
