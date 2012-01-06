C MEMBER WPRDC
C  (from old member PRDWPRDC)
C-----------------------------------------------------------------------
C
C                             LAST UPDATE: 02/06/95.17:08:15 BY $WC21DT
C
C @PROCESS LVL(77)
C
      SUBROUTINE WPRDC (ITSID,ITYPE,IUNITS,RLOCT,INFO,ITSIDF,
     *   NX,XBUF,LWKBUF,IWKBUF,IRECNO,ISTAT)
C
C  ROUTINE TO CHANGE HEADER FOR REGULAR TIME SERIES.
C
      DIMENSION IWKBUF(LWKBUF)
      DIMENSION ITSID(2),RLOCT(2),INFO(5),ITSIDF(2),XBUF(1)
C
      INCLUDE 'uio'
      INCLUDE 'udebug'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/db_prdrw/RCS/wprdc.f,v $
     . $',                                                             '
     .$Id: wprdc.f,v 1.1 1995/09/17 18:45:59 dws Exp $
     . $' /
C    ===================================================================
C
C
C
      IF (IPRTR.GT.0) WRITE (IOGDB,99)
99    FORMAT (' *** ENTER WPRDC')
C
      IFUT=0
      CALL WPRDCT (ITSID,ITYPE,IUNITS,RLOCT,INFO,ITSIDF,
     *   NX,XBUF,LWKBUF,IWKBUF,IRECNO,IFUT,ISTAT)
C
C
      IF (IPRTR.GT.0) WRITE (IOGDB,98)
98    FORMAT (' *** EXIT WPRDC')
C
      RETURN
C
      END
