C MEMBER PRDCTS
C  (from old member PRDDEFTS)
C-----------------------------------------------------------------------
C
C                             LAST UPDATE: 03/17/95.11:58:32 BY $WC20SV
C
C @PROCESS LVL(77)
C
      SUBROUTINE PRDCTS (ITSID,ITYPE,ITSTEP,IUNIT,NVAL,RLOCT,
     *   ITSIDF,INFO,NX,XBUF,LWKBUF,IWKBUF,ISTAT)
C
C  CHANGE HEADER FOR REGULAR TIME SERIES.
C
      DIMENSION IWKBUF(LWKBUF)
      DIMENSION ITSID(2),RLOCT(2),INFO(5),ITSIDF(2),XBUF(1)
C
      INCLUDE 'udebug'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/prdutil/RCS/prdcts.f,v $
     . $',                                                             '
     .$Id: prdcts.f,v 1.1 1995/09/17 19:16:40 dws Exp $
     . $' /
C    ===================================================================
C
C
C
      IF (IPRTR.GT.0) WRITE (IOGDB,10)
10    FORMAT (' *** ENTER PRDCTS')
C
      IFUT=0
      CALL PRDCTT (ITSID,ITYPE,ITSTEP,IUNIT,NVAL,RLOCT,ITSIDF,
     *   INFO,NX,XBUF,LWKBUF,IWKBUF,IFUT,ISTAT)
C
      IF (IPRTR.GT.0) WRITE (IOGDB,20)
20    FORMAT (' *** EXIT PRDCTS')
C
      RETURN
C
      END
