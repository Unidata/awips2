C MEMBER WPRDFH
C  (from old member PRDWPRDH)
C-----------------------------------------------------------------------
C
C                             LAST UPDATE: 06/20/95.09:15:52 BY $WC20SV
C
C @PROCESS LVL(77)
C
      SUBROUTINE WPRDFH (ITSID,ITYPE,ITSTEP,IUNIT,NVAL,RLOCT,
     *   DESCRP,LXBUF,BUF,LWKBUF,IWKBUF,IRECNO,ISTAT)
C
C  THIS ROUTINE CREATES A FUTURE TIME SERIES.
C
      INCLUDE 'uio'
      INCLUDE 'udebug'
      INCLUDE 'ucommon/uppint'
      INCLUDE 'prdcommon/pdftbl'
C
      DIMENSION IWKBUF(LWKBUF)
      DIMENSION ITSIDF(2)
      DIMENSION ITSID(2),RLOCT(2),DESCRP(5),XBUF(1)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/db_prdrw/RCS/wprdfh.f,v $
     . $',                                                             '
     .$Id: wprdfh.f,v 1.1 1995/09/17 18:46:05 dws Exp $
     . $' /
C    ===================================================================
C
C
      DATA ITSIDF/4H    ,4H    /
C
C
      IF (IPRTR.GT.0) WRITE (IOGDB,10) ITSID,ITYPE
10    FORMAT (' *** ENTER WPRDFH - ITSID=',2A4,3X,'ITYPE=',A4)
C
      CALL PFFTYP (ITYPE,IITYPE,IND,INDF)
      IF (IND.NE.0) GO TO 30
      IF (IPRDB.GT.0.OR.IPPFLG.EQ.0) WRITE (LP,20) ITYPE
20    FORMAT ('0**ERROR** IN WPRDFH - DATA TYPE ',A4,' NOT IN DATA ',
     *   'TYPE INDEX.')
      ISTAT=4
      GO TO 60
C
30    IF (INDF.GT.0) GO TO 50
      IF (IPRDB.GT.0.OR.IPPFLG.EQ.0) WRITE (LP,40) ITYPE
40    FORMAT ('0**ERROR** IN WPRDFH - DATA TYPE ',A4,' DOES NOT HAVE ',
     * 'FUTURE TIME SERIES')
      ISTAT=8
      GO TO 60
C
50    CALL WPRDH (ITSID,IITYPE,ITSTEP,IUNIT,NVAL,RLOCT,ITSIDF,
     *   DESCRP,LXBUF,XBUF,LWKBUF,IWKBUF,IRECNO,ISTAT)
C
60    IF (IPRTR.GT.0) WRITE (IOGDB,70) ISTAT
70    FORMAT (' *** EXIT WPRDFH - ISTAT=',I4)
C
      RETURN
C
C
      END
