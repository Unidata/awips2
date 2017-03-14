C MEMBER PTSREC
C  (from old member PRDPRINT)
C-----------------------------------------------------------------------
C
C                             LAST UPDATE: 05/12/95.10:54:24 BY $WC20SV
C
C @PROCESS LVL(77)
C
      SUBROUTINE PTSREC (ITSID,ITYPE,IUNITS,IFREC,
     *   IPAGE,IFORM,IKEY,
     *   LWKBUF,IWKBUF,NDCNOW,ISTAT)
C
C  ROUTINE TO GET TIME SERIES RECORD
C
      INCLUDE 'uio'
      INCLUDE 'udebug'
      INCLUDE 'prdcommon/pdftbl'
      INCLUDE 'prdcommon/punits'
C
      DIMENSION IWKBUF(LWKBUF)
      DIMENSION ITSID(2),IXBUF(4)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/prdutil/RCS/ptsrec.f,v $
     . $',                                                             '
     .$Id: ptsrec.f,v 1.1 1995/09/17 19:16:59 dws Exp $
     . $' /
C    ===================================================================
C
C
C
      IF (IPRTR.GT.0) WRITE (IOGDB,50) ITYPE
C
      ISTAT=0
C
C  FIND DATA TYPE
      CALL PFDTYP (ITYPE,INDXT)
      IF (INDXT.EQ.0) GO TO 10
C
C  SET LOGICAL UNIT
      IUNIT=DATFIL(2,INDXT)
C
      IF (IPRDB.GT.0) WRITE (IOGDB,60) ITYPE,INDXT,IUNIT
C
C  FIND TIME SERIES
      CALL PSERCH (ITSID,ITYPE,IFREE,IXREC,IXBUF)
      IF (IXREC.EQ.0) GO TO 20
C
C  TIME SERIES FOUND
      ISTRT=IXBUF(4)
      CALL PTSRDG (IUNIT,ISTRT,NXTREC,IUNITS,IFREC,
     *   IPAGE,IFORM,IKEY,
     *   LWKBUF,IWKBUF,NDCNOW,ISTAT)
      GO TO 40
C
10    CALL ULINE (LP,2)
      WRITE (LP,70) ITYPE
      GO TO 30
C
20    CALL ULINE (LP,2)
      WRITE (LP,80) ITSID,ITYPE
C
30    ISTAT=1
C
40    IF (IPRTR.GT.0) WRITE (IOGDB,90) ISTAT
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
50    FORMAT (' *** ENTER PTSREC - ITYPE=',A4)
60    FORMAT (' ITYPE,INDXT,IUNIT=',A4,2I4)
70    FORMAT ('0**ERROR** IN PTSREC - ',A4,' IS AN INVALID DATA TYPE.')
80    FORMAT ('0**ERROR** IN PTSREC - TIME SERIES FOR ID ',2A4,
     *  ' AND TYPE ',A4,' NOT FOUND.')
90    FORMAT (' *** EXIT PTSREC - ISTAT=',I2)
C
      END
