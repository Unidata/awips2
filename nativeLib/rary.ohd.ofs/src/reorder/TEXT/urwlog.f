C MEMBER URWLOG
C-----------------------------------------------------------------------
C
C                             LAST UPDATE: 01/28/94.11:48:46 BY $WC20SV
C
C @PROCESS LVL(77)
C
C DESC ROUTINE TO WRITE INFORMATION TO PROGRAM LOG
C
      SUBROUTINE URWLOG (CARRAG,TEXT,ACTION,SEQNUM,LPUNIT)
C
C
      CHARACTER*(*) CARRAG,TEXT,ACTION,SEQNUM
C
      INCLUDE 'uio'
      INCLUDE 'upagex'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/reorder/RCS/urwlog.f,v $
     . $',                                                             '
     .$Id: urwlog.f,v 1.1 1995/09/17 19:18:08 dws Exp $
     . $' /
C    ===================================================================
C
C
C
      IF (LPUNIT.EQ.0) GO TO 10
C
      IF (NPSPAG(LP).EQ.0) GO TO 10
C
      IF (SEQNUM.EQ.' ') WRITE (LPUNIT,20) CARRAG,TEXT,ACTION,
     *   NPSPAG(LP)
      IF (SEQNUM.NE.' ') WRITE (LPUNIT,30) CARRAG,TEXT,SEQNUM,ACTION,
     *   NPSPAG(LP)
C
10    RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
20    FORMAT (A,A,1X,A,' ON PAGE ',I3)
30    FORMAT (A,A,1X,' FOUND AT INPUT LINE ',A,1X,A,
     *   ' ON PAGE ',I3)
C
      END
