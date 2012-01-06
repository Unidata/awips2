C MEMBER HGTDFR
C  (from old member HCLCMPST)
C-----------------------------------------------------------------------
C
C                             LAST UPDATE: 04/07/95.14:40:21 BY $WC20SV
C
C @PROCESS LVL(77)
C
      SUBROUTINE HGTDFR (ITYPE,IDREC1,IDREC2,MDFBUF,IDFBUF,ISTAT)
C
C   THIS ROUTINE WILL GET A DEFAULT RECORD FOR A DEFINITION.
C
C     ARGUMENT LIST:
C
C         NAME    TYPE  I/O   DIM   DESCRIPTION
C
C      ITYPE       I     I     1     TYPE OF DEFINITION +LOCAL, -GLOBAL
C      IDREC1      I     I     1     RECORD OF LOCAL DEFAULT
C      IDREC2      I     I     1     RECORD OF GLOBAL DEFAULT
C      MDFBUF      I     I     1     DIMENSION OF IDFBUF
C      IDFBUF      I     O   MDFBUF  BUFFER TO RECEIVE THE DEFAULT REC
C      ISTAT       I     O      1    STATUS 0=OK, ELSE ERROR
C
      CHARACTER*8 RTYPE/' '/
C
      DIMENSION IDFBUF(1)
      DIMENSION ITEMP(2)
C
      INCLUDE 'uio'
      INCLUDE 'udebug'
      INCLUDE 'common/where'
      INCLUDE 'hclcommon/hunits'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/db_hclrw/RCS/hgtdfr.f,v $
     . $',                                                             '
     .$Id: hgtdfr.f,v 1.1 1995/09/17 18:42:25 dws Exp $
     . $' /
C    ===================================================================
C
C
C
      CALL UMEMOV (OPNAME,ITEMP,2)
      CALL UMEMOV ('HGTDFR  ',OPNAME,2)
C
      IF (ITYPE) 30,10,40
C
10    WRITE (LP,20) ITYPE
20    FORMAT ('0**ERROR** IN HGTDFR - INVALID VALUE OF ITYPE : ',I3)
      CALL ERROR
      GO TO 70
C
C  GET GLOBAL RECORD
30    CALL HGTRDN (KDEFNG,IDREC2,IDFBUF,MDFBUF,ISTAT)
      RTYPE='GLOBAL'
      IFILE=KDEFNG
      IREC=IDREC2
      IF (ISTAT.NE.0) GO TO 50
      GO TO 70
C
C  GET LOCAL DEFAULT
40    CALL HGTRDN (KDEFNL,IDREC1,IDFBUF,MDFBUF,ISTAT)
      RTYPE='LOCAL'
      IFILE=KDEFNL
      IREC=IDREC1
      IF (ISTAT.EQ.0) GO TO 70
C
50    WRITE (LP,60) RTYPE(1:LENSTR(RTYPE)),IREC,IFILE
60    FORMAT ('0**ERROR** IN HGTDFR - TRYING TO READ ',A,
     *   'DEFAULTS FROM RECORD ',I6,' OF UNIT ',I2,'.')
      CALL ERROR
C
70    IF (IHCLDB.GT.0) WRITE (IOGDB,80) IDREC1,IDREC2,ISTAT
80    FORMAT (' IN HGTDFR - DEFAULT RECS ',2I6,' STAT=',I6)
C
      CALL UMEMOV (ITEMP,OPNAME,2)
C
      RETURN
C
      END
