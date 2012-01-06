C MODULE HGTRCD
C-----------------------------------------------------------------------
C
      SUBROUTINE HGTRCD (ITYPE,NAME,MAXWDS,BUFFER,ISTAT)
C
C  ROUTINE TO GET A RECORD FROM THE APPROPRIATE HCL FILE.
C  FINDS NAME IN INDEX AND GETS POINTER TO CORRESPONDING
C  HCL FILE.  READS ALL RECORDS FOR NAMED DEFINITION INTO BUFFER.
C  CHECKS FOR VALID NAME, RECORD TYPE, SYSTEM ERRORS, AND FOR
C  AVAILABLE SPACE IN BUFFER.
C
C  ARGUMENT LIST:
C
C    NAME    TYPE  I/O     DIM   DESCRIPTION
C    ------  ----  -----   ---   ------------
C    ITYPE    I    INPUT    1    RECORD TYPE:
C                                  -=GLOBAL
C                                  +=LOCAL
C                                  1=PROCEDURE
C                                  2=FUNCTION
C                                  3=TECHNIQUE
C                                  4=NAMED OPTIONS
C    NAME     A    INPUT    2    DEFINED NAME
C    MAXWDS   I    INPUT    1    SIZE OF BUFFER
C    BUFFER   I    OUTPUT MAXWDS RETRIEVED RECORD
C    ISTAT    I    OUTPUT  1     STATUS INDICATOR:
C                                  0=RECORD RETRIEVED
C                                  1=NOT FOUND
C                                  2=INVALID RECORD TYPE
C                                  3=BUFFER TO SMALL
C                                  4=SYSTEM ERROR
C
      CHARACTER*8 RTNNAM,RTNOLD
      INTEGER BUFFER(MAXWDS)
C
      DIMENSION NAME(2),I1(2)
C
      INCLUDE 'uiox'
      INCLUDE 'udebug'
      INCLUDE 'hclcommon/hunits'
      INCLUDE 'hclcommon/hcntrl'
      INCLUDE 'hclcommon/hwords'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/db_hclrw/RCS/hgtrcd.f,v $
     . $',                                                             '
     .$Id: hgtrcd.f,v 1.3 2001/06/13 12:09:19 mgm Exp $
     . $' /
C    ===================================================================
C
C
      RTNNAM='HGTRCD'
C
      IF (IHCLTR.GT.1) WRITE (LP,*) 'ENTER ',RTNNAM
C
      IOPNUM=-1
      CALL FSTWHR (RTNNAM,IOPNUM,RTNOLD,IOLDOP)
C
      NWDS=0
      ISTAT=0
      NWDS=1
C
C  CHECK FOR VALID TYPE
      IF (ITYPE.EQ.0) GO TO 20
      IF (ITYPE.LT.-3.OR.ITYPE.GT.4) GO TO 20
      GO TO 40
C
20    WRITE (LP,30) NAME,ITYPE
30    FORMAT ('0**ERROR** IN HGTRCD - RECORD ',2A4,' HAS INVALID TYPE ',
     *   I2,' WAS PASSED - VALID RANGE IS -3 TO -1, 1 TO 4.')
      CALL ERROR
      ISTAT=2
      GO TO 170
C
C  FIND RECORD NUMBER FROM INDEX
40    CALL HFNDDF (NAME,IREC,ITYPE,IXREC)
      IF (IREC.EQ.0) THEN
         WRITE (LP,50) NAME
50    FORMAT ('0**ERROR** IN HGTRCD - RECORD ',2A4,' NOT FOUND.')
         CALL ERROR
         ISTAT=1
         GO TO 170
         ENDIF
C
C  CHECK ITYPE TO DETERMINE FILES TO USE
      IDUNIT=KDEFNL
      ISUB=1
      CALL UMEMOV (LLOCAL,I1,2)
C
      IF (ITYPE.GE.1) GO TO 70
      IDUNIT=KDEFNG
      ISUB=2
      CALL UMEMOV (LGLOBL,I1,2)
C
C  READ THE RECORD
70    IF (IREC.GT.HCNTL(7,ISUB)) GO TO 130
C
      LRECL=HCNTL(4,ISUB)
      IF (LRECL.GT.MAXWDS) GO TO 100
      CALL UREADT (IDUNIT,IREC,BUFFER,ISTA)
      IF (ISTA.NE.0) GO TO 130
      IF (IHCLDB.GT.2) WRITE (IOGDB,80) NAME,I1,IREC,IDUNIT
80    FORMAT (' IN HGTRCD - NAME=',2A4,' I1=',2A4,' IREC=',I6,
     *   ' IDUNIT=',I2)
      CALL UNAMCP (NAME,BUFFER(4),ISTA)
      IF (ISTA.NE.0) GO TO 130
C
      NLREC=BUFFER(1)
C
      NUMREC=NLREC-1
      MAXREC=HCNTL(6,ISUB)
      IF (IREC+NUMREC.GT.MAXREC) GO TO 130
C
      ISPO=LRECL+1
      NXTREC=IREC+1
      NWDS=NLREC*LRECL
      IF (NLREC.EQ.1) GO TO 170
      IF (NWDS.LE.MAXWDS) GO TO 120
C
100   WRITE (LP,110) NAME
110   FORMAT ('0**ERROR** IN HGTRCD - ARRAY TOO SMALL FOR RECORD ',2A4,
     *   '.')
      CALL ERROR
      ISTAT=3
      GO TO 190
C
C  READ REST OF RECORD
120   CALL RVLRCD (IDUNIT,NXTREC,NUMREC,BUFFER(ISPO),LRECL,ISTA)
      IF (ISTA.EQ.0) GO TO 170
C
C  SYSTEM ERROR
130   WRITE (LP,140) NAME
140   FORMAT ('0**ERROR** IN HGTRCD - SYSTEM ERROR FOR RECORD ',2A4,'.')
      CALL ERROR
      ISTAT=4
      GO TO 190
C
C  WRONG RECORD TYPE
155   WRITE (LP,160) NAME
160   FORMAT ('0**ERROR** IN HGTRCD - RECORD ',2A4,' FOUND IN LOCAL ',
     *   'INDEX BUT GLOBAL WAS SPECIFIED')
      CALL ERROR
      ISTAT=1
      GO TO 190
C
170   IF (NWDS.EQ.0) GO TO 190
      IF (IHCLDB.GT.2) WRITE (IOGDB,180) (BUFFER(I),I=1,NWDS)
180   FORMAT (' RECORD ',3I4,1X,3A4,(1X,10I6))
C
190   CALL FSTWHR (RTNOLD,IOLDOP,RTNOLD,IOLDOP)
C
      IF (IHCLTR.GT.2) WRITE (IOGDB,*) 'EXIT ',RTNNAM,' - ISTAT=',ISTAT
C
      RETURN
C
      END
