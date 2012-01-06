C MEMBER PMOVTS
C  (from old member PRDWPRDC)
C-----------------------------------------------------------------------
C
C                             LAST UPDATE: 02/06/95.17:08:15 BY $WC21DT
C
C @PROCESS LVL(77)
C
      SUBROUTINE PMOVTS (IREC,INDX,IFUT,NREC,LU,ITSID,ITYPE,ISTAT)
C
C          ROUTINE:  PMOVTS
C
C             VERSION:  1.0.0
C
C                DATE:  7-21-82
C
C              AUTHOR:  JIM ERLANDSON
C                       DATA SCIENCES INC
C
C***********************************************************************
C
C          DESCRIPTION:
C
C    THIS ROUTINE MOVES A TIME SERIES RECORD TO THE END OF THE FILE.
C    IT CHECKS FOR ROOM IN FILE. THE INDEX RECORD, DATA TYPE INDEX,
C    TIME SERIES FILE CONTROL RECORD, AND LAST RECORD OF TYPE ARE
C    UPDATED.  THE OLD RECORD IS DELETED. IF ITS A FUTURE, ALL REGULAR
C    RECORDS ARE CHECKED AND ANY USING OLD ARE CHANGED TO NEW RECNO.
C
C***********************************************************************
C
C          ARGUMENT LIST:
C
C         NAME    TYPE  I/O   DIM   DESCRIPTION
C
C       IREC       I    I/O    1    RECORD NUMBER OF TS RECORD
C       INDX       I     I     1    INDEX TO TYPE IN DATA TYPE INDEX
C       IFUT       I     I     1    FUTURE TYPE INDICATOR
C                                    0=NOT FUTURE
C                                    1=FUTURE
C       NREC       I     I     1    NUMBER OF LOGICAL RECORDS IN REC
C       LU         I     I     1    LOGICAL UNIT FOR RECORD
C       ITSID     A8     I     2    TIME SERIES ID
C       ITYPE     A4     I     1    DATA TYPE CODE
C       ISTAT      I     O     1    STATUS INDICATOR
C
C***********************************************************************
C
C          COMMON:
C
      INCLUDE 'uio'
      INCLUDE 'udebug'
      INCLUDE 'prdcommon/ptsctl'
      INCLUDE 'prdcommon/pdftbl'
      INCLUDE 'prdcommon/punits'
C
C***********************************************************************
C
C          DIMENSION AND TYPE DECLARATIONS:
C
      INTEGER TBUF(16),IXBUF(4),LDEL(2),ITSID(2)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/db_prdrw/RCS/pmovts.f,v $
     . $',                                                             '
     .$Id: pmovts.f,v 1.1 1995/09/17 18:45:43 dws Exp $
     . $' /
C    ===================================================================
C
C
C***********************************************************************
C
C          DATA:
C
      DATA LDEL/4HDELE,4HTED /
C
C***********************************************************************
C
C
      IF (IPRTR.GT.0) WRITE (IOGDB,99)
99    FORMAT (' *** ENTER PMOVTS')
C
      ISTAT=0
C
C  GET NEW RECORD NUMBER
      CALL PGETLU (LU,IX)
      INREC=TSCNTR(3,IX)
C
C  SEE IF ROOM IN THE FILE
      IF (INREC+NREC-1.LE.TSCNTR(2,IX)) GO TO 50
      WRITE (LPE,49) LU
 49   FORMAT (' **ERROR** IN PMOVTS - NOT ENOUGH ROOM IN FILE # ',I2,
     *   ' TO WRITE A NEW RECORD.')
      ISTAT=1
      GO TO 999
C
C  PUT POINTER TO NEW RECORD IN LAST OF TYPE
50    CALL UREADT (LU,DATFIL(9,INDX),TBUF,ISTAT)
      IF (ISTAT.NE.0) GO TO 900
      TBUF(13)=INREC
      CALL UWRITT (LU,DATFIL(9,INDX),TBUF,ISTAT)
      IF (ISTAT.NE.0) GO TO 900
      IF (IPRDB.GT.0) WRITE (IOGDB,59) (TBUF(I),I=4,6),TBUF(13)
 59   FORMAT ('  CHANGED POINTER IN LAST RECORD ',2A4,2X,A4,' TO ',I5)
C
C  UPDATE CONTROLS
      TSCNTR(3,IX)=INREC+NREC
      DATFIL(9,INDX)=INREC
C
C  CHANGE INDEX RECORD
      CALL PSERCH (ITSID,ITYPE,IFREE,IXREC,IXBUF)
      IF (IXREC.EQ.0) GO TO 999
C
C  TIME SERIES FOUND
      IXBUF(4)=INREC
      CALL UWRITT (KINDEX,IXREC,IXBUF,ISTAT)
      IF (ISTAT.NE.0) GO TO 900
      IF (IPRDB.GT.0) WRITE (IOGDB,69) IXBUF
 69   FORMAT ('  CHANGED INDEX RECORD TO ',2A4,2X,A4,I7)
C
C  DELETE THE OLD RECORD
      CALL UREADT (LU,IREC,TBUF,ISTAT)
      IF (ISTAT.NE.0) GO TO 900
      IF (IPRDB.GT.0) WRITE (IOGDB,79) (TBUF(I),I=4,5)
 79   FORMAT ('  DELETEING TIME SERIES ',2A4)
      CALL UMEMOV (LDEL,TBUF(4),2)
      TBUF(11)=0
      CALL UWRITT (LU,IREC,TBUF,ISTAT)
      IF (ISTAT.NE.0) GO TO 900
C
C  CHECK IF IS A FUTURE TIME SERIES
      IF (IFUT.EQ.0) GO TO 999
      IXREG=-DATFIL(7,INDX)
      IF (IXREG.LE.0) GO TO 999
      IS=DATFIL(8,IXREG)
      LUR=DATFIL(2,IXREG)
C
C  READ A REGULAR RECORD-SEE IF USED FUT
100   CALL UREADT (LUR,IS,TBUF,ISTAT)
      IF (ISTAT.NE.0) GO TO 900
      IF (TBUF(11).NE.IREC) GO TO 150
C
C  CHANGE POINTER TO FUTURE RECORD AND WRITE IT BACK
      TBUF(11)=INREC
      CALL UWRITT (LUR,IS,TBUF,ISTAT)
      IF (ISTAT.NE.0) GO TO 900
      IF (IPRDB.GT.0) WRITE (IOGDB,149) (TBUF(I),I=4,6),TBUF(11)
 149  FORMAT ('  CHANGING POINTER IN RECORD ',2A4,2X,A4,' TO ',I6)
C
C  SEE IF THERE ARE MORE
150   IS=TBUF(13)
      IF (IS.NE.0) GO TO 100
      GO TO 999
C
C  SYSTEM ERROR
900   WRITE (LPE,909)
 909  FORMAT (' **ERROR** IN PMOVTS - SYSTEM ERROR')
      ISTAT=1
C
 999  CONTINUE
C
      IREC=INREC
C
      IF (IPRTR.GT.0) WRITE (IOGDB,98)
98    FORMAT (' *** EXIT PMOVTS')
C
      RETURN
C
      END
