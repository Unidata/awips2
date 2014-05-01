C MEMBER PDWRRR
C  (from old member PDBRWRRS)
C-----------------------------------------------------------------------
C
C                             LAST UPDATE: 03/28/95.11:17:31 BY $WC20SV
C
C @PROCESS LVL(77)
C
      SUBROUTINE PDWRRR (IREC,LRECL,IRRBUF,ISTAT)
C
C          ROUTINE: PDWRRR
C
C             VERSION:  1.0.0
C
C                DATE:  1-6-83
C
C              AUTHOR:  JIM ERLANDSON
C                       DATA SCIENCES INC
C
C***********************************************************************
C
C          DESCRIPTION:
C
C    THIS ROUTINE WRITES A RRS RECORD. IT CALCULATES THE NUMBER
C    OF RECORDS TO WRITE FROM THE FIRST WORD OF THE RECORD.
C
C***********************************************************************
C
C          ARGUMENT LIST:
C
C         NAME    TYPE  I/O   DIM   DESCRIPTION
C
C       IREC       I     I     1    RECORD NUMBER TO WRITE
C
C       LRECL      I     I     1    LOGICAL RECORD LENGTH
C
C       IRRBUF     I     I     ?    ARRAY CONTAINING RECORD
C
C       ISTAT      I     O     1    STATUS INDICATOR
C                                     0=NORMAL RETURN
C                                     OTHER=ERROR
C
C***********************************************************************
C
C          COMMON:
C
      INCLUDE 'uio'
      INCLUDE 'udebug'
      INCLUDE 'pdbcommon/pdunts'
      INCLUDE 'pdbcommon/pdi2max'

C
C***********************************************************************
C
C          DIMENSION AND TYPE DECLARATIONS:
C
      INTEGER IRRBUF(1)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/db_pdbrw/RCS/pdwrrr.f,v $
     . $',                                                             '
     .$Id: pdwrrr.f,v 1.2 2002/05/15 13:45:17 hank Exp $
     . $' /
C    ===================================================================
C
C
C***********************************************************************
C
C          DATA:
C
C
C***********************************************************************
C
C
      IF (IPDTR.GT.1) WRITE (IOGDB,10)
10    FORMAT (' *** ENTER PDWRRR')
C
      ISTAT=0
C
CMGM 4/2002 THE RRS RECORD NUMBER (IREC) MAY BE STORED IN THE ISIBUF
C    ARRAY AS A NEGATIVE NUMBER. THIS ALLOWS TWICE AS MANY RECORD 
C    NUMBERS IN THE I2 ARRAY. CONVERT IREC TO A POSITIVE RECORD NUMBER
C    IREC2 TO READ/WRITE (UREADT/UWRITT) THE KPDRRS FILE.
CMGM
	IF(IREC.LT.0)IREC=IREC+2*I2MAX

C  CALCULATE NUMBER OF RECORDS
      NREC=(IRRBUF(1)+LRECL-1)/LRECL
C
C  WRITE RRS RECORDS
      CALL WVLRCD (KPDRRS,IREC,NREC,IRRBUF,LRECL,ISTAT)
      IF (IPDDB.GT.1) WRITE (IOGDB,20) IREC,NREC
20    FORMAT (' IREC=',I6,3X,'NREC=',I5)
C
      IF (IPDTR.GT.1) WRITE (IOGDB,30)
30    FORMAT (' *** EXIT PDWRRR')
C
      RETURN
C
      END
