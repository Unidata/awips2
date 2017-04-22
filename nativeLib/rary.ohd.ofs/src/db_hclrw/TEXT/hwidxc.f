C MEMBER HWIDXC
C  (from old member HCLIO1)
C-----------------------------------------------------------------------
C
C                             LAST UPDATE: 09/07/94.11:46:02 BY $WC20SV
C
C @PROCESS LVL(77)
C
      SUBROUTINE HWIDXC (ISTAT)
C
C          ROUTINE:  HWIDXC
C
C             VERSION:  1.0.0
C
C                DATE: 7-29-81
C
C              AUTHOR:  JIM ERLANDSON
C                       DATA SCIENCES INC
C
C***********************************************************************
C
C          DESCRIPTION:
C
C    ROUTINE TO WRITE THE CONTENTS OF COMMON HINDEX TO THE
C    FIRST 4 RECORDS OF THE LOCAL AND GLOBAL INDEXES
C
C***********************************************************************
C
C          ARGUMENT LIST:
C
C         NAME    TYPE  I/O   DIM   DESCRIPTION
C
C       ISTAT      I    O     1     STATUS INDICATOR
C                                       0=NORMAL RETURN
C                                       1=READ ERROR
C
C***********************************************************************
C
C          COMMON:
C
      INCLUDE 'uio'
      INCLUDE 'udebug'
      INCLUDE 'hclcommon/hdatas'
      INCLUDE 'hclcommon/hunits'
      INCLUDE 'hclcommon/hindx'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/db_hclrw/RCS/hwidxc.f,v $
     . $',                                                             '
     .$Id: hwidxc.f,v 1.1 1995/09/17 18:43:17 dws Exp $
     . $' /
C    ===================================================================
C
C
C***********************************************************************
C
C
      ISTAT=0
C
C  WRITE FIRST 4 RECORDS OF LOCAL INDEX FROM COMMON HINDEX
      IUNIT=KINDXL
      CALL WVLRCD (IUNIT,1,4,HINDEX,LRECLI,ISTAT)
      IF (ISTAT.NE.0) GO TO 10
C
C  WRITE FIRST 4 RECORDS OF GLOBAL INDEX FROM COMMON HINDEX
      IUNIT=KINDXG
      CALL WVLRCD (IUNIT,1,4,HINDEX(1,5),LRECLI,ISTAT)
      IF (ISTAT.NE.0) GO TO 10
      GO TO 30
C
10    ISTAT=1
      WRITE (LP,20) IUNIT
20    FORMAT ('0**ERROR** IN HWIDXC - WRITING TO UNIT ',I3,'.')
      CALL ERROR
C
30    IF (IHCLTR.GT.2) WRITE (IOGDB,40) ISTAT
40    FORMAT (' EXIT HWIDXC - ISTAT=',I2)
C
      RETURN
C
      END
