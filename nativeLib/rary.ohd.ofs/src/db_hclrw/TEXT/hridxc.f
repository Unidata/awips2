C MEMBER HRIDXC
C  (from old member HCLIO1)
C-----------------------------------------------------------------------
C
C                             LAST UPDATE: 09/07/94.11:46:02 BY $WC20SV
C
C @PROCESS LVL(77)
C
      SUBROUTINE HRIDXC (ISTAT)
C
C          ROUTINE:  HRIDXC
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
C    ROUTINE TO READ THE CONTENTS OF THE FIRST 4 RECORDS FROM THE
C    INDEX FILES FOR HCL INTO COMMON HINDEX
C    COMMON HINDEX CONTAINS THE INDEX CONTROL POINTERS
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
     .$Source: /fs/hseb/ob72/rfc/ofs/src/db_hclrw/RCS/hridxc.f,v $
     . $',                                                             '
     .$Id: hridxc.f,v 1.1 1995/09/17 18:43:02 dws Exp $
     . $' /
C    ===================================================================
C
C
C***********************************************************************
C
C
      ISTAT=0
C
C  READ FIRST 4 RECORDS OF LOCAL INDEX INTO COMMON HINDEX
      IUNIT=KINDXL
      CALL RVLRCD (IUNIT,1,4,HINDEX,LRECLI,ISTAT)
      IF (ISTAT.NE.0) GO TO 10
C
C  READ FIRST 4 RECORDS OF GLOBAL INDEX INTO COMMON HINDEX
      IUNIT=KINDXG
      CALL RVLRCD (IUNIT,1,4,HINDEX(1,5),LRECLI,ISTAT)
      IF (ISTAT.NE.0) GO TO 10
      GO TO 30
C
10    ISTAT=1
      WRITE (LP,20) IUNIT
20    FORMAT ('0**ERROR** IN HRIDXC - READING FROM UNIT ',I3,'.')
      CALL ERROR
C
30    IF (IHCLTR.GT.2) WRITE (IOGDB,40) ISTAT
40    FORMAT (' EXIT HRIDXC - ISTAT=',I2)
C
      RETURN
C
      END
