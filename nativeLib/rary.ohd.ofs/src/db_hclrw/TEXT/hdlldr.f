C MEMBER HDLLDR
C  (from old member HCLDLLDR)
C-----------------------------------------------------------------------
C
C @PROCESS LVL(77)
C
      SUBROUTINE HDLLDR (NAME,ITYPE,ISTAT)
C
C          ROUTINE:  HDLLDR
C
C             VERSION:  1.0.0
C
C                DATE:  3-18-82
C
C              AUTHOR:  JIM ERLANDSON
C                       DATA SCIENCES INC
C
C***********************************************************************
C
C          DESCRIPTION:
C
C    ROUTINE TO DELETE LOCAL DEFINITION REFERENCE RECORDS.
C
C***********************************************************************
C
C          ARGUMENT LIST:
C
C         NAME    TYPE  I/O   DIM   DESCRIPTION
C
C       NAME       A8    I     2    NAME OF DEFINITION
C
C       ITYPE      I     I     1    TYPE OF RECORD
C
C       ISTAT      I     O     1    STATUS INDICATOR
C                                   0=OK   1= ERROR
C
C***********************************************************************
C
C          DIMENSION AND TYPE DECLARATIONS:
C
      INTEGER LDRARR(1),NAME(2)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/db_hclrw/RCS/hdlldr.f,v $
     . $',                                                             '
     .$Id: hdlldr.f,v 1.1 1995/09/17 18:42:05 dws Exp $
     . $' /
C    ===================================================================
C
C
C  SET FLAG TO INDICATE JUST DELETION
C
      IFLAG=1
C
C  DELETE
C   (LDRARR AND NUMLDR ARE DUMMY VARIABES ONLY
C    AND NOT USED BY HWNDLR FOR DELETIONS ONLY)
C
      CALL HDNWLD (NAME,ITYPE,LDRARR,NUMLDR,IFLAG,ISTAT)
C
C
      RETURN
C
      END
