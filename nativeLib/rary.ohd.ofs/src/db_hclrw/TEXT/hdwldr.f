C MEMBER HDWLDR
C  (from old member HCLDLLDR)
C-----------------------------------------------------------------------
C
C                             LAST UPDATE: 07/21/94.14:27:21 BY $WC20SV
C
      SUBROUTINE HDWLDR (NAME,ITYPE,LDRARR,NUMLDR,ISTAT)
C
C          ROUTINE:  HDWLDR
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
C    ROUTINE TO DELETE AND WRITE LOCAL DEFINITION REFERENCE
C    RECORDS.
C    AND REWRITING.
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
C       LDRARR     I     I     ?    ARRAY CONTAINIG POINTERS
C
C       NUMLDR     I     I     1    NUMBER OF POINTERS IN LDRARR
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
     .$Source: /fs/hseb/ob72/rfc/ofs/src/db_hclrw/RCS/hdwldr.f,v $
     . $',                                                             '
     .$Id: hdwldr.f,v 1.1 1995/09/17 18:42:11 dws Exp $
     . $' /
C    ===================================================================
C
C
C  SET FLAG TO INDICATE BOTH DELETION AND REWRITING
C
      IFLAG=0
C
C  DELETK
C
      CALL HDNWLD (NAME,ITYPE,LDRARR,NUMLDR,IFLAG,ISTAT)
C
C
      RETURN
C
      END
