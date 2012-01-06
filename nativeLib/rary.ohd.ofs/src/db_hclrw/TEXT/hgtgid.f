C MEMBER HGTGID
C  (from old member HCLPROPT)
C-----------------------------------------------------------------------
C
C                             LAST UPDATE: 07/10/95.12:41:55 BY $WC20SV
C
C @PROCESS LVL(77)
C
      SUBROUTINE HGTGID (ID)
C
C          ROUTINE:  HGTGID
C
C             VERSION:  1.0.0
C
C                DATE:  9-3-81
C
C              AUTHOR:  JIM ERLANDSON
C                       DATA SCIENCES INC
C
C***********************************************************************
C
C          DESCRIPTION:
C
C    ROUTINE TO CONVERT THE GROUP IDENTIFIER FROM AN INTEGER TO
C    A CHARACTER CODE.
C
C***********************************************************************
C
C          ARGUMENT LIST:
C
C         NAME    TYPE  I/O   DIM   DESCRIPTION
C
C       ID         I    I/O    1    GROUP ID CODE
C                                   1=S
C                                   2=F
C                                   3=C
C                                   4=A
C
C***********************************************************************
C
C          COMMON:
C
      INCLUDE 'uio'
      INCLUDE 'udebug'
C
C***********************************************************************
C
C          DIMENSION AND TYPE DECLARATIONS:
C
      DIMENSION IDARY(4)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/db_hclrw/RCS/hgtgid.f,v $
     . $',                                                             '
     .$Id: hgtgid.f,v 1.1 1995/09/17 18:42:27 dws Exp $
     . $' /
C    ===================================================================
C
C
C***********************************************************************
C
C          DATA:
C
      DATA IDARY/4HS   ,4HF   ,4HC   ,4HA   /
C
C***********************************************************************
C
C
      IF (ID.LT.1.OR.ID.GT.4) GO TO 10
         ID=IDARY(ID)
         GO TO 20
10    ID=0
C
20    IF (IHCLDB.GT.0) WRITE (IOGDB,30) ID
30    FORMAT (' IN HGTGID - ID=',A4)
C
      RETURN
C
      END
