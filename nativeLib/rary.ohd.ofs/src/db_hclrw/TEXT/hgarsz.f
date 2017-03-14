C MEMBER HGARSZ
C  (from old member HCLPRTEC)
C-----------------------------------------------------------------------
C
C                             LAST UPDATE: 04/05/95.11:35:56 BY $WC20SV
C
C @PROCESS LVL(77)
C
      SUBROUTINE HGARSZ (KBUF,ISIZE)
C
C          ROUTINE:  HGARSZ
C
C             VERSION:  1.0.0
C
C                DATE:  4-8-82
C
C              AUTHOR:  JIM ERLANDSON
C                       DATA SCIENCES INC
C                       8555 16TH ST, SILVER SPRING, MD 587-3700
C***********************************************************************
C
C          DESCRIPTION:
C
C    ROUTINE LOOPS THROUGH A TECHNIQUE RECORD TO DETERMINE
C    THE SIZE OF ALL THE ARGUMENTS.
C
C***********************************************************************
C
C          ARGUMENT LIST:
C
C         NAME    TYPE  I/O   DIM   DESCRIPTION
C
C       KBUF       I     I     ?    ARRAY CONTAINING TECHNIQUE
C
C       ISIZE      I     O     1    SIZE OF ARGUMENTS
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
      INTEGER KBUF(*)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/db_hclrw/RCS/hgarsz.f,v $
     . $',                                                             '
     .$Id: hgarsz.f,v 1.1 1995/09/17 18:42:20 dws Exp $
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
      ISIZE=0
      NUM=KBUF(10)
      IF (NUM.EQ.0) GO TO 30
      IPOINT=13
C
C       LOOP THROUGH THE RECORD COUNTING THE SIZE OF ARGS
C
      DO 20 I=1,NUM
         IF (KBUF(IPOINT).LT.0) GO TO 10
         ISIZE=ISIZE+1
         IF (KBUF(IPOINT).EQ.5) ISIZE=ISIZE+6
         GO TO 20
C
C   HAVE A STRING TYPE
C
10       ISIZE=ISIZE+(-KBUF(IPOINT)+8) /4
         IPOINT=IPOINT+4
20       CONTINUE
C
30    IF (IHCLDB.GT.1) WRITE (IOGDB,40) NUM,ISIZE
40    FORMAT (' HGARSZ EXECUTED FOR ',I3,' ARGUMENTS - SIZE=',I3)
C
      RETURN
C
      END
