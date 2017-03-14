C MEMBER PIXACC
C  (from old member PRDISTAT)
C-----------------------------------------------------------------------
C
C                             LAST UPDATE: 07/22/94.08:14:23 BY $WC20SV
C
C @PROCESS LVL(77)
C
      SUBROUTINE PIXACC
C
C          ROUTINE:  PIXACC
C
C             VERSION:  1.0.0
C
C                DATE:  7-26-82
C
C              AUTHOR:  JIM ERLANDSON
C                       DATA SCIENCES INC
C
C***********************************************************************
C
C          DESCRIPTION:
C
C    THIS ROUTINE PRINTS THE NUMBER OF ACCESSES MADE TO THE
C    PROCESSED DATA BASE INDEX BY THE HASHING ALGORITHM.
C
C***********************************************************************
C
C          ARGUMENT LIST:
C
C         NAME    TYPE  I/O   DIM   DESCRIPTION
C
C       NONE
C
C***********************************************************************
C
C          COMMON:
C
      INCLUDE 'uio'
      INCLUDE 'udebug'
      INCLUDE 'prdcommon/pdftbl'
      INCLUDE 'prdcommon/preads'
      INCLUDE 'prdcommon/pmaxdm'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/prdutil/RCS/pixacc.f,v $
     . $',                                                             '
     .$Id: pixacc.f,v 1.1 1995/09/17 19:16:37 dws Exp $
     . $' /
C    ===================================================================
C
C
C***********************************************************************
C
C          DIMENSION AND TYPE DECLARATIONS:
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
      WRITE(LP,10)
10    FORMAT('0  *** TIME SERIES INDEX STATISTICS FOR READ ROUTINE ***'/
     1       '0',7X,'DATA TYPE   NUMBER OF ACCESSES'/8X,9('-'),
     2       3X,18('-')/)
C
C  CHECK IF ANY DATA TYPES
      IF(NUMDTP.GT.0) GO TO 30
      WRITE(LP,20)
20    FORMAT(' **NOTE** NO DATA TYPES ARE DEFINED')
      GO TO 60
C
C  PROCESS EACH DATA TYPE
30    DO 50 I=1,NUMDTP
         WRITE(LP,40) DATFIL(1,I),NUMACC(I)
40       FORMAT(10X,A4,12X,I5)
50       CONTINUE
C
60    RETURN
C
      END
