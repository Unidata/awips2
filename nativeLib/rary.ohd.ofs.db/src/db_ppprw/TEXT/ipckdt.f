C MEMBER IPCKDT
C  (from old member PPPCHECK)
C-----------------------------------------------------------------------
C
       FUNCTION IPCKDT (ITYPE)
C
C          FUNCTION:  IPCKDT
C
C             VERSION:  1.0.0
C
C                DATE:  11-30-82
C
C              AUTHOR:  SONJA R SIEGEL
C                       DATA SCIENCES INC
C                       8555 16TH ST, SILVER SPRING, MD 587-3700
C***********************************************************************
C
C          DESCRIPTION:
C
C  ROUTINE SEARCHES THE PREPROCESSOR PARAMETRIC DATA BASE
C  DATA TYPE DIRECTORY FOR SPECIFIED TYPE.
C
C***********************************************************************
C
C          ARGUMENT LIST:
C
C         NAME    TYPE  I/O   DIM   DESCRIPTION
C
C       ITYPE      A4    I     1    PARAMETER TYPE
C
C***********************************************************************
C
C          COMMON:
C
      INCLUDE 'udebug'
      INCLUDE 'ucommon/uordrx'
      INCLUDE 'pppcommon/ppdtdr'
      INCLUDE 'pppcommon/ppxctl'
      INCLUDE 'urcommon/urxctl'
      INCLUDE 'urcommon/urppdt'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/db_ppprw/RCS/ipckdt.f,v $
     . $',                                                             '
     .$Id: ipckdt.f,v 1.1 1995/09/17 18:45:06 dws Exp $
     . $' /
C    ===================================================================
C
C
C***********************************************************************
C
C
      IF (IPPTR.GT.0) WRITE (IOGDB,10)
C
      IF (IAMORD.EQ.0) NTYPE=NMPTYP
      IF (IAMORD.EQ.1) NTYPE=NUMPTP
C
      IF (IPPDB.GT.0) WRITE (IOGDB,15) IAMORD,NTYPE
C
      DO 20 I=1,NTYPE
         IF (IAMORD.EQ.0.AND.ITYPE.EQ.IPDTDR(1,I)) GO TO 30
         IF (IAMORD.EQ.1.AND.ITYPE.EQ.JPDTDR(1,I)) GO TO 30
20       CONTINUE
C
C  TYPE NOT FOUND
      IPCKDT=0
      GO TO 40
C
C  TYPE FOUND - SET TO SUBSCRIPT
30    IPCKDT=I
C
40    IF (IPPDB.GT.0) WRITE (IOGDB,50) ITYPE,IPCKDT
C
      IF (IPPTR.GT.0) WRITE (IOGDB,60)
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
10    FORMAT (' *** ENTER IPCKDT')
15    FORMAT (' IAMORD=',I2,3X,'NTYPE=',I3)
50    FORMAT (' ITYPE=',A4,3X,'IPCKDT=',I3)
60    FORMAT (' *** EXIT IPCKDT')
C
      END
