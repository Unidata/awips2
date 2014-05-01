C MEMBER SUCOMP
C  (from old member suparse)
C-----------------------------------------------------------------------
C
C                             LAST UPDATE: 08/25/94.12:28:06 BY $WC20SV
C
C @PROCESS LVL(77)
C
      SUBROUTINE SUCOMP (NWORDS,NAME1,NAME2,ISTAT)
C
C  ROUTINE COMPARES TWO CHARACTER STRINGS
C
      DIMENSION NAME1(1),NAME2(1)
C
      INCLUDE 'udebug'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shared_s/RCS/sucomp.f,v $
     . $',                                                             '
     .$Id: sucomp.f,v 1.1 1995/09/17 19:21:39 dws Exp $
     . $' /
C    ===================================================================
C
C
      ISTAT=0
C
      DO 10 I=1,NWORDS
         IF (NAME1(I).NE.NAME2(I)) GO TO 20
10    CONTINUE
C
      ISTAT=1
C
20    IF (IUTLDB.GT.2) WRITE (IOGDB,30) (NAME1(I),I=1,NWORDS),
     *   (NAME2(I),I=1,NWORDS),ISTAT
C
      RETURN
C
30    FORMAT (' *** EXIT SUCOMP : NAME1=',2A4,3X,'NAME2=',2A4,3X,
     *  'ISTAT=',I3)
C
      END
