C MEMBER HSTATS
C  (from old member HCLSTATS)
C-----------------------------------------------------------------------
C
C                             LAST UPDATE: 04/06/95.08:46:22 BY $WC20SV
C
C @PROCESS LVL(77)
C
C  PRINT STATUS OF HCL DATABASE
C
      SUBROUTINE HSTATS (ISTAT)
C
C
      CHARACTER*15 PTYPE1,PTYPE2
C
      INCLUDE 'uio'

      INCLUDE 'udebug'
      INCLUDE 'hclcommon/hcntrl'
      INCLUDE 'hclcommon/hindx'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/db_hclrw/RCS/hstats.f,v $
     . $',                                                             '
     .$Id: hstats.f,v 1.1 1995/09/17 18:43:12 dws Exp $
     . $' /
C    ===================================================================
C
C
C
      IF (IHCLTR.GT.0) THEN
         CALL ULINE (IOGDB,1)
         WRITE (IOGDB,20)
         ENDIF
C
      ISTAT=0
C
      CALL ULINE (LP,2)
      WRITE (LP,30)
      CALL ULINE (LP,0)
      WRITE (LP,40)
      WRITE (LP,40)
      WRITE (LP,40)
C
C  OPEN DATA BASE
CCC   CALL SUDOPN (1,'UPRM',IERR)
CCC   IF (IERR.GT.0) GO TO 10
C
C  PRINT FILE INFORMATION
      DO 5 J=1,7
         ITYPE=HINDEX(4,J)
         CALL UREPET ('?',PTYPE1,LEN(PTYPE1))
         CALL UREPET ('?',PTYPE2,LEN(PTYPE2))
         IF (ITYPE.EQ.4) THEN
            PTYPE1='NAMED'
            PTYPE2='OPTIONS'
            ELSE
               IF (ITYPE.GT.0) PTYPE1='LOCAL'
               IF (ITYPE.LE.0) PTYPE1='GLOBAL'
               IF (IABS(ITYPE).EQ.1) PTYPE2='PROCEDURE'
               IF (IABS(ITYPE).EQ.2) PTYPE2='FUNCTION'
               IF (IABS(ITYPE).EQ.3) PTYPE2='TECHNIQUES'
            ENDIF
         CALL ULINE (LP,2)
         WRITE (LP,50)
     *      PTYPE1(1:LENSTR(PTYPE1)),
     *      PTYPE2(1:LENSTR(PTYPE2)),
     *      (HINDEX(I,J),I=1,3)
5        CONTINUE
      CALL ULINE (LP,2)
      WRITE (LP,55) (HINDEX(I,8),I=1,4)
C
10    IF (IHCLTR.GT.0) THEN
         CALL ULINE (IOGDB,1)
         WRITE (IOGDB,150)
         ENDIF
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
20    FORMAT (' *** ENTER HSTATS')
30    FORMAT ('0')
40    FORMAT ('+*--> HYDROLOGIC COMMAND LANGUAGE FILE STATUS')
50    FORMAT ('0TYPE=',A,' ',A,' : ',3(I5,1X))
55    FORMAT ('0',3(I5,1X),I5)
150   FORMAT (' *** EXIT HSTATS')
C
      END
