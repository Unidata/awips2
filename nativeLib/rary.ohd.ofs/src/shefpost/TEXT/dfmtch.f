C MODULE DFMTCH
C----------------------------------------------------------------------
C
      SUBROUTINE DFMTCH (MSHPDB,ISHPDB,NSHPDB,ICODE,ITYPE,INDEX)
C
C  THIS ROUTINE FINDS THE SHEF ENCODED PARAMETER CODE IN THE
C  SHEF/PPDB FILE AND RETURNS THE CORRESPONDING PPDB DATA TYPE.
C
C  ARGUMENT LIST:
C
C    NAME      TYPE   I/O   DIM   DESCRIPTION
C    ------    ----   ---   ---   -----------
C    ICODE      A8     I     2    SHEF EXPANDED PARAMETER CODE
C    ITYPE      A4     O     1    PPDB DATA TYPE
C    INDEX      I      O     1    SUBSCRIPT OF TYPE IN ISHPDB
C
      DIMENSION ISHPDB(9,MSHPDB)
C
      INCLUDE 'uiox'
      INCLUDE 'udebug'
      INCLUDE 'udatas'
C
      DIMENSION ICODE(2),ICODE2(2)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shefpost/RCS/dfmtch.f,v $
     . $',                                                             '
     .$Id: dfmtch.f,v 1.2 2002/02/11 21:29:33 dws Exp $
     . $' /
C    ===================================================================
C
      DATA IBLANK/4H    /,LETF/4HF   /
C
C
      IF (IDETR.EQ.1) WRITE(LP,*) 'ENTER DFMTCH'
C
      ITYPE = 0
      INDEX = 0
      ISITF=IBLANK
C
C  SAVE CODE FOR ERROR MESSAGE AND RETURN
      CALL UMEMOV (ICODE,ICODE2,2)
C
C  MATCH SHEF PARAMETER CODE
      DO 200 I=1,NSHPDB
         CALL UNAMCP (ICODE,ISHPDB(2,I),MTCH)
         IF (MTCH.EQ.0) GO TO 250
  200    CONTINUE
C
C  BLANK DURATION CODE IN SHEF PARAMETER CODE
C  AND CHECK FOR MATCH AGAIN
      CALL UMOVEX (IBLNK,1,ICODE(1),3,1)
      DO 210 I=1,NSHPDB
         IF (ISHPDB(5,I).NE.-1) GO TO 210
         CALL UNAMCP (ICODE,ISHPDB(2,I),MTCH)
         IF (MTCH.EQ.0) GO TO 250
  210    CONTINUE
C
C  CODE NOT FOUND
      CALL UMOVEX (ICODE(1),4,ISITF,1,1)
      IF (ISITF.NE.LETF) GO TO 230
      ITYPE=-1
      GO TO 900
C
230   WRITE (LP,235) ICODE2
  235 FORMAT ('0**WARNING** SHEF PARAMETER CODE ',2A4,
     *   ' CANNOT BE POSTED TO THE PPDB.')
      GO TO 900
C
C  GET DATA TYPE FROM TRANSLATION TABLE
250   ITYPE = ISHPDB(1,I)
      INDEX = I
C
C  RESET CODE
900   CALL UMEMOV (ICODE2,ICODE,2)
C
      IF(IDETR.EQ.1) WRITE (LP,925) INDEX,ITYPE
  925 FORMAT (' EXIT DFMTCH - INDEX=',I3,' ITYPE=',A4)
C
      RETURN
C
      END
