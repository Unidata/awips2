C MODULE SNSORT
C-----------------------------------------------------------------------
C
C  ROUTINE TO SORT THE NETWORK COMMON BLOCK.
C
      SUBROUTINE SNSORT (ISORT,LARRAY,ARRAY,ISTAT)
C
      CHARACTER*4 XSORT
C
      DIMENSION ARRAY(LARRAY)
      DIMENSION ISTATE(1),ISTAID(2,1),IDESCR(5,1)
C
      INCLUDE 'uiox'
      INCLUDE 'scommon/sudbgx'
      INCLUDE 'scommon/sntwkx'
C
      CHARACTER*2 ISTATE
      CHARACTER*4 ISTAID,IDESCR
      EQUIVALENCE (ISTATE(1),STATNW(1))
      EQUIVALENCE (ISTAID(1,1),STIDNW(1,1))
      EQUIVALENCE (IDESCR(1,1),STIDNW(1,1))
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/ppinit_network/RCS/snsort.f,v $
     . $',                                                             '
     .$Id: snsort.f,v 1.3 2002/02/11 20:55:29 dws Exp $
     . $' /
C    ===================================================================
C
C
C  SET TRACE LEVEL
      CALL SBLTRC ('NTWK','NTWKSORT','SNSORT  ',LTRACE)
C
      IF (LTRACE.GT.0) THEN
         WRITE (IOSDBG,*) 'ENTER SNSORT'
         CALL SULINE (IOSDBG,1)
         ENDIF
C
C  SET DEBUG LEVEL
      CALL SBLDBG ('NTWK','NTWKSORT','SNSORT  ',LDEBUG)
C
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,*) 'ISORT=',ISORT,' LARRAY=',LARRAY
         CALL SULINE (IOSDBG,1)
         ENDIF
C      
      ISTAT=0
C
C  CHECK FOR VALID ORDER TYPE :
C    1 = SORT BY IDENTIFIER
C    2 = SORT BY DESCRIPTION
      IF (ISORT.EQ.1.OR.ISORT.EQ.2) THEN
         ELSE
            WRITE (LP,400) ISORT
            CALL SUERRS (LP,2,-1)
            ISTAT=1
            GO TO 370
         ENDIF
C
C  CHECK IF NETWORK COMMON BLOCK LOADED
      IF (ISORT.EQ.1) IRTP=3
      IF (ISORT.EQ.2) IRTP=4
      IF (INWTYP.NE.IRTP) THEN
         INWSRT=0
         CALL SNSTAN (IRTP,LARRAY,ARRAY,IERR)
         IF (IERR.NE.0) THEN
            WRITE (LP,410)
            CALL SUERRS (LP,2,-1)
            ISTAT=1
            GO TO 370
            ENDIF
         ENDIF
C
C  CHECK NUMBER OF STATIONS IN COMMON BLOCK
      IF (INWFIL.LE.0) THEN
         WRITE (LP,420)
         CALL SUWRNS (LP,2,-1)
         ISTAT=1
         GO TO 370
         ENDIF
C
      IF (LDEBUG.GT.0) THEN
         IF (IOSDBG.EQ.LP.AND.ISLEFT(5).GT.0) CALL SUPAGE
         WRITE (IOSDBG,470)
         CALL SULINE (IOSDBG,2)
         CALL SUNTWK (IOSDBG)
         ENDIF
C
C  CHECK IF NETWORK COMMON BLOCK SORTED
      IF (INWSRT.EQ.0) GO TO 45
         IF (LDEBUG.GT.0) THEN
            WRITE (IOSDBG,473)
            CALL SULINE (IOSDBG,1)
            ENDIF
         GO TO 370
C
45    NUM=INWFIL-1
C
      IF (NUM.LE.0) GO TO 370
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  SORT BY STATE
C
      DO 60 N=1,NUM
         IND=0
         DO 50 I=1,NUM
            IF (LDEBUG.GT.0) THEN
               WRITE (IOSDBG,*) 'I=',I,
     *            ' ISTATE(I)=',ISTATE(I),
     *            ' ISTATE(I+1)=',ISTATE(I+1)
               CALL SULINE (IOSDBG,1)
               ENDIF
            IF (ISTATE(I).LE.ISTATE(I+1)) GO TO 50
               CALL SNSOR2 (ISORT,I)
               IND=1
50          CONTINUE
            IF (IND.EQ.0) GO TO 70
60       CONTINUE
C
70    IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,430)
         CALL SULINE (IOSDBG,2)
         CALL SUNTWK (IOSDBG)
         ENDIF
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
      IF (ISORT.EQ.2) GO TO 180
C
C  SORT BY IDENTIFIER
C
C  SORT BY WORD 1
      DO 120 N=1,NUM
         IND=0
         DO 110 I=1,NUM
            IF (ISTATE(I).NE.ISTATE(I+1)) GO TO 110
            IF (ISTAID(1,I).LE.ISTAID(1,I+1)) GO TO 110
               CALL SNSOR2 (ISORT,I)
               IND=1
110         CONTINUE
            IF (IND.EQ.0) GO TO 130
120      CONTINUE
C
130   IF (LDEBUG.GT.0) THEN
         IF (IOSDBG.EQ.LP.AND.ISLEFT(5).GT.0) CALL SUPAGE
         WRITE (IOSDBG,450)
         CALL SULINE (IOSDBG,2)
         CALL SUNTWK (IOSDBG)
         ENDIF
C
C  SORT BY WORD 2
      DO 170 N=1,NUM
         IND=0
         DO 160 I=1,NUM
            IF (ISTATE(I).NE.ISTATE(I+1)) GO TO 160
            IF (ISTAID(1,I).NE.ISTAID(1,I+1)) GO TO 160
            IF (ISTAID(2,I).LE.ISTAID(2,I+1)) GO TO 160
               CALL SNSOR2 (ISORT,I)
               IND=1
160         CONTINUE
            IF (IND.EQ.0) GO TO 360
170      CONTINUE
C
      GO TO 360
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  SORT BY DESCRIPTION
C
C  SORT BY WORD 1
180   DO 200 N=1,NUM
         IND=0
         DO 190 I=1,NUM
            IF (ISTATE(I).NE.ISTATE(I+1)) GO TO 190
            IF (IDESCR(1,I).LE.IDESCR(1,I+1)) GO TO 190
               CALL SNSOR2 (ISORT,I)
               IND=1
190         CONTINUE
            IF (IND.EQ.0) GO TO 210
200      CONTINUE
C
210   IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,460)
         CALL SULINE (IOSDBG,2)
         CALL SUNTWK (IOSDBG)
         ENDIF
C
C  SORT BY WORD 2
      DO 260 N=1,NUM
         IND=0
         DO 250 I=1,NUM
            IF (ISTATE(I).NE.ISTATE(I+1)) GO TO 250
            IF (IDESCR(1,I).NE.IDESCR(1,I+1)) GO TO 250
            IF (IDESCR(2,I).LE.IDESCR(2,I+1)) GO TO 250
               CALL SNSOR2 (ISORT,I)
               IND=1
250         CONTINUE
            IF (IND.EQ.0) GO TO 270
260      CONTINUE
C
C  SORT BY WORD 3
270   DO 290 N=1,NUM
         IND=0
         DO 280 I=1,NUM
            IF (ISTATE(I).NE.ISTATE(I+1)) GO TO 280
            IF (IDESCR(1,I).NE.IDESCR(1,I+1)) GO TO 280
            IF (IDESCR(2,I).NE.IDESCR(2,I+1)) GO TO 280
            IF (IDESCR(3,I).LE.IDESCR(3,I+1)) GO TO 280
               CALL SNSOR2 (ISORT,I)
               IND=1
280         CONTINUE
            IF (IND.EQ.0) GO TO 300
290      CONTINUE
C
C  SORT BY WORD 4
300   DO 320 N=1,NUM
         IND=0
         DO 310 I=1,NUM
            IF (ISTATE(I).NE.ISTATE(I+1)) GO TO 310
            IF (IDESCR(1,I).NE.IDESCR(1,I+1)) GO TO 310
            IF (IDESCR(2,I).NE.IDESCR(2,I+1)) GO TO 310
            IF (IDESCR(3,I).NE.IDESCR(3,I+1)) GO TO 310
            IF (IDESCR(4,I).LE.IDESCR(4,I+1)) GO TO 310
               CALL SNSOR2 (ISORT,I)
               IND=1
310         CONTINUE
            IF (IND.EQ.0) GO TO 330
320      CONTINUE
C
C  SORT BY WORD 5
330   DO 350 N=1,NUM
         IND=0
         DO 340 I=1,NUM
            IF (ISTATE(I).NE.ISTATE(I+1)) GO TO 340
            IF (IDESCR(1,I).NE.IDESCR(1,I+1)) GO TO 340
            IF (IDESCR(2,I).NE.IDESCR(2,I+1)) GO TO 340
            IF (IDESCR(3,I).NE.IDESCR(3,I+1)) GO TO 340
            IF (IDESCR(4,I).NE.IDESCR(4,I+1)) GO TO 340
            IF (IDESCR(5,I).LE.IDESCR(5,I+1)) GO TO 340
               CALL SNSOR2 (ISORT,I)
               IND=1
340         CONTINUE
            IF (IND.EQ.0) GO TO 360
350      CONTINUE
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
360   CALL UREPET ('?',XSORT,LEN(XSORT))
      IF (ISORT.EQ.1) XSORT='ID'
      IF (ISORT.EQ.2) XSORT='DESC'
      WRITE (LP,475) XSORT
      CALL SULINE (LP,2)
      INWSRT=1
C
      IF (LDEBUG.GT.0) THEN
         IF (IOSDBG.EQ.LP.AND.ISLEFT(5).GT.0) CALL SUPAGE
         WRITE (IOSDBG,480)
         CALL SULINE (IOSDBG,1)
         CALL SUNTWK (IOSDBG)
         ENDIF
C
370   IF (LTRACE.GT.0) THEN
         WRITE (IOSDBG,*) 'EXIT SNSORT'
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
400   FORMAT ('0*** ERROR - IN SNSORT - INVALID SORT VALUE : ',I2)
410   FORMAT ('0*** ERROR - NETWORK COMMON BLOCK NOT SUCCESSFULLY ',
     *   'FILLED. ALPHABETICAL ORDER WILL NOT BE RUN.')
420   FORMAT ('0*** WARNING - IN SNSORT - NO STATIONS WITH COMPLETE ',
     *   'DEFINITIONS FOUND. NETWORK COMMAND EXECUTION STOPPED.')
473   FORMAT (' NETWORK COMMON BLOCK IS ALREADY SORTED')
475   FORMAT ('0*** NOTE - CONTENTS OF NETWORK COMMON BLOCK WAS ',
     *   'SORTED BY ',A,'.')
470   FORMAT ('0CONTENTS OF NETWORK ARRAYS - BEFORE SORTING')
430   FORMAT ('0SORTED BY STATE')
450   FORMAT ('0SORTED BY FIRST WORD OF IDENTIFIER')
460   FORMAT ('0SORTED BY FIRST WORD OF DESCRIPTION')
480   FORMAT ('0CONTENTS OF NETWORK ARRAYS - AFTER SORTING')
C
      END
