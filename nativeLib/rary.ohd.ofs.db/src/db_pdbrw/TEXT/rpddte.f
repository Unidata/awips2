C MEMBER RPDDTE
C  (from old member PDBUTILS)
C-----------------------------------------------------------------------
C
C                             LAST UPDATE: 10/20/94.16:16:57 BY $WC21DT
C
C @PROCESS LVL(77)
C
      SUBROUTINE RPDDTE (IDLYTP,JEDATE,JLDATE,ISTAT)
C
C***********************************************************************
C
C          DESCRIPTION:
C
C    THIS ROUTINE OBTAINS THE FIRST AND LAST JULIAN DATE FOR
C    WHICH THE OBSERVED DATA ARE ON THE PARAMETRIC DATA BASE
C    FOR A SPECIFIED GROUP OF DATA STORED BY DAYS.
C
C
C
C***********************************************************************
C
C          ARGUMENT LIST:
C
C         NAME    TYPE  I/O   DIM   DESCRIPTION
C
C         IDLYTP   A4    I    1      DAILY DATA TYPE
C         JEDATE   I     O     1     FIRST JULIAN DAY OF DATA
C         JLDATE   I     O     1     LAST JULIAN DAY OF DATA
C         ISTAT    I     O     1     STATUS INDICATOR
C                                       0=OK
C                                       1=INVALID DATA TYPE
C                                       2=VALID TYPE NOT FOUND
C                                       3=NO STATIONS OF TYPE
C
C***********************************************************************
C
C          COMMON:
C
      INCLUDE 'uio'
      INCLUDE 'udebug'
      INCLUDE 'pdbcommon/pddtdr'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/db_pdbrw/RCS/rpddte.f,v $
     . $',                                                             '
     .$Id: rpddte.f,v 1.1 1995/09/17 18:44:37 dws Exp $
     . $' /
C    ===================================================================
C
C
C***********************************************************************
C
C
      ISTAT=0
C
      IF (IPDTR.GT.0) WRITE (IOGDB,50)
C
C  CHECK IF DATA TYPE IS VALID
      IX=IPDCKD(IDLYTP)
      IF (IX.NE.0) GO TO 10
      IF (IPDDB.GT.0) WRITE (LPE,60) IDLYTP
         ISTAT=1
         GO TO 40
C
C  CHECK IF DATA TYPE DEFINED
10    IF (IPDDB.GT.0) WRITE (IOGDB,70) IDDTDR(16,IX)
      IF (IDDTDR(16,IX).EQ.0) GO TO 20
C
C  CHECK IF ANY STATION DEFINED FOR DATA TYPE
      IF (IPDDB.GT.0) WRITE (IOGDB,80) IDDTDR(17,IX)
      IF (IDDTDR(17,IX).EQ.0) GO TO 30
C
C  SET FIRST AND LAST JULIAN DATES
      CALL UMEMOV (IDDTDR(8,IX),JEDATE,1)
      CALL UMEMOV (IDDTDR(11,IX),JLDATE,1)
      IF (IPDDB.GT.0) WRITE (IOGDB,90) JEDATE,JLDATE
      GO TO 40
C
C  DATA TYPE NOT DEFINED
20    ISTAT=2
      GO TO 40
C
C  NO STATION DEFINED
30    ISTAT=3
C
40    IF (IPDTR.GT.0) WRITE (IOGDB,100)
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
50    FORMAT (' *** ENTER RPDDTE')
60    FORMAT (' INVALID DATA TYPE ',A4)
70    FORMAT (' IDDTDR(16,IX)=',I5)
80    FORMAT (' IDDTDR(17,IX)=',I5)
90    FORMAT (' JEDATE=',I5,3X,'JLDATE=',I5)
100   FORMAT (' *** EXIT RPDDTE')
C
      END
