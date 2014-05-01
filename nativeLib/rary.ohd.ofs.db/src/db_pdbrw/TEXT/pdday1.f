C MEMBER PDDAY1
C  (from old member PDPDDAY1)
C-----------------------------------------------------------------------
C
C                             LAST UPDATE: 06/19/95.12:40:13 BY $WC20SV
C
C @PROCESS LVL(77)
C
       SUBROUTINE PDDAY1 (JULDAY)
C
C          ROUTINE:  PDDAY1
C
C             VERSION:  1.0.0
C
C                DATE:  2-10-83
C
C              AUTHOR:  SONJA R SIEGEL
C                       DATA SCIENCES INC
C
C***********************************************************************
C
C          DESCRIPTION:
C
C    THIS ROUTINE INITIALIZES THE FIRST AND LAST DAY FOR ALL
C    DATA TYPES EXCEPT TF24, WHEN THE FILES ARE EMPTY.
C
C***********************************************************************
C
C          ARGUMENT LIST:
C
C         NAME    TYPE  I/O   DIM   DESCRIPTION
C
C       JULDAY   I     I     1     JULIAN DAY OF DATA
C
C***********************************************************************
C
C          COMMON:
C
      INCLUDE 'uio'
      INCLUDE 'udebug'
      INCLUDE 'pdbcommon/pddtdr'
      INCLUDE 'pdbcommon/pdsifc'
C
C***********************************************************************
C
C          DIMENSION AND TYPE DECLARATIONS:
C
      CHARACTER*4 DTYPE
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/db_pdbrw/RCS/pdday1.f,v $
     . $',                                                             '
     .$Id: pdday1.f,v 1.1 1995/09/17 18:43:39 dws Exp $
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
      IF (IPDTR.GT.0) WRITE (IOGDB,*) '*** ENTER PDDAY1 - ',
     *   ' JULDAY=',JULDAY,
     *   ' '
C
C  PROCESS EACH DATA TYPE
      DO 50 IX=1,NMDTYP
         CALL UMEMOV (IDDTDR(2,IX),DTYPE,1)
C     CHECK IF TF24 DATA TYPE
         IF (DTYPE.EQ.'TF24') GO TO 50
C     CHECK MAXIMUM DAYS OF DATA
         IF (IDDTDR(7,IX).LE.0) GO TO 50
C        SET DATE AND RECORD NUMBER OF EARLIEST DATA
            IF (JULDAY.GT.0) THEN
               CALL UMEMOV (JULDAY,IDDTDR(8,IX),1)
               ENDIF
            IDDTDR(10,IX)=IDDTDR(15,IX)
C        SET DATE AND RECORD NUMBER OF LATEST DATA
            IF (JULDAY.GT.0) THEN
               CALL UMEMOV (JULDAY,IDDTDR(11,IX),1)
               ENDIF
            IDDTDR(13,IX)=IDDTDR(15,IX)
C        INITIALIZE DATA RECORDS
            MAXDAY=IDDTDR(7,IX)
            IZREC=IDDTDR(10,IX)
            CALL PDVALS (IX,NNRC1D,NNRCAL,LLSTRC)
            IFILEL=IDDTDR(4,IX)
            DO 40 N=1,MAXDAY
C           INITIALIZE ALL DATA RECORDS FOR DATA TYPE
               CALL PDSET0 (DTYPE,IZREC,NNRC1D,IFILEL,ISTAT)
               IF (ISTAT.NE.0) GO TO 60
               IZREC=IZREC+NNRC1D
40             CONTINUE
50       CONTINUE
C
      WRITE (LP,70)
C
60    IF (IPDTR.GT.0) WRITE (IOGDB,*) '*** EXIT PDDAY1'
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
70    FORMAT ('0**NOTE** IN PDDAY1 - DATA RECORDS FOR FIRST AND LAST ',
     *   'DAY INITIALIZED.')
C
      END
