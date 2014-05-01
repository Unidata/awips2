C MEMBER DAPSCM
C-----------------------------------------------------------------------
C
C @PROCESS LVL(77)
C
       SUBROUTINE DAPSCM (IRFLG,IERNPR,ISTAT)
C
C          ROUTINE:  DAPSCM
C
C             VERSION:  1.0.1  ADD 'IERNPR' TO ARGUMENT LIST AND CHECK
C                              IF ERROR MESSAGE PRINT OPTION IS PRESENT.
C                              RAE 4-10-90
C
C             VERSION:  1.0.0
C
C                DATE:  5-23-88
C
C              AUTHOR:  SHIRLEY F BARTASH
C                       DATA SCIENCES INC
C                       8555 16TH ST, SILVER SPRING, MD 587-3700
C***********************************************************************
C
C          DESCRIPTION:
C
C    THIS ROUTINE READS COMMAND CARDS FOR THE PROGRAM SHEFPOST.
C
C***********************************************************************
C
C          ARGUMENT LIST:
C
C         NAME    TYPE  I/O   DIM   DESCRIPTION
C
C       IRFLG       I    O     1    REVISION FLAG
C
C       IERNPR      I    O     1    ERROR MESSAGE PRINT OPTION
C
C       ISTAT       I    O     1    0=NO ERRORS
C                                  -1=END OF FILE
C
C***********************************************************************
C
C
C          DIMENSION AND TYPE DECLARATIONS:
C
      CHARACTER*20 CHAR/' '/
C
C***********************************************************************
C          COMMON:
C
      INCLUDE 'uio'
      INCLUDE 'udsi'
      INCLUDE 'udebug'
      INCLUDE 'dacommon/dadbgx'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shefpost/RCS/dapscm.f,v $
     . $',                                                             '
     .$Id: dapscm.f,v 1.1 1995/09/17 19:26:09 dws Exp $
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
      IF (IDATR.GT.0) WRITE (IODADB,80)
C
      ISTAT=0
      IRFLG=0
      IERNPR=0
C
      LDEBUG=0
C
C  SET DIMENSION OF ARRAY TO HOLD CHARACTERS
      LCHAR=LEN(CHAR)/4
C
C  SET FIELD NUMBER AND LOCATION OF FIELD
      NFLD=0
      ISTRT=0
      NUMFLD=0
      NDEBUG=0
C
C  GET FIELD FROM CARD
10    CALL UFIELD (NFLD,ISTRT,LENGTH,ITYPE,NREP,
     *   INTEGR,REAL,LCHAR,CHAR,
     *   LLPAR,LRPAR,LASK,LATSGN,LAMPS,LEQUAL,IERR)
C
C  CHECK FOR END OF INPUT
      IF (NFLD.EQ.-1) THEN
         IF (NUMFLD.EQ.0) ISTAT=-1
         GO TO 70
         ENDIF
C
C  PRINT CARD IF FIRST FIELD
      IF (NFLD.EQ.1) CALL UPRCRD (LP)
C
C  PRINT ERROR MESSAGES
      IF (IERR.NE.6)
     *   CALL UFLDST (NFLD,ISTRT,LENGTH,ITYPE,NREP,INTEGR,REAL,
     *      LCHAR,CHAR,LLPAR,LRPAR,LASK,LATSGN,LAMPS,LEQUAL,IERR,
     *      NUMERR,NUMWRN)
      IF (IERR.EQ.6) THEN
         CALL UWARN (LP,1,NUMWRN)
         WRITE (LP,90) NFLD
         ENDIF
C
C  PRINT VALUES OBTAINED FOR FIELD
      IF (LDEBUG.GT.0)
     *   CALL UPRFLD (NFLD,ISTRT,LENGTH,ITYPE,NREP,INTEGR,REAL,
     *      LCHAR,CHAR,LLPAR,LRPAR,LASK,LATSGN,LAMPS,LEQUAL,IERR)
C
      NUMFLD=NUMFLD+1
C
C  CHECK FOR POST OPTION
      IF (CHAR(1:4).NE.'POST') GO TO 30
15       CALL UFIELD (NFLD,ISTRT,LENGTH,ITYPE,NREP,
     *      INTEGR,REAL,LCHAR,CHAR,
     *      LLPAR,LRPAR,LASK,LATSGN,LAMPS,LEQUAL,IERR)
         IF (NFLD.EQ.-1) GO TO 70
         IF (NFLD.EQ.1) CALL UPRCRD (LP)
         IF (CHAR(1:4).EQ.'DBUG') GO TO 30
         IF (ITYPE.NE.0) THEN
            WRITE (LP,120) NFLD
            ELSE
C        SET REVISION FLAG
               IRFLG=INTEGR
            ENDIF
         CALL UFIELD (NFLD,ISTRT,LENGTH,ITYPE,NREP,
     *      INTEGR,REAL,LCHAR,CHAR,
     *      LLPAR,LRPAR,LASK,LATSGN,LAMPS,LEQUAL,IERR)
         IF (NFLD.EQ.-1) GO TO 70
         IF (NFLD.EQ.1) CALL UPRCRD (LP)
         IF (CHAR(1:4).EQ.'DBUG') GO TO 30
         IF (ITYPE.NE.0) THEN
            WRITE (LP,120) NFLD
            ELSE
C        SET ERROR PRINT OPTION
               IERNPR=INTEGR
            ENDIF
         GO TO 10
C
C  DEBUG COMMAND
30    IF (CHAR(1:4).EQ.'DBUG') THEN
65       CALL UFIELD (NFLD,ISTRT,LENGTH,ITYPE,NREP,
     *      INTEGR,REAL,LCHAR,CHAR,
     *      LLPAR,LRPAR,LASK,LATSGN,LAMPS,LEQUAL,IERR)
         IF (NFLD.EQ.-1) GO TO 70
         IF (NFLD.EQ.1) CALL UPRCRD (LP)
         IF (CHAR(1:4).EQ.'POST') GO TO 15
         IF (ITYPE.NE.0) THEN
            WRITE (LP,120) NFLD
            ELSE
               NDEBUG=NDEBUG+1
               IF (NDEBUG.EQ.1) NOBUG=INTEGR
               IF (NDEBUG.EQ.2) IDETR=INTEGR
               IF (NDEBUG.EQ.3) IDEDB=INTEGR
               IF (NDEBUG.EQ.4) IPDTR=INTEGR
               IF (NDEBUG.EQ.5) IPDDB=INTEGR
            ENDIF
         GO TO 65
         ENDIF
C
C  INVALID COMMAND
      WRITE (LP,100) CHAR
      GO TO 10
C
70    WRITE (LP,125) IRFLG,IERNPR
C
      IF (NDEBUG.GT.0)
     *   WRITE (LP,110) NOBUG,IDETR,IDEDB,IPDTR,IPDDB
      IF (IDATR.GT.0) WRITE (IODADB,130) ISTAT
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
80    FORMAT (' *** ENTER DAPSCM')
90    FORMAT ('0**WARNING** IN DAPSCM - FIELD ',I2,' CONTAINED ',
     *   'UNPARIED QUOTES.')
100   FORMAT (' **ERROR** IN DAPSCM - INVALID COMMAND : ',A)
110   FORMAT ('0DEBUG OPTIONS SET : NOBUG=',I2,3X,
     *   'IDETR=',I2,3X,
     *   'IDEDB=',I2,3X,
     *   'IPDTR=',I2,3X,
     *   'IPDDB=',I2)
120   FORMAT ('0**ERROR** IN DAPSCM - FIELD ',I2,' OF INPUT CARD IS ',
     1       'NOT AN INTEGER.')
125   FORMAT ('0OPTIONS IN EFFECT : ',
     *   'REVISION FLAG = ',I2,5X,
     *   'ERROR PRINT OPTION = ',I2)
130   FORMAT (' *** EXIT DAPSCM : ISTAT=',I2)
C
      END
