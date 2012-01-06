C MEMBER RPDFIL
C  (from old member PDBUTILS)
C-----------------------------------------------------------------------
C
C                             LAST UPDATE: 10/20/94.16:16:57 BY $WC21DT
C
C @PROCESS LVL(77)
C
      SUBROUTINE RPDFIL (IDLYTP,JDAY,LPFIL,LDFIL,ISTAT)
C
C***********************************************************************
C
C          DESCRIPTION:
C
C    THIS ROUTINE OBTAINS THE NUMBER OF WORDS TO BE FILLED IN
C    POINTER AND DATA ARRAYS BY DAILY DATA READ ROUTINE(RPDDLY)
C    FOR A SPECIFIED DATA TYPE.
C
C***********************************************************************
C
C          ARGUMENT LIST:
C
C         NAME    TYPE  I/O   DIM   DESCRIPTION
C
C         IDLYTP  A4     I     1     DAILY DATA TYPE
C         JDAY    I      I     1     JULIAN DAY FOR LPFIL & LDFIL
C                                    (LDFIL WILL CHANGE FOR PPSR ONLY)
C         LPFIL   I      O     1     NUMBER OF I*2 POINTER WORDS
C         LDFIL   I      O     1     NUMBER OF I*2 DATA WORDS
C         ISTAT   I      O     1     STATUS INDICATOR
C                                       0=TYPE FOUND
C                                       1=INVALID DATA TYPE
C                                       2=VALID DATA TYPE NOT DEFINED
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
     .$Source: /fs/hseb/ob72/rfc/ofs/src/db_pdbrw/RCS/rpdfil.f,v $
     . $',                                                             '
     .$Id: rpdfil.f,v 1.1 1995/09/17 18:44:38 dws Exp $
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
      DATA LMDR/4HMDR6/,LPPSR/4HPPSR/
C
C***********************************************************************
C
C
      ISTAT=0
C
      LDFIL=0
      LPFIL=0
C
      IF (IPDTR.GT.0) WRITE (IOGDB,50)
C
C  CHECK IF DAILY DATA TYPE IS VALID
      IX=IPDCKD(IDLYTP)
      IF (IX.NE.0) GO TO 10
      IF (IPDDB.GT.0) WRITE (LPE,60) IDLYTP
      ISTAT=1
      GO TO 40
C
10    IF (IDLYTP.EQ.LMDR.OR.IDLYTP.EQ.LPPSR) GO TO 20
C
C  OBTAIN POINTER WORDS FOR THIS DATA TYPE
      IF (IDDTDR(18,IX).EQ.0) GO TO 30
      LPFIL=IDDTDR(18,IX)
C
C  OBTAIN DATA WORDS FOR THIS DATA TYPE
20    IF (IDDTDR(19,IX).EQ.0) GO TO 30
         LDFIL=IDDTDR(19,IX)
         GO TO 40
30    ISTAT=2
C
40    IF (IPDTR.GT.0) WRITE (IOGDB,70)
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
50    FORMAT (' *** ENTER RPDFIL')
60    FORMAT (' **ERR0R** INVALID DATA TYPE : ',A4)
70    FORMAT (' *** EXIT RPDFIL')
C
      END
