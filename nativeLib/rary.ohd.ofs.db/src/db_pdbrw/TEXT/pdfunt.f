C MEMBER PDFUNT
C  (from old member PDBFDUNT)
C-----------------------------------------------------------------------
C
C                             LAST UPDATE: 06/21/95.14:03:03 BY $WC20SV
C
C @PROCESS LVL(77)
C
      SUBROUTINE PDFUNT (IDLYTP,IUNFLG,IUNIT)
C
C          ROUTINE:  PDFUNT
C
C             VERSION:  1.0.0
C
C                DATE:  6-21-83
C
C              AUTHOR:  JANINE FRANZOI
C                       DATA SCIENCES INC
C
C***********************************************************************
C
C          DESCRIPTION:
C
C    THIS ROUTINE FINDS THE UNITS FOR A SPECIFIED DAILY DATA TYPE.
C    THE CONVERSION FLAG IS CHECKED AND EITHER THE STORED UNITS,
C    OR THE UNITS FOR CONVERSION ARE PASSED TO THE CALLING ROUTINE.
C
C***********************************************************************
C
C          ARGUMENT LIST:
C
C         NAME    TYPE  I/O   DIM   DESCRIPTION
C
C         IDLYTP   A    I      1     DAILY DATA TYPE
C         IUNFLG   I    I      1     UNITS FLAG
C                                      0=ENGLISH
C                                      1=METRIC
C         IUNIT    A    O      1     UNITS
C
C***********************************************************************
C
C          COMMON:
C
      INCLUDE 'uio'
      INCLUDE 'udebug'
      INCLUDE 'pdbcommon/pddtdr'
C
C***********************************************************************
C
C          DIMENSION AND TYPE DECLARATIONS:
C
      PARAMETER (NUNTYP=24)
C
      DIMENSION IUNTYP(2,NUNTYP),IUNTBL(2,8)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/db_pdbrw/RCS/pdfunt.f,v $
     . $',                                                             '
     .$Id: pdfunt.f,v 1.1 1995/09/17 18:43:53 dws Exp $
     . $' /
C    ===================================================================
C
C
C***********************************************************************
C
C          DATA:
C
      DATA IUNTYP/4HPP24,1,4HPPVR,1,4HPPSR,1,4HPP01,1,4HPP03,1,
     *            4HPP06,1,4HTAVR,2,4HTM24,2,4HTF24,2,4HTX24,2,
     *            4HTN24,2,4HTFMN,2,4HTFMX,2,4HTA01,2,4HTA03,2,
     *            4HTA06,2,4HTA24,2,4HTD24,2,4HUS24,3,4HRC24,4,
     *            4HRP24,5,4HRI24,6,4HMDR6,7,4HEA24,8/
      DATA IUNTBL/4HIN  ,4HMM  ,4HDEGF,4HDEGC,4HMI/H,4HKM/H,4HPCTD,
     *            4HPCTD,4HPCT ,4HPCT ,4HLY  ,4HLY  ,4HINT ,4HINT ,
     *            4HENGL,4HMETR/
C
C***********************************************************************
C
C
      IF (IPDTR.GT.0) WRITE (IOGDB,70)
C
C  FIND DAILY TYPE IN DIRECTORY
      IDX=IPDCKD(IDLYTP)
      IF (IDX.EQ.0) THEN
         WRITE (LPE,80) IDLYTP
         GO TO 50
         ENDIF
C
C  FIND DATA TYPE IN ARRAY
      DO 20 I=1,NUNTYP
         IF (IDLYTP.EQ.IUNTYP(1,I)) GO TO 30
20       CONTINUE
30    IRSAV=IUNTYP(2,I)
C
C  CHECK UNITS FLAG
      IF (IUNFLG.EQ.1) GO TO 40
C
C  UNITS ARE ENGLISH
      IUNIT=IUNTBL(1,IRSAV)
      GO TO 50
C
C  UNITS ARE METRIC
40    IUNIT=IUNTBL(2,IRSAV)
C
50    IF (IPDTR.GT.0) WRITE (IOGDB,90)
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
70    FORMAT (' *** ENTER PDFUNT')
80    FORMAT ('0**ERROR** IN PDFUNT - DAILY TYPE ',A4,
     *   'NOT FOUND.')
90    FORMAT (' *** EXIT PDFUNT')
C
      END
