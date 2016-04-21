C MEMBER PFDUNT
C  (from old member PDBFDUNT)
C-----------------------------------------------------------------------
C
C @PROCESS LVL(77)
C
C                             LAST UPDATE: 06/21/95.14:03:03 BY $WC20SV
C
      SUBROUTINE PFDUNT (IRRSTP,IUNFLG,IUNIT)
C
C          ROUTINE:  PFDUNT
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
C    THIS ROUTINE FINDS THE UNITS FOR A SPECIFIED RRS DATA TYPE.
C    THE CONVERSION FLAG IS CHECKED AND EITHER THE STORED UNITS,
C    OR THE UNITS FOR CONVERSION ARE PASSED TO THE CALLING ROUTINE.
C
C***********************************************************************
C
C          ARGUMENT LIST:
C
C         NAME    TYPE  I/O   DIM   DESCRIPTION
C
C         IRRSTP   A    I      1     RRS DATA TYPE
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
      INCLUDE 'pdbcommon/pdtrrx'
C
C***********************************************************************
C
C          DIMENSION AND TYPE DECLARATIONS:
C
      PARAMETER (NUNTBL=7)
C
      DIMENSION IUNTBL(2,NUNTBL)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/db_pdbrw/RCS/pfdunt.f,v $
     . $',                                                             '
     .$Id: pfdunt.f,v 1.1 1995/09/17 18:44:27 dws Exp $
     . $' /
C    ===================================================================
C
C
C***********************************************************************
C
C          DATA:
C
      DATA IUNTBL/4HCFS ,4HCMS ,4HFT  ,4HM   ,4HIN  ,4HMM  ,4HCFSD,
     *            4HCMSD,4HPCTD,4HPCTD,4HREAL,4HREAL,4HINT ,4HINT /
C
C***********************************************************************
C
C
      IF (IPDTR.GT.0) WRITE (IOGDB,70)
C
C  FIND RRS TYPE IN DIRECTORY
      IRX=IPDCKR(IRRSTP)
      IF (IRX.EQ.0) THEN
         WRITE (LPE,60) IRRSTP
         GO TO 50
         ENDIF
C
C  CHECK UNITS FLAG
      IF (IUNFLG.EQ.1) GO TO 20
C
C  UNITS ARE ENGLISH
      IUNIT=IRSUNT(IRX)
      GO TO 50
C
C  FIND CORRESPONDING UNITS FOR METRIC
20    DO 30 I=1,NUNTBL
         IF (IRSUNT(IRX).EQ.IUNTBL(1,I)) GO TO 40
30       CONTINUE
      WRITE (LPE,80) IRRSTP,IRSUNT(IRX)
      GO TO 50
C
40    IUNIT=IUNTBL(2,I)
C
50    IF (IPDTR.GT.0) WRITE (IOGDB,90)
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
60    FORMAT ('0**ERROR** IN PFDUNT - RRS TYPE ',A4,' INVALID. ',
     *  'NOT FOUND.')
70    FORMAT (' *** ENTER PFDUNT')
80    FORMAT ('0**ERROR** RRS TYPE ',A4,' HAS INVALID UNITS OF ',A4)
90    FORMAT (' *** EXIT PFDUNT')
C
      END
