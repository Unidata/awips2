C MEMBER HREPRD
C  (from old member HCLREPRD)
C-----------------------------------------------------------------------
C
C                             LAST UPDATE: 07/21/94.18:11:37 BY $WC20SV
C
C @PROCESS LVL(77)
C
      SUBROUTINE HREPRD (IRCBUF,NRCWDS,IDFBUF,NDFWDS,ISTAT)
C
C          ROUTINE:  HREPRD
C
C             VERSION:  1.0.0
C
C                DATE:  1-7-82
C
C              AUTHOR:  JIM ERLANDSON
C                       DATA SCIENCES INC
C
C***********************************************************************
C
C          DESCRIPTION:
C
C   ROUTINE THAT CALLS HRPRDN AFTER SETTING THE DEFAULT UPDATE
C    FLAG FOR UPDATING.  ROUTINE HRPRDN IS THE ACTUAL
C    ROUTINE TO WRITE HCL FUNCTION AND TECHNIQUE DEFINITIONS
C    FOR REPLACE TYPE COMMANDS. RECORD MUST EXIST AND PASSWORD
C    MUST BE THE SAME. UPDATES ALL CONTROLS. CHECKS FOR ROOM
C    IN ORIGINAL RECORD BEFORE WRITING A NEW ONE. ALSO TAKES CARE
C    OF DEFAULT RECORDS.
C
C***********************************************************************
C
C          ARGUMENT LIST:
C
C         NAME    TYPE  I/O   DIM   DESCRIPTION
C
C       IRCBUF     I     I/O   ?    BUFFER CONTAINING DEFINITION
C
C       NRCWDS     I      I     1   NUMBER OF WORDS USED IN IRCBUF
C
C       IDFBUF     I     I/O    ?   BUFFER CONTAINING DEFAULT
C
C       NDFWDS     I      I     1   NUMBER OF WORDS USED IN IDFBUF
C
C       ISTAT      I      O     1   STATUS INDICATOR
C                                   0=OK  OTHER=ERROR
C
C***********************************************************************
C
      DIMENSION IRCBUF(*),IDFBUF(*)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/db_hclrw/RCS/hreprd.f,v $
     . $',                                                             '
     .$Id: hreprd.f,v 1.1 1995/09/17 18:43:00 dws Exp $
     . $' /
C    ===================================================================
C
C
C***********************************************************************
C
C
C  SET FLAG TO CHANGE DEFAULTS
C
      IDFFLG=1
C
C  CALL ROUTINE TO ACTUALLY CHANGE RECORDS
C
      CALL HRPRDN (IRCBUF,NRCWDS,IDFBUF,NDFWDS,IDFFLG,ISTAT)
C
      RETURN
C
      END
