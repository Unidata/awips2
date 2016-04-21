C MEMBER HREPRN
C  (from old member HCLREPRD)
C-----------------------------------------------------------------------
C
C @PROCESS LVL(77)
C
      SUBROUTINE HREPRN (IRCBUF,NRCWDS,ISTAT)
C
C          ROUTINE:  HREPRN
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
C    FLAG FOR NO UPDATING.  ROUTINE HRPRDN IS THE ACTUAL
C    ROUTINE TO WRITE HCL FUNCTION AND TECHNIQUE DEFINITIONS
C    FOR REPLACE TYPE COMMANDS. RECORD MUST EXIST AND PASSWORD
C    MUST BE THE SAME. UPDATES ALL CONTROLS. CHECKS FOR ROOM
C    IN ORIGINAL RECORD BEFORE WRITING A NEW ONE.
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
C       ISTAT      I      O     1   STATUS INDICATOR
C                                   0=OK  OTHER=ERROR
C
C***********************************************************************
C
      DIMENSION IRCBUF(*),IDFBUF(1)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/db_hclrw/RCS/hreprn.f,v $
     . $',                                                             '
     .$Id: hreprn.f,v 1.1 1995/09/17 18:43:01 dws Exp $
     . $' /
C    ===================================================================
C
C
C***********************************************************************
C
C
C  SET FLAG TO NOT CHANGE DEFAULTS
C
      IDFFLG=0
C
C  CALL ROUTINE TO ACTUALLY CHANGE RECORDS
C
      CALL HRPRDN (IRCBUF,NRCWDS,IDFBUF,NDFWDS,IDFFLG,ISTAT)
C
C
      RETURN
C
      END
