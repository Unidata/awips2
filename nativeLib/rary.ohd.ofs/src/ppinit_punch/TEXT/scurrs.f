C MODULE SCURRS
C-----------------------------------------------------------------------
C
C  ROUTINE TO PUNCH GENERAL RRS PARAMETERS.
C
      SUBROUTINE SCURRS (IVURRS,NTYPE,TYPES,MNDAY,NMOBS,ISTAT)
C
C
      CHARACTER*8 CHAR
      CHARACTER*80 CARD/' '/
C
      DIMENSION TYPES(NTYPE),MNDAY(NTYPE),NMOBS(NTYPE)
C
      INCLUDE 'uio'
      INCLUDE 'scommon/sudbgx'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/ppinit_punch/RCS/scurrs.f,v $
     . $',                                                             '
     .$Id: scurrs.f,v 1.2 1998/04/07 15:00:41 page Exp $
     . $' /
C    ===================================================================
C
C
      IF (ISTRCE.GT.1) THEN
         WRITE (IOSDBG,40)
         CALL SULINE (IOSDBG,1)
         ENDIF
C
C  SET DEBUG LEVEL
      LDEBUG=ISBUG('URRS')
C
      ISTAT=0
C
C  PRINT PARAMETER ARRAY VERSION NUMBER
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,50) IVURRS
         CALL SULINE (IOSDBG,2)
         ENDIF
C
C  PUNCH 'URRS' STARTING IN COLUMN 1
      NPOS=1
      CALL UTOCRD (ICDPUN,NPOS,'URRS',4,1,CARD,0,0,LNUM,IERR)
      IF (IERR.GT.0) GO TO 20
C
C  PUNCH THE FOLLOWING FOR EACH URRS DATA TYPE
      DO 10 ITYPE=1,NTYPE
C     PUNCH THE DATA TYPE CODE
         CALL UTOCRD (ICDPUN,NPOS,TYPES(ITYPE),4,1,CARD,0,9,LNUM,IERR)
         IF (IERR.GT.0) GO TO 20
C     PUNCH MINIMUM DAYS OF DATA TO BE RETAINED ON PPDB
         CALL UINTCH (MNDAY(ITYPE),LEN(CHAR),CHAR,NFILL,IERR)
         CALL UTOCRD (ICDPUN,NPOS,CHAR,LEN(CHAR),1,CARD,1,0,LNUM,IERR)
         IF (IERR.GT.0) GO TO 20
C     PUNCH TYPICAL NUMBER OF OBSERVATIONS TO BE HELD ON THE PPDB
         CALL UINTCH (NMOBS(ITYPE),LEN(CHAR),CHAR,NFILL,IERR)
         CALL UTOCRD (ICDPUN,NPOS,CHAR,LEN(CHAR),3,CARD,1,0,LNUM,IERR)
         IF (IERR.GT.0) GO TO 20
10       CONTINUE
      CALL UPNCRD (ICDPUN,CARD)
      GO TO 30
C
20    ISTAT=1
C
30    IF (ISTRCE.GT.1) THEN
         WRITE (IOSDBG,60)
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
40    FORMAT (' *** ENTER SCURRS')
50    FORMAT ('0PARAMETER ARRAY VERSION NUMBER = ',I2)
60    FORMAT (' *** EXIT SCURRS')
C
      END
