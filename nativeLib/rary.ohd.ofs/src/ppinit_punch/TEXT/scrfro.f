C MEMBER SCRFRO
C-----------------------------------------------------------------------
C
C                             LAST UPDATE: 06/07/95.15:04:27 BY $WC21DT
C
C @PROCESS LVL(77)
C
C DESC PUNCH RAINFALL-RUNOFF RELATION PARAMETERS
C
      SUBROUTINE SCRFRO (IVRFRO,RFROID,NURFRO,RFROPM,
     *   ISTAT)
C
      DIMENSION CHAR(2),CARD(20)
      INCLUDE 'scommon/dimrfro'
C
      INCLUDE 'uio'
      INCLUDE 'scommon/sudbgx'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/ppinit_punch/RCS/scrfro.f,v $
     . $',                                                             '
     .$Id: scrfro.f,v 1.1 1995/09/17 19:14:40 dws Exp $
     . $' /
C    ===================================================================
C
C
      DATA MAXCHR/8/,CARD/20*4H    /
C
      IF (ISTRCE.GT.1) WRITE (IOSDBG,140)
      IF (ISTRCE.GT.1) CALL SULINE (IOSDBG,1)
C
C  SET DEBUG LEVEL
      LDEBUG=ISBUG(4HRFRO)
C
      ISTAT=0
C
C  PRINT PARAMETER ARRAY VERSION NUMBER
      IF (LDEBUG.EQ.0) GO TO 10
         WRITE (IOSDBG,160) IVRFRO
         CALL SULINE (IOSDBG,2)
C
C  PUNCH 'RFRO' STARTING IN COLUMN 1
10    NPOS=1
      CALL UTOCRD (ICDPUN,NPOS,4HRFRO,4,1,CARD,0,0,LNUM,IERR)
      IF (IERR.GT.0) GO TO 120
C
C  PUNCH RAINFALL-RUNOFF RELATION IDENTIFIER
      CALL UTOCRD (ICDPUN,NPOS,RFROID,8,1,CARD,3,0,LNUM,IERR)
      IF (IERR.GT.0) GO TO 120
C
C  PUNCH RAINFALL-RUNOFF RELATIONSHIP NUMBER
      CALL UINTCH (NURFRO,MAXCHR,CHAR,LFILL,IERR)
      CALL UTOCRD (ICDPUN,NPOS,CHAR,MAXCHR,1,CARD,0,0,LNUM,IERR)
      IF (IERR.GT.0) GO TO 120
C
C  PUNCH RAINFALL-RUNOFF RELATION PARAMETERS
      MAXDEC=2
      DO 60 I=1,10
         CALL URELCH (RFROPM(I),MAXCHR,CHAR,MAXDEC,LFILL,IERR)
         CALL UTOCRD (ICDPUN,NPOS,CHAR,MAXCHR,1,CARD,0,0,LNUM,IERR)
         IF (IERR.GT.0) GO TO 120
60       CONTINUE
C
110   CALL UPNCRD (ICDPUN,CARD)
      IF (LDEBUG.GT.0) WRITE (IOSDBG,150)
      IF (LDEBUG.GT.0) CALL SULINE (IOSDBG,1)
      GO TO 130
C
120   ISTAT=1
C
130   IF (ISTRCE.GT.1) WRITE (IOSDBG,170)
      IF (ISTRCE.GT.1) CALL SULINE (IOSDBG,1)
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
140   FORMAT (' *** ENTER SCRFRO')
150   FORMAT (' *** NOTE - RFRO PARAMETERS SUCCESSFULLY PUNCHED FOR ',
     *   'IDENTIFIER ',2A4,'.')
160   FORMAT ('0PARAMETER ARRAY VERSION NUMBER = ',I2)
170   FORMAT (' *** EXIT SCRFRO')
C
      END
