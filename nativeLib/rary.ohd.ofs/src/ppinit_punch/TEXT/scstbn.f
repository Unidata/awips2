C MEMBER SCSTBN
C-----------------------------------------------------------------------
C
C                             LAST UPDATE: 06/07/95.15:05:31 BY $WC21DT
C
C @PROCESS LVL(77)
C
C
C DESC PUNCH STATE BOUNDARY PARAMETER RECORD
C
      SUBROUTINE SCSTBN (IVSTBN,MDRBND,STBNPT,ISTAT)
C
      DIMENSION MDRBND(1),STBNPT(89,1)
      DIMENSION CARD(20),CHAR(2)
C
      INCLUDE 'uio'
      INCLUDE 'scommon/sudbgx'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/ppinit_punch/RCS/scstbn.f,v $
     . $',                                                             '
     .$Id: scstbn.f,v 1.1 1995/09/17 19:14:43 dws Exp $
     . $' /
C    ===================================================================
C
C
      DATA MAXCHR/8/,CARD/20*4H    /
      DATA BLNK/4H    /
C
      IF (ISTRCE.GT.1) WRITE (IOSDBG,120)
      IF (ISTRCE.GT.1) CALL SULINE (IOSDBG,1)
C
C  SET DEBUG LEVEL
      LDEBUG=ISBUG(4HSTBN)
C
      ISTAT=0
      IUNDEF=-997
C
C  PRINT PARAMETER ARRAY VERSION NUMBER
      IF (LDEBUG.EQ.0) GO TO 10
         WRITE (IOSDBG,140) IVSTBN
         CALL SULINE (IOSDBG,2)
10    CONTINUE
C
C  PUNCH 'STBN' STARTING IN COLUMN 1
      NPOS=1
      CALL UTOCRD (ICDPUN,NPOS,4HSTBN,4,1,CARD,0,0,LNUM,IERR)
      IF (IERR.GT.0) GO TO 100
C
C  PUNCH MDR GRID SUBSET
      DO 20 I=1,4
         IF (MDRBND(I).EQ.IUNDEF) GO TO 40
20       CONTINUE
C
      DO 30 I=1,4
         CALL UINTCH (MDRBND(I),MAXCHR,CHAR,LFILL,IERR)
         CALL UTOCRD (ICDPUN,NPOS,CHAR,MAXCHR,1,CARD,1,0,LNUM,IERR)
         IF (IERR.GT.0) GO TO 100
30       CONTINUE
         GO TO 50
C
40    CALL UTOCRD (ICDPUN,NPOS,6H4*',,',6,1,CARD,1,0,LNUM,IERR)
      IF (IERR.GT.0) GO TO 100
C
50    CALL UPNCRD (ICDPUN,CARD)
C
C  PUNCH STATE BOUNDARY POINTS FOR MDR SUBSET
      NCOL=MDRBND(2)
      IR=MDRBND(3)+MDRBND(4)-1
      NROW=MDRBND(4)
      DO 90 J=1,NROW
         IROW=IR-J+1
         NPOS=6
         DO 60 I=1,NCOL
            IF (STBNPT(IROW,I).NE.BLNK) GO TO 70
60          CONTINUE
            GO TO 90
70       CALL UINTCH (IROW,MAXCHR,CHAR,LFILL,IERR)
         CALL UTOCRD (ICDPUN,NPOS,CHAR,MAXCHR,1,CARD,1,0,LNUM,IERR)
         IF (IERR.GT.0) GO TO 100
         DO 80 I=1,NCOL
            CALL SUBSTR (STBNPT(IROW,I),2,1,CHAR,1)
            CALL UTOCRD (ICDPUN,NPOS,CHAR,1,0,CARD,3,0,LNUM,IERR)
            IF (IERR.GT.0) GO TO 100
80          CONTINUE
         CALL UPNCRD (ICDPUN,CARD)
90       CONTINUE
C
      IF (LDEBUG.GT.0) WRITE(IOSDBG,130)
      IF (LDEBUG.GT.0) CALL SULINE (IOSDBG,1)
      GO TO 110
C
C  ERROR ENCOUNTERED
100   ISTAT=1
C
110   IF (ISTRCE.GT.1) WRITE (IOSDBG,150)
      IF (ISTRCE.GT.1) CALL SULINE (IOSDBG,1)
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
120   FORMAT (' *** ENTER SCSTBN')
130   FORMAT (' *** NOTE - USER STATE BOUNDARY PARAMETERS ',
     *'SUCCESSFULLY PUNCHED.')
140   FORMAT ('0PARAMETER ARRAY VERSION NUMBER = ',I2)
150   FORMAT (' *** EXIT SCSTBN')
C
      END
