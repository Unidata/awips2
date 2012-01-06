C MODULE SURCRD
C-----------------------------------------------------------------------
C
C  ROUTINE TO READ CARD AND CALL ROUTINES TO FILL COMMON BLOCK UFREEX.
C
      SUBROUTINE SURCRD (IEND,ISTAT)
C
      CHARACTER*1 CHK1
      CHARACTER*80 ICARD
C
      INCLUDE 'uio'
      INCLUDE 'ufreex'
      INCLUDE 'scommon/sudbgx'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/ppinit_util/RCS/surcrd.f,v $
     . $',                                                             '
     .$Id: surcrd.f,v 1.2 1998/07/06 12:49:13 page Exp $
     . $' /
C    ===================================================================
C
C
      IF (ISTRCE.GT.1) THEN
         WRITE (IOSDBG,40)
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      IEND=0
      ISTAT=0
C
C  READ CARD
10    READ (ICD,50,END=20) ICARD
      NRDCRD=NRDCRD+1
      IPRTCD=0
      GO TO 15
C
C  END-OF-FILE ENCOUNTERED
20    IEND=1
      GO TO 30
C
C  MOVE CARD INTO BUFFER
15    CALL UNPAKS (ICARD,ICDBUF,LEN(ICARD)/4,LEN(ICARD),IERR)
      IF (ISDBUG.GT.1) THEN
         WRITE (IOSDBG,60) IERR
         CALL SULINE (IOSDBG,1)
         ENDIF
C
C  FIND FIELDS ON CARD
      CALL UFFIND (ICDSTR,ICDSTP,IERR)
      IF (ISDBUG.GT.1) THEN
         WRITE (IOSDBG,70) IERR
         CALL SULINE (IOSDBG,1)
         ENDIF
C
C  CHECK NUMBER OF FIELDS FOUND
      IF (NFIELD.EQ.0) GO TO 30
C
C  CHECK FOR COMMENT CARD
      CHK1=' '
      CALL SUBSTR (ICDBUF(IFSTRT(1):IFSTRT(1)),1,1,CHK1,1)
      IF (ISDBUG.GT.1) THEN
         WRITE (IOSDBG,80) XCHK,XDOLR
         CALL SULINE (IOSDBG,1)
         ENDIF
      IF (CHK1.EQ.'$') THEN
         IF (IPRCRD.EQ.1) CALL SUPCRD
         GO TO 10
         ENDIF
C
30    IF (ISTRCE.GT.1) THEN
         WRITE (IOSDBG,90)
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
40    FORMAT (' *** ENTER SURCRD')
50    FORMAT (A80)
60    FORMAT (' UNPAKS CALLED : STATUS CODE=',I2)
70    FORMAT (' SUFREE CALLED : STATUS CODE=',I2)
80    FORMAT (' XCHK=',A4,3X,'XDOLR=',A4)
90    FORMAT (' *** EXIT SURCRD')
C
      END
