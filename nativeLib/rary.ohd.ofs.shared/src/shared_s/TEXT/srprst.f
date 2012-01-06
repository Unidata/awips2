C MEMBER SRPRST
C-----------------------------------------------------------------------
C
C  ROUTINE TO PRINT ERROR MESSAGES BASED ON STATUS CODE FROM THE
C  PROCESSED DATA BASE READ ROUTINES.
C
      SUBROUTINE SRPRST (CALLER,ID,TYPE,MAXBUF,NUMERR,ISTAT)
C
C
      CHARACTER*4 TYPE
      CHARACTER*8 CALLER,ID
C      
      INCLUDE 'uio'
      INCLUDE 'scommon/sudbgx'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shared_s/RCS/srprst.f,v $
     . $',                                                             '
     .$Id: srprst.f,v 1.2 1997/04/06 12:25:43 page Exp $
     . $' /
C    ===================================================================
C
C
C
      IF (ISTRCE.GT.1) THEN
         WRITE (IOSDBG,60)
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      NUMERR=0
C
C  CHECK FOR VALID CALLER CODE
      IF (CALLER.EQ.'RPRDH'.OR.CALLER.EQ.'RPRDFH') GO TO 5
         WRITE (LP,65) CALLER
         CALL SUERRS (LP,2,NUMERR)
         GO TO 50
C
C  CHECK FOR STATUS CODE=0
5     IF (ISTAT.NE.0) GO TO 10
         WRITE (LP,70) CALLER,ISTAT
         CALL SUERRS (LP,2,NUMERR)
         GO TO 50
C
C  CHECK FOR INVALID STATUS CODE
10    IF (ISTAT.GE.1.AND.ISTAT.LE.3.OR.ISTAT.EQ.5) GO TO 20
         WRITE (LP,80) CALLER,ISTAT
         CALL SUERRS (LP,2,NUMERR)
         GO TO 50
C
C  TIME SERIES NOT FOUND
20    IF (ISTAT.NE.1) GO TO 30
         WRITE (LP,110) CALLER,TYPE,ID
         CALL SUERRS (LP,2,NUMERR)
         GO TO 50
C
C  EXTRA BUFFER TOO SMALL
30    IF (ISTAT.NE.2) GO TO 35
         WRITE (LP,120) CALLER,TYPE,ID
         CALL SUERRS (LP,2,NUMERR)
         WRITE (LP,130) MAXBUF
         CALL SULINE (LP,1)
         GO TO 50
C
C  SPEFICIED ID AND TYPE NOT SAME AS THAT ON FILE
35    IF (ISTAT.NE.3) GO TO 40
         WRITE (LP,135) CALLER,TYPE,ID
         CALL SUERRS (LP,2,NUMERR)
         GO TO 50
C
C  SYSTEM ERROR ACCESSING FILE
40    IF (ISTAT.NE.5) GO TO 50
         WRITE (LP,90) CALLER,TYPE,ID
         CALL SUERRS (LP,2,NUMERR)
         GO TO 50
C
50    IF (ISTRCE.GT.1) THEN
         WRITE (IOSDBG,140)
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
60    FORMAT (' *** ENTER SRPRST')
70    FORMAT ('0*** NOTE - IN SRPRST - ',A,' STATUS CODE=',I3)
65    FORMAT ('0*** ERROR - IN SRPRST - CALLER NOT RECOGNIZED : ',A)
80    FORMAT ('0*** ERROR - IN SRPRST - STATUS CODE NOT RECOGNIZED ',
     *   'FOR CALLER ',A,' : ',I3)
90    FORMAT ('0*** ERROR - IN ',A,' - SYSTEM ERROR WHILE READING ',
     *   A,' TIME SERIES FOR IDENTIFIER ',A,' FROM DATA FILE.')
110   FORMAT ('0*** ERROR - IN ',A,' - ',A,' TIME SERIES NOT ',
     *   'FOUND FOR IDENTIFIER ',A,'.')
120   FORMAT ('0*** ERROR - IN ',A,' - EXTRA BUFFER TOO SMALL FOR ',
     *   A,' TIME SERIES ',A,'.')
130   FORMAT (23X,'NUMBER OF WORDS IN EXTRA BUFFER=',I3)
135   FORMAT ('0*** ERROR - IN ',A,' - SPECIFIED ID AND TYPE NOT ',
     *   'SAME AS THAT ON FILE FOR ',A,' TIME SERIES ',A,'.')
140   FORMAT (' *** EXIT SRPRST')
C
      END
