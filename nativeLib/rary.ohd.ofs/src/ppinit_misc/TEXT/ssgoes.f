C MODULE SSGOES
C-----------------------------------------------------------------------
C
C  ROUTINE TO PRINT STATUS OF GOES CONTROL FILE.
C
      SUBROUTINE SSGOES (USERID,LARRAY,IARRAY,LEVEL,STAID,ISTAT)
C
      CHARACTER*8 USERID,STAID
      CHARACTER*128 FILENAME
      DIMENSION IARRAY(LARRAY)
C
      INCLUDE 'uio'
      INCLUDE 'udebug'
      INCLUDE 'dgcommon/dgunts'
      INCLUDE 'scommon/suoptx'
C
C  =================================== RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/ppinit_misc/RCS/ssgoes.f,v $
     . $',                                                             '
     .$Id: ssgoes.f,v 1.3 1998/04/07 18:08:49 page Exp $
     . $' /
C  =====================================================================
C
C
C  OPEN FILE
      IOPEN=2
      CALL UDOPEN (KDGRCF,IOPEN,FILENAME,LRECL,IERR)
      IF (IERR.NE.0) THEN
         WRITE (LP,10) FILENAME(1:LENSTR(FILENAME))
10       FORMAT ('0*** WARNING - IN DGPRT - CANNOT OPEN FILE ',A,'.')
         CALL SULINE (LP,2)
         ISTAT=1
         GO TO 110
         ENDIF
C
C  READ HEADER RECORD
      READ (KDGRCF,REC=1,ERR=60) NHEAD
      IF (STAID.EQ.' ') THEN
         WRITE (LP,20)
20    FORMAT('0')
         CALL SULINE (LP,2)
         WRITE (LP,30)
         CALL SULINE (LP,0)
30    FORMAT ('+*--> GOES CONTROL FILE STATUS')
         IF (IOPOVP.EQ.1) THEN
            WRITE (LP,30)
            CALL SULINE (LP,0)
            WRITE (LP,30)
            CALL SULINE (LP,0)
            ENDIF
         ENDIF
      IF (NHEAD.EQ.0) THEN
         WRITE (LP,40)
40       FORMAT ('0*** NOTE - NO STATIONS ARE DEFINED.')
         CALL SULINE (LP,2)
         ISTAT=1
         GO TO 100
         ENDIF
      IF (STAID.NE.' ') GO TO 90
      WRITE (LP,50) NHEAD
50    FORMAT ('0NUMBER OF STATIONS DEFINED = ',I4)
      CALL SULINE (LP,2)
      GO TO 80
60    WRITE (LP,70) FILENAME(1:LENSTR(FILENAME))
70    FORMAT ('0*** ERROR - IN DGPRT - BAD READ IN FILE ',A,'.')
      CALL SULINE (LP,2)
      GO TO 100
C
80    IF (LEVEL.LT.2) GO TO 100
C
90    CALL DGPRT (USERID,STAID,ISTAT)
C
100   CALL UPCLOS (KDGRCF,' ',IERR)
C
110   RETURN
C
      END
