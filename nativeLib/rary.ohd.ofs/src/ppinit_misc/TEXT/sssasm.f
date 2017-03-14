C MODULE SSSASM
C-----------------------------------------------------------------------
C
C  ROUTINE TO PRINT STATUS OF SASM CONTROL FILE.
C
      SUBROUTINE SSSASM (USERID,LARRAY,IARRAY,LEVEL,DEGMIN,STAID,ISTAT)
C
      CHARACTER*8 USERID,STAID
      CHARACTER*128 FILENAME
      DIMENSION IARRAY(LARRAY)
C
      INCLUDE 'uio'
      INCLUDE 'udebug'
      INCLUDE 'dscommon/dsunts'
      INCLUDE 'scommon/suoptx'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/ppinit_misc/RCS/sssasm.f,v $
     . $',                                                             '
     .$Id: sssasm.f,v 1.2 1998/04/07 18:10:31 page Exp $
     . $' /
C    ===================================================================
C
C
C  OPEN FILE
      IOPEN=2
      CALL UDOPEN (KDSRCF,IOPEN,FILENAME,LRECL,IERR)
      IF (IERR.NE.0) THEN
         WRITE (LP,10) FILENAME(1:LENSTR(FILENAME))
         CALL SULINE (LP,2)
10       FORMAT ('0*** WARNING - IN DSPRT - CANNOT OPEN FILE ',A,'.')
         ISTAT=1
         GO TO 110
         ENDIF
C
C  READ HEADER RECORD
      READ (KDSRCF,REC=1,ERR=60,IOSTAT=IERR) NHEAD
      IF (STAID.EQ.' ') THEN
         WRITE (LP,20)
         CALL SULINE (LP,2)
20    FORMAT ('0')
         WRITE (LP,30)
         CALL SULINE (LP,0)
30    FORMAT ('+*--> SASM CONTROL FILE STATUS')
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
60    WRITE (LP,70) IERR,FILENAME(1:LENSTR(FILENAME))
70    FORMAT ('0*** ERROR - IN DSPRT - IOSTAT=',I5,' ENCOUNTERED ',
     *   'READING RECORD 1 FROM FILE ',A,'.')
      CALL SULINE (LP,2)
      GO TO 100
C
80    IF (LEVEL.LT.2) GO TO 100
C
90    CALL DSPRT (USERID,STAID,ISTAT)
C
100   CALL UPCLOS (KDSRCF,' ',IERR)
C
110   RETURN
C
      END
