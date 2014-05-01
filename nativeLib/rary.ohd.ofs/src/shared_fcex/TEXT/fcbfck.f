C MEMBER FCBFCK
C  (from old member FCFCBFCK)
C-----------------------------------------------------------------------
C
C                             LAST UPDATE: 01/05/95.11:57:22 BY $WC21DT
C
C
C @PROCESS LVL(77)
C
C
      SUBROUTINE FCBFCK(NBUF,LOC,LOCSVE)
C
C
C  DESC --THIS ROUTINE IS CALLED BY FPUTCO AND FGETCO. IT KEEPS TRACK
C          OF WORD LOCATIONS WHEN FILLING (OR DUMPING) THE ZZZBUF BUFFER
C          AND ALSO DETERMINES PROGRAM CONTROL IN BOTH CALLING ROUTINES.
C
C
C
C
C  ARGUMENT LIST:
C     NBUF   - RELATIVE WORD LOCATION IN PHYSICAL RECORD
C     LOC    - PROGRAM CONTROL SWITCH WHEN RETURNING TO THE CALLING
C              ROUTINES
C     LOCSVE - SWITCH TO EITHER CONTINUE FILLING BUFFER OR TO WRITE
C              TO FILE
C
C..................................................................
C
C   ROUTINE ORIGINALLY WRITTEN BY --
C        JOE OSTROWSKI -- HRL -- 791228
C
C...................................................................
C
      INCLUDE 'common/fciobf'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shared_fcex/RCS/fcbfck.f,v $
     . $',                                                             '
     .$Id: fcbfck.f,v 1.1 1995/09/17 19:19:30 dws Exp $
     . $' /
C    ===================================================================
C
C
      LOCSVE=1
      NBUF=NBUF+1
      LOC = LOC+1
C
      IF (LOC.GT.11) LOC=11
C
C  IF NBUF>MZZBUF THEN THE BUFFER IS FULL AND A RECORD SHOULD BE WRITTEN
C    (OR READ) TO THE FILE.  IF NOT, THEN CONTINUE TO FILL THE BUFFER.
C
      IF (NBUF.LE.MZZBUF) RETURN
C
C  SET WORD LOCATION IN THE BUFFER TO ONE AS NEXT RECORD MUST BE WRITTEN
C    (OR READ) TO COMPLETE THE ENTIRE CARRYOVER TRANSFER FROM FILE TO
C    STORAGE OR VICE-VERSA.
C  SET LOCSVE=2 TO INDICATE THAT THE RECORD SHOULD BE WRITTEN (OR READ).
C
      NBUF=1
      LOCSVE=2
C
      RETURN
      END
