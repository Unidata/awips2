C MODULE PSERCH
C-----------------------------------------------------------------------
C    MODIFIED 5/20/87 - GFS - HRL - ADD IXBUF TO ARGUMENT LIST
C    8/13/86 TASK 282 UPDATES TO USE REORDER COMMON
C    10/28/87 SRS UPDATE TO USE COMMON/UDEBUG/
C
C             VERSION:  1.0.0
C                DATE:  11-18-81
C              AUTHOR:  JIM ERLANDSON
C                       DATA SCIENCES INC
C***********************************************************************
C          DESCRIPTION:
C
C    THIS ROUTINE SERCHES FOR A TIME SERIES RECORD OR FOR A
C    FREE SLOT IN THE TIME SERIES INDEX. THE HASHING ALGORITHM
C    IS CALLED TO GENERATED THE RECORD NUMBER. RETURNS THE FOUND
C    RECORD NUMBER OR THE FREE SLOT NUMBER.
C***********************************************************************
C          ARGUMENT LIST:
C
C         NAME    TYPE  I/O   DIM   DESCRIPTION
C
C       ITSID     A8     I     2    TIME SERIES IDENTIFIER
C       ITYPE     A4     I     1    DATA TYPE CODE
C       IFREE     I      O     1    RECORD NUMBER OF FREE SLOT
C                                   OR 0 IF RECORD FOUND
C       IFIND     I      O     1    RECORD NUMBER IF FOUND OR
C                                   0 IF NOT FOUND
C       IXBUF     I      O     4    BUFFER READ FROM INDEX AT
C                                   RECORD NUMBER=IFIND.
C                                   EMPTY OR DELETED SLOT CONTENTS
C                                   IF IFIND=0.
C***********************************************************************
      SUBROUTINE PSERCH (ITSID,ITYPE,IFREE,IFIND,IXBUF)
C
      INCLUDE 'uio'
      INCLUDE 'udebug'
      INCLUDE 'prdcommon/punits'
      INCLUDE 'prdcommon/pmaxdm'
      INCLUDE 'urcommon/urunts'
      INCLUDE 'ucommon/uordrx'
      INCLUDE 'urcommon/urmaxm'
C
      DIMENSION KEY(3),IXBUF(4),ITSID(2)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/db_prdrw/RCS/pserch.f,v $
     . $',                                                             '
     .$Id: pserch.f,v 1.2 1999/07/06 12:56:22 page Exp $
     . $' /
C    ===================================================================
C
C
      DATA IPRIME/17/
C
      IF (IPRTR.GT.1) WRITE (IOGDB,10) ITSID,ITYPE,IAMORD
10    FORMAT (' *** ENTER PSERCH - ITSID=',2A4,3X,'ITYPE=',A4,3X,
     *   'IAMORD=',I2)
C
      ITRY=0
      ISAVE=0
C
      IMAX=MXTIME*2
      LUNIT=KUPRIX
      IF (IAMORD .NE. 1) THEN
         IMAX=MAXTMS*2
         LUNIT=KINDEX
      ENDIF
C
      IF (IPRDB.GT.0) WRITE (IOGDB,25) LUNIT
25    FORMAT (' LUNIT=',I3)
C
      FMAX=IMAX
      IOVFL=FMAX*0.87225
      IHOLD=IOVFL-1
      IRETRY=IPRIME/2+1
C
C  CALL HASHING ROUTINE TO GET RECORD NUMBER
      KEY(1)=ITYPE
      KEY(2)=ITSID(1)
      KEY(3)=ITSID(2)
      CALL PPHASH (IHOLD,KEY,IHASH)
C
C  SEARCH INDEX FOR RECORD
      DO 50 I =1,IRETRY
         IXREC=IHASH
         CALL UREADT (LUNIT,IXREC,IXBUF,ISTAT)
         IF (ISTAT.NE.0) GO TO 100
C     CHECK IF EMPTY SLOT
         IF (IXBUF(1) .EQ. -1) THEN
C     SAVE DELETED RECORD NUMBER
            IF (ISAVE .EQ. 0) ISAVE = IXREC
         ELSE
            IF (IXBUF(1).EQ.0) GO TO 80
C     COMPARE KEY
            IF (IXBUF(1).EQ.KEY(2).AND.
     *          IXBUF(2).EQ.KEY(3).AND.
     *          IXBUF(3).EQ.KEY(1)) GO TO 90
         ENDIF
C     TRY NEXT RECORD
         ITRY=ITRY+1
         IHASH=IHASH+IPRIME
         IF (IHASH .GT. IHOLD) IHASH = IHASH-IHOLD+1
50    CONTINUE
C
C  COULD NOT FIND - SEARCH OVERFLOW AREA
      DO 60 I=IOVFL,IMAX
         IXREC=I
         CALL UREADT (LUNIT,IXREC,IXBUF,ISTAT)
         IF (ISTAT.NE.0) GO TO 100
C     CHECK IF EMPTY SLOT OR RECORD
         IF (IXBUF(1).EQ.0) GO TO 80
         IF (IXBUF(1).EQ.KEY(2).AND.
     *       IXBUF(2).EQ.KEY(3).AND.
     *       IXBUF(3).EQ.KEY(1)) GO TO 90
60    CONTINUE
C
C  OVERFLOW AREA IS FULL
      WRITE (LPE,70)
70    FORMAT ('0*** ERROR - IN PSERCH - INDEX OVERFLOW AREA IS FULL.')
      IFREE=0
      IFIND=0
      GO TO 120
C
C  FOUND EMPTY SLOT
80    IFREE=IXREC
      IF (ISAVE.NE.0) IFREE=ISAVE
      IFIND=0
      GO TO 120
C
C  FOUND TIME SERIES
90    IFIND=IXREC
      IFREE=0
      GO TO 120
C
C  SYSTEM ERROR
100   WRITE (LP,110)
110   FORMAT ('0*** ERROR - IN PSERCH - DAIO READ ERROR.')
C
120   IF (IPRDB .GE. 2) THEN
         WRITE (IOGDB,130) ITRY
130      FORMAT (' ITRY=',I4)
         IF (IFIND.EQ.0) WRITE (IOGDB,140) IXBUF
140      FORMAT (' IXBUF= ',4I11)
         IF (IFIND.GT.0) WRITE (IOGDB,150) IXBUF
150      FORMAT (' IXBUF= ',2A4,1X,A4,I11)
      ENDIF
C
      IF (IPRDB.GT.1) WRITE (IOGDB,170) IFREE,IFIND
170   FORMAT (' *** EXIT PSERCH - IFREE=',I6,3X,'IFIND=',I6)
C
      RETURN
      END
