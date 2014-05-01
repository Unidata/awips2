C MODULE FDCODE
C-----------------------------------------------------------------------
C
      SUBROUTINE FDCODE (DTYPE,UNITS,DIM,MSNG,NPDT,TSCALE,NUMADD,IERR)
C
C  THIS ROUTINE CHECKS IF A DATA TYPE CODE CAN BE USED BY THE
C  FORECAST COMPONENT. IF THE DATA TYPE CODE IS VALID, THE
C  FOLLOWING ARE RETURNED:
C     - STANDARD UNITS (UNITS)
C     - DIMENSION (DIM)
C     - WHETHER MISSING VALUES ARE ALLOWED (MSNG - 0=NO, 1=YES)
C     - THE NUMBER OF VALUES PER TIME INTERVAL (NPDT)
C     - THE TIME SCALE (TSCALE - INST,MEAN,ACCM)
C     - THE NUMBER OF ADDITIONAL PIECES OF INFORMATION NEEDED FOR THIS
C       TIME SERIES (NUMADD)
C  IERR=1 IF THE CODE IS NOT NOT VALID.
C
      CHARACTER*4 DTYPE,UNITS,DIM,TSCALE
      CHARACTER*4 SRCH
      CHARACTER*4 TYPE,UNIT,DIMN,TIME
      CHARACTER*8 OLDOPN
C
      include 'common/ionum'
      include 'common/fdbug'
      include 'common/udtypc'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shared_util/RCS/fdcode.f,v $
     . $',                                                             '
     .$Id: fdcode.f,v 1.3 1999/04/22 14:09:48 page Exp $
     . $' /
C    ===================================================================
C
C
      IF (ITRACE.GE.3) WRITE (IODBUG,10)
10    FORMAT (' ENTER FDCODE')
C
      LBUG=IFBUG('SEGI')
C
      IERR=0
      IFILL=0
C
      IOPNUM=0
      CALL FSTWHR ('FDCODE  ',IOPNUM,OLDOPN,IOLDOP)
C
C  CHECK IF UDTYPC COMMON BLOCK HAS BEEN FILLED
      IF (ICFULL.EQ.1) GO TO 40
C
C  FILL UDTYPC COMMON BLOCK
      SRCH='FCST'
      IPRC=-99
      CALL UDTYPE (SRCH,MTYPE,NTYPE,TYPE,DIMN,UNIT,MISS,NVAL,TIME,
     *   NADD,IPRC,ISTAT)
      ICFULL=1
      IFILL=1
      IF (ISTAT.NE.0) THEN
         IF (ISTAT.EQ.1) THEN
            WRITE (IPR,20) SRCH,NTYPE,MTYPE
20    FORMAT ('0**ERROR** '
     *   'NUMBER OF DATA TYPES FOUND FOR SEARCH CODE ',A4,' (',I3,') ',
     *   'EXCEEDS THE MAXIMUM THAT COMMON BLOCK UDTYPC CAN HOLD (',
     *   I3,').')
            CALL ERROR
            GO TO 60
            ENDIF
         WRITE (IPR,30) SRCH
30    FORMAT ('0**ERROR** DATA TYPE DEFINITION FILE NOT SUCCESSFULLY '
     *   'READ FOR SEARCH CODE ',A4,'.')
         CALL ERROR
         GO TO 60
         ENDIF
C
C  CHECK FOR BLANK DATA TYPE CODE
40    IF (DTYPE.EQ.' ') GO TO 60
C
C  CHECK FOR VALID DATA TYPE
      DO 50 I=1,NTYPE
         IF (LBUG.EQ.1) WRITE (IPR,*)
     *      ' NTYPE=',NTYPE,
     *      ' I=',I,
     *      ' DTYPE=',DTYPE,
     *      ' TYPE(I)=',TYPE(I),
     *      ' '
         IF (DTYPE.EQ.TYPE(I)) THEN
            UNITS=UNIT(I)
            DIM=DIMN(I)
            MSNG=MISS(I)
            NPDT=NVAL(I)
            TSCALE=TIME(I)
            NUMADD=NADD(I)
            GO TO 70
            ENDIF
50       CONTINUE
C
60    IERR=1
      UNITS=' '
      DIM=' '
      MSNG=0
      NPDT=0
      TSCALE=' '
      NUMADD=0
      GO TO 70
C
70    IF (LBUG.EQ.1) THEN
         WRITE (IODBUG,*)
     *      ' DTYPE=',DTYPE,
     *      ' UNITS=',UNITS,
     *      ' DIM=',DIM,
     *      ' MSNG=',MSNG,
     *      ' NPDT=',NPDT,
     *      ' TSCALE=',TSCALE,
     *      ' NUMADD=',NUMADD,
     *      ' IERR=',IERR,
     *      ' '
         IF (IFILL.EQ.0) GO TO 100
            WRITE (IODBUG,80) TYPE
80    FORMAT (' ',20(' ',A))
            WRITE (IODBUG,80) DIMN
            WRITE (IODBUG,80) UNIT
            WRITE (IODBUG,90) MISS
90    FORMAT (' ',20(' ',I4))
            WRITE (IODBUG,90) NVAL
            WRITE (IODBUG,80) TIME
            WRITE (IODBUG,90) NADD
         ENDIF
C
100   CALL FSTWHR (OLDOPN,IOLDOP,OLDOPN,IOLDOP)
C
      RETURN
C
      END
