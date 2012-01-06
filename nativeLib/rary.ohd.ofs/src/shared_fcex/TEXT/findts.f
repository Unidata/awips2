C MODULE FINDTS
C-----------------------------------------------------------------------
C
      SUBROUTINE FINDTS (TSID,DTYPE,ITIME,LOCD,LOCTS,DIMN)
C
C   THIS ROUTINE FINDS A TIME SERIES IN THE TS ARRAY.
C
C   SUBROUTINE INITIALLY WRITTEN BY ERIC ANDERSON - HRL - 6/1979
C
      CHARACTER*4 DIMN
      DIMENSION TSID(2)
C
      INCLUDE 'common/fdbug'
      INCLUDE 'common/fts'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shared_fcex/RCS/findts.f,v $
     . $',                                                             '
     .$Id: findts.f,v 1.2 2001/06/13 09:34:40 mgm Exp $
     . $' /
C    ===================================================================
C
C
      IF (ITRACE.GE.3) WRITE (IODBUG,*) 'ENTER FINDTS'
C
      IBUG=IFBUG('SEGI')
C
      IF (IBUG.EQ.1) THEN
         WRITE (IODBUG,10) TSID,DTYPE,ITIME
         ENDIF
10    FORMAT (' IN FINDTS : TSID=',2A4,' DTYPE=',A,' ITIME=',I2)
C
C   START CHECKING AT THE BEGINNING OF THE TS ARRAY
      LOCTS=1
C
C  CHECK IF ANOTHER TIME SERIES
20    IT=TS(LOCTS)
      IF (IBUG.EQ.1) THEN
         WRITE (IODBUG,*) ' IN FINDTS : LOCTS=',LOCTS,' IT=',IT
         ENDIF
      IF (IT.EQ.0) GO TO 50
C
      IF (IBUG.EQ.1) THEN
         WRITE (IODBUG,30) LOCTS,
     *     TS(LOCTS+2),TS(LOCTS+3),TS(LOCTS+4),TS(LOCTS+5)
30    FORMAT (' IN FINDTS : LOCTS=',I5,
     *   ' TSIDX=',2A4,' DTYPEX=',A4,' ITIMEX=',I2)
         ENDIF
C
      IF (TS(LOCTS+2).NE.TSID(1)) GO TO 40
      IF (TS(LOCTS+3).NE.TSID(2)) GO TO 40
      IF (TS(LOCTS+4).NE.DTYPE) GO TO 40
      IT=TS(LOCTS+5)
      IF (IT.NE.ITIME) GO TO 40
C
C  TIME SERIES FOUND
      LOCD=TS(LOCTS+7)
C
C  GET DATA DIMENSIONS
      CALL FDCODE (DTYPE,UNITS,DIMN,MSG,NPDT,TSCALE,NADD,IERR)
      GO TO 60
C
C  INCREMENT TO THE NEXT TIME SERIES
40    NTS=TS(LOCTS+1)
      LOCTS=NTS
      IF (LOCTS.LE.MTS) GO TO 20
C
C  TIME SERIES NOT FOUND
50    LOCTS=0
      LOCD=0
      DIMN=' '
C
60    IF (IBUG.EQ.1) THEN
         WRITE (IODBUG,*) ' IN FINDTS : LOCD=',LOCD,' LOCTS=',LOCTS,
     *      ' DIMN=',DIMN
         ENDIF
C
      RETURN
C
      END
