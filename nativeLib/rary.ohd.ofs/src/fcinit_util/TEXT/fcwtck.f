C MODULE FCWTCK
C-----------------------------------------------------------------------
C
      SUBROUTINE FCWTCK (TS,MTS)
C
C  THIS ROUTINE CHECKS TO MAKE SURE DATA VALUES HAVE BEEN ENTERED INTO
C  ALL 'OUTPUT' TIME SERIES.
C
C  ROUTINE INITIALLY WRITTEN BY - ERIC ANDERSON - HRL   MAY 1980
C
      DIMENSION TS(MTS)
      DIMENSION TSID(2)
C
      INCLUDE 'common/ionum'
      INCLUDE 'common/fdbug'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_util/RCS/fcwtck.f,v $
     . $',                                                             '
     .$Id: fcwtck.f,v 1.4 2002/02/11 13:06:48 michaelo Exp $
     . $' /
C    ===================================================================
C
C
      IF (ITRACE.GE.1) WRITE (IODBUG,*) 'ENTER FCWTCK'
C
      IOPNUM=0
      CALL FSTWHR ('FCWTCK  ',IOPNUM,OLDOPN,IOLDOP)
C
C  SEARCH THE TS ARRAY FOR OUTPUT TIME SERIES
      LOC=1
10    NCTS=TS(LOC)
      IF (NCTS.EQ.0) GO TO 30
      NXLOC=TS(LOC+1)
      IF (NCTS.EQ.3) THEN
         IDATA=TS(LOC+8)
         IF (IDATA.NE.1) THEN
            TSID(1)=TS(LOC+2)
            TSID(2)=TS(LOC+3)
            TYPE=TS(LOC+4)
            IDT=TS(LOC+5)
            WRITE (IPR,20) TSID,TYPE,IDT
20    FORMAT ('0**ERROR** NO OPERATIONS WILL WRITE DATA TO ',
     *   'THE OUTPUT TIME SERIES FOR: ID=',2A4,2X,
     *   'TYPE=',A4,2X,I2,' HOURS')
            CALL ERROR
            ENDIF
         ENDIF
C
C  INCREMENT TO THE NEXT TIME SERIES
      LOC=NXLOC
      IF (LOC.LE.MTS) GO TO 10
C
30    CALL FSTWHR (OLDOPN,IOLDOP,OLDOPN,IOLDOP)
C
      RETURN
C
      END
