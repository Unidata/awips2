C MODULE CKINPT
C-----------------------------------------------------------------------
C
      SUBROUTINE CKINPT (TSID,DTYPE,ITIME,LD,TS,MTS,IERR)
C.
C  ROUTINE TO CHECK INPUT TIME SERIES TO SEE IF VALUES HAVE BEEN
C  ASSIGNED
C
C  SUBROUTINE INITIALLY WRITTEN BY ERIC ANDERSON - HRL - 11/1979
C
      CHARACTER*4 DTYPE
      CHARACTER*8 TSID
      DIMENSION TS(MTS)
C
C 
      INCLUDE 'common/ionum'
      INCLUDE 'common/fdbug' 
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_util/RCS/ckinpt.f,v $
     . $',                                                             '
     .$Id: ckinpt.f,v 1.2 2001/06/13 09:33:54 mgm Exp $
     . $' /
C    ===================================================================
C
C
      IF (ITRACE.GE.3) WRITE (IODBUG,*) 'ENTER CKINPT '
C
      IERR=0
C
C  FIND TIME SERIES
      CALL FINDTS (TSID,DTYPE,ITIME,LD,LTS,DIMN)
      ICK=TS(LTS+8)
      IF (ICK.EQ.0) THEN
         WRITE (IPR,10) TSID,DTYPE,ITIME
10    FORMAT('0**ERROR** NO VALUES HAVE BEEN ASSIGNED TO THE INPUT ',
     * 'TIME SERIES: ',
     * 'TSID=',A,' DTYPE=',A,' ITIME=',I2)
         CALL ERROR
         IERR=1
         ENDIF
C
      RETURN
C
      END
