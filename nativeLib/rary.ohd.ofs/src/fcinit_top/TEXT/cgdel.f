C MODULE CGDEL
C-----------------------------------------------------------------------
C
C  ROUTINE TO DELETE A CARRYOVER GROUP.
C
      SUBROUTINE CGDEL (CGIDIN)
C
C  ROUTINE ORIGINALLY WRITTEN BY -- ED JOHNSON - HRL - 11/1979
C
      CHARACTER*8 RTNNAM,OPNOLD
      DIMENSION CGIDIN(2)
C
      INCLUDE 'common/ionum'
      INCLUDE 'common/fdbug'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_top/RCS/cgdel.f,v $
     . $',                                                             '
     .$Id: cgdel.f,v 1.2 2000/03/14 11:56:02 page Exp $
     . $' /
C    ===================================================================
C
C
      IF (ITRACE.GT.0) WRITE(IODBUG,*) 'ENTER CGDEL'
C
      RTNNAM='CGDEL'
      IOPNUM=0
      CALL FSTWHR (RTNNAM,IOPNUM,OPNOLD,IOLDOP)
C
C  REMOVE CARRYOVER GROUP NAME FROM ALL SEGMENTS
      CALL CGDELC (CGIDIN)
C
C  REMOVE CARRYOVER GROUP NAME FROM ALL FORECAST GROUPS
      CALL CGDELB (CGIDIN)
C
C  DELETE CARRYOVER GROUP
      CALL CGDELA (CGIDIN,IER)
      IF (IER.EQ.0) WRITE(IPR,10) CGIDIN
10    FORMAT ('0**NOTE** CARRYOVER GROUP ',2A4,' DELETED.')
C     
      CALL FSTWHR (OPNOLD,IOLDOP,OPNOLD,IOLDOP)
C
      IF (ITRACE.GT.0) WRITE (IPR,*) 'EXIT CGDEL'
C
      RETURN
C
      END
