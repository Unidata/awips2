C MEMBER MCMDNA
C  (from old member MIFCMD)
C-----------------------------------------------------------------------
C                             LAST UPDATE: 07/07/95.11:30:54 BY $WC21DT
C
C @PROCESS LVL(77)
C
      SUBROUTINE MCMDNA(ICMND,CMDNAM)
C
C     THIS SUBROUTINE RETURNS THE 8-CHARACTER MOD COMMAND NAME
C     GIVEN AN INTEGER ONE THROUGH NUMCMD
C
      CHARACTER*8 CMDNAM
C
      INCLUDE 'common/fdbug'
      COMMON/MFG/ISFG,NDTS
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_mods/RCS/mcmdna.f,v $
     . $',                                                             '
     .$Id: mcmdna.f,v 1.2 1995/11/14 21:21:24 erb Exp $
     . $' /
C    ===================================================================
C
C
      IBUG=IFBUG(4HMODS)
      IF(IBUG.EQ.1)WRITE(IODBUG,903)ICMND
  903 FORMAT(1H0,10X,'** ENTERING MCMDNA ** ICMND=',I3)
C
      CALL MCOMND(ICMND,CMDNAM,ISFG,NDTS)
C
   30 IF(IBUG.EQ.1)WRITE(IODBUG,904)ICMND,CMDNAM
  904 FORMAT(1H0,10X,'** LEAVING MCMDNA ** ICMND=',I3,', CMDNAM= ',A8)
C
      RETURN
      END
