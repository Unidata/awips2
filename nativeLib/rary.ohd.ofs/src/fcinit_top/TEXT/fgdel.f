C MODULE FGDEL
C-----------------------------------------------------------------------
C
C  ROUTINE TO DELETE A FORECAST GROUPS.
C
      SUBROUTINE FGDEL (FGIDIN)
C
C  ROUTINE ORIGINALLY WRITTEN BY -- ED JOHNSON - HRL - 11/1979
C
      CHARACTER*8 RTNNAM,OPNOLD
C
      DIMENSION FGIDIN(2),XIFGID(2)
C
      INCLUDE 'common/ionum'
      INCLUDE 'common/fdbug'
      INCLUDE 'common/fcfgs'
      INCLUDE 'common/fcsegn'
      INCLUDE 'common/fcsegp'
      INCLUDE 'common/fcunit'
C
      EQUIVALENCE (XIFGID(1),IFGID(1))
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_top/RCS/fgdel.f,v $
     . $',                                                             '
     .$Id: fgdel.f,v 1.4 2000/03/14 11:55:50 page Exp $
     . $' /
C    ===================================================================
C
      DATA BLANK/4H    /
C
C
      IF (ITRACE.GT.0) WRITE (IODBUG,*) 'ENTER FGDEL'
C
      RTNNAM='FGDEL'
      IOPNUM=0
      CALL FSTWHR (RTNNAM,IOPNUM,OPNOLD,IOLDOP)
C
      IBUG=IFBUG('DLTE')
C
      IER=0
C
      IF (IBUG.EQ.1) WRITE(IODBUG,10) FGIDIN
10    FORMAT (' FGIDIN=',2A4)
C
C  FIND FORECAST GROUP
      CALL FGGET (FGIDIN,IFGREC,IFOUND)
      IF (IFOUND.EQ.0) THEN
         WRITE (IPR,20) FGIDIN
20    FORMAT ('0**WARNING** FORECAST GROUP ',2A4,' NOT FOUND.')
         CALL WARN
         IER=1
         GO TO 110
         ENDIF
C
      IF (CGIDF(1).NE.BLANK.AND.CGIDF(2).NE.BLANK) THEN
         WRITE (IPR,30) FGID,CGIDF
30    FORMAT ('0**ERROR** FORECAST GROUP ',2A4,' BELONG TO ',
     *   'CARRYOVER GROUP ',2A4,'.')
         CALL ERROR
         IER=1
         GO TO 90
         ENDIF
C
      IF (IBUG.GT.0) WRITE (IODBUG,40) IFGREC,ISPEC,CGIDF
40    FORMAT (' IFGREC=',I6,' ISPEC=',I1,' CGIDF=',2A4)
C
      IF (ISPEC.EQ.1) GO TO 70
C
C  REMOVE FORECAST GROUP NAME FROM ALL SEGMENTS
      IF (NRSTS.GT.0) THEN
         DO 60 IRSEG=1,NRSTS
            CALL UREADT (KFSGST,IRSEG,IDSEGN,IERR)
            IF (FGIDIN(1).EQ.XIFGID(1).AND.FGIDIN(2).EQ.XIFGID(2)) THEN
               XIFGID(1)=BLANK
               XIFGID(2)=BLANK
               CALL UWRITT (KFSGST,IRSEG,IDSEGN,IERR)
               IF (IBUG.EQ.1) WRITE (IODBUG,50) IDSEGN,IRSEG
50    FORMAT (' IDSEGN=',2A4,' IRSEG=',I6)
               ENDIF
60          CONTINUE
         ENDIF
C
C  REMOVE CARRYOVER GROUP
70    CALL UMEMOV ('OBSOLETE',FGID,2)
      CALL UWRITT (KFFGST,IFGREC,FGID,IERR)
      IF (IBUG.GT.0) WRITE (IODBUG,80) FGIDIN,IFGREC
80    FORMAT (' FGIDIN=',2A4,' IFGREC=',I6)
C
90    IF (IER.EQ.0) WRITE(IPR,100) FGIDIN
100   FORMAT ('0**NOTE** FORECAST GROUP ',2A4,' DELETED.')
C
110   CALL FSTWHR (OPNOLD,IOLDOP,OPNOLD,IOLDOP)
C
      IF (ITRACE.GT.0) WRITE (IODBUG,*) 'EXIT FGDEL'
C
      RETURN
C
      END
