C MODULE DELSEG
C-----------------------------------------------------------------------
C
C  ROUTINE TO DELETE A SEGMENT.
C
      SUBROUTINE DELSEG (INSEG,IER)
C
C  ORIGINALLY WRITTEN BY -- ED JOHNSON - HRL - 11/1979
C
      CHARACTER*8 RTNNAM,OPNOLD,XNAME
C
      DIMENSION INSEG(2)
      DIMENSION XIFGID(2),INTEST(3)
C
      INCLUDE 'common/ionum'
      INCLUDE 'common/fdbug'
      INCLUDE 'common/fcfgs'
      INCLUDE 'common/fcsegn'
      INCLUDE 'common/fcsegp'
      INCLUDE 'common/fcunit'
      INCLUDE 'common/fsglst'
      INCLUDE 'common/oldp'
      INCLUDE 'common/oldt'
      INCLUDE 'common/oldts'
C
      EQUIVALENCE (IFGID(1),XIFGID(1))
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_top/RCS/delseg.f,v $
     . $',                                                             '
     .$Id: delseg.f,v 1.7 2001/06/13 13:16:50 dws Exp $
     . $' /
C    ===================================================================
C
C
      RTNNAM='DELSEG'
C
      IF (ITRACE.GT.0) WRITE (IODBUG,*) 'ENTER ',RTNNAM
C
      IOPNUM=0
      CALL FSTWHR (RTNNAM,IOPNUM,OPNOLD,IOLDOP)
C
      IBUG=IFBUG('DLTE')
C
      IF (IBUG.GT.0) WRITE (IODBUG,10) INSEG
10    FORMAT (' INSEG=',2A4)
C
      IER=0
C
C  GET SEGMENT DEFINITION
      CALL FGETSG (INSEG,IRSEG,MOLDP,OLDP,MOLDT,OLDT,MOLDTS,OLDTS,
     *   0,0,IERR)
      IF (IERR.NE.0) THEN
         WRITE (IPR,20) INSEG
20    FORMAT ('0**WARNING** SEGMENT ',2A4,' NOT FOUND.')
         CALL WARN
         IER=1
         GO TO 140
         ENDIF
C
C  CHECK IF SEGMENT BELONGS TO A CARRYOVER GROUP
      CALL UMEMOV (ICGID,XNAME,2)
      IF (XNAME.NE.' ') THEN
         WRITE (IPR,30) INSEG,ICGID,IFGID
30    FORMAT ('0**ERROR** SEGMENT ',2A4,'BELONGS TO',
     *   ' CARRRYOVER GROUP ',2A4,
     *   ' AND FORECAST GROUP ',2A4,'.')
         CALL ERROR
         IER=1
         GO TO 140
         ENDIF
C
C  CHECK IF SEGMENT BELONGS TO A FORECAST GROUP
      CALL UMEMOV (IFGID,XNAME,2)
      IF (XNAME.NE.' '.AND.XNAME.NE.'OBSOLETE') THEN
         WRITE (IPR,40) INSEG,IFGID
40    FORMAT ('0**ERROR** SEGMENT ',2A4,' BELONGS TO',
     *   'FORECAST GROUP ',2A4,'.')
         CALL ERROR
         IER=1
         GO TO 140
         ENDIF
C
      INDERR=0
C
C  CHECK IF ANY FORECAST GROUPS DEFINED
      IF (NFGREC.EQ.0) GO TO 70
C
C  CHECK THE SPECIAL FORECAST GROUPS
      DO 60 IFGREC=1,NFGREC
         CALL UREADT (KFFGST,IFGREC,FGID,IERR)
C     CHECK FORECAST GROUP IS OBSOLETE
         CALL UMEMOV (FGID,XNAME,2)
         IF (XNAME.EQ.'OBSOLETE') GO TO 60
         IF (ISPEC.NE.1) GO TO 60
         IR1=IREC
         IR2=IREC+NSEG-1
         DO 50 IR=IR1,IR2
            CALL UREADT (KFFGL,IR,INTEST,IERR)
            IF (INTEST(1).EQ.INSEG(1).AND.INTEST(2).EQ.INSEG(2)) THEN
               WRITE (IPR,40) INSEG,FGID
               CALL ERROR
               IER=1
               INDERR=1
               ENDIF
50          CONTINUE
60       CONTINUE
C
70    IF (INDERR.EQ.1) GO TO 140
C
      CALL UREADT (KFSGPT,1,NS,IERR)
      CALL UREADT (KFSGPT,2,NRP,IERR)
C
C  CHECK EACH SEGMENT IN POINTER FILE
      DO 100 I=1,NS
         J=I+2
         CALL UREADT (KFSGPT,J,INTEST,IERR)
         IF (INTEST(1).EQ.INSEG(1).AND.INTEST(2).EQ.INSEG(2)) THEN
C        SET NLIST TO ZERO TO FORCE COMMON BLOCK FSGLST TO BE REFILLED
            NLIST=0
            IF (IBUG.GT.0) WRITE (IODBUG,80) INTEST(1),INTEST(2),J
80       FORMAT (' INTEST=',2A4,' J=',I6)
            CALL UMEMOV ('OBSOLETE',INTEST,2)
            CALL UWRITT (KFSGPT,J,INTEST,IERR)
C        SET ID'S ON FCSEGSTS, FCPARAM AND FCCARRY TO 'OBSOLETE'
            ISREC=INTEST(3)
            CALL FOBSSG (INSEG,ISREC)
C        remove the old fs5files for the segment
CCC            ier = rmRESJfs5files( OLDP, INSEG )
C        CHECK FOR ESP SEGMENT DEFINITION AND DELETE IF ONE EXISTS
            IF (IEREC.GT.0) THEN
               CALL ESPRDF (0,1,IEREC,X,1,X,1,X,1,IERR)
               IF (IERR.NE.0) GO TO 140
               CALL EDELSG (0,IEREC,INSEG,0)
               ENDIF
            GO TO 120
            ENDIF
100      CONTINUE
C
      WRITE (IPR,110) INSEG
110   FORMAT ('0**WARNING** SEGMENT ',2A4,' NOT FOUND ON SEGMENT ',
     *   'POINTER FILE.')
         CALL WARN
      IER=0
      GO TO 140
C
C  CHECK FOR FLASH FLOOD GUIDANCE PARAMETERS IN PPPDB
120   CALL DEL32 (OLDP,MOLDP,X,1)
C
      WRITE (IPR,130) INSEG
130   FORMAT ('0**NOTE** SEGMENT ',2A4,' DELETED.')
C
140   CALL FSTWHR (OPNOLD,IOLDOP,OPNOLD,IOLDOP)
C
      IF (ITRACE.GT.0) WRITE (IODBUG,*) 'EXIT ',RTNNAM
C
      RETURN
C
      END
