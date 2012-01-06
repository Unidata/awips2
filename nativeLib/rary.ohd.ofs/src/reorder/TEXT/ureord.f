C MODULE UREORD
C-----------------------------------------------------------------------
C
C  ROUTINE TO REORDER THE ESP PARAMETER FILE USING THE COMPUTATIONAL
C  ORDER INFORMATION IN THE FORECAST COMPONENT DATA BASE.
C
      SUBROUTINE UREORD (IINIT,IORFCC,
     *   MD,D,MP,P,MT,T,MTS,TS,
     *   IOESPP,NOVPRT,
     *   MFUNIT,KFUNIT,LFUNIT,KFUNITO,KFUNITT,ISTAT)
C
      CHARACTER*8 DDNAME/'FTXXF001'/
C
      DIMENSION D(MD),P(MP),T(MT),TS(MTS)
      DIMENSION KFUNIT(MFUNIT),LFUNIT(MFUNIT),KFUNITO(MFUNIT)
      DIMENSION KFUNITT(MFUNIT)
C   The value of MLISTC in ureord, and MLIST in urfcc and mlist in common/fsglst
C    and in block datas eeinbloc and fcblock must all be the same.
      PARAMETER (MLISTC=15000)
      DIMENSION LISTC(MLISTC)
      DIMENSION OBSLT(2),IOBSLT(2)
C
      INCLUDE 'common/ionum'
      INCLUDE 'common/sysbug'
      INCLUDE 'common/fdbug'
      INCLUDE 'common/fcunit'
      INCLUDE 'common/fcrunc'
      INCLUDE 'common/fccgd'
      INCLUDE 'common/fcsegn'
      INCLUDE 'common/fcfgs'
      INCLUDE 'common/fcsegp'
      INCLUDE 'common/eunit'
      INCLUDE 'common/espfle'
      INCLUDE 'common/esprec'
      INCLUDE 'common/ep'
      INCLUDE 'common/esp'
      INCLUDE 'common/ets'
      INCLUDE 'ucommon/uordrx'
      INCLUDE 'urcommon/urunts'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/reorder/RCS/ureord.f,v $
     . $',                                                             '
     .$Id: ureord.f,v 1.7 2004/10/22 17:43:38 edwin Exp $
     . $' /
C    ===================================================================
C
      DATA OBSLT/4hOBSO,4hLETE/
      DATA BLANK/4h    /
C
C
      ISTAT=0
C
C  SET DEBUG LEVEL
      LDEBUG=IFBUG('ESPP')
C
      CALL UMEMST (0,LISTC,MLISTC)
      CALL UMEMOV (OBSLT,IOBSLT,2)
C
      CALL SULINE (IPR,2)
      WRITE (IPR,240) '0'
      DO 10 I=1,NOVPRT
         CALL SULINE (IPR,0)
         WRITE (IPR,240) '+'
10       CONTINUE
      CALL SULINE (IPR,2)
      WRITE (IPR,*)
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
      IPRERR=1
C
C  CHECK IF OLD ESPPARM FILE ALLOCATED
      CALL UFXDDN (DDNAME,KUESPP,IERR)
      CALL UDDST (DDNAME,IPRERR,IERR1)
C
C  CHECK IF NEW ESPPARM FILE ALLOCATED
      CALL UFXDDN (DDNAME,LUESPP,IERR)
      CALL UDDST (DDNAME,IPRERR,IERR2)
C
      IF (IERR1.GT.0.OR.IERR2.GT.0) GO TO 200
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  CHECK IF NEW FILE TO BE INITIALIZED
      IF (IINIT.EQ.0) GO TO 30
C
      CALL SULINE (IPR,2)
      WRITE (IPR,250) '0'
      DO 20 I=1,NOVPRT
         CALL SULINE (IPR,0)
         WRITE (IPR,250) '+'
20       CONTINUE
      CALL SULINE (IPR,2)
      WRITE (IPR,*)
C
      IXINIT=IINIT
      IF (IXINIT.EQ.0) IXINIT=-1
C
      CALL SULINE (IPR,2)
      IF (IXINIT.EQ.1) WRITE (IPR,260)
      IF (IXINIT.EQ.-1) WRITE (IPR,270)
C
C  READ CONTROL RECORD
      IF (IXINIT.EQ.1) CALL UREADT (LUESPP,1,ESPDAT,ISTAT)
      IF (IXINIT.EQ.-1) CALL UREADT (KUESPP,1,ESPDAT,ISTAT)
C
C  RESET NEXT AVAILABLE RECORD
      ESPDAT(2)=2
C
C  WRITE CONTROL RECORD TO NEW FILE
      CALL UWRITT (LUESPP,1,ESPDAT,ISTAT)

      CALL SULINE (IPR,2)
      WRITE (IPR,280) LUESPP
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  PRINT DATA BASE STATUS BEFORE REORDERING
C
30    CALL SULINE (IPR,2)
      WRITE (IPR,*)
      CALL SULINE (IPR,2)
      WRITE (IPR,290) '0'
      DO 40 I=1,NOVPRT
         CALL SULINE (IPR,0)
         WRITE (IPR,290) '+'
40       CONTINUE
      CALL SULINE (IPR,2)
      WRITE (IPR,*)
C
      IREC=1
      CALL UREADT (KUESPP,IREC,ESPDAT,ISTAT)
      IF (ISTAT.NE.0) THEN
         WRITE (IPR,300) IREC,KUESPP
         CALL SUERRS (IPR,2,-1)
         GO TO 190
         ENDIF
C
      MXREC=ESPDAT(1)
      NXREC=ESPDAT(2)
      LRECL=ESPDAT(3)
C
C  CHECK IF FILE IS EMPTY
      IF (NXREC.GT.2) GO TO 50
         CALL SULINE (IPR,2)
         WRITE (IPR,310)
         GO TO 200
C
50    KEPARM=KUESPP
      IAMORD=0
      CALL UMEMOV (KFUNIT,KFUNITT,MFUNIT)
      CALL UMEMOV (KFUNITO,KFUNIT,MFUNIT)
      CALL ESTS (MTSESP,MPESP,MSPESP)
      CALL UMEMOV (KFUNITT,KFUNIT,MFUNIT)
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
      CALL SULINE (IPR,2)
      WRITE (IPR,320) '0'
      DO 60 I=1,NOVPRT
         CALL SULINE (IPR,0)
         WRITE (IPR,320) '+'
60       CONTINUE
      CALL SULINE (IPR,2)
      WRITE (IPR,*)
C
C  CALL UREADT TO FILL COMMON.FCCGD FROM FILE FCCOGDEF, READ RECORD 1
C  OF OLD ESP PARAM FILE, AND INITIALZE NEW ESP PARAM FILE.
      IREC=1
      CALL UREADT (KFCGD,IREC,NSLOTS,ISTAT)
      IF (ISTAT.NE.0) THEN
         WRITE (IPR,300) IREC,KFCGD
         CALL SUERRS (IPR,2,-1)
         GO TO 190
         ENDIF
C
      IREC=1
      CALL UREADT (LUESPP,IREC,ESPDAT,ISTAT)
      IF (ISTAT.NE.0) THEN
         WRITE (IPR,300) IREC,LUESPP
         CALL SUERRS (IPR,2,-1)
         GO TO 190
         ENDIF
C
      MXRECN=ESPDAT(1)
      NXRECN=ESPDAT(2)
      LRECLN=ESPDAT(3)
C
C  CALL UREADT TO FILL COMMON FCSEGP FROM FILE FCSEGPTR TO GET NRSTS
C  (THE NUMBER OF RECORDS USED ON THE FILE FCSEGSTS)
      IREC=1
      CALL UREADT (KFSGPT,IREC,NS,ISTAT)
      IF (ISTAT.NE.0) THEN
         WRITE (IPR,300) IREC,KFSGPT
         CALL SUERRS (IPR,2,-1)
         GO TO 190
         ENDIF
      IF (LDEBUG.GT.0) THEN
         WRITE (IPR,*)
     *      ' NS=',NS,
     *      ' NRSTS=',NRSTS,
     *      ' MRSTS=',MRSTS,
     *      ' '
         CALL SULINE (IPR,1)
         ENDIF
C
      IF (NRSTS.GT.MLISTC) THEN
         WRITE (IPR,340) MLISTC,NRSTS
         CALL SUERRS (IPR,2,-1)
         GO TO 190
         ENDIF
C
C  SET RECORD NUMBER TO BE WRITTEN TO NEW FILE
      IRECN=1
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
      IF (LDEBUG.GT.0) THEN
         WRITE (IPR,*) ' NCG=',NCG
         CALL SULINE (IPR,1)
         ENDIF
C
C  CHECK NUMBER OF CARRYOVER GROUPS DEFINED
      IF (NCG.LE.0) GO TO 80
C
C  CHECK FOR SEGMENTS USED IN CARRYOVER GROUPS
C
      WRITE (IPR,350)
      CALL SULINE (IPR,2)
C
C  PROCESS EACH CARRYOVER GROUP
      DO 70 ICG=1,NCG
         IF (LDEBUG.GT.0) THEN
            WRITE (IPR,360) NCG,ICG,(CGIDS(I,ICG),I=1,2)
            CALL SULINE (IPR,1)
            ENDIF
         WRITE (IPR,370) (CGIDS(I,ICG),I=1,2)
         CALL SULINE (IPR,2)
         NSEGCP=0
C     CALL FCORDR TO FIND THE ORDER OF SEGMENTS IN THE FC COMPONENT.
C     THE LIST OF RECORD NUMBERS OF SEGMENTS ON FILE FCSEGSTS IS
C     STORED IN THE ARRAY IRSGEX IN COMMON.FCRUNC
         CALL FCORDR (1,CGIDS(1,ICG),IERR,D,MD)
         IF (LDEBUG.GT.0) THEN
            WRITE (IPR,390) RUNID,ISEGEX,NSEGEX,MSEGEX
            CALL SULINE (IPR,1)
            ENDIF
         IF (IERR.NE.0) THEN
            WRITE (IPR,400) (CGIDS(I,ICG),I=1,2)
            CALL SUERRS (IPR,2,-1)
            GO TO 70
            ENDIF
         IF (NSEGEX.EQ.0) THEN
            CALL SULINE (IPR,2)
            WRITE (IPR,410) (CGIDS(I,ICG),I=1,2)
            GO TO 80
            ENDIF
C     COPY SEGMENTS IN CARRYOVER GROUP
         DO 65 ISEGEX=1,NSEGEX
            IRSEG=IRSGEX(ISEGEX)
            CALL UREOR2 (IORFCC,OBSLT,IRECN,MLISTC,LISTC,NSEGCP,
     *         MP,P,MT,T,MTS,TS,IERR)
            IF (IERR.NE.0) THEN
               WRITE (IPR,420) 'CARRYOVER GROUP',(CGIDS(I,ICG),I=1,2)
               CALL SUERRS (IPR,2,-1)
               ISTAT=1
               GO TO 70
               ENDIF
65          CONTINUE
         WRITE (IPR,380) NSEGCP
         CALL SULINE (IPR,2)
70       CONTINUE
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  CHECK FOR SEGMENTS USED IN FORECAST GROUPS BUT NOT IN CARRYOVER
C  GROUPS
C
80    CALL UREADT (KFFGST,1,FGID,ISTAT)
C
      IF (LDEBUG.GT.0) THEN
         WRITE (IPR,*) ' NFGREC=',NFGREC
         CALL SULINE (IPR,1)
         ENDIF
C
      IF (NFGREC.LE.0) GO TO 110
C
      WRITE (IPR,430)
      CALL SULINE (IPR,2)
C
      NSEGCP=0
C
C  PROCESS EACH FORECAST GROUP
      DO 100 IFGREC=1,NFGREC
C     GET THE NAME OF FORECAST GROUP
         CALL UREADT (KFFGST,IFGREC,FGID,ISTAT)
         IF (LDEBUG.GT.0) THEN
            WRITE (IPR,*) ' IFGREC=',IFGREC
            CALL SULINE (IPR,1)
            ENDIF
C     CHECK IF USED IN CARRYOVER GROUP
         IF (CGIDF(1).NE.BLANK.OR.CGIDF(2).NE.BLANK) GO TO 100
C     CHECK IF OBSOLETE
         IF (FGID(1).EQ.OBSLT(1).AND.FGID(2).EQ.OBSLT(2)) GO TO 100
C     GET THE ORDER OF THE SEGMENTS
         CALL FCORDR (2,FGID,IERR,D,MD)
         IF (IERR.EQ.0) GO TO 90
            WRITE (IPR,460) (FGID(K),K=1,2)
            CALL SUERRS (IPR,2,-1)
            GO TO 190
90       IF (LDEBUG.GT.0) THEN
            WRITE (IPR,450) IFGREC,FGID
            CALL SULINE (IPR,1)
            ENDIF
         IF (NSEGEX.EQ.0) THEN
            CALL SULINE (IPR,2)
            WRITE (IPR,470)
            GO TO 100
            ENDIF
C        COPY THE SEGMENTS IN THE FORECAST GROUP
            DO 95 ISEGEX=1,NSEGEX
               IRSEG=IRSGEX(ISEGEX)
               CALL UREOR2 (IORFCC,OBSLT,IRECN,MLISTC,LISTC,NSEGCP,
     *            MP,P,MT,T,MTS,TS,IERR)
               IF (IERR.NE.0) THEN
                  WRITE (IPR,420) 'FORECAST GROUP',FGID
                  CALL SUERRS (IPR,2,-1)
                  ISTAT=1
                  GO TO 100
                  ENDIF
95             CONTINUE
100      CONTINUE
C
      WRITE (IPR,380) NSEGCP
      CALL SULINE (IPR,2)
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  CHECK FOR SEGMENTS NOT USED IN FORECAST GROUPS
C
110   WRITE (IPR,500)
      CALL SULINE (IPR,2)
C
      NSEGCP=0
C
      DO 150 IRSEG=1,NRSTS
C     CHECK IF SEGMENT ALREAY PROCESSED
         IF (LISTC(IRSEG).EQ.1) GO TO 150
         CALL UREADT (KFSGST,IRSEG,IDSEGN,ISTAT)
C     CHECK IF OBSOLETE SEGMENT
         IF (IDSEGN(1).EQ.IOBSLT(1).AND.IDSEGN(2).EQ.IOBSLT(2)) THEN
            GO TO 140
            ENDIF
         CALL UREOR2 (IORFCC,OBSLT,IRECN,MLISTC,LISTC,NSEGCP,
     *      MP,P,MT,T,MTS,TS,IERR)
         IF (IERR.NE.0) THEN
            WRITE (IPR,420) 'SEGMENT',IDSEGN
            CALL SUERRS (IPR,2,-1)
            ISTAT=1
            GO TO 150
            ENDIF
140      LISTC(IRSEG)=1
150      CONTINUE
C
         WRITE (IPR,380) NSEGCP
         CALL SULINE (IPR,2)
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  UPDATE CONTROL RECORD
      ESPDAT(1)=MXRECN+.01
      IF (IRECN.GT.1) NXRECN=IRECN+1
      ESPDAT(2)=NXRECN+.01
      ESPDAT(3)=LRECLN+.01
      CALL UWRITT (LUESPP,1,ESPDAT,ISTAT)
C
      NAVAIL=MXRECN-NXRECN+1
      CALL SULINE (IPR,2)
      WRITE (IPR,550) NAVAIL
C
      CALL SULINE (IPR,2)
      WRITE (IPR,560) '0'
      DO 170 I=1,NOVPRT
         CALL SULINE (IPR,0)
         WRITE (IPR,560) '+'
170      CONTINUE
      CALL SULINE (IPR,2)
      WRITE (IPR,*)
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  PRINT STATUS AFTER REODERING
         CALL SULINE (IPR,2)
         WRITE (IPR,570) '0'
      DO 180 I=1,NOVPRT
         CALL SULINE (IPR,0)
         WRITE (IPR,570) '+'
180      CONTINUE
      CALL SULINE (IPR,2)
      WRITE (IPR,*)
C
      CALL UREADT (LUESPP,1,ESPDAT,ISTAT)
      MXREC=ESPDAT(1)
      NXREC=ESPDAT(2)
      LRECL=ESPDAT(3)
      KEPARM=LUESPP
      IAMORD=1
      CALL ESTS (MTSESP,MPESP,MSPESP)
      GO TO 200
C
190   ISTAT=1
      GO TO 210
C
200   IOESPP=1
C
210   CALL SULINE (IPR,2)
      WRITE (IPR,*)
      CALL SULINE (IPR,2)
      WRITE (IPR,580) '0'
      DO 220 I=1,NOVPRT
         CALL SULINE (IPR,0)
         WRITE (IPR,580) '+'
220      CONTINUE
      CALL SULINE (IPR,2)
      WRITE (IPR,*)
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
240   FORMAT (A,3('***  BEGIN ESPPARM FILE REORDER  '),'***')
250   FORMAT (A,3('***  INITIALIZE ESPPARM NEW FILE  '),'***')
260   FORMAT ('0*** BEGIN TO INITIALIZE ESP PARAMETER ',
     *   'NEW FILE.')
270   FORMAT ('0*** BEGIN TO INITIALIZE ESP PARAMETER ',
     *   'NEW FILE BY COPYING CONTROLS FROM OLD FILES.')
280   FORMAT ('0*** NOTE - RECORD 1 OF UNIT ',I2,' SUCCESSFULLY ',
     *   'INITIALIZED.')
290   FORMAT (A,3('***  ESPPARM STATUS BEFORE REORDERING  '),'***')
300   FORMAT ('0*** ERROR - READING RECORD ',I4,' FROM UNIT ',I2,'.')
310   FORMAT ('0*** NOTE - ESP PARAMETER FILE IS EMPTY.')
320   FORMAT (A,3('***  BEGIN ESPPARM FILE REORDERING  '),'***')
340   FORMAT ('0*** ERROR - LISTC IS DIMENSIONED AT ',I4,
     *   '. THERE ARE ',I4,' RECORDS IN FILE FCSEGSTS.')
350   FORMAT ('0*** NOTE - BEGIN TO COPY SEGMENTS USED IN ',
     *   'CARRYOVER GROUPS.')
360   FORMAT (' NCG=',I2,' ICG=',I2,' CGIDS=',2A4)
370   FORMAT ('0*** NOTE - BEGIN TO COPY SEGMENTS IN CARRYOVER GROUP ',
     *   2A4,'.')
380   FORMAT ('0*** NOTE - ',I4,' SEGMENTS SUCCESSFULLY REORDERED.')
390   FORMAT (' RUNID=',2A4,' ISEGEX=',I5,' NSEGEX=',I6,' MSEGEX=',I6)
400   FORMAT ('0*** ERROR - CALLING ROUTINE FCORER FOR ',
     *   'CARRYOVER GROUP ',2A4,'.')
410   FORMAT ('0*** NOTE - NO SEGMENTS BELONG TO CARRYOVER GROUP ',
     *   2A4,'.')
420   FORMAT ('0*** ERROR - IN UREORD - CALLING UREOR2 FOR ',A,' ',
     *   2A4,'.')
430   FORMAT ('0*** NOTE - BEGIN TO COPY SEGMENTS USED IN ',
     *   'FORECAST GROUPS BUT NOT IN CARRYOVER GROUPS.')
450   FORMAT (' CALLING FCORDR FOR FORECAST GROUP NUMBER ',I4,
     *  ' NAMED ',2A4)
460   FORMAT ('0*** ERROR - IN UREORD - CALLING FCORDR FOR ',
     *  'FORECAST GROUP ',2A4,'.')
470   FORMAT ('0*** NOTE -  NSEGEX IS ZERO. UREOR2 NOT CALLED.')
500   FORMAT ('0*** NOTE - BEGIN TO COPY SEGMENTS NOT USED IN ',
     *   'FORECAST GROUPS.')
510   FORMAT (' IRSEG=',I5,' IEREC=',I5,' IDSEGN=',2A4)
520   FORMAT ('0*** ERROR - IN UREORD - CALLING FGETSG FOR ',
     *   'SEGMENT NUMBER ',I4,'.')
530   FORMAT ('0*** ERROR - IN UREORD - RECORD NUMBER TO BE READ ',
     *   'FROM UNIT ',I3,'(',I5,') EXCEEDS MAXIMUM RECORDS IN FILE ',
     *   I5,').')
540   FORMAT (' IEREC=',I4,' IDSEGN=',2A4,' IRECN=',I4)
550   FORMAT ('0*** NOTE - ',I4,' RECORDS AVAILABLE IN NEW ESP ',
     *   'PARAMETER FILE.')
560   FORMAT (A,3('***  END ESPPARM FILE REORDERING  '),'***')
570   FORMAT (A,3('***  ESPPARM STATUS AFTER REORDERING  '),'***')
580   FORMAT (A,3('***  END ESPPARM FILE REORDER  '),'***')
C
      END
