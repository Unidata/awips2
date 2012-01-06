C MODULE URFCC
C-----------------------------------------------------------------------
C
C  THIS ROUTINE COPIES SEGMENT AND CARRYOVER INFORMATION FROM
C  THE OLD FILES TO THE NEW FILES.
C
      SUBROUTINE URFCC (MLSTRC,LSTRC,NLSTRC,
     *   MC,C,MD,D,MP,P,MT,T,MTS,TS,ISTAT)
C
C  LISTC IS USED TO CHECK WHICH OLD SEGMENTS WERE NOT COPIED:
C  LISTC(I)=0 IF RECORD NUMBER I ON OLD SEGMENT STATUS FILE NOT COPIED
C  LISTC(I)=1 IF RECORD NUMBER I ON OLD SEGMENT STATUS FILE COPIED
C   The value of MLISTC in ureord, and MLIST in urfcc and mlist in common/fsglst 
C    and in block datas eeinbloc and fcblock must all be the same.
      PARAMETER (MLIST=15000)
      INTEGER LISTC(MLIST)/MLIST*0/
      CHARACTER*8 LISTN(MLIST)/MLIST*' '/
C
      CHARACTER*7 TYPMSG
      CHARACTER*8 XDSEGN
C      
      DIMENSION C(MC),D(MD),P(MP),T(MT),TS(MTS)
      DIMENSION LSTRC(2,MLSTRC)
      DIMENSION IUSESL(20)
C
      INCLUDE 'common/ionum'
      INCLUDE 'common/where'
      INCLUDE 'common/fdbug'
      INCLUDE 'common/sysbug'
      INCLUDE 'common/fccgd'
      INCLUDE 'common/fccgd1'
      INCLUDE 'common/fccgd2'
      INCLUDE 'common/fcfgs2'
      INCLUDE 'common/fcrunc'
      INCLUDE 'common/fcsegc'
      INCLUDE 'common/fcsegn'
      INCLUDE 'common/fcsegp'
      INCLUDE 'common/fcsgp2'
      INCLUDE 'common/fcunit'
      INCLUDE 'common/fctime'
      INCLUDE 'urcommon/urunts'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/reorder/RCS/urfcc.f,v $
     . $',                                                             '
     .$Id: urfcc.f,v 1.6 2004/10/22 17:43:46 edwin Exp $
     . $' /
C    ===================================================================
C
C
      DATA BLANK/4H    /
C          
C      
      IF (ITRACE.GT.0) THEN
         CALL SULINE (IODBUG,1)
         WRITE (IODBUG,*) '*** ENTER URFCC'
         ENDIF
C
      IBUG=IFBUG('FCC ')
C
      ISTAT=0
C
      CALL UREPET ('?',TYPMSG,LEN(TYPMSG))
      NSGTOT=0
C
      IF (NRSTS.GT.MLIST) THEN
         WRITE (IPR,180) NRSTS,MLIST
         CALL SUERRS (IPR,2,-1)
         ISTAT=1
         GO TO 170
         ENDIF
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
      IF (NCG2.LE.0) GO TO 60
C
C  COPY NON-OBSOLETE SEGMENTS
C
      CALL SULINE (IPR,2)
      WRITE (IPR,190)
C
C  PROCESS EACH CARRYOVER GROUP
      DO 50 ICG2=1,NCG2
C     GET LIST OF SEGMENTS IN EXECUTION ORDER
         IDTYP=1
         CALL FCORDR (IDTYP,CGIDS2(1,ICG2),IERR,D,MD)
         IF (IERR.GT.0) THEN
            WRITE (IPR,200) CGIDS2(1,ICG2),CGIDS2(2,ICG2)
            CALL SUERRS (IPR,2,-1)
            ISTAT=1
            GO TO 60
            ENDIF
         IF (NSEGEX.LE.0) GO TO 50
C     COPY SEGMENTS BELONGING TO A CARRYOVER GROUP
         CALL SULINE (IPR,2)
         WRITE (IPR,210) CGIDS2(1,ICG2),CGIDS2(2,ICG2)
         NUMSEG=0
         IFILSL=0
C     PROCESS EACH SEGMENT
         DO 30 ISEGEX=1,NSEGEX
            IRSEG=IRSGEX(ISEGEX)
C        GET SEGMENT STATUS AND PARAMETERS FROM OLD FILES
            CALL FGETSG (IDSEGN,IRSEG,MP,P,MT,T,MTS,TS,1,0,IERR)
            IF (IERR.GT.0) THEN
               WRITE (IPR,230) 'ERROR',IRSEG
               CALL SUERRS (IPR,2,-1)
               ISTAT=1
               GO TO 20
               ENDIF
            CALL UMEMOV (IDSEGN,XDSEGN,2)
            IF (IBUG.GT.0) THEN
               CALL SULINE (IODBUG,1)
               WRITE (IODBUG,220) XDSEGN,IRSEG
               ENDIF
C        CHECK IF SEGMENT ALREADY PROCESSED
            IF (NRSTS.GT.0) THEN
               DO 10 I=1,NRSTS
                  IF (XDSEGN.EQ.LISTN(I)) THEN
                     WRITE (IPR,370) XDSEGN,IRSEG
                     CALL SULINE (IPR,2)
                     GO TO 30
                     ENDIF
10                CONTINUE
               ENDIF
            ISEG(1)=IDSEGN(1)
            ISEG(2)=IDSEGN(2)
C        ADD RATING CURVES TO LIST
            CALL URFCRL (MLSTRC,LSTRC,NLSTRC,MP,P,MT,T,IERR)
            IF (IERR.GT.0) THEN
               WRITE (IPR,240) 'ERROR'
               CALL SUERRS (IPR,2,-1)
               ISTAT=1
               GO TO 20
               ENDIF
C        COPY SEGMENT STATUS AND PARAMETERS TO NEW FILES
            CALL URFCCA (MP,P,MT,T,MTS,TS,MC,C,KWOCRY,IERR)
            IF (IERR.GT.0) THEN
               WRITE (IPR,250) 'ERROR',XDSEGN
               CALL SUERRS (IPR,2,-1)
               ISTAT=1
               GO TO 20
               ENDIF
C        COPY CARRYOVER TO NEW FILES
            TYPMSG='ERROR'
            CALL URFCCB (IFILSL,IUSESL,MC,C,KWOCRY,TYPMSG,IERR)
            IF (IERR.GT.0) THEN
               WRITE (IPR,260) 'ERROR',XDSEGN
               CALL SUERRS (IPR,2,-1)
               ISTAT=1
               GO TO 20
               ENDIF
            NUMSEG=NUMSEG+1
            NSGTOT=NSGTOT+1
20          LISTC(IRSEG)=1
            LISTN(IRSEG)=XDSEGN
            IF (IBUG.GT.0) THEN
               CALL SULINE (IODBUG,1)
               WRITE (IODBUG,*)
     *            ' IRSEG=',IRSEG,
     *            ' LISTC(IRSEG)=',LISTC(IRSEG),
     *            ' LISTN(IRSEG)=',LISTN(IRSEG),
     *            ' '
               ENDIF
30          CONTINUE
         CALL SULINE (IPR,2)
         WRITE (IPR,270) NUMSEG,'REORDERED'
         IF (IBUG.GT.0) THEN
            CALL ULINE (IODBUG,1)
            WRITE (IODBUG,280) NSLOTS,NSLOT2,ICG2,IFILSL
            ENDIF
C     IF NSLOT2 LESS THAN NSLOTS, RESET DATES OF CARRYOVER IN /FCCGD1/
         IF (NSLOTS.LE.NSLOT2) GO TO 50
            IF (IBUG.GT.0) THEN
               CALL SULINE (IODBUG,1)
               WRITE (IODBUG,290) (IUSESL(JJ),JJ=1,NSLOTS)
               ENDIF
C        HAVE REMOVED AT LEAST ONE CARRYOVER SLOT - USE ARRAY IUSESL
C        TO DETERMINE WHICH DATES HAVE BEEN TRANSFERED
            ISLT2=0
            DO 40 ISLOT=1,NSLOTS
               IF (IUSESL(ISLOT).EQ.0) GO TO 40
C              SLOT TRANSFERED
                  ISLT2=ISLT2+1
                  ICODAY(ISLT2)=ICODAY(ISLOT)
                  ICOTIM(ISLT2)=ICOTIM(ISLOT)
                  LUPDAY(ISLT2)=LUPDAY(ISLOT)
                  LUPTIM(ISLT2)=LUPTIM(ISLOT)
                  IPC(ISLT2)=IPC(ISLOT)
                  IF (IBUG.GT.0) THEN
                     CALL SULINE (IODBUG,1)
                     WRITE (IODBUG,300) ISLOT,ISLT2
                     ENDIF
40             CONTINUE
C        UPDATE RECORD IN NEW FILE
            IF (IBUG.GT.0) THEN
               CALL SULINE (IODBUG,1)
               WRITE (IODBUG,310) LFCGD,ICORE2(ICG2),CGIDC
               ENDIF
            CALL UWRITT (LFCGD,ICORE2(ICG2),CGIDC,IERR)
            IF (IERR.GT.0) THEN
               WRITE (IPR,330) ICORE2(ICG2),LFCGD
               CALL SUERRS (IPR,2,-1)
               ENDIF
50       CONTINUE
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  COPY SEGMENTS IN FORECAST GROUPS BUT NOT IN CARRYOVER GROUPS
C
60    CALL SULINE (IPR,2)
      WRITE (IPR,340)
C
      NUMSEG=0
      IFILSL=0
C
      IF (NFGRC2.LE.0) GO TO 110
C
      DO 100 IFGREC=1,NFGRC2
         CALL UREADT (LFFGST,IFGREC,FGID2,IERR)
         IF (IERR.GT.0) THEN
            WRITE (IPR,320) IFGREC,LFFGST
            CALL SUERRS (IPR,2,-1)
            GO TO 100
            ENDIF
C     CHECK IF USED IN CARRYOVER GROUP            
         IF (CGIDF2(1).NE.BLANK.OR.CGIDF2(2).NE.BLANK) GO TO 100
C     GET LIST OF SEGMENTS IN EXECUTION ORDER
         IDTYP=2
         CALL FCORDR (IDTYP,FGID2,IERR,D,MD)
         IF (IERR.GT.0) THEN
            WRITE (IPR,350) FGID2
            CALL SUERRS (IPR,2,-1)
            GO TO 100
            ENDIF
         IF (NSEGEX.GT.0) THEN
            DO 90 ISEGEX=1,NSEGEX
               IRSEG=IRSGEX(ISEGEX)
               IF (IBUG.GT.0) THEN
                  CALL SULINE (IODBUG,1)
                  WRITE (IODBUG,*)
     *               ' NSEGEX=',NSEGEX,
     *               ' ISEGEX=',ISEGEX,
     *               ' IRSEG=',IRSEG,
     *               ' LISTC(IRSEG)=',LISTC(IRSEG),
     *               ' '
                  ENDIF
C           CHECK IF ALREADY COPIED THIS SEGMENT
               IF (LISTC(IRSEG).EQ.1) GO TO 90
C           GET SEGMENT STATUS AND PARAMETERS FROM OLD FILES
               CALL FGETSG (IDSEGN,IRSEG,MP,P,MT,T,MTS,TS,1,0,IERR)
               IF (IERR.GT.0) THEN
                  WRITE (IPR,230) 'WARNING',IRSEG
                  CALL SUWRNS (IPR,2,-1)
                  GO TO 90
                  ENDIF
               CALL UMEMOV (IDSEGN,XDSEGN,2)
               IF (IBUG.GT.0) THEN
                  CALL SULINE (IODBUG,1)
                  WRITE (IODBUG,220) XDSEGN,IRSEG
                  ENDIF
C           CHECK IF SEGMENT ALREADY PROCESSED
               IF (NRSTS.GT.0) THEN
                  DO 70 I=1,NRSTS
                     IF (XDSEGN.EQ.LISTN(I)) THEN
                        WRITE (IPR,370) XDSEGN,IRSEG
                        CALL SULINE (IPR,2)
                        GO TO 90
                        ENDIF
70                   CONTINUE
                  ENDIF
               ISEG(1)=IDSEGN(1)
               ISEG(2)=IDSEGN(2)
C           ADD RATING CURVES TO LIST
               CALL URFCRL (MLSTRC,LSTRC,NLSTRC,MP,P,MT,T,IERR)
               IF (IERR.GT.0) THEN
                  WRITE (IPR,240) 'WARNING'
                  CALL SUWRNS (IPR,2,-1)
                  GO TO 80
                  ENDIF
C           COPY SEGMENT STATUS AND PARAMETERS TO NEW FILES
               CALL URFCCA (MP,P,MT,T,MTS,TS,MC,C,KWOCRY,IERR)
               IF (IERR.GT.0) THEN
                  WRITE (IPR,250) 'WARNING',XDSEGN
                  CALL SUWRNS (IPR,2,-1)
                  GO TO 80
                  ENDIF
C           COPY CARRYOVER TO NEW FILES
               TYPMSG='ERROR'
               CALL URFCCB (IFILSL,IUSESL,MC,C,KWOCRY,TYPMSG,IERR)
               IF (IERR.GT.0) THEN
                  WRITE (IPR,260) 'WARNING',XDSEGN
                  CALL SUWRNS (IPR,2,-1)
                  GO TO 80
                  ENDIF
               NUMSEG=NUMSEG+1
               NSGTOT=NSGTOT+1
80             LISTC(IRSEG)=1
               LISTN(IRSEG)=XDSEGN
               IF (IBUG.GT.0) THEN
                  CALL SULINE (IODBUG,1)
                  WRITE (IODBUG,*)
     *               ' IRSEG=',IRSEG,
     *               ' LISTC(IRSEG)=',LISTC(IRSEG),
     *               ' LISTN(IRSEG)=',LISTN(IRSEG),
     *               ' '
                  ENDIF
90             CONTINUE
            ENDIF
100      CONTINUE
C
110   CALL SULINE (IPR,2)
      WRITE (IPR,270) NUMSEG,'COPIED'
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  COPY SEGMENTS NOT IN FORECAST GROUPS
C
      CALL SULINE (IPR,2)
      WRITE (IPR,360)
C
      NUMSEG=0
      IFILSL=0
C
      IF (NRSTS.LE.0) GO TO 150
C
      DO 140 IRSEG=1,NRSTS
         IF (IBUG.GT.0) THEN
            CALL SULINE (IODBUG,1)
            WRITE (IODBUG,*)
     *         ' NRSTS=',NRSTS,
     *         ' IRSEG=',IRSEG,
     *         ' LISTC(IRSEG)=',LISTC(IRSEG),
     *         ' '
            ENDIF
         IF (LISTC(IRSEG).EQ.1) GO TO 140
         CALL UREADT (KFSGST,IRSEG,IDSEGN,IERR)
         IF (IERR.GT.0) THEN
            WRITE (IPR,320) IRSEG,KFSGST
            CALL SUERRS (IPR,2,-1)
            GO TO 140
            ENDIF
         CALL UMEMOV (IDSEGN,XDSEGN,2)
C     CHECK IF SEGMENT IS OBSOLETE
         IF (XDSEGN.EQ.'OBSOLETE') GO TO 140
C        CHECK IF SEGMENT ALREADY PROCESSED
            DO 120 I=1,NRSTS
               IF (XDSEGN.EQ.LISTN(I)) THEN
                  WRITE (IPR,370) XDSEGN,IRSEG
                  CALL SULINE (IPR,2)
                  GO TO 140
                  ENDIF
120            CONTINUE
C        GET SEGMENT STATUS AND PARAMETERS FROM OLD FILES
            CALL FGETSG (IDSEGN,IRSEG,MP,P,MT,T,MTS,TS,1,0,IERR)
            IF (IBUG.GT.0) THEN
               CALL SULINE (IODBUG,1)
               WRITE (IODBUG,220) XDSEGN,IRSEG
               ENDIF
            IF (IERR.GT.0) THEN
               WRITE (IPR,230) 'WARNING',IRSEG
               CALL SUWRNS (IPR,2,-1)
               GO TO 130
               ENDIF
            ISEG(1)=IDSEGN(1)
            ISEG(2)=IDSEGN(2)
C        ADD RATING CURVES TO LIST
            CALL URFCRL (MLSTRC,LSTRC,NLSTRC,MP,P,MT,T,IERR)
            IF (IERR.GT.0) THEN
               WRITE (IPR,240) 'WARNING'
               CALL SUWRNS (IPR,2,-1)
               GO TO 130
               ENDIF
C        COPY SEGMENT STATUS AND PARAMETERS TO NEW FILES
            CALL URFCCA (MP,P,MT,T,MTS,TS,MC,C,KWOCRY,IERR)
            IF (IERR.GT.0) THEN
               WRITE (IPR,250) 'WARNING',XDSEGN
               CALL SUWRNS (IPR,2,-1)
               GO TO 130
               ENDIF
C        COPY CARRYOVER TO NEW FILES
            TYPMSG='WARNING'
            CALL URFCCB (IFILSL,IUSESL,MC,C,KWOCRY,TYPMSG,IERR)
            IF (IERR.GT.0) THEN
               WRITE (IPR,260) 'WARNING',XDSEGN
               CALL SUWRNS (IPR,2,-1)
               GO TO 130
               ENDIF
            NUMSEG=NUMSEG+1
            NSGTOT=NSGTOT+1
130         LISTC(IRSEG)=1
            LISTN(IRSEG)=XDSEGN
            IF (IBUG.GT.0) THEN
               CALL SULINE (IODBUG,1)
               WRITE (IODBUG,*)
     *            ' IRSEG=',IRSEG,
     *            ' LISTC(IRSEG)=',LISTC(IRSEG),
     *            ' LISTN(IRSEG)=',LISTN(IRSEG),
     *            ' '
               ENDIF
140      CONTINUE
C
150   CALL SULINE (IPR,2)
      WRITE (IPR,270) NUMSEG,'COPIED'
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
      IF (ISTAT.GT.0) GO TO 170
C
C  UPDATE RECORDS 1 AND 2 OF FILE FCSEGSTS
      IREC=1
      CALL UWRITT (LFSGPT,IREC,NS2,IERR)
      IF (IERR.GT.0) THEN
          WRITE (IPR,330) IREC,LFSGPT
          CALL SUERRS (IPR,2,-1)
          ENDIF
      IREC=2
      CALL UWRITT (LFSGPT,IREC,NRP2,IERR)
      IF (IERR.GT.0) THEN
          WRITE (IPR,330) IREC,LFSGPT
          CALL SUERRS (IPR,2,-1)
          ENDIF
C
C  UPDATE RECORD 1 OF FILE FCCOGDEF
      IREC=1
      CALL UWRITT (LFCGD,IREC,NSLOT2,IERR)
      IF (IERR.GT.0) THEN
          WRITE (IPR,330) IREC,LFCGD
          CALL SUERRS (IPR,2,-1)
          ENDIF
C
      CALL SULINE (IPR,2)
      WRITE (IPR,380)
C
C  PROCESS SEGMENTS NOT COPIED
      NUMSEG=0
      IF (NRSTS.GT.0) THEN
         DO 160 IRSEG=1,NRSTS
            IF (LISTC(IRSEG).EQ.0) THEN
               CALL UREADT (KFSGST,IRSEG,IDSEGN,IERR)
               IF (IERR.GT.0) THEN
                  WRITE (IPR,320) IRSEG,KFSGST
                  CALL SUERRS (IPR,2,-1)
                  GO TO 160
                  ENDIF
               IF (IBUG.GT.0) THEN
                  CALL SULINE (IODBUG,1)
                  WRITE (IODBUG,390) XDSEGN,IRSEG
                  ENDIF
               NUMSEG=NUMSEG+1
               ENDIF
160         CONTINUE
         ENDIF
      CALL SULINE (IPR,2)
      WRITE (IPR,400) NUMSEG
C
170   IF (ITRACE.GT.0) THEN
         CALL SULINE (IODBUG,1)
         WRITE (IODBUG,*) '*** EXIT URFCC'
         ENDIF
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
180   FORMAT ('0*** ERROR - IN URFCC - OLD SEGMENT STATUS ',
     *  'HAS ',I5,' RECORDS USED. ',
     *  'A MAXIMUM OF ',I5,' CAN BE PROCESSED.')
190   FORMAT ('0*** NOTE - BEGIN TO REORDER NON-OBSOLETE SEGMENT ',
     *   'DEFINITIONS.')
200   FORMAT ('0*** ERROR - IN URFCC - ERROR CALLING ROUTINE FCORDR ',
     *   'FOR CARRYOVER GROUP ',2A4,'.')
210   FORMAT ('0*** NOTE - BEGIN TO COPY SEGMENTS IN ',
     *   'CARRYOVER GROUP ',2A4,'.')
220   FORMAT (' BEGIN COPY OF SEGMENT ',A,' AT RECORD',I7,
     *  ' ON OLD STATUS FILE')
230   FORMAT ('0*** ',A,' - IN URFCC - ROUTINE FGETSG ',
     *   'STATUS INDICATES THAT SEGMENT AT RECORD ',I7,
     *   ' COULD NOT BE COPIED.')
240   FORMAT ('0*** ',A,' - IN URFCC - URFCRL STATUS CODE IS ',I2,'.')
250   FORMAT ('0*** ',A,' - IN URFCC - ROUTINE URFCCA ',
     *  'STATUS INDICATES SEGMENT ',A,' COULD NOT BE COPIED.')
260   FORMAT ('0*** ',A,' - IN URFCC - ROUTINE URFCCB ',
     *  'STATUS INDICATES THAT CARRYOVER FOR SEGMENT ',A,
     *  ' COULD NOT BE COPIED.')
270   FORMAT ('0*** NOTE - ',I4,' SEGMENTS SUCCESSFULLY ',A,'.')
280   FORMAT (' IN URFCC - NSLOTS=',I4,3X,
     *   'NSLOT2=',I4,3X,'ICG2=',I4,3X,'IFILSL=',I5)
290   FORMAT (' IUSESL=',20(I2,1X))
300   FORMAT (' TRANSFERING SLOT ',I4,' INTO SLOT ',I4)
310   FORMAT (' WRITING TO UNIT ',I3,', RECORD ',I7,', CGIDC= ',2A4)
320   FORMAT ('0**ERROR** IN URFCC - READING RECORD ',I6,
     *   ' FROM UNIT ',I2,'.')
330   FORMAT ('0**ERROR** IN URFCC - WRITING RECORD ',I6,
     *   ' TO UNIT ',I2,'.')
340   FORMAT ('0*** NOTE - BEGIN TO COPY SEGMENTS IN ',
     *   'FORECAST GROUPS BUT NOT IN CARRYOVER GROUPS.')
350   FORMAT ('0*** ERROR - IN URFCC - ERROR CALLING ROUTINE FCORDR ',
     *   'FOR FORECAST GROUP ',2A4,'.')
360   FORMAT ('0*** NOTE - BEGIN TO COPY SEGMENTS NOT IN FORECAST ',
     *    'GROUPS.')
370   FORMAT ('0*** NOTE - IN URFCC - SEGMENT ',A,
     *   ' AT RECORD ',I6,
     *   ' HAS ALREADY BEEN PROCESSED AND WILL NOT BE COPIED.')
380   FORMAT ('0*** NOTE - ALL NON-OBSOLETE SEGMENTS SUCCESSFULLY ',
     *  'COPIED.')
390   FORMAT (' SEGMENT ',A,' AT RECORD ',I6,' NOT COPIED')
400   FORMAT ('0*** NOTE - ',I4,' OBSOLETE SEGMENTS NOT COPIED.')
C
      END
