C MODULE URFCB
C-----------------------------------------------------------------------
C
C  ROUTINE TO CREATE NEW FILES FCFGSTAT AND FCFGLIST.
C
      SUBROUTINE URFCB (IER)
C
C  STEPS IN THE ROUTINE ARE --
C    1.COPY OLD FORECAST GROUPS WHICH BELONG TO CARRYOVER GROUPS
C      INTO THE NEW FORECAST GROUP STATUS FILE IN ORDER OF THEIR
C      EXECUTION IN THE CARRYOVER GROUP.
C    2.COPY OLD FORECAST GROUPS WHICH DO NOT BELONG TO ANY
C      CARRYOVER GROUP TO THE NEW FORECAST GROUP STATUS FILE.
C    3.SCAN NEW FORECAST GROUP STATUS FILE TO GET LOCATIONS OF
C      SEGMENT LISTS ON OLD SEGMENT LIST FILE, MOVE SEGMENT LIST
C      TO NEW SEGMENT LIST FILE, AND UPDATE POINTERS TO SEGMENT
C      LIST ON NEW STATUS FILE.
C
C  ROUTINE ORIGINALLY WRITTEN BY - ED JOHNSON - HRL - 12/1979
C
      CHARACTER*8 RTNNAM,OLDNAM
C
      INCLUDE 'common/ionum'
      INCLUDE 'common/fdbug'
      INCLUDE 'common/fcfgs'
      INCLUDE 'common/fccgd1'
      INCLUDE 'common/fccgd2'
      INCLUDE 'common/fcfgs2'
      INCLUDE 'common/fginfo'
      INCLUDE 'common/fginf2'
      INCLUDE 'common/fcunit'
      INCLUDE 'urcommon/urunts'
C
      DIMENSION OBSLET(2),TEMP2(2)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/reorder/RCS/urfcb.f,v $
     . $',                                                             '
     .$Id: urfcb.f,v 1.2 2002/02/11 21:14:42 dws Exp $
     . $' /
C    ===================================================================
C
      DATA OBSLET/4HOBSO,4HLETE/
      DATA BLANK/4H    /
C
C
      RTNNAM='URFCB'
C
      IF (ITRACE.GE.1) WRITE (IODBUG,*) 'ENTER ',RTNNAM
C
      IOPNUM=0
      CALL FSTWHR (RTNNAM,IOPNUM,OLDNAM,IOLDOP)
C
      LDEBUG=IFBUG('FCB ')
C
      IER=0
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  COPY STATUS FILE FOR ALL FORECAST GROUPS IN A CARRYOVER GROUP
C
      CALL SULINE (IPR,2)
      WRITE (IPR,110)
C
      IF (LDEBUG.GT.0) WRITE (IODBUG,*) ' NCG2=',NCG2,
     *   ' NFGREC=',NFGREC,' NFG=',NFG,' NFGRC2=',NFGRC2
      IF (NFGREC.LE.0) GO TO 60
      IF (NCG2.LE.0) GO TO 40
      IF (NFG.LE.0) GO TO 40
C
      IF (LDEBUG.GT.0) WRITE (IODBUG,120)
C
      DO 30 ICG=1,NCG2
         CALL UREADT (LFCGD,ICORE2(ICG),CGIDC,ISTAT)
         DO 20 JCOSEQ=1,NFG
            DO 10 IFGREC=1,NFGREC
               CALL UREADT (KFFGST,IFGREC,FGID,ISTAT)
               IF (FGID(1).EQ.OBSLET(1).AND.FGID(2).EQ.OBSLET(2))
     *            GO TO 10
               IF (ICOSEQ.NE.JCOSEQ) GO TO 10
               IF (CGIDF(1).NE.CGIDC(1)) GO TO 10
               IF (CGIDF(2).NE.CGIDC(2)) GO TO 10
               NFGRC2=NFGRC2+1
               CALL UWRITT (LFFGST,NFGRC2,FGID,ISTAT)
               IF (LDEBUG.GT.0) WRITE (IODBUG,130) FGID,CGIDC
               GO TO 20
10             CONTINUE
            WRITE (IPR,140) JCOSEQ,CGIDC
            CALL SUERRS (IPR,3,-1)
            IER=1
            GO TO 90
20          CONTINUE
30       CONTINUE
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  ADD FORECAST GROUPS WHICH ARE NOT PART OF ANY CARRYOVER GROUP
C
40    DO 50 IFGREC=1,NFGREC
         CALL UREADT (KFFGST,IFGREC,FGID,ISTAT)
         IF (CGIDF(1).NE.BLANK) GO TO 50
         IF (CGIDF(2).NE.BLANK) GO TO 50
         IF (FGID(1).EQ.OBSLET(1).AND.FGID(2).EQ.OBSLET(2)) GO TO 50
         NFGRC2=NFGRC2+1
         CALL UWRITT (LFFGST,NFGRC2,FGID,ISTAT)
         IF (LDEBUG.GT.0) WRITE (IODBUG,130) FGID,CGIDF
50       CONTINUE
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  ALL FORECAST GROUPS COPIED TO NEW STATUS FILE
C
C  MAKE SURE NFGRC2 IS ADDED TO RECORD ONE OF NEW FILE
60    CALL UREADT (LFFGST,1,FGID2,ISTAT)
      IDUMG2=NFGRC2
      CALL UWRITT (LFFGST,1,FGID2,ISTAT)
C
C  MAKE SURE MAXFG2 IS ADDED TO RECORD TWO OF NEW FCFGSTAT FILE
C  (MAXIMUM NUMBER OF FORECAST GROUPS ALLOWS ON NEW STATUS FILE)
      CALL UREADT (LFFGST,2,FGID2,ISTAT)
      IDUMG2=MAXFG2
      CALL UWRITT (LFFGST,2,FGID2,ISTAT)
C
      CALL SULINE (IPR,2)
      WRITE (IPR,150) NFGRC2,MAXFG2
C
      IF (NFGRC2.EQ.0) GO TO 90
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  CREATE NEW SEGMENT LIST FILE
C
      CALL SULINE (IPR,2)
      WRITE (IPR,160)
C
      NRLIST=0
C
      DO 80 IFGREC=1,NFGRC2
         CALL UREADT (LFFGST,IFGREC,FGID2,ISTAT)
         IF (NSEG2.EQ.0) GO TO 80
         DO 70 ICOUNT=1,NSEG2
            IROLD=IREC2+ICOUNT-1
            IRNEW=NRLIST+ICOUNT
            CALL UREADT (KFFGL,IROLD,TEMP2,ISTAT)
            CALL UWRITT (LFFGL,IRNEW,TEMP2,ISTAT)
70          CONTINUE
         IREC2=NRLIST+1
         NRLIST=NRLIST+NSEG2
         CALL UWRITT (LFFGST,IFGREC,FGID2,ISTAT)
80       CONTINUE
C
      CALL SULINE (IPR,2)
      WRITE (IPR,170)
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
90    CALL FSTWHR (OLDNAM,IOLDOP,OLDNAM,IOLDOP)
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
110   FORMAT ('0*** NOTE - BEGIN TO COPY FORECAST GROUP DEFINITION ',
     *   'FILE.')
120   FORMAT ('0- LIST OF FORECAST GROUPS -' /
     *   '0',2X,'FORECAST   CARRYOVER' /
     *   ' ',2X,'GROUP',10X,'GROUP' /
     *   ' ',2X,8('-'),4X,8('-'))
130   FORMAT (' ',2X,2A4,4X,2A4)
140   FORMAT ('0*** ERROR - IN URFCB - FORECAST GROUP ',
     *   15HTO BE EXECUTED ,I5,23H-TH IN CARRYOVER GROUP ,2A4 /
     *     10X,'COULD NOT BE FOUND ON OLD FILES.')
150   FORMAT ('0*** NOTE - ',I4,' FORECAST GROUPS SUCCESSFULLY ',
     *   'COPIED. MAXIMUM ALLOWED IS ',I4,'.')
160   FORMAT ('0*** NOTE - BEGIN TO COPY SEGMENT LIST FILE.')
170   FORMAT ('0*** NOTE - SEGMENT LIST FILE SUCCESSFULLY COPIED.')
C
      END
