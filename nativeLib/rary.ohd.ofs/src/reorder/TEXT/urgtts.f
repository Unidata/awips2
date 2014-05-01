C MODULE URGTTS
C-----------------------------------------------------------------------
C
      SUBROUTINE URGTTS (LFCWRK,IFCWRK,LWORK1,IWORK1,LWORK2,IWORK2,
     *   LWORK3,IWORK3,MP,P,MT,T,MTS,TS,ISTAT)
C
C  THIS ROUTINE READS THE FORECAST COMPONENT CARRYOVER GROUPS,
C  THE FORECAST GROUPS IN THE CARRYOVER GROUPS AND THE SEGMENTS
C  IN EACH FORECAST GROUP. THE TIMES SERIES USED BY EACH SEGMENT
C  ARE WRITTEN TO THE NEW FILES.
C
C  ARGUMENT LIST:
C
C        NAME    TYPE  I/O  DIM     DESCRIPTION
C        ------  ----  ---  ------  -----------
C        LFCWRK     I    I    1     LENGTH OF ARRAY IFCWRK
C        IFCWRK     I    I  LFCWRK  ARRAY FOR FORECAST SEGMENTS
C        LWORK1     I    I    1     LENGTH OF ARRAY IWORK1
C        IWORK1     I    I  LWORK1  ARRAY FOR TIME SERIES
C        LWORK2     I    I    1     LENGTH OF ARRAY IWORK2
C        IWORK2     I    I  LWORK2  ARRAY FOR FUTURE TIME SERIES
C        LWORK3     I    I    1     LENGTH OF ARRAY IWORK3
C        IWORK3     I    I  LWORK3  ARRAY FOR RRS PARAMETER RECORD
C        ISTAT      I    O     1    STATUS CODE
C                                     0=NORMAL RETURN
C                                     OTHER=ERROR
C
      CHARACTER*4 IDTYPE
      CHARACTER*8 ITSID
C 
      DIMENSION P(MP),T(MT),TS(MTS)     
      DIMENSION IFCWRK(LFCWRK)
      DIMENSION IWORK1(LWORK1),IWORK2(LWORK2),IWORK3(LWORK3)
      PARAMETER (MFGIDS=100)
      DIMENSION CGID(2),FGIDS(2,MFGIDS),SEGID(2)
C
      INCLUDE 'uio'
      INCLUDE 'udebug'
      INCLUDE 'common/fcunit'
      INCLUDE 'common/fccgd'
      INCLUDE 'common/fccgd1'
      INCLUDE 'common/fcfgs'
      INCLUDE 'common/fcrunc'
      INCLUDE 'common/fcsegn'
      INCLUDE 'urcommon/urcdta'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/reorder/RCS/urgtts.f,v $
     . $',                                                             '
     .$Id: urgtts.f,v 1.4 1999/01/20 15:02:00 page Exp $
     . $' /
C    ===================================================================
C
C
      IF (IPRTR.GT.0) THEN
         WRITE (IOGDB,140)
         CALL SULINE (IOGDB,1)
         ENDIF
C
      WRITE (LP,150)
      CALL SULINE (LP,2)
C
      ISTAT=0
C
      NUMTS=0
      NUMSEG=0
C
C  GET NUMBER OF CARRYOVER GROUPS AND NAMES
      CALL UREADT (KFCGD,1,NSLOTS,ISTAT)
      IF (ISTAT.GT.0) GO TO 110
      IF (IPRDB.GT.0) THEN
         WRITE (IOGDB,*) 'NCG=',NCG
         CALL SULINE (IOGDB,1)
         ENDIF
      IF (IPRDB.GT.0) THEN
         WRITE (IOGDB,160) ((CGIDS(I,J),I=1,2),J=1,NCG)
         CALL SULINE (IOGDB,1)
         ENDIF
C
C  CHECK IF ANY CARRYOVER GROUPS DEFINED
      IF (NCG.EQ.0) THEN
         WRITE (LP,170)
         GO TO 130
         ENDIF
C
C  GET NUMBER FOR FORCAST GROUPS IN CARRYOVER GROUP
      CALL UREADT (KFFGST,1,FGID,ISTAT)
      IF (ISTAT.GT.0) GO TO 110
      NFGREC=IDUMYG
      IF (IPRDB.GT.0) THEN
         WRITE (IOGDB,*) 'NFGREC=',NFGREC
         CALL SULINE (IOGDB,1)
         ENDIF
C
C  PROCESS EACH CARRYOVER GROUP
      DO 100 ICG=1,NCG
         CALL UREADT (KFCGD,ICOREC(ICG),CGIDC,ISTAT)
         IF (ISTAT.GT.0) GO TO 110
         IF (IPRDB.GT.0) THEN
            WRITE (IOGDB,*) 'NFG=',NFG
            CALL SULINE (IOGDB,1)
            ENDIF
         CGID(1)=CGIDS(1,ICG)
         CGID(2)=CGIDS(2,ICG)
         NREC1=1
C     PROCESS EACH FORECAST GROUP
         DO 30 IFG=1,NFG
            IF (NREC1.GT.NFGREC) THEN
               WRITE (LP,190) CGID,ICOSEQ,NFG
               CALL SUERRS (LP,2,-1)
               IWURFL=1
               GO TO 100
               ENDIF
C        GET FORECAST GROUP IDENTIFIER
            DO 10 NREC=NREC1,NFGREC
               CALL UREADT (KFFGST,NREC,FGID,ISTAT)
               IF (ISTAT.GT.0) GO TO 110
               IF (ISPEC.EQ.1) GO TO 10
               IF (CGIDS(1,ICG).NE.CGIDF(1).OR.CGIDS(2,ICG).NE.CGIDF(2))
     *            GO TO 10
               IF (IPRDB.GT.0) THEN
                  WRITE (IOGDB,'(A,2A4)') 'FGID=',FGID
                  CALL SULINE (IOGDB,1)
                  ENDIF
               IF (ICOSEQ.GT.MFGIDS) THEN
                  WRITE (LP,200) MFGIDS
                  CALL SUERRS (LP,2,-1)
                  IWURFL=1
                  GO TO 130
                  ENDIF
               FGIDS(1,ICOSEQ)=FGID(1)
               FGIDS(2,ICOSEQ)=FGID(2)
               GO TO 20
10             CONTINUE
20          NREC1=NREC+1
30          CONTINUE
C     PROCESS EACH FORECAST GROUP
         DO 90 IFG=1,NFG
C        GET LIST OF SEGMENTS IN ORDER FOR EACH FORECAST GROUP
            ITYPE=2
            CALL FCORDR (ITYPE,FGIDS(1,IFG),IERR,IFCWRK,LFCWRK)
            IF (NSEGEX.EQ.0) THEN
               WRITE (LP,210) (FGIDS(J,IFG),J=1,2),NSEGEX
               CALL SUERRS (LP,2,-1)
               IWURFL=1
               GO TO 100
               ENDIF
C        PROCESS EACH SEGMENT
            DO 80 NSG=1,NSEGEX
C           GET SEGMENT DEFINITION BY SEARCHING BY RECORD NUMBER       
               IOPT=1
               IREC=IRSGEX(NSG)
               NOPARM=0
               CALL FGETSG (SEGID,IREC,MP,P,MT,T,MTS,TS,IOPT,NOPARM,
     *            IERR)
               IF (IERR.NE.0) THEN
                  IF (IERR.EQ.1) THEN
                     WRITE (LP,220) IOPT
                     CALL SUERRS (LP,2,-1)
                     IWURFL=1
                     ENDIF
                  IF (IERR.EQ.2) THEN
                     WRITE (LP,230) MP,MT,MTS
                     CALL SUERRS (LP,2,-1)
                     IWURFL=1
                     ENDIF
                  GO TO 130
                  ENDIF
               NTPOS=1
               IUPDTE=0
C           GET TYPE CODE
40             ITSTYP=TS(NTPOS)
               IF (ITSTYP.GE.0.AND.ITSTYP.LE.4) THEN
                  ELSE
                     WRITE (LP,235) ITSTYP,NSG,FGID,CGID
                     CALL SUERRS (LP,2,-1)
                     IWURFL=1
                     GO TO 80
                  ENDIF
C           CHECK IF LAST ENTRY IN TS ARRAY                  
               IF (ITSTYP.EQ.0) GO TO 70
C           CHECK IF INTERNAL TIME SERIES               
               IF (ITSTYP.EQ.4) GO TO 60
C           GET TIME SERIES IDENTIFIER
               CALL UMEMOV (TS(NTPOS+12),ITSID,2)
               IF (IPRDB.GT.0) THEN
                  WRITE (IOGDB,*) 
     *               ' ITSTYP=',ITSTYP,
     *               ' '
                  CALL SULINE (IOGDB,1)
                  ENDIF
C           GET DATA TYPE
               CALL UMEMOV (TS(NTPOS+14),IDTYPE,1)
C           READ TIME SERIES DATA AND WRITE TO NEW FILES
               IF (IPRDB.GT.0) THEN
                  WRITE (IOGDB,250) ITSID,IDTYPE,
     *               TS(NTPOS+5),TS(NTPOS+6)
                  CALL SULINE (IOGDB,1)
                  ENDIF
               CALL URRDTS (ITSID,IDTYPE,LWORK1,IWORK1,LWORK2,IWORK2,
     *            LWORK3,IWORK3,NUMTS,LTSHDR,ISTAT)
               IF (ISTAT.GT.0) THEN
                  WRITE (LP,260) IDSEGN,FGID,CGID
                  CALL SUERRS (LP,2,-1)
                  IWURFL=1
                  ENDIF
               IF (IPRDB.GT.0) THEN
                  WRITE (IOGDB,*)
     *               ' ITSTYP=',ITSTYP,
     *               ' LTSHDR=',LTSHDR,
     *               ' '
                  CALL SULINE (IOGDB,1)
                  ENDIF
C           CHECK IF UPDATE OR OUTPUT TIME SERIES
               IF (ITSTYP.EQ.2) GO TO 50
               IF (ITSTYP.EQ.3) GO TO 50
               GO TO 60
C           UPDATE TIME SERIES RECORD LOCATION
50             TS(NTPOS+15)=LTSHDR+.01
               IUPDTE=1
C           GET NEXT IDENTIFIER IN THIS SEGMENT
60             NTPOS=TS(NTPOS+1)
               GO TO 40
C           CHECK IF SEGMENT DEFINITION NEEDS TO BE UPDATED
70             IF (IUPDTE.EQ.1) THEN
                  IADD=0
                  NPLACE=1
                  NNCOM=0
                  NOPARM=0
                  MSGE=0
                  CALL FPUTSG (P,T,TS,IADD,NPLACE,NNCOM,NOPARM,MSGE,
     *               IERR)
                  IF (IERR.EQ.0) NUMSEG=NUMSEG+1
                  IF (IERR.GT.0) THEN
                     WRITE (LP,270) IERR
                     CALL SUERRS (LP,2,-1)
                     IWURFL=1
                     ENDIF
                  ENDIF
80             CONTINUE
90          CONTINUE
100      CONTINUE
C
      CALL SULINE (LP,2)
      WRITE (LP,280) NUMTS
      CALL SULINE (LP,2)
      WRITE (LP,290) NUMSEG
      GO TO 130
C
C  FILE READ ERROR
110   WRITE (LP,120)
120   FORMAT ('0*** ERROR - IN URGTTS - FILE READ ERROR.')
      CALL SUERRS (LP,2,-1)
      IWURFL=1
C
130   IF (IPRTR.GT.0) THEN
         WRITE (IOGDB,300)
         CALL SULINE (IOGDB,1)
         ENDIF
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
140   FORMAT (' *** ENTER URGTTS')
150   FORMAT ('0*** NOTE - BEGIN TO REORDER  << TIME SERIES >>  ',
     *   'USED IN CARRYOVER GROUPS.')
160   FORMAT (' CARRYOVER GROUP IDENTIFIERS=' / 25(1X,2A4))
170   FORMAT ('0*** WARNING - NO CARRYOVER GROUPS DEFINED IN THE ',
     *   'FORECAST COMPONENT DATA BASE.')
190   FORMAT ('0*** ERROR - IN URGTTS - NUMBER OF FORECAST GROUPS ',
     *   'FOUND FOR CARRYOVER GROUP ',2A4,' (',I3,
     *   ') DOES NOT EQUAL THE NUMBER EXPECTED (',I3,').')
200   FORMAT ('0*** ERROR - IN URGTTS - MAXIMUM NUMBER OF FORECAST ',
     *   'GROUPS THAT CAN BE PROCESSED (',I5,') EXCEEDED.')
210   FORMAT ('0*** ERROR - IN URGTTS - FORECAST GROUP ',2A4,
     *   ' HAS NO SEGMENTS.')
220   FORMAT ('0*** ERROR - IN URGTTS - INVALID ARGUMENTS IN CALL TO ',
     *   'FGETSG : IOPT=',I2)
230   FORMAT ('0*** ERROR - IN URGTTS - P, T OR TS ARRAYS ',
     *   'NOT FILLED BECAUSE ARRAYS TOO SMALL. ',
     *   'MP=',I5,' MT=',I5,' MTS=',I5)
235   FORMAT ('0*** ERROR - IN URGTTS - INVALID TS TYPE (',I3,')',
     *   ' FOR SEGMENT NUMBER ',I3,
     *   ' OF FORECAST GROUP ',2A4,
     *   ' AND CARRYOVER GROUP ',2A4,'.')
250   FORMAT (' ITSID=',A,3X,'IDTYPE=',A,3X,
     *   'TS(NTPOS+5)=',F5.0,3X,
     *   'TS(NTPOS+6)=',F5.0)
260   FORMAT ('0*** ERROR - IN URGTTS - PROCESSING TIME SERIES',
     *   ' FOR SEGMENT ',2A4,
     *   ' OF FORECAST GROUP ',2A4,
     *   ' AND CARRYOVER GROUP ',2A4,'.')
270   FORMAT ('0*** ERROR - IN URGTTS - FPUTSG NOT SUCCESSFULLY ',
     *   'CALLED. STATUS CODE=',I3)
280   FORMAT ('0*** NOTE - ',I5,' TIME SERIES HAVE BEEN SUCCESSFULLY ',
     *   'REORDERED.')
290   FORMAT ('0*** NOTE - ',I5,' SEGMENTS HAVE BEEN SUCCESSFULLY ',
     *   'UPDATED.')
300   FORMAT (' *** EXIT URGTTS')
C
      END
