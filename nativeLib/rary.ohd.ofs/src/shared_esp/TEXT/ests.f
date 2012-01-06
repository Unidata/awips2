C MODULE ESTS
C-----------------------------------------------------------------------
C
      SUBROUTINE ESTS
C
C  ROUTINE TO PRINT STATUS INFORMATION FOR THE ESP PARAMETER FILE.
C
C  ORIGINALLY WRITTEN BY ED VANBLARGAN - HRL - JUNE,1981
C
      CHARACTER*8 OLDOPN,SEGID,ESEGID
      CHARACTER*32 FILENAME
      CHARACTER*160 PATHNAME
C
      DIMENSION IBUF(3)
C
      INCLUDE 'common/ionum'
      INCLUDE 'common/fdbug'
      INCLUDE 'common/where'
      INCLUDE 'common/espseg'
      INCLUDE 'common/espfle'
      INCLUDE 'common/fcunit'
      INCLUDE 'common/fcsegp'
      INCLUDE 'common/fsglst'
      INCLUDE 'common/fcsegn'
      INCLUDE 'ucommon/uordrx'
      INCLUDE 'urcommon/urunts'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shared_esp/RCS/ests.f,v $
     . $',                                                             '
     .$Id: ests.f,v 1.5 2002/02/12 16:50:12 dws Exp $
     . $' /
C    ===================================================================
C
C
      IOPNUM=-1
      CALL FSTWHR ('ESTS    ',IOPNUM,OLDOPN,IOLDOP)
C
      IF (ITRACE.GE.1) WRITE (IODBUG,*) 'ENTER ESTS'
C
      IBUG=IFBUG('EINI')
C
      WRITE (IPR,5)
5     FORMAT ('0- ESP PARAMETER FILE STATUS -')
C
C  CHECK IF FILE ESPPARM EXISTS
      FILENAME='ESPPARM'
      LFILENAME=LENSTR(FILENAME)
      CALL UPPFIX ('OPER',FILENAME,PATHNAME,LPATHNAME)
      IUNIT=0
      CALL UPEXIS (IUNIT,PATHNAME,IERR)
      IF (IERR.NE.0) THEN
         WRITE (IPR,10) FILENAME(1:LFILENAME)
10    FORMAT ('0**WARNING** FILE ',A,' NOT FOUND.')
         CALL ERROR
         GO TO 160
         ENDIF
C
C  READ ESP PARAMETER FILE CONTROL RECORD
      CALL EREAD1 (IERR)
      IF (IERR.NE.0) THEN
         WRITE (IPR,20) FILENAME(1:LFILENAME)
20    FORMAT ('0**ERROR** ENCOUNTERED READING CONTROL RECORD FROM ',
     *   'FILE ',A,'.')
         CALL ERROR
         GO TO 160
         ENDIF
C
      NUSED=NXREC-1
      NFREE=MXREC-NXREC+1
      IPCTU=0
      IF (NUSED.GT.0) THEN
         IPCTU=(FLOAT(NUSED)/FLOAT(MXREC))*100.+.5
         ENDIF
      WRITE (IPR,30) MXREC,NUSED,NFREE,IPCTU
30    FORMAT ('0',
     *   'MAXIMUM RECORDS = ',I5,5X,
     *   'USED RECORDS = ',I5,5X,
     *   'UNUSED RECORDS = ',I5,5X,
     *   'PERCENT USED = ',I3)
      IF (NXREC.EQ.2) THEN
         WRITE (IPR,35)
35    FORMAT ('0**NOTE** THE ESP PARAMETER FILE IS EMPTY.')
         GO TO 160
         ENDIF
C
      WRITE (IPR,40)
40    FORMAT (
     * '0',4X,1X,
     *     '          ',3X,'FIRST ',3X,'DATE      ',3X,
     *     'LENGTH OF',3X,'LENGTH OF',3X,'LENGTH OF' /
     * ' ',4X,1X,
     *     'SEGMENT ID',3X,'RECORD',3X,'CREATED   ',3X,
     *     'TS ARRAY ',3X,'P ARRAY  ',3X,'SP ARRAY ' /
     * ' ',4X,1X,
     *     '----------',3X,'------',3X,'----------',3X,
     *     '---------',3X,'---------',3X,'---------')
C
      IREC=2
      NSEG=0
      NOBS=0
C
C  READ FIRST RECORD FOR EACH SEGMENT
      IFILLA=0
      ICHKID=0
      MTSESP=1
      MPESP=1
      MSPESP=1
50    CALL ESPRDF (IFILLA,ICHKID,IREC,TSESP,MTSESP,PESP,MPESP,
     *   SPESP,MSPESP,IERR)
      IF (IERR.NE.0) THEN
         WRITE (IPR,60) IREC,FILENAME(1:LFILENAME)
60    FORMAT ('0**ERROR** ENCOUNTERED IN ROUTINE ESPRDF READING ',
     *   'RECORD ',I5,' FROM FILE ',A,'.')
         CALL ERROR
         GO TO 160
         ENDIF
      IF (NSREC.EQ.0) GO TO 100
C
      CALL UMEMOV (ID,SEGID,2)
C
C  CHECK IF OBSOLETE
      IF (SEGID.EQ.'OBSOLETE') THEN
         NOBS=NOBS+1
         GO TO 90
         ENDIF
C
C  PRINT SEGMENT INFORMATION
      NSEG=NSEG+1
      WRITE (IPR,70) NSEG,SEGID,IREC,(IECRDT(I),I=1,3),
     *   LTSESP,LPESP,LSPESP
70    FORMAT (' ',I4,1X,A,5X,I6,3X,I2.2,'/',I2.2,'/',I4,3X,
     *   I9,3X,I9,3X,I9)
C
C  CHECK IF DEFINED IN THE FORECAST COMPONENT DATA BASE
      CALL FLOCSG (SEGID,IRSEG)
      IF (IRSEG.EQ.0) THEN
         WRITE (IPR,80) SEGID
80    FORMAT ('0**WARNING** ESP SEGMENT ',A,' NOT FOUND IN THE ',
     *   'FORECAST COMPONENT DATA BASE.')
         CALL WARN
         ENDIF
C
90    IF (IBUG.GT.0) WRITE (IODBUG,*) 'NSREC=',NSREC
      IREC=NSREC
C  SET INDICATOR IF TO CHECK IF AT NEXT UNUSED RECORD
      INXREC=1
      IF (INXREC.EQ.1.AND.IREC.EQ.NXREC) GO TO 100
      GO TO 50
C
100   WRITE (IPR,110) NSEG,NOBS
110   FORMAT ('0NUMBER OF NON-OBSOLETE SEGMENTS = ',I5,5X,
     *   'NUMBER OF OBSOLETE SEGMENTS = ',I5)
      IF (IREC.NE.NXREC) THEN
         WRITE (IPR,120) IREC,NXREC
120   FORMAT ('0**WARNING** RECORD NUMBER OF LAST UNUSED RECORD (',I5,
     *   ') DOES NOT MATCH THAT IN THE CONTROL RECORD (',I5,').')
         CALL WARN
         ENDIF
C
C  CHECK IF ALL FORECAST COMPONENT SEGMENTS WITH ESP SEGMENT DEFINITIONS
C  FOUND IN ESP PARAMETER FILE
      IREC=1
      IF (IAMORD.EQ.0) IUNIT=KFSGPT
      IF (IAMORD.EQ.1) IUNIT=LFSGPT
      IF (IBUG.EQ.1) WRITE (IODBUG,*) 'IAMORD=',IAMORD,' IUNIT=',IUNIT
      CALL UREADT (IUNIT,IREC,NS,IERR)
      DO 150 I=1,NS
         IREC=I+2
         CALL UREADT (IUNIT,IREC,IBUF(1),IERR)
         CALL UMEMOV (IBUF(1),SEGID,2)
         IRSEG=IBUF(3)
         MP=1
         MT=1
         MTS=1
         ISRCH=1
         NOPARM=1
         CALL FGETSG (SEGID,IRSEG,MP,P,MT,T,MTS,TS,ISRCH,NOPARM,IERR)
         IF (IERR.NE.0) THEN
            WRITE (IPR,130) SEGID,'FORECAST COMPONENT DATA BASE'
130   FORMAT ('0**WARNING** FORECAST COMPONENT SEGMENT ',A,
     *   ' NOT FOUND IN ',A,'.')
            CALL WARN
            ELSE
               IF (SEGID.NE.'OBSOLETE'.AND.IEREC.GT.0) THEN
C              READ FIRST RECORD FOR EACH SEGMENT
                  IREC=2
                  IFILLA=0
                  ICHKID=0
140               CALL ESPRDF (IFILLA,ICHKID,IREC,TSESP,MTSESP,
     *               PESP,MPESP,SPESP,MSPESP,IERR)
                  IF (IERR.NE.0) THEN
                     WRITE (IPR,60) IREC,FILENAME(1:LFILENAME)
                     CALL ERROR
                     GO TO 150
                     ENDIF
                  IF (NSREC.EQ.0) THEN
                     WRITE (IPR,130) SEGID,'FILE ESPPARM'
                     CALL WARN
                     GO TO 150
                     ENDIF
                  CALL UMEMOV (ID,ESEGID,2)
                  IF (ESEGID.EQ.SEGID) THEN
                     IF (IBUG.EQ.1) WRITE (IODBUG,*) 'IREC=',IREC,
     *                  ' ESEGID=',ESEGID
C                 SEGMENT FOUND
                     IF (IREC.NE.IEREC) THEN
                        WRITE (IPR,145) FILENAME(1:LFILENAME),IREC,
     *                     IEREC,SEGID
145   FORMAT ('0**WARNING** RECORD NUMBER IN FILE ',A,' (',I5,
     *   ') DOES NOT MATCH THAT IN THE FORECAST COMPONENT FILES (',I5,
     *   ') FOR SEGMENT ',A,'.')
                        CALL WARN
                        ENDIF
                     GO TO 150
                     ENDIF
                  IREC=NSREC
                  IF (IBUG.EQ.1) WRITE (IODBUG,*) 'IREC=',IREC
                  IF (IREC.EQ.NXREC) THEN
                     WRITE (IPR,147) SEGID
147   FORMAT ('0**WARNING** SEGMENT ',A,' NOT FOUND IN THE ESP ',
     *   'PARAMETER FILE.')
                     CALL WARN
                     GO TO 150
                     ENDIF
                  GO TO 140
                  ENDIF
            ENDIF
150      CONTINUE
C
160   CALL FSTWHR (OLDOPN,IOLDOP,OLDOPN,IOLDOP)
C
      RETURN
C
      END
