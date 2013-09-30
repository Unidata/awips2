C MODULE FGETSG
C-----------------------------------------------------------------------
C
C  ROUTINE TO GET SEGMENT DEFINITION.
C
      SUBROUTINE FGETSG (ISEGID,ISEGRC,MP,P,MT,T,MTS,TS,IOPT,NOPARM,IER)
C
C  READS A RECORD FROM FILE FCSEGSTS INTO COMMON BLOCK FCSEGN.
C  READS P, T AND TS ARRAYS FROM FILE FCPARAM.
C
C  IOPT: 0=READ SEGMENT BY IDENTIFIER (ISEGID)
C        1=READ SEGMENT BY RECORD NUMBER (ISEGRC)
C
C  NOPARM: 0=READ SEGMENT STATUS AND PARAMETERS
C          1=READ ONLY SEGMENT STATUS - NOT PARAMETERS
C
C  IER: 0=NORMAL RETURN
C       1=IVALID ARGUMENTS OR SEGMENT DOES NOT EXIST
C       2=MP, MT OR MTS TOO SMALL
C
      CHARACTER*8 OLDOPN,SEGID,SEGOLD
      INTEGER T
C      
      DIMENSION ISEGID(2),P(MP),T(MT),TS(MTS)
C
      INCLUDE 'common/ionum'
      INCLUDE 'common/fdbug'
      INCLUDE 'common/fprog'
      INCLUDE 'common/where'
      INCLUDE 'common/errdat'
      INCLUDE 'common/flarys'
      INCLUDE 'common/fcsegn'
      INCLUDE 'common/fctsgn'
      INCLUDE 'common/fcsegp'
      INCLUDE 'common/fcunit'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shared_fcex/RCS/fgetsg.f,v $
     . $',                                                             '
     .$Id: fgetsg.f,v 1.11 2005/08/01 15:55:12 hank Exp $
     . $' /
C    ===================================================================
C
C
      IF (ITRACE.GT.0) WRITE (IODBUG,*) 'ENTER FGETSG'
C


CHDH INITIALIZE THE THREE ARRAYS TO BE 0's.
CHDH Added by Hank on 2005-04-07 for bug r26-24.
CHDH The routine fcppts assumes that the first value after
CHDH the end of the TS array is 0.  It triggers off this to
CHDH know it has reached the end of the time series.  But,
CHDH since the arrays are not initialized, it doesn't work. 
      DO 10 I=1,MP
          P(I)=0
10    CONTINUE
      DO 11 I=1,MT
          T(I)=0
11    CONTINUE
      DO 12 I=1,MTS
          TS(I)=0
12    CONTINUE



      IBUG=IFBUG('FCIO')
C
      IOPNUMN=0
      CALL FSTWHR ('FGETSG  ',IOPNUMN,OLDOPN,IOLDOP)
C
      IF (IBUG.GT.0) WRITE (IODBUG,'(A,2A4)') ' IN FGETSG - ISEG=',ISEG
      CALL UMEMOV (ISEG,SEGOLD,2)
C
      IF (IBUG.GT.0) WRITE (IODBUG,'(1X,A,2A4)')
     *   ' ISEGID=',ISEGID
      IF (IBUG.GT.0) WRITE (IODBUG,*)
     *   ' ISEGRC=',ISEGRC,
     *   ' IOPT=',IOPT,
     *   ' NOPARM=',NOPARM,
     *   ' MP=',MP,
     *   ' MT=',MT,
     *   ' MTS=',MTS,
     *   ' '
C
      IER=0
C
      NNCOM=0
      IF (ISEGRC.LT.0) NNCOM=2
      ISEGRCA=IABS(ISEGRC)
C
C  IF NNCOM=0 FILL COMMON FCSEGN
C  IF NNCOM=2 FILL COMMON FCTSGN
      IF (NNCOM.EQ.0) IRSEG=ISEGRCA
      IF (NNCOM.EQ.2) KRSEG=ISEGRCA
C
C  CHECK ARGUMENTS
      IF (IOPT.EQ.0) GO TO 50
      IF (IOPT.EQ.1) GO TO 30
         WRITE (IPR,20) IOPT
20    FORMAT ('0**ERROR** IN FGETSG - VALUE OF IOPT (',I6,
     *   ') MUST BE 0 OR 1.')
         CALL ERROR
         IER=1
         GO TO 160
30    IF (ISEGRCA.GT.0.AND.ISEGRCA.LE.NRSTS) GO TO 70
         WRITE (IPR,40) ISEGRCA,NRSTS
40    FORMAT ('0**ERROR** IN FGETSG - RECORD NUMBER TO BE READ (',I6,
     *   ') IS GREATER THAN LAST USED IN FILE FCSEGSTS (',I6,').')
         CALL ERROR
         IER=1
         GO TO 160
C
C  FIND SEGMENT
50    IFOUND=0
      IF (NNCOM.EQ.0) THEN
         CALL FLOCSG (ISEGID,IRSEG)
         IF (IRSEG.GT.0) IFOUND=1
         ENDIF
      IF (NNCOM.EQ.2) THEN
         CALL FLOCSG (ISEGID,KRSEG)
         IF (KRSEG.GT.0) IFOUND=1
         ENDIF
      IF (IFOUND.EQ.0) THEN
         WRITE (IPR,60) ISEGID
60    FORMAT ('0**ERROR** IN FGETSG - SEGMENT ',2A4,' NOT FOUND.')
         CALL ERROR
         IER=1
         GO TO 160
         ENDIF
C
C  READ FILE SEGMENT STATUS
70    IF (IBUG.GT.0) WRITE (IODBUG,*)
     *   ' KFSGST=',KFSGST,
     *   ' IRSEG=',IRSEG,
     *   ' KRSEG=',KRSEG
      IF (NNCOM.EQ.0) CALL UREADT (KFSGST,IRSEG,IDSEGN,IERR)
      IF (NNCOM.EQ.2) CALL UREADT (KFSGST,KRSEG,KDSEGN,IERR)
      IF (IERR.GT.0) THEN
         WRITE (IPR,80)
80    FORMAT ('0**ERROR** IN FGETSG - READING RECORD ',I6,
     *   ' FROM UNIT ',I2,'.')
         CALL ERROR
         IER=1
         GO TO 160
         ENDIF
C
      IF (NNCOM.EQ.0) THEN
         CALL UMEMOV (IDSEGN,SEGID,2)
         CALL UMEMOV (IDSEGN,ISEG,2)
         IF (IOPT.EQ.1) CALL UMEMOV (IDSEGN,ISEGID,2)
         IF (IBUG.GT.0) CALL FCDMP1 (NNCOM,IRSEG)
         ENDIF
      IF (NNCOM.EQ.2) THEN
         CALL UMEMOV (KDSEGN,SEGID,2)
         CALL UMEMOV (KDSEGN,ISEG,2)
         IF (IOPT.EQ.1) CALL UMEMOV (KDSEGN,ISEGID,2)
         IF (IBUG.GT.0) CALL FCDMP1 (NNCOM,KRSEG)
         ENDIF
C
C  CHECK IF SEGMENT DEFINITION IS COMPLETE
      IF (NNCOM.EQ.0) ICOMPL=IDEFSG
      IF (NNCOM.EQ.2) ICOMPL=KDEFSG
      IF (ICOMPL.NE.0) THEN
         IF (ICOMPL.EQ.1) THEN
            IF (MAINUM.EQ.1) THEN
               WRITE (IPR,100) 'WARNING',SEGID
100   FORMAT ('0**',A,'** NO PARAMETERS DEFINED FOR SEGMENT ',A,'.')
               CALL WARN
               GO TO 160
               ENDIF
            WRITE (IPR,100) 'ERROR',SEGID
            CALL ERROR
            IER=1
            GO TO 160
            ENDIF
         IF (ICOMPL.EQ.2) THEN
            IF (MAINUM.EQ.1) THEN
CMGM  4/26/2002 Modified warning to make it more informative.
               WRITE (IPR,101) 'WARNING',SEGID
101   FORMAT ('0**',A,'** ONE OR MORE TIME SERIES NOT FOUND WHEN ',
     *   'SEGMENT ',A,' WAS DEFINED. SEGMENT SHOULD BE REDEFINED ',
     *   'OR DELETED.')
               CALL WARN
               GO TO 160
            ENDIF

CGUOXIAN******11/04/2002 Cancel the error message
C
C            WRITE (IPR,101) 'ERROR',SEGID
C            CALL ERROR
            WRITE (IPR,101) 'WARNING',SEGID
            CALL WARN
            IER=1
            GO TO 160
            ENDIF
         WRITE (IPR,102) ICOMPL,SEGID
102   FORMAT ('0**ERROR** COMPLETE STATUS INDICATOR (',I2,
     *   ') FOR SEGMENT ',A,' NOT RECOGNIZED.')
         CALL ERROR
         IER=1
         GO TO 160
         ENDIF
C
C  FILL COMMON FLARYS WITH VALUES FROM SEGMENT STATUS FILE
110   IF (NNCOM.EQ.0) THEN
         LTS=NTS
         LP=NP
         LC=NC
         LT=NT
         LD=ND
         ENDIF
      IF (NNCOM.EQ.2) THEN
         LTS=KNTS
         LP=KNP
         LC=KNC
         LT=KNT
         LD=KND
         ENDIF
C
C  CHECK IF TO READ PARAEMTERS
      IF (NOPARM.EQ.1) GO TO 160
C
      IF (LP.GT.MP.OR.LT.GT.MT.OR.LTS.GT.MTS) THEN
         WRITE (IPR,120) SEGID,LP,MP,LT,MT,LTS,MTS
120   FORMAT ('0**ERROR** IN FGETSG - ONE OR MORE ARRAYS TOO SMALL ',
     *      'FOR SEGMENT ',A,'.' /
     *   11X,'LP=',I6,' MP=',I6,
     *      ' LT=',I5,' MT=',I5,
     *      ' LTS=',I5,' MTS=',I5)
         CALL ERROR
         IER=2
         GO TO 160
         ENDIF
C
C  READ PARAMETERS
      IF (NNCOM.EQ.0) IRECP=IPREC
      IF (NNCOM.EQ.2) IRECP=KPREC
      IF (IBUG.GT.0) WRITE (IODBUG,*) 'IRECP=',IRECP
      CALL FCRDPF (IRECP,SEGID,P,LP,T,LT,TS,LTS,IERR)
      IF (IBUG.GT.0) THEN
         WRITE (IODBUG,*)
     *      'DUMP OF P, T AND TS ARRAYS AFTER FCRDPF:'
         CALL FDMPA ('P   ',P,MP)
         CALL FDMPT ('T   ',T,MT)
         CALL FDMPA ('TS  ',TS,MTS)
         ENDIF
      IF (IERR.NE.0) THEN
         WRITE (IPR,150) SEGID
150   FORMAT ('0**ERROR** IN FGETSG - READING PARAMETERS FOR SEGMENT ',
     *   A,'.')
         CALL ERROR
         IER=2
         ENDIF
C
160   IF (NNCOM.EQ.2) CALL UMEMOV (SEGOLD,ISEG,2)
      IF (IBUG.GT.0) WRITE (IODBUG,'(A,2A4)') ' IN FGETSG - ISEG=',ISEG
C
      CALL FSTWHR (OLDOPN,IOLDOP,OLDOPN,IOLDOP)
C
      IF (ITRACE.GT.0) WRITE (IODBUG,*) 'EXIT FGETSG'
C
      RETURN
C
      END
