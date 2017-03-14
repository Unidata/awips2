C MODULE URGTFC
C-----------------------------------------------------------------------
C
      SUBROUTINE URGTFC (LW1,IWORK1,LW2,IWORK2,LW3,IWORK3,
     *   MP,P,MT,T,MTS,TS,ISTAT)
C
C  THIS ROUTINE READS THE SEGMENTS THAT ARE NOT USED IN CARRYOVER
C  GROUPS AND COPIES ANY FORECAST COMPONENT TIME SERIES THAT HAVE
C  NOT BEEN COPIED TO THE NEW FILES.
C
C  ARGUMENT LIST:

C        NAME     TYPE  I/O   DIM    DESCRIPTION
C        ------   ----  ---   ---    -----------
C        LWORK1     I    I     1     LENGTH OF ARRAY IWORK1
C        IWORK1     I    I   LWORK1  ARRAY FOR TIME SERIES
C        LWORK2     I    I     1     LENGTH OF ARRAY IWORK2
C        IWORK2     I    I   LWORK2  ARRAY FOR FUTURE TIME SERIES
C        LWORK3     I    I     1     LENGTH OF ARRAY IWORK3
C        IWORK3     I    I   LWORK3  ARRAY FOR RRS PARAMETER RECORD
C        ISTAT      I    O      1    STATUS CODE:
C                                      0=NORMAL RETURN
C                                      OTHER=ERROR
C
      CHARACTER*8 SEGNAM,CGNAM
      DIMENSION P(MP),T(MT),TS(MTS)
      DIMENSION IWORK1(LW1),IWORK2(LW2),IWORK3(LW3)
      DIMENSION SEGID(2),ITSID(2)
      DIMENSION KFARRY(3),ISEGRY(65)
C
      INCLUDE 'uio'
      INCLUDE 'udebug'
      INCLUDE 'common/fcunit'
      INCLUDE 'common/fccgd'
      INCLUDE 'common/fccgd1'
      INCLUDE 'common/fcfgs'
      INCLUDE 'common/fcrunc'
      INCLUDE 'common/fcsegn'
      INCLUDE 'ucommon/uordrx'
      INCLUDE 'urcommon/urcdta'
      INCLUDE 'urcommon/urftbl'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/reorder/RCS/urgtfc.f,v $
     . $',                                                             '
     .$Id: urgtfc.f,v 1.6 2000/07/21 20:11:14 page Exp $
     . $' /
C    ===================================================================
C
C
      IF (IPRTR.GT.0) THEN
         CALL SULINE (IOGDB,1)
         WRITE (IOGDB,*) 'ENTER URGTFC'
         ENDIF
C
      CALL SULINE (LP,2)
      WRITE (LP,110)
C
      ISTAT=0
C
      NUMTS=0
      NUMSEG=0
C
C  GET NUMBER OF SEGMENT STATUS RECORDS
      CALL UREADT (KFSGPT,1,KFARRY,ISTAT)
      IF (ISTAT.GT.0) GO TO 80
      NSEG=KFARRY(2)
      IF (IPRDB.GT.0) THEN
         CALL SULINE (IOGDB,1)
         WRITE (IOGDB,*) 'NSEG=',NSEG
         ENDIF
C
C  CHECK IF ANY SEGMENTS GROUPS DEFINED
      IF (NSEG.EQ.0) THEN
         WRITE (LP,140)
         CALL SUWRNS (LP,2,-1)
         GO TO 90
         ENDIF
C
      IREC=3
C
C  PROCESS EACH SEGMENT
      DO 70 NSG=1,NSEG
         CALL UREADT (KFSGPT,IREC,KFARRY,ISTAT)
         IF (ISTAT.GT.0) GO TO 80
	 CALL UMEMOV (KFARRY(1),SEGNAM,2)
         IF (IPRDB.GT.0) THEN
            CALL SULINE (IOGDB,1)
            WRITE (IOGDB,*) 'SEGNAM=',SEGNAM,' KFARRY(3)=',KFARRY(3)
            ENDIF
         IF (SEGNAM.EQ.' ') GO TO 60
C     CHECK FOR OBSOLETE SEGMENT
         IF (SEGNAM.EQ.'OBSOLETE') GO TO 60
	 IF (KFARRY(3).EQ.0) THEN
            WRITE (LP,147) SEGNAM
            CALL SUERRS (LP,2,-1)
	    GO TO 60
	    ENDIF
         CALL UREADT (KFSGST,KFARRY(3),ISEGRY,ISTAT)
         IF (ISTAT.GT.0) GO TO 80
	 CALL UMEMOV (ISEGRY(21),CGNAM,2)
C     CHECK IF IN CARRYOVER GROUP
         IF (IPRDB.GT.0) THEN
            CALL SULINE (IOGDB,1)
            WRITE (IOGDB,*) CGNAM
            ENDIF
         IF (CGNAM.NE.' ') GO TO 60
         IOPT=1
         NOPARM=0
         CALL FGETSG (SEGID,KFARRY(3),MP,P,MT,T,MTS,TS,IOPT,NOPARM,IERR)
         IF (IERR.GT.0) THEN
            IF (IERR.EQ.1) THEN
               WRITE (LP,160)
               CALL SUERRS (LP,2,-1)
               ENDIF
            IF (IERR.EQ.2) THEN
               WRITE (LP,170) MP,MT,MTS
               CALL SUERRS (LP,2,-1)
               ENDIF
            IWURFL=1
            GO TO 90
            ENDIF
         NTPOS=1
         IUPDTE=0
C     GET TIME SERIES DATA TYPE
20       ITSTYP=TS(NTPOS)
         IF (ITSTYP.EQ.0) GO TO 50
         IF (ITSTYP.EQ.4) GO TO 40
C     GET TIME SERIES ID
         CALL UMEMOV (TS(NTPOS+12),ITSID,2)
         IF (IPRDB.GT.0) THEN
            CALL SULINE (IOGDB,1)
            WRITE (IOGDB,*) 'ITSTYP=',ITSTYP
            ENDIF
C     GET DATA TYPE CODE
         CALL UMEMOV (TS(NTPOS+14),IDTYPE,1)
C     CHECK DATA TYPE FOR 'FC'
         IAMORD=1
         CALL PFDTYP (IDTYPE,INDX)
         IF (INDX.EQ.0) GO TO 40
         IF (IDATFL(11,INDX).NE.1) GO TO 40
C     READ TIME SERIES RECORDS FOR THIS DATA TYPE AND WRITE TO NEW FILES
         IF (IPRDB.GT.0) THEN
            CALL SULINE (IOGDB,1)
            WRITE (IOGDB,180) ITSID,IDTYPE,TS(NTPOS+5),
     *         TS(NTPOS+6)
            ENDIF
         CALL URRDTS (ITSID,IDTYPE,LW1,IWORK1,LW2,IWORK2,LW3,IWORK3,
     *      NUMTS,LTSHDR,ISTAT)
         IF (ISTAT.GT.0) THEN
            WRITE (LP,185) IDSEGN
            CALL SUERRS (LP,2,-1)
            IWURFL=1
            ENDIF
         IF (IPRDB.GT.0) THEN
            CALL SULINE (IOGDB,1)
            WRITE (IOGDB,*)
     *         ' ITSTYP=',ITSTYP,
     *         ' LTSHDR=',LTSHDR,
     *         ' '
            ENDIF
C     CHECK IF UPDATE OR OUTPUT TIME SERIES
         IF (ITSTYP.EQ.2) GO TO 30
         IF (ITSTYP.EQ.3) GO TO 30
         GO TO 40
C     UPDATE TIME SERIES RECORD LOCATION
30       TS(NTPOS+15)=LTSHDR+.01
         IUPDTE=1
C     GET NEXT ID IN THIS SEGMENT
40       NTPOS=TS(NTPOS+1)
         GO TO 20
C     CHECK IF SEGMENT DEFINITION NEEDS TO BE WRITTEN
50       IF (IUPDTE.EQ.0) GO TO 60
            IADD=0
            NPLACE=1
            NNCOM=0
            NOPARM=0
            MSGE=0
            CALL FPUTSG (P,T,TS,IADD,NPLACE,NNCOM,NOPARM,MSGE,IERR)
            IF (IERR.EQ.0) NUMSEG=NUMSEG+1
            IF (IERR.GT.0) THEN
               WRITE (LP,190) IERR
               CALL SUERRS (LP,2,-1)
               ENDIF
60       IREC=IREC+1
70       CONTINUE
C
      CALL SULINE (LP,2)
      WRITE (LP,200) NUMTS
      CALL SULINE (LP,2)
      WRITE (LP,210) NUMSEG
      GO TO 90
C
C  FILE READ ERROR
80    WRITE (LP,120)
      CALL SUERRS (LP,2,-1)
      IWURFL=1
C
90    IF (IPRTR.GT.0) THEN
         WRITE (IOGDB,*) 'EXIT URGTFC'
         CALL SULINE (IOGDB,1)
         ENDIF
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
110   FORMAT ('0*** NOTE - BEGIN TO COPY  << FC TIME SERIES >>  ',
     *   'IN SEGMENTS BUT NOT IN CARRYOVER GROUPS.')
120   FORMAT ('0*** ERROR - IN URGTFC - FILE READ ERROR.')
140   FORMAT ('0*** WARNING - NO SEGMENTS DEFINED IN THE ',
     *   'FORECAST COMPONENT DATA BASE.')
147   FORMAT ('0*** ERROR - IN URGTFC - RECORD NUMBER FOR SEGMENT ',A,
     *   'IS ZERO.')
160   FORMAT ('0*** ERROR - IN URGTFC - ERROR ENCOUNTERED CALLING ',
     *   'ROUTINE FGETSG.')
170   FORMAT ('0*** ERROR - IN URGTFC - UNABLE TO DEFINE P, T, OR TS ',
     *   'ARRAY DUE TO LACK OF SPACE : MP=',I5,' MT=',I5,' MST=',I5)
180   FORMAT (' ITSID=',2A4,3X,'IDTYPE=',A4,3X,'TS(NTPOS+5)=',F5.0,3X,
     *   'TS(NTPOS+6)=',F5.0)
185   FORMAT ('0*** ERROR - IN URGTFC - PROCESSING TIME SERIES ',
     *   'FOR SEGMENT ',2A4,'.')
190   FORMAT ('0*** ERROR - IN URGTFC - FPUTSG NOT SUCCESSFULLY ',
     *   'CALLED. STATUS CODE=',I3)
200   FORMAT ('0*** NOTE - ',I4,' TIME SERIES HAVE BEEN SUCCESSFULLY ',
     *   'COPIED.')
210   FORMAT ('0*** NOTE - ',I4,' SEGMENTS HAVE BEEN SUCCESSFULLY ',
     *   'UPDATED.')
C
      END
