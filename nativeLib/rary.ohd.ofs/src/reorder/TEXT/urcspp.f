C MEMBER URCSPP
C-----------------------------------------------------------------------
C
C                             LAST UPDATE: 01/11/94.10:44:06 BY $WC20SV
C
C @PROCESS LVL(77)
C
       SUBROUTINE URCSPP (ISTAT)
C
C          SUBROUTINE:  URCSPP
C
C             VERSION:  1.0.0
C
C                DATE:  7-12-84
C
C              AUTHOR:  JANINE FRANZOI
C                       DATA SCIENCES INC
C                       8555 16TH ST, SILVER SPRING, MD 587-3700
C***********************************************************************
C
C          DESCRIPTION:
C
C    THIS ROUTINE WILL COPY THE CONTROL AND PARAMETER RECORDS FOR THE
C    SPECIAL PARAMETER TYPES.
C    THE MAXIMUM NUMBER OF ENTRIES CAN BE CHANGED IN THE NEW FILE.
C
C***********************************************************************
C
C          ARGUMENT LIST:
C
C         NAME    TYPE  I/O   DIM   DESCRIPTION
C
C       ISTAT      I    O     1      STATUS CODE
C                                     0=NORMAL RETURN
C                                     1=FILE UNIT OUT OF RANGE
C                                     2=NOT ENOUGH SPACE AVAILABLE
C                                     OTHER=SYSTEM ERROR
C
C***********************************************************************
C
C          COMMON:
C
      INCLUDE 'uio'
      INCLUDE 'udebug'
      INCLUDE 'ucommon/uordrx'
      INCLUDE 'pppcommon/ppunts'
      INCLUDE 'pppcommon/pppdta'
      INCLUDE 'pppcommon/ppdtdr'
      INCLUDE 'pppcommon/ppxctl'
      INCLUDE 'urcommon/urunts'
      INCLUDE 'urcommon/urxctl'
      INCLUDE 'urcommon/urcdta'
      INCLUDE 'urcommon/urppdt'
C
C***********************************************************************
C
C          DIMENSION AND TYPE DECLARATIONS:
C
      CHARACTER*4 ITYPES(2)/'CHAR','MMMT'/
C
      DIMENSION IARRAY(16),JARRAY(16),IWORK(16)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/reorder/RCS/urcspp.f,v $
     . $',                                                             '
     .$Id: urcspp.f,v 1.1 1995/09/17 19:17:17 dws Exp $
     . $' /
C    ===================================================================
C
C
C***********************************************************************
C
C          DATA:
C
C
C***********************************************************************
C
C
      IF (IPPTR.GT.0) CALL SULINE (IOGDB,1)
      IF (IPPTR.GT.0) WRITE (IOGDB,170)
C
      ISTAT=0
C
      NTYPES=2
C
C  FIND FILE NUMBER AND INDEX OF SPECIAL TYPE IN OLD FILES
      DO 110 I=1,NTYPES
         CALL SULINE (LP,2)
         WRITE (LP,180) ITYPES(I)
         CALL UMEMST (0,IARRAY,LRECPP)
         CALL UMEMST (0,JARRAY,LRECPP)
         IAMORD=0
         IDXOLD=IPCKDT(ITYPES(I))
         IF (IDXOLD.GT.0) GO TO 10
            CALL SULINE (LP,2)
            WRITE (LP,300) ITYPES(I)
            GO TO 110
10       IDFOLD=IPDTDR(2,IDXOLD)
         IF (IDFOLD.LE.NMPFIL) GO TO 20
            WRITE (LP,190) KPPRMU(IDFOLD),ITYPES(I)
            CALL SUERRS (LP,2,-1)
            ISTAT=1
            GO TO 140
C     READ SPECIAL PARAMETER CONTROL RECORD FROM ORIGINAL FILE
20       IROLD=IPDTDR(3,IDXOLD)
         IF (IROLD.GT.0) GO TO 25
            CALL SULINE (LP,2)
            WRITE (LP,195) ITYPES(I)
            GO TO 110
25       CALL UREADT (KPPRMU(IDFOLD),IROLD,IARRAY,ISTAT)
         IF (ISTAT.NE.0) GO TO 120
         IF (IPPDB.GT.0) CALL SULINE (IOGDB,1)
         IF (IPPDB.GT.0)
     *      WRITE (IOGDB,200) IROLD,(IARRAY(J),J=1,9),KPPRMU(IDFOLD)
C     CALCULATE NUMBER OF RECORDS TO BE COPIED TO NEW FILE
         NSPREC=IARRAY(3)*IARRAY(4)*IARRAY(6)
         IF (IPPDB.GT.0) CALL SULINE (IOGDB,1)
         IF (IPPDB.GT.0) WRITE (IOGDB,210) NSPREC
         IAMORD=1
         IDXNEW=IPCKDT(ITYPES(I))
         IF (IDXNEW.GT.0) GO TO 30
            WRITE (LP,310) ITYPES(I)
            CALL SUERRS (LP,2,-1)
            GO TO 110
C     READ CONTROL FROM NEW FILE
30       IDFNEW=JPDTDR(2,IDXNEW)
         IF (IDFNEW.LE.NUMPFL) GO TO 40
            WRITE (LP,190) KUPRMI(IDFNEW),ITYPES(I)
            ISTAT=1
            GO TO 140
40       IRNEW=JPDTDR(3,IDXNEW)
         IF (IPPDB.GT.0) CALL SULINE (IOGDB,1)
         IF (IPPDB.GT.0) WRITE (IOGDB,215) IDXNEW,IRNEW,KUPRMI(IDFNEW)
         CALL UREADT (KUPRMI(IDFNEW),IRNEW,JARRAY,ISTAT)
         IF (ISTAT.NE.0) GO TO 120
         IF (IPPDB.GT.0) CALL SULINE (IOGDB,1)
         IF (IPPDB.GT.0) WRITE (IOGDB,220) IRNEW,(JARRAY(J),
     *      J=1,9),KUPRMI(IDFNEW)
C     CHECK IF MAX NUMBER OF STATIONS ON OLD FILE EXCEEDS MAX NUMBER ON
C     NEW FILES
         IF (JARRAY(8).EQ.IARRAY(8)) GO TO 60
C     MAX NUMBER IS DIFFERENT NOW SEE IF ACTUAL NUMBER IS LESS THAN MAX
         IF (IARRAY(9).LT.JARRAY(8)) GO TO 50
            WRITE (LP,230) ITYPES(I)
            CALL SUERRS (LP,2,-1)
            GO TO 110
C     READ PARAMETERS
50       CALL URMMCH (ITYPES(I),IARRAY,JARRAY,ISTAT)
         IF (ISTAT.NE.0) GO TO 150
         GO TO 110
C     CALCULATE NUMBER OF RECORD SLOTS AVAILABLE IN NEW FILES
60       NEWR=JARRAY(3)*JARRAY(4)*JARRAY(6)
         IF (IPPDB.GT.0) CALL SULINE (IOGDB,1)
         IF (IPPDB.GT.0) WRITE (IOGDB,240) NEWR
C     SEE IF THE NUMBER OF RECORDS TO BE COPIED IS THE SAME FOR BOTH
C     FILES. IF NOT, CHECK WORD 3 OF CONTROL RECORD FOR BOTH
C     FILES. DUE TO INITIALIZATION ERROR WORD 3 IS INCORRECT IN THE
C     OLD CONTROL RECORD AND ADJUSTMENTS WILL BE MADE.
         IF (NEWR.EQ.NSPREC) GO TO 70
         IF (IARRAY(3).NE.JARRAY(3)) GO TO 70
            WRITE (LP,250) ITYPES(I)
            CALL SUERRS (LP,2,-1)
            IWURFL=1
            ISTAT=2
            GO TO 110
C     FIND WHERE THE PARAMETER RECORDS BEGIN IN OLD FILE. DUE TO ERROR
C     IN THE OLD CONTROL RECORD. THE NUMBER OF RECORDS COPIED WILL BE
C     INCREMENTED BY WORD 3 OF THE NEW CONTROL RECORD. BUT THE NUMBER OF
C     RECORDS READ INCREMENTED BY WORD 3 OF THE OLD FILE.
70       IREC=IROLD+1
         JREC=IRNEW+1
C     DO REST OF RECORDS
         NUMREC=JARRAY(4)*JARRAY(6)
         NUMR=JARRAY(3)
         IF (IPPDB.GT.0) CALL SULINE (IOGDB,1)
         IF (IPPDB.GT.0) WRITE (IOGDB,260) NEWR,NUMR,NUMREC,
     *      IREC,JREC
         DO 100 II=1,NUMREC
            DO 80 K=1,NUMR
               CALL UREADT (KPPRMU(IDFOLD),IREC,IWORK,ISTAT)
               IF (ISTAT.NE.0) GO TO 120
               CALL UWRITT (KUPRMI(IDFNEW),JREC,IWORK,ISTAT)
               IF (ISTAT.NE.0) GO TO 120
               IF (IPPDB.GT.0) CALL SULINE (IOGDB,1)
               IF (IPPDB.GT.0) WRITE (IOGDB,270) IREC,JREC
               IREC=IREC+1
               JREC=JREC+1
80             CONTINUE
            IF (IARRAY(3).NE.NUMR) GO TO 90
               GO TO 100
90          IREC=IREC+(IARRAY(3)-NUMR)
100         CONTINUE
C     UPDATE CONTROL AND WRITE IT
         JARRAY(7)=IARRAY(7)
         JARRAY(9)=IARRAY(9)
         CALL UWRITT (KUPRMI(IDFNEW),IRNEW,JARRAY,ISTAT)
         IF (ISTAT.NE.0) GO TO 120
         IF (IPPDB.GT.0) CALL SULINE (IOGDB,1)
         IF (IPPDB.GT.0)
     *      WRITE (IOGDB,280) IRNEW,(JARRAY(J),J=1,9),IRNEW
         NEWR=NEWR+1
         CALL SULINE (LP,2)
         WRITE (LP,290) NEWR,ITYPES(I)
110      CONTINUE
      GO TO 150
C
120   WRITE (LPE,130) ISTAT
      CALL SUERRS (LP,2,-1)
130   FORMAT ('0*** ERROR - IN URCSPP - DAIO READ OR WRITE ERROR. ',
     *   'ISTAT=',I2)
C
C  SET ERROR FLAG
140   IWURFL=1
C
150   IF (IPPTR.GT.0) CALL SULINE (IOGDB,1)
      IF (IPPTR.GT.0) WRITE (IOGDB,320)
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
170   FORMAT (' *** ENTER URCSPP')
180   FORMAT ('0*** NOTE - BEGIN TO COPY  << SPECIAL PARAMETER TYPE ',
     *    A4,' >>.')
190   FORMAT ('0*** ERROR - IN URCSPP - FILE ',I2,' OUT OF RANGE FOR ',
     *   'PARAMETER TYPE ',A4,'.')
195   FORMAT ('0*** NOTE - NO ',A4,' RECORDS DEFINED.')
200   FORMAT (' OLD CONTROL RECORD ',I2,'=',4I4,A4,4I4,' UNIT=',I4)
210   FORMAT (' NSPREC=',I4)
215   FORMAT (' IDXNEW=',I2,3X,'IRNEW=',I6,3X,
     *   'KUPRMI(IDFNEW)=',I2)
220   FORMAT (' NEW CONTROL RECORD ',I2,'=',4I4,A4,4I4,' UNIT=',I4)
230   FORMAT ('0*** ERROR - IN URCSPP - NUMBER OF STATIONS ON OLD ',
     *  'FILES EXCEEDS MAXIMUM NUMBER ON NEW FILES FOR TYPE ',A4,'.')
240   FORMAT (' NEWR=',I4)
250   FORMAT ('0*** ERROR - IN URCSPP - SPECIAL PARAMETER TYPE ',A4,
     *   ' WILL NOT BE COPIED AT THIS TIME. FILES ARE NOT EQUAL ',
     *   'IN SIZE.')
260   FORMAT (' NEWR=',I6,3X,'NUMR=',I6,3X,'NUMREC=',I6,3X,
     *   'IREC=',I6,3X,'JREC=',I6)
270   FORMAT (' READ RECORD ',I6,' WRITE RECORD ',I6)
280   FORMAT (' NEW CONTROL RECORD AFTER UPDATE ',I2,'=',4I4,A4,4I4,
     *    ' AT SLOT ',I4)
290   FORMAT ('0*** NOTE - ',I4,1X,A4,' RECORDS SUCCESSFULLY COPIED.')
300   FORMAT ('0*** NOTE - SPECIAL PARAMETER TYPE ',A4,' NOT FOUND ',
     *   'IN OLD PREPROCESSOR PARAMETRIC DATA BASE.')
310   FORMAT ('0*** ERROR - SPECIAL PARAMETER TYPE ',A4,' NOT FOUND ',
     *   'IN NEW PREPROCESSOR PARAMETRIC DATA BASE.')
320   FORMAT (' *** EXIT URCSPP')
C
      END
