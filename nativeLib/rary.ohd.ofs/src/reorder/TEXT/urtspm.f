C MEMBER URTSPM
C-----------------------------------------------------------------------
C
C                             LAST UPDATE: 12/07/93.14:08:39 BY $WC21DT
C
       SUBROUTINE URTSPM (ITSID,ISTRT,ITYPE,LIARRY,IARRY,ISTAT)
C
C          ROUTINE:  URTSPM
C
C  7/8/86 TASK 282 UPDATES, PASS WORK ARRAY
C  1/13/86 CHANGE ERROR IF PARM NOT FOUND TO WARNING
C
C             VERSION:  1.0.0
C
C                DATE:  9-02-85
C
C              AUTHOR:  JANINE FRANZOI
C                       DATA SCIENCES INC
C                       8555 16TH ST, SILVER SPRING, MD 587-3700
C***********************************************************************
C
C          DESCRIPTION:
C
C    THIS ROUTINE WILL READ THE RRS PARAMETER RECORD AND SEARCH FOR
C    A RRS DATA TYPE WITHIN THAT RECORD. IF THE TYPE IS FOUND, THE
C    RECORD NUMBER OF THE TIME SERIES IS UPDATED IN THE RRS
C    PARAMETER RECORD AND THE PARAMETER RECORD IS WRITTEN BACK TO
C    THE NEW FILE.
C
C***********************************************************************
C
C          ARGUMENT LIST:
C
C         NAME    TYPE  I/O   DIM    DESCRIPTION
C
C       ITSID      A     I     2     TIME SERIES IDENTIFIER
C       ISTRT      I     I     1     TIME SERIES RECORD NUMBER
C       ITYPE      A     I     1     RRS DATA TYPE
C       LIARRY     I     I     1     LENGTH OF IARRY IARAY
C       IARRY      I     IO  LIARAY  WORK IARRY FOR RRS PARM(NEW)
C       ISTAT      I     O     1     STATUS CODE
C                                     0=NORMAL RETURN
C
C***********************************************************************
C
C          COMMON:
C
      INCLUDE 'uio'
      INCLUDE 'udebug'
      INCLUDE 'pdbcommon/pdtrrx'
      INCLUDE 'urcommon/urcdta'
      INCLUDE 'ucommon/uordrx'
C
C***********************************************************************
C
C          DIMENSION AND TYPE DECLARATIONS:
C
      DIMENSION IARRY(LIARRY)
      DIMENSION ITSID(2)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/reorder/RCS/urtspm.f,v $
     . $',                                                             '
     .$Id: urtspm.f,v 1.1 1995/09/17 19:18:04 dws Exp $
     . $' /
C    ===================================================================
C
C
C***********************************************************************
C
C          DATA:
C
      DATA XRRS/4hRRS /
      DATA ISAME/4hSAME/
C
C***********************************************************************
C
C  DEBUG
C
      IF (IPRTR.GT.0) CALL SULINE (IOGDB,1)
      IF (IPRTR.GT.0) WRITE (IOGDB,120)
C
      ISTAT=0
      IPTR=0
C
C  GET RRS PARAMETER RECORD FROM NEW FILES
      IAMORD=1
      CALL RPPREC (ITSID,XRRS,IPTR,LIARRY,IARRY,NFILL,IPTRNX,IERR)
      IF (IERR.EQ.0) GO TO 30
         IF (IERR.NE.2) GO TO 10
            WRITE (LP,130) XRRS,ITSID,ITYPE
            CALL SUWRNS (LP,2,-1)
            GO TO 110
10       CALL SRPPST (ITSID,XRRS,IPTR,LIARAY,NFILL,IPTRNX,IERR)
         GO TO 110
C
C  SET NUMBER OF RRS TYPES
30    CALL UMEMOV (IARRY(12),REAL,1)
      NRTYPE=REAL
      IF (IPRDB.GT.0) CALL SULINE (IOGDB,1)
      IF (IPRDB.GT.0) WRITE (IOGDB,150) ITYPE,NRTYPE
C
C  FIND DATA TYPE IN RRS PARAMETER RECORD TO MATCH DATA TYPE IN TIME
C  SERIES
      IPOS1=15
      DO 90 I=1,NRTYPE
         IXTYPE=IARRY(IPOS1)
C     CHECK IF TIMES SERIES CAN HAVE MISSING DATA
         IF (IARRY(IPOS1+NRTYPE).EQ.ISAME) GO TO 70
C        FIND DATA TYPE IN RRS TYPES COMMON BLOCK
            DO 40 N=1,NTYPE
               IF (IXTYPE.EQ.IRTYPE(N)) GO TO 50
40             CONTINUE
               WRITE (LP,140) IXTYPE
               CALL SUERRS (LP,2,-1)
               ISTAT=1
               GO TO 110
50          IF (IPRDB.GT.0) CALL SULINE (IOGDB,1)
            IF (IPRDB.GT.0) WRITE (IOGDB,190) IXTYPE,N,IUMISS(N)
C        CHECK IF MISSING DATA ALLOWED FOR THIS TYPE
            IF (IUMISS(N).NE.ISAME) GO TO 60
               WRITE (LP,170) IXTYPE,IUMISS(N)
               CALL SUWRNS (LP,2,-1)
60          IXTYPE=IARRY(IPOS1+NRTYPE)
            IF (IPRDB.GT.0) CALL SULINE (IOGDB,1)
            IF (IPRDB.GT.0) WRITE (IOGDB,180) IPOS1,IXTYPE
70       IF (IPRDB.GT.0) CALL SULINE (IOGDB,1)
         IF (IPRDB.GT.0) WRITE (IOGDB,160) IARRY(IPOS1),IARRY(2),
     *      IARRY(3),IXTYPE
         IF (ITYPE.NE.IXTYPE) GO TO 80
C        GET POSITION OF TIME SERIES RECORD NUMBER
            IPOS2=IPOS1+NRTYPE*6
            IF (IPRDB.GT.0) CALL SULINE (IOGDB,1)
            IF (IPRDB.GT.0) WRITE (IOGDB,200) IPOS2
            REAL=ISTRT+.01
            CALL UMEMOV (REAL,IARRY(IPOS2),1)
            GO TO 100
80       IPOS1=IPOS1+1
90       CONTINUE
      WRITE (LP,210) ITSID,ITYPE
      CALL SUWRNS (LP,2,-1)
      GO TO 110
C
C  REWRITE PARAMETER RECORD BACK TO SAME SLOT ON FILE
100   IF (IPRDB.GT.0) CALL SULINE (IOGDB,1)
      IF (IPRDB.GT.0) WRITE (IOGDB,220) IPTR
      IAMORD=1
      CALL WPPREC (ITSID,XRRS,NFILL,IARRY,IPTR,IERR)
      IF (IERR.EQ.0) GO TO 110
      CALL SWPPST (ITSID,XRRS,NFILL,IPTR,IERR)
      WRITE (LP,230) XRRS,ITSID,IERR
      CALL SUERRS (LP,2,-1)
      ISTAT=1
C
110   IF (IPRTR.GT.0) CALL SULINE (IOGDB,1)
      IF (IPRTR.GT.0) WRITE (IOGDB,240)
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
120   FORMAT (' *** ENTER URTSPM')
130   FORMAT ('0*** WARNING - IN URTSPM - ',A4,' PARAMETER RECORD ',
     *   'FOR IDENTIFIER ',2A4,' NOT FOUND. TIME SERIES DATA TYPE IS ',
     *   A4,'.')
140   FORMAT ('0*** ERROR - IN URTSPM - DATA TYPE ',A4,
     *   'NOT FOUND IN COMMON BLOCK PDTRRX.')
150   FORMAT (' ITYPE=',A4,3X,'NRTYPE=',I2)
160   FORMAT (' TYPE IN RECORD=',A4,' IDENTIFIER=',2A4,3X,
     *   'IXTYPE=',A4)
170   FORMAT ('0*** WARNING - IN URTSPM - DATA TYPE ',A4,
     *   'HAS A MISSING INDICATOR OF ',A4,'.')
180   FORMAT (' IPOS1=',I3,3X,'IXTYPE=',A4)
190   FORMAT (' IXTYPE=',A4,3X,'N=',I2,3X,'IUMISS(N)=',A4)
200   FORMAT (' IPOS2=',I6)
210   FORMAT ('0*** WARNING - IN URTSPM - RRS  PARAMETER RECORD FOR ',
     *   'IDENTIFIER ',2A4,' DOES NOT CONTAIN DATA TYPE ',A4,' BUT A ',
     *   'TIME SERIES EXISTS.')
220   FORMAT (' IPTR=',I5)
230   FORMAT ('0*** ERROR - IN URTSPM - WRITING ',A4,' PARAMETER ',
     *   'RECORD FOR IDENTIFIER ',2A4,'. WPPREC STATUS CODE=',I3,'.')
240   FORMAT (' *** EXIT URTSPM')
C
      END
