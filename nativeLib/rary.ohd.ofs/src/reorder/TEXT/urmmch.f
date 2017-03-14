C MEMBER URMMCH
C-----------------------------------------------------------------------
C
C                             LAST UPDATE: 01/11/94.10:46:15 BY $WC20SV
C
C @PROCESS LVL(77)
C
      SUBROUTINE URMMCH (ITYPE,IARRAY,JARRAY,ISTAT)
C
C          SUBROUTINE:  URMMCH
C
C             VERSION:  1.0.0
C
C                DATE:  9-5-85
C
C              AUTHOR:  JANINE FRANZOI
C                       DATA SCIENCES INC
C                       8555 16TH ST, SILVER SPRING, MD 587-3700
C***********************************************************************
C
C          DESCRIPTION:
C
C    THIS ROUTINE COPIES THE SPECIAL PARAMETER RECORDS TO THE NEW
C    FILES WHEN THE ACTUAL STATIONS IS LESS THAN THE MAXIMUM NUMBER
C    STATION ON THE NEW SET OF FILES.
C
C***********************************************************************
C
C          ARGUMENT LIST:
C
C         NAME    TYPE  I/O   DIM   DESCRIPTION
C
C       ITYPE     A4     I     1     PARAMETER TYPE
C       IARRAY    I      I     ?     OLD FILE CONTROL RECORD
C       JARRAY    I      I     ?     NEW FILE CONTROL RECORD
C       ISTAT     I      O     1     STATUS CODE
C                                      0=NORMAL RETURN
C                                      OTHER=ERROR
C
C***********************************************************************
C
C          COMMON:
C
      INCLUDE 'uio'
      INCLUDE 'udebug'
      INCLUDE 'ucommon/uordrx'
      INCLUDE 'urcommon/urunts'
      INCLUDE 'urcommon/urppdt'
      INCLUDE 'urcommon/urcdta'
      INCLUDE 'pppcommon/pppdta'
      INCLUDE 'pppcommon/ppunts'
      INCLUDE 'pppcommon/ppdtdr'
C
C***********************************************************************
C
C          DIMENSION AND TYPE DECLARATIONS:
C
      CHARACTER*4 XDISP
C
      DIMENSION IARRAY(*),JARRAY(*)
      DIMENSION IWORK(16)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/reorder/RCS/urmmch.f,v $
     . $',                                                             '
     .$Id: urmmch.f,v 1.1 1995/09/17 19:17:54 dws Exp $
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
      IF (IPPTR.GT.0) WRITE (IOGDB,110)
C
      ISTAT=0
      LRCP=LRECPP*2
C
C  GET SUBSCRIPT OF TYPE IN OLD FILES
      IAMORD=0
      IDXOLD=IPCKDT(ITYPE)
      XDISP='OLD'
      IF (IDXOLD.EQ.0) GO TO 80
C
C  GET SUBSCRIPT OF TYPE IN NEW FILES
      IAMORD=1
      IDXNEW=IPCKDT(ITYPE)
      IAMORD=0
      XDISP='NEW'
      IF (IDXNEW.EQ.0) GO TO 80
C
C  GET FILE NUMBER
      IDFOLD=IPDTDR(2,IDXOLD)
      IDFNEW=JPDTDR(2,IDXNEW)
C
C  CHECK IF ANY STATION DEFINED
      IF (IARRAY(9).GT.0) GO TO 10
         CALL SULINE (LP,2)
         WRITE (LP,120) ITYPE
         GO TO 100
C
C  CALCULATE NUMBER OF RECORDS FROM OLD FILE
10    NRECS=IUNRCD(IARRAY(9),LRCP)
      IF (IPPDB.GT.0) CALL SULINE (IOGDB,1)
      IF (IPPDB.GT.0) WRITE (IOGDB,130) ITYPE,NRECS
      NUMMO=IARRAY(6)*IARRAY(4)
      IF (IPPDB.GT.0) CALL SULINE (IOGDB,1)
      IF (IPPDB.GT.0) WRITE (IOGDB,140) NUMMO
C
C  GET STARTING RECORD FROM OLD AND NEW CONTROLS
      IFREC=IARRAY(2)
      JFREC=JARRAY(2)
      IREC=IFREC
      JREC=JFREC
      IF (IPPDB.GT.0) CALL SULINE (IOGDB,1)
      IF (IPPDB.GT.0) WRITE (IOGDB,150) IFREC,JFREC
C
C  COPY RECORDS
      NCOPY=0
      DO 30 I=1,NUMMO
         IF (IPPDB.GT.0) CALL SULINE (IOGDB,1)
         IF (IPPDB.GT.0) WRITE (IOGDB,160) IREC,JREC
         DO 20 J=1,NRECS
            CALL UREADT (KPPRMU(IDFOLD),IREC,IWORK,ISTAT)
            IF (ISTAT.NE.0) GO TO 40
            CALL UWRITT (KUPRMI(IDFNEW),JREC,IWORK,ISTAT)
            IF (ISTAT.NE.0) GO TO 60
            NCOPY=NCOPY+1
            IREC=IREC+1
            JREC=JREC+1
20          CONTINUE
C     INCREMENT TO NEXT MONTH AND COPY RECORDS FOR THAT ONE
         IREC=IFREC+IARRAY(3)*I
         JREC=JFREC+JARRAY(3)*I
30       CONTINUE
C
C  SET NEW CONTROLS TO NUMBER OF STATIONS AND LAST STATION SLOTS
      JARRAY(7)=IARRAY(7)
      JARRAY(9)=IARRAY(9)
      NCREC=JPDTDR(3,IDXNEW)
      CALL UWRITT (KUPRMI(IDFNEW),NCREC,JARRAY,ISTAT)
      IF (ISTAT.NE.0) GO TO 60
      NCOPY=NCOPY+1
C
      CALL SULINE (LP,2)
      WRITE (LP,170) NCOPY,ITYPE
      GO TO 100
C
40    WRITE (LP,50) ITYPE,IREC
      CALL SUERRS (LP,2,-1)
50    FORMAT ('0*** ERROR - IN URMMCH - READING PARAMETER TYPE ',A4,
     *   ' RECORD ',I4,'.')
      IWURFL=1
      GO TO 100
C
60    WRITE (LP,70) ITYPE,JREC
      CALL SUERRS (LP,2,-1)
70    FORMAT ('0*** ERROR - IN URMMCH - WRITING PARAMETER TYPE ',A4,
     *   ' RECORD ',I4,'.')
      IWURFL=1
      GO TO 100
C
80    WRITE (LP,90) ITYPE,XDISP
      CALL SUERRS (LP,2,-1)
90    FORMAT ('0*** ERROR - IN URMMCH - PARAMETER TYPE ',A4,
     *   ' NOT FOUND IN ',A4,' PARAMETRIC DATA BASE DIRECTORY.')
      IWURFL=1
C
100   IF (IPPTR.GT.0) CALL SULINE (IOGDB,1)
      IF (IPPTR.GT.0) WRITE (IOGDB,180)
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
110   FORMAT (' *** ENTER URMMCH')
120   FORMAT ('0*** NOTE - NO ENTRIES DEFINED FOR PARAMETER TYPE ',A4,
     *   '.')
130   FORMAT (' ITYPE=',A4,' NRECS=',I6)
140   FORMAT (' NUMMO=',I4)
150   FORMAT (' IFREC=',I6,3X,'JFREC=',I6)
160   FORMAT (' IREC=',I6,3X,'JREC=',I6)
170   FORMAT ('0*** NOTE - ',I4,1X,A4,' RECORDS SUCCESSFULLY COPIED.')
180   FORMAT (' *** EXIT URMMCH')
C
      END
