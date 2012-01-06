C MEMBER WPPCOF
C  (from old member PPWPPCOF)
C-----------------------------------------------------------------------
C
C @PROCESS LVL(77)
C
      SUBROUTINE WPPCOF (ISTAT)
C
C          ROUTINE:  WPPCOF
C
C             VERSION:  1.0.1
C
C                DATE:  3-9-83
C                   CHANGES MADE 3-9-83 TO INCLUDE ADDITIONAL
C                   PARAMETER 'ORDR' AS A NEW COMPUTATIONAL PARM
C
C
C              AUTHOR:  JANINE FRANZOI
C                       DATA SCIENCES INC
C
C***********************************************************************
C
C          DESCRIPTION:
C
C  THIS ROUTINE CHECKS THE DATA TYPE DIRECTORY RECORDS FOR THE
C  COMPUTATIONAL ORDER PARAMETER TYPES. WHEN ONE OR MORE ARE FOUND IN
C  A FILE WITHOUT ANY OTHER TYPES PRESENT, THE PARAMETER RECORDS ARE
C  DELETED AND THE CONTROL RECORDS UPDATED.
C
C***********************************************************************
C
C          ARGUMENT LIST:
C
C         NAME    TYPE  I/O   DIM   DESCRIPTION
C
C         ISTAT     I    O     1     STATUS INDICATOR
C                                      0=ALL PARMS RESET
C                                      1=ONE OR MORE PARMS NOT RESET
C
C***********************************************************************
C
C          COMMON:
C
      INCLUDE 'uio'
      INCLUDE 'udebug'
      INCLUDE 'ucommon/uordrx'
      INCLUDE 'pppcommon/ppdtdr'
      INCLUDE 'pppcommon/ppmctl'
      INCLUDE 'pppcommon/ppxctl'
      INCLUDE 'urcommon/urppdt'
      INCLUDE 'urcommon/urppmc'
      INCLUDE 'urcommon/urxctl'
C
C***********************************************************************
C
C          DIMENSION AND TYPE DECLARATIONS:
C
      CHARACTER*8 ID
C
      DIMENSION IPARM(5),IDONE(5),NFILE(5)
      DIMENSION ARRAY(100)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/db_ppprw/RCS/wppcof.f,v $
     . $',                                                             '
     .$Id: wppcof.f,v 1.1 1995/09/17 18:45:20 dws Exp $
     . $' /
C    ===================================================================
C
C
C***********************************************************************
C
C          DATA:
C
      DATA IPARM/4HMPCO,4HMPFO,4HFMPO,4HMXCO,4HXGRD/
      DATA NPARM/5/
C
C***********************************************************************
C
C
      IF (IPPTR.GT.0) WRITE (IOGDB,130)
C
      ISTAT=0
C
      NRESET=0
      IPTRNX=0
      LARRAY=100
C
C  INITIALIZE ARRAYS
      CALL UMEMST (0,IDONE,NPARM)
      CALL UMEMST (0,NFILE,NPARM)
      ID=' '
C
C  SET NUMBER OF PARAMETER TYPES IN DIRECTORY
      IF (IAMORD.EQ.0) NTPARM=NMPTYP
      IF (IAMORD.EQ.1) NTPARM=NUMPTP
C
      JPARM=1
      IDONE(JPARM)=IPARM(JPARM)
C
C  SET FILE UNIT FOR FIRST PARAMETER
      IDX=IPCKDT(IPARM(JPARM))
      IF (IAMORD.EQ.0) IDF=IPDTDR(2,IDX)
      IF (IAMORD.EQ.1) IDF=JPDTDR(2,IDX)
C
C  CHECK DIRECTORY FOR COMPUTIONAL ORDER PARAMETER TYPES
10    NDONE=0
      DO 40 I=1,NTPARM
C     CHECK FILE NUMBERS
         IF (IAMORD.EQ.0.AND.IDF.NE.IPDTDR(2,I)) GO TO 40
         IF (IAMORD.EQ.1.AND.IDF.NE.JPDTDR(2,I)) GO TO 40
C     CHECK PARAMETERS
         IF (IAMORD.EQ.0.AND.IPDTDR(1,I).EQ.IPARM(JPARM)) GO TO 40
         IF (IAMORD.EQ.1.AND.JPDTDR(1,I).EQ.IPARM(JPARM)) GO TO 40
            IF (JPARM.EQ.NPARM) GO TO 40
            L=JPARM+1
            DO 20 K=L,NPARM
               IF (IAMORD.EQ.0.AND.IPDTDR(1,I).EQ.IPARM(K)) GO TO 30
               IF (IAMORD.EQ.1.AND.JPDTDR(1,I).EQ.IPARM(K)) GO TO 30
20             CONTINUE
C           NON COMPUTATIONAL ORDER PARAMETER TYPE FOUND
               WRITE (LP,140) IDF,IPARM(K)
               CALL SULINE (LP,2)
               NRESET=1
               GO TO 50
C        FOUND PARAMETER
30          IDONE(K)=IPARM(K)
            NDONE=NDONE+1
40       CONTINUE
C
C  SET FILE NUMBER FOR FILE WITH ONLY COMPUTATIONAL ORDER TYPES
      IF (NDONE.GT.0) NFILE(JPARM)=IDF
C
50    IF (IPPDB.GT.0) WRITE (IOGDB,150) IDONE,NFILE
C
C  GET FILE NUMBER FOR NEXT PARAMETER
55    IF (JPARM.GE.NPARM) GO TO 60
         JPARM=JPARM+1
         IF (IDONE(JPARM).NE.0) GO TO 55
            IDX=IPCKDT(IPARM(JPARM))
            IF (IAMORD.EQ.0) IDF=IPDTDR(2,IDX)
            IF (IAMORD.EQ.1) IDF=JPDTDR(2,IDX)
            GO TO 10
C
60    IF (IPPDB.GT.0) WRITE (IOGDB,150) IDONE,NFILE
C
C  DELETE ALL COMPUTATIONAL ORDER PARAMETER RECORDS
      DO 90 I=1,NPARM
         IPTR=0
         ID=' '
C     CHECK IF SINGLE RECORD PARAMETER TYPE
         IF (IAMORD.EQ.0.AND.IPMCTL(6,NFILE(I)).EQ.1) GO TO 80
         IF (IAMORD.EQ.1.AND.JPMCTL(6,NFILE(I)).EQ.1) GO TO 80
70       CALL RPPREC (ID,IPARM(I),IPTR,LARRAY,ARRAY,NFILL,IPTRNX,ISTAT)
         IF (ISTAT.NE.3.AND.ISTAT.NE.0) GO TO 90
         CALL WPPDEL (ID,IPARM(I),ISTAT)
         IF (IPTRNX.EQ.0) GO TO 90
            ID=' '
            IPTR=IPTRNX
            GO TO 70
80       CALL WPPDEL (ID,IPARM(I),ISTAT)
90       CONTINUE
C
      ISTAT=0
      IF (NRESET.GT.0) THEN
         ISTAT=1
         GO TO 125
         ENDIF
C
C  RESET THE FILES WITH ONLY COMPUTATIONAL PARAMETERS
      DO 120 J=1,NPARM
         IF (NFILE(J).GT.0) THEN
            IF (IAMORD.EQ.0) THEN
               IPMCTL(2,NFILE(J))=1
               IPMCTL(3,NFILE(J))=0
               ENDIF
            IF (IAMORD.EQ.1) THEN
               JPMCTL(2,NFILE(J))=1
               JPMCTL(3,NFILE(J))=0
               ENDIF
            DO 110 I=1,NTPARM
               IF (IAMORD.EQ.0) THEN
                  IF (IPDTDR(2,I).NE.NFILE(J)) GO TO 110
                  IPDTDR(3,I)=0
                  IPDTDR(4,I)=0
                  IPDTDR(5,I)=0
                  GO TO 110
                  ENDIF
               IF (IAMORD.EQ.1) THEN
                  IF (JPDTDR(2,I).NE.NFILE(J)) GO TO 110
                  JPDTDR(3,I)=0
                  JPDTDR(4,I)=0
                  JPDTDR(5,I)=0
                  ENDIF
110            CONTINUE
            ENDIF
120      CONTINUE
C
125   IF (IPPTR.GT.0) WRITE (IOGDB,160) ISTAT
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
130   FORMAT (' *** ENTER WPPCOF')
140   FORMAT ('0*** NOTE - PARAMETER FILE ',I2,' WITH PARAMETER TYPE ',
     *   A,' WILL NOT BE RESET TO EMPTY.')
150   FORMAT (' IDONE=',5(A5,1X),' NFILE=',5(I4,1X))
160   FORMAT (' *** EXIT WPPCOF : ISTAT=',I2)
C
      END
