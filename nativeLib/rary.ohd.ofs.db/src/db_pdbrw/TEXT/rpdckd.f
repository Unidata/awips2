C MEMBER RPDCKD
C  (from old member PDBUTILS)
C-----------------------------------------------------------------------
C
C                             LAST UPDATE: 10/20/94.16:16:57 BY $WC21DT
C
C @PROCESS LVL(77)
C
      SUBROUTINE RPDCKD (NDLYTP,IDLYTP,IADD,IERROR,ISTAT)
C
C***********************************************************************
C
C          DESCRIPTION:
C
C    THIS ROUTINE CHECKS TO DETERMINE IF THERE IS ROOM ON THE
C    PPDB TO ADD A STATION TO EACH OF THE SPECIFIED DATA TYPES
C    THAT ARE STORED BY DAYS IN THE DAILY DATA FILES.
C
C***********************************************************************
C
C          ARGUMENT LIST:
C
C         NAME    TYPE  I/O   DIM   DESCRIPTION
C
C         NDLYTP   I     I     1     NUMBER OF DAILY DATA TYPES
C         IDLYTP   A4    I   NDLYTP  DATA TYPES
C         IADD     I     O   NDLYTP  SPACE INDICATOR
C                                      0=TYPE ADDED
C                                      1=SPACE NOT AVAILABLE
C         IERROR     I     O     60    ERROR INDICATOR
C                                      (I,1)=MAXIMUM STATION EXCEEDED
C                                      (I,2)=MAXIMUM STATION POINTER
C                                            WORD EXCEEDED
C                                      (I,3)=MAXIMUM DATA WORD EXCEEDED
C         ISTAT    I     O     1     STATUS INDICATOR
C                                      0=OK TO ADD ALL TYPES
C                                      1=SPACE NOT AVAILABLE FOR
C                                          ONE OR MORE TYPES
C                                      2=SPACE NOT AVAILABLE FOR ALL
C                                          TYPES
C
C***********************************************************************
C
C          COMMON:
C
      INCLUDE 'uio'
      INCLUDE 'udebug'
      INCLUDE 'pdbcommon/pddtdr'
      INCLUDE 'pdbcommon/pdsifc'
C
C***********************************************************************
C
C          DIMENSION AND TYPE DECLARATIONS:
C
      DIMENSION IDLYTP(1),IADD(1),IERROR(20,3)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/db_pdbrw/RCS/rpdckd.f,v $
     . $',                                                             '
     .$Id: rpdckd.f,v 1.1 1995/09/17 18:44:33 dws Exp $
     . $' /
C    ===================================================================
C
C
C***********************************************************************
C
C
      ISTAT=0
C
      NSET=0
      ISET=0
C
      IF (IPDTR.GT.0) WRITE (IOGDB,60)
C
      IF (NDLYTP.EQ.0) GO TO 50
C
C  PROCESS EACH TYPE
      DO 30 I=1,NDLYTP
C     CHECK IF TYPE IS VALID
         IX=IPDCKD(IDLYTP(I))
         IF (IX.NE.0) GO TO 10
            IF (IPDDB.GT.0) WRITE (LPE,70) IDLYTP(I)
            GO TO 30
10       MAXDAT=IDDTDR(21,IX)*LRCPDD*2
         NDATA=IDDTDR(6,IX)
         MXPNTR=IDDTDR(16,IX)*IDDTDR(5,IX)
         NMPNTR=IDDTDR(18,IX)
         NMDATA=IDDTDR(19,IX)
         NPNTR=IDDTDR(5,IX)
         IF (NDATA.LT.0) NDATA=NDATA*(-1)
         IF (IDDTDR(17,IX)+1.GT.IDDTDR(16,IX)) IERROR(I,1)=1
         IF (NMPNTR+NPNTR.GT.MXPNTR) IERROR(I,2)=1
         IF (NMDATA+NDATA.GT.MAXDAT) IERROR(I,3)=1
         IF (IERROR(I,1).EQ.1.OR.IERROR(I,2).EQ.1.OR.IERROR(I,3).EQ.1)
     *      GOTO 15
         GOTO 20
C        SPACE NOT AVAILABLE
15          NSET=NSET+1
            IADD(I)=1
            GO TO 30
C     SPACE AVAILABLE FOR THIS TYPE
20       ISET=ISET+1
         IADD(I)=0
30       CONTINUE
C
C  SET STATUS
      IF (ISET.EQ.NDLYTP) GO TO 50
      ISTAT=1
      IF (NSET.EQ.NDLYTP) GO TO 40
      GO TO 50
C
C  SPACE NOT AVAILABLE
40    ISTAT=2
C
50    IF (IPDTR.GT.0) WRITE (IOGDB,80) ISTAT
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
60    FORMAT (' *** ENTER RPDCKD')
70    FORMAT (' **ERROR** INVALID DATA TYPE : ',A4)
80    FORMAT (' *** EXIT RPDCKD - ISTAT=',I2)
C
      END
