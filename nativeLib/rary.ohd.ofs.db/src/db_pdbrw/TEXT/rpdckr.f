C MEMBER RPDCKR
C  (from old member PDBUTILS)
C-----------------------------------------------------------------------
C
C                             LAST UPDATE: 10/20/94.16:16:57 BY $WC21DT
C
C @PROCESS LVL(77)
C
      SUBROUTINE RPDCKR (NRRSTP,IRRSTP,NVAL,NUMOBV,IADDR,ISTAT)
C
C***********************************************************************
C
C          DESCRIPTION:
C
C    THIS ROUTINE CHECKS TO DETERMINE IF THERE IS ROOM ON THE
C    PREPROCESSOR DATA BASE TO ADD A STATION FOR EACH OF THE
C    SPECIFIED RRS DATA TYPES.
C
C***********************************************************************
C
C          ARGUMENT LIST:
C
C         NAME    TYPE  I/O   DIM   DESCRIPTION
C
C       NRRSTP     I     I     1     NUMBER OF RRS DATA TYPES
C       IRRSTP     A4    I   NRRSTP  RRS DATA TYPES
C       NVAL       I     I   NRRSTP  NUMBER OF VALUES PER OBSERVATION
C       NUMOBV     I     I   NRRSTP  NUMBER OF OBSERVATIONS FOR EACH
C                                    TYPE
C       IADDR      I     O   NRRSTP  SPACE INDICATOR
C                                      0=SPACE AVAILABLE ON PPDB
C                                      1=SPACE NOT AVAILABLE
C       ISTAT     I     O     1      STATUS INDICATOR
C                                      0=OK TO ADD ALL TYPES
C                                      1=SPACE NOT AVAILABLE FOR 1
C                                           OR MORE TYPES
C                                      2=SPACE NOT AVAILABLE FOR
C                                           ALL TYPES
C
C
C***********************************************************************
C
C          COMMON:
C
      INCLUDE 'uio'
      INCLUDE 'udebug'
      INCLUDE 'pdbcommon/pdbdta'
      INCLUDE 'pdbcommon/pdrrsc'
      INCLUDE 'pdbcommon/pdsifc'
C
C***********************************************************************
C
C          DIMENSION AND TYPE DECLARATIONS:
C
      DIMENSION IRRSTP(1),NVAL(1),NUMOBV(1),IADDR(1)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/db_pdbrw/RCS/rpdckr.f,v $
     . $',                                                             '
     .$Id: rpdckr.f,v 1.1 1995/09/17 18:44:34 dws Exp $
     . $' /
C    ===================================================================
C
C
C***********************************************************************
C
      ISTAT=0
      NSET=0
      ISET=0
C
      IF (IPDTR.GT.0) WRITE (IOGDB,50)
C
C  SET LAST USED RECORD
      LSTREC=LXRRSR
C
      IF (NRRSTP.EQ.0) GO TO 40
C
C  PROCESS EACH TYPE
      DO 30 I=1,NRRSTP
C     CHECK IF TYPE IS VALID
         IX=IPDCKR(IRRSTP(I))
         IF (IX.NE.0) GO TO 10
            IF (IPDDB.GT.0) WRITE (LPE,60) IRRSTP(I)
            GO TO 30
C     CALCULATE NUMBER OF RECORDS TO ADD
10       NWRDS=NVAL(I)*NUMOBV(I)+LHDRRS+NRSTAT
         NRECS=IUNRCD(NWRDS,LRCPDR)
         NUSED=LSTREC+NRECS
         IF (IPDDB.GT.0) WRITE (IOGDB,70) NVAL(I),NUMOBV(I),LHDRRS,
     *      NRSTAT,NRECS,NUSED,MXRRSF
         IF (NUSED.LE.MXRRSF) GO TO 20
C     SPACE NOT AVAILABLE FOR TYPE
            IADDR(I)=1
            NSET=NSET+1
            GO TO 30
C     SPACE AVAILABLE FOR THIS TYPE
20       IADDR(I)=0
         ISET=ISET+1
         LSTREC=NUSED
30       CONTINUE
C
C  SET STATUS
      IF (ISET.EQ.NRRSTP) GO TO 40
      ISTAT=1
      IF (NSET.EQ.NRRSTP) ISTAT=2
C
40    IF (IPDTR.GT.0) WRITE (IOGDB,80) ISTAT
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
50    FORMAT (' *** ENTER RPDCKR')
60    FORMAT (' **ERROR** INVALID DATA TYPE : ',A4)
70    FORMAT (' NVAL(I)=',I3,3X,'NUMOBS(I)=',I3,3X,
     *   'LHDRRS=',I2,3X,'NRSTAT=',I2,3X,
     *   'NRECS=',I3,3X,'NUSED=',I5,3X,'MXRRSF=',I6)
80    FORMAT (' *** EXIT RPDCKR - ISTAT=',I2)
C
      END
