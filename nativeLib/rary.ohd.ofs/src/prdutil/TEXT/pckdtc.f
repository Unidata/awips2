C MEMBER PCKDTC
C  (from old member PRDFUNC)
C-----------------------------------------------------------------------
C
C                             LAST UPDATE: 03/21/95.14:53:01 BY $WC20SV
C
C @PROCESS LVL(77)
C
      SUBROUTINE PCKDTC (IREC,NFCODE,ISTAT)
C
C          ROUTINE:  PCKDTC
C
C             VERSION:  1.0.0
C
C                DATE:  10-20-81
C
C              AUTHOR:  JIM ERLANDSON
C                       DATA SCIENCES INC
C
C***********************************************************************
C
C          DESCRIPTION:
C
C    ROUTINE TO CHECK FOR VALID DATA TYPE CODE AND ASSIGN
C    VALUES WHICH ARE AUTOMATIC FOR THAT DATA TYPE CODE.
C    VALUES ARE ASSIGNED FOR PROCESSING CODE, WRITE KEY, UNITS
C    DIMENSION, AND FUTURE DATA TYPE CODE.  A BLANK IS RETURNED
C    IF THERE IS NO FUTURE DATA TYPE.
C
C***********************************************************************
C
C          ARGUMENT LIST:
C
C         NAME    TYPE  I/O   DIM   DESCRIPTION
C
C       IREC       I    I/O    18   ARRAY IN WHICH TO STORE WORDS
C                                   IREC(1) IS THE DATA TYPE
C
C       NFCODE     A4     O    1    FUTURE DATA TYPE CODE
C
C       ISTAT      I      O    1    STATUS INDICATOR
C                                     0=NORMAL RETURN
C                                     1=ERROR
C
C***********************************************************************
C
C          DIMENSION AND TYPE DECLARATIONS:
C
      CHARACTER*4 NFCODE
C
      DIMENSION IREC(*)
      DIMENSION LDIMS(13)
      DIMENSION LPKOFF(3)
C
C***********************************************************************
C
C          DATA:
C
      DATA LMAP/4HMAP /,LMAT/4HMAT /
      DATA LDIMS/4HL   ,4HL2  ,4HL3  ,4HL/T ,4HL3/T,4HE/L2,
     *           4HPRES,4HTEMP,4HDLES,4HTIME,4HDIR ,4HE   ,4HE/T /
      DATA LPKOFF/4HINST,4HACCM,4HMEAN/
C
C***********************************************************************
C
C          COMMON:
C
      INCLUDE 'uio'
      INCLUDE 'udebug'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/prdutil/RCS/pckdtc.f,v $
     . $',                                                             '
     .$Id: pckdtc.f,v 1.1 1995/09/17 19:16:30 dws Exp $
     . $' /
C    ===================================================================
C
C
C***********************************************************************
C
C
      ISTAT=0
C
      IF (IPRTR.GT.0) WRITE (IOGDB,60)
C
      IF (IPRDB.GT.0) WRITE (IOGDB,70) IREC(1)
C
C  GET DATA TYPE ATTRIBUTES
      CALL UDTATR ('FCST',IREC(1),
     *   IDTDIM,DTUNIT,IDTMIS,NDTVAL,IDTIME,NDTADD,IDTWRT,ISTAT)
      IF (ISTAT.NE.0) GO TO 50
C
      DO 10 J=1,3
         IF (IDTIME.EQ.LPKOFF(J)) THEN
            IREC(6)=J
            GO TO 20
            ENDIF
10       CONTINUE
C
20    IREC(11)=IDTWRT
C
      DO 30 J=1,13
         IF (IDTDIM.EQ.LDIMS(J)) THEN
            IREC(12)=J
            GO TO 40
            ENDIF
30       CONTINUE
      J=9
C
40    IREC(13)=NDTVAL
      IREC(14)=NDTADD
C
      NFCODE=' '
      IF (IREC(1).EQ.LMAP) NFCODE='FMAP'
      IF (IREC(1).EQ.LMAT) NFCODE='FMAT'
C
      IF (IPRDB.GT.0) THEN
         WRITE (IOGDB,80) (IREC(I),I=1,14)
         WRITE (IOGDB,90) (IREC(I),I=1,14)
         WRITE (IOGDB,*) 'NFCODE=',NFCODE
         ENDIF
C
50    IF (IPRTR.GT.0) WRITE (IOGDB,100) ISTAT
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
60    FORMAT (' *** ENTER PCKDTC')
70    FORMAT (' REC(1)=',A4)
80    FORMAT (' USING A4 FORMAT: IREC(1...14)=',14(A4,1X))
90    FORMAT (' USING I4 FORMAT: IREC(1...14)=',14(I4,1X))
100   FORMAT (' *** EXIT PCKDRC - ISTAT=',I2)
C
      END
