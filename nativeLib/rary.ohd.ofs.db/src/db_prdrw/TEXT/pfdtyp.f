C MEMBER PFDTYP
C  (from old member PRDCHECK)
C-----------------------------------------------------------------------
C
C  10/27/87 SRS UPDATE DEBUG TO USE COMMON/UDEBUG/
C **VERSION 5.0.0 4-15-82 ADD FUNCTIONS IPRDMD,IPRDMR,IPRDMF
C  **VERSION 1.0.2 7-7-81 ADD CHECK ON NO TS TO PFNDTS
C     ** VERSION 1.0.1 (6-24-81) ADD FUNCTION PDTKEY
C     ** VERSION 1.0.0 (5-1-81)
C
C     AUTHOR: SONJA R. SIEGEL
C             DATA SCIENCES, INC
C             8555 16TH STREET
C              SILVER SPRING, MD 20795   587-3700
C
C
C  THESE ROUTINES DO VARIOUS CHECKS AND SEARCHES FOR THE PROCESSED
C  DATA BASE
C
C-----------------------------------------------------------------------
C
      SUBROUTINE PFDTYP (ITYPE,INDXD)
C
C  ROUTINE TO SEARCH THE DATA TYPE DIRECTORY FOR A DATA TYPE
C
C  ITYPE = DATA TYPE
C  INDXD = SUBSCRIPT OR 0 IF NOT FOUND
C
      INCLUDE 'udebug'
      INCLUDE 'ucommon/uordrx'
      INCLUDE 'prdcommon/pdftbl'
      INCLUDE 'prdcommon/pmaxdm'
      INCLUDE 'urcommon/urftbl'
      INCLUDE 'urcommon/urmaxm'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/db_prdrw/RCS/pfdtyp.f,v $
     . $',                                                             '
     .$Id: pfdtyp.f,v 1.1 1995/09/17 18:45:36 dws Exp $
     . $' /
C    ===================================================================
C
C
C
      IF (IPRTR.GT.0) WRITE (IOGDB,40) ITYPE
C
C  CHECK IF ANY DATA TYPES DEFINED
      IF (IAMORD.EQ.0) NTYPE=NUMDTP
      IF (IAMORD.EQ.1) NTYPE=NMTYPE
      IF (NTYPE.EQ.0) GO TO 20
C
C  SEARCH DIRECTORY FOR TYPE
      DO 10 INDXD=1,NTYPE
         IF (IPRDB.GT.0) WRITE (IOGDB,50) IAMORD,NTYPE,INDXD,
     *      DATFIL(1,INDXD)
         IF (IAMORD.EQ.0.AND.ITYPE.EQ.DATFIL(1,INDXD)) GO TO 30
         IF (IAMORD.EQ.1.AND.ITYPE.EQ.IDATFL(1,INDXD)) GO TO 30
10       CONTINUE
C
C  DATA TYPE NOT FOUND
20    INDXD=0
C
30    IF (IPRTR.GT.0) WRITE (IOGDB,60) INDXD
C
      RETURN
C
40    FORMAT (' *** ENTER PFDTYP - ITYPE=',A4)
50    FORMAT (' IAMORD=',I2,3X,'NTYPE=',I3,3X,'INDXD=',I3,3X,
     *   'DATFIL(1,INDXD)=',A4)
60    FORMAT (' *** EXIT PFDTYP - INDXD=',I3)
C
      END
