C MEMBER PDGFUD
C  (from old member PDPDFDAT)
C-----------------------------------------------------------------------
C
C @PROCESS LVL(77)
C
      SUBROUTINE PDGFUD (IDATES,LDATES,ITX,IREC,ISTAT)
C
C          ROUTINE:  PDGFUD
C
C             VERSION:  1.0.0
C
C                DATE:  1-20-83
C
C              AUTHOR:  SONJA R SIEGEL
C                       DATA SCIENCES INC
C                       8555 16TH ST, SILVER SPRING, MD 587-3700
C***********************************************************************
C
C          DESCRIPTION:
C
C    ROUTINE TO COMPUTE AND READ THE RECORDS CONTAINING THE
C    FUTURE DATA DATES.
C
C***********************************************************************
C
C          ARGUMENT LIST:
C
C         NAME    TYPE  I/O   DIM   DESCRIPTION
C
C       IDATES     I     O     1    DATES ARRAY
C       LDATES     I     I     1    LENGTH OF DATES ARRAY
C       ITX        I     I     1    SUBSCRIPT OF DATA TYPE
C        IREC      I     O     1    RECORD NUMBER OF DATES ARRAY
C       ISTAT      I     O     1    STATUS O=OK, NOT 0 =READ ERROR
C***********************************************************************
C
C          COMMON:
C
      INCLUDE 'uio'
      INCLUDE 'udebug'
      INCLUDE 'ucommon/uordrx'
      INCLUDE 'pdbcommon/pdbdta'
      INCLUDE 'pdbcommon/pddtdr'
      INCLUDE 'pdbcommon/pdunts'
      INCLUDE 'pdbcommon/pdsifc'
      INCLUDE 'urcommon/urpddt'
      INCLUDE 'urcommon/urunts'
C
C***********************************************************************
C
C          DIMENSION AND TYPE DECLARATIONS:
C
      DIMENSION IDATES(1)
C
      INTEGER*2 ITEMP(2)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/db_pdbrw/RCS/pdgfud.f,v $
     . $',                                                             '
     .$Id: pdgfud.f,v 1.1 1995/09/17 18:43:56 dws Exp $
     . $' /
C    ===================================================================
C
C
C***********************************************************************
C
C
      IF (IPDTR.GT.0) WRITE (IOGDB,30)
C
      ISTAT=0
C
      IDATES(1)=0
C
C  COMPUTE RECORD NUMBER THAT HAS THE DATES FOR FUTURE DATA
      LRCPD2=LRCPDD*2
      IF (IAMORD.EQ.0) NWDSP=IDDTDR(16,ITX)
      IF (IAMORD.EQ.1) NWDSP=JDDTDR(16,ITX)
      IF (IAMORD.EQ.0) LPTRS=IDDTDR(14,ITX)
      IF (IAMORD.EQ.1) LPTRS=JDDTDR(14,ITX)
      IREC=IUNRCD(NWDSP,LRCPD2)+LPTRS
      IF (IREC.LE.0) GO TO 25
C
C  COMPUTE NUMBER OF RECORDS TO BE READ
      IF (IAMORD.EQ.0) NWDSD=IDDTDR(7,ITX)*2+1
      IF (IAMORD.EQ.1) NWDSD=JDDTDR(7,ITX)*2+1
      NREC=IUNRCD(NWDSD,LRCPDD)
C
C  CHECK SIZE OF DATES ARRAY
      N=NREC*LRCPDD
      IF (N.LE.LDATES) GO TO 10
         WRITE (LPE,40) N,LDATES
         ISTAT=1
         GO TO 20
C
10    IF (IAMORD.EQ.0) IFILE=IDDTDR(4,ITX)
      IF (IAMORD.EQ.1) IFILE=JDDTDR(4,ITX)
      IF (IAMORD.EQ.0) NUNIT=KPDDDF(IFILE)
      IF (IAMORD.EQ.1) NUNIT=KURDDF(IFILE)
      IF (IPDDB.GT.0) WRITE (IOGDB,50) IAMORD,NUNIT,IREC,NREC
      CALL RVLRCD (NUNIT,IREC,NREC,IDATES,LRCPDD,ISTAT)
      IF (ISTAT.NE.0) GO TO 20
C
C  CHECK IF ANY DATES OF DATA
      CALL UMEMOV (IDATES(1),ITEMP(1),1)
      IF (ITEMP(1).EQ.MISSNG) IDATES(1)=0
C
20    NDTS=IDATES(1)*2+1
C
25    IF (IPDTR.GT.0) WRITE (IOGDB,60) (IDATES(N),N=1,NDTS+1)
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
30    FORMAT (' *** ENTER PDGFUD')
40    FORMAT (' **ERROR** IN PDGFUD - DATE ARRAY IS',I5,' WORDS.',I5,
     *  ' WERE PASSED.')
50    FORMAT (' IAMORD=',I2,3X,
     *   'NUNIT=',I2,3X,'IREC=',I4,3X,'NREC=',I3)
60    FORMAT (' *** EXIT PDGFUD : DATES=',10I6)
C
      END
