C MEMBER RPPPCO
C  (from old member PPPCOIO1)
C-----------------------------------------------------------------------
C
      SUBROUTINE RPPPCO (ISTAT)
C
C          ROUTINE:  RPPPCO
C
C             VERSION:  1.0.0
C
C                DATE:  12-4-1982
C
C              AUTHOR:  JANINE FRANZOI
C                       DATA SCIENCES INC
C                       8555 16TH ST, SILVER SPRING, MD 587-3700
C***********************************************************************
C
C          DESCRIPTION:
C
C   THIS ROUTINE READS THE CONTROL RECORDS FROM THE INDEX AND
C   PARAMETER FILES INTO COMMON BLOCKS.
C
C***********************************************************************
C
C          ARGUMENT LIST:
C
C         NAME    TYPE  I/O   DIM   DESCRIPTION
C
C         ISTAT     I     O    1      STATUS INDICATOR
C                                       0=NORMAL RETURN
C                                       1=ERROR
C
C***********************************************************************
C
C          COMMON:
C
      INCLUDE 'uio'
      INCLUDE 'udebug'
      INCLUDE 'ucommon/uordrx'
      INCLUDE 'pppcommon/ppmctl'
      INCLUDE 'pppcommon/ppxctl'
      INCLUDE 'pppcommon/ppunts'
      INCLUDE 'pppcommon/ppdtdr'
      INCLUDE 'pppcommon/pppdta'
      INCLUDE 'urcommon/urppmc'
      INCLUDE 'urcommon/urxctl'
      INCLUDE 'urcommon/urunts'
      INCLUDE 'urcommon/urppdt'
C
C***********************************************************************
C
C          DIMENSION AND TYPE DECLARATIONS:
C
      DIMENSION IPPNDX(8),JPPNDX(8),IARR(16)
C
      EQUIVALENCE (IPPNDX(1),MXPXRC)
      EQUIVALENCE (JPPNDX(1),MAXPXR)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/db_ppprw/RCS/rpppco.f,v $
     . $',                                                             '
     .$Id: rpppco.f,v 1.1 1995/09/17 18:45:15 dws Exp $
     . $' /
C    ===================================================================
C
C
C***********************************************************************
C
C          DATA:
C
C***********************************************************************
C
C
      IF (IPPTR.GT.0) WRITE (IOGDB,50)
C
      ISTAT=0
C
C  SET UNIT NUMBER
      IF (IAMORD.EQ.0) IUNIT=KPPIDX
      IF (IAMORD.EQ.1) IUNIT=KURIDX
C
C  READ INDEX CONTROL FROM FILE
      IREC=1
      IF (IAMORD.EQ.0) CALL UREADT (IUNIT,IREC,IPPNDX,ISTAT)
      IF (IAMORD.EQ.1) CALL UREADT (IUNIT,IREC,JPPNDX,ISTAT)
      IF (ISTAT.NE.0) GO TO 30
      IREC=2
      IF (IAMORD.EQ.0) CALL UREADT (IUNIT,IREC,IPPNDX(5),ISTAT)
      IF (IAMORD.EQ.1) CALL UREADT (IUNIT,IREC,JPPNDX(5),ISTAT)
      IF (ISTAT.NE.0) GO TO 30
C
C  READ DIRECTORY RECORDS
      IF (IAMORD.EQ.0) NTYPE=NMPTYP
      IF (IAMORD.EQ.1) NTYPE=NUMPTP
      DO 10 I=1,NTYPE
         IREC=IREC+1
         IF (IAMORD.EQ.0) CALL UREADT (IUNIT,IREC,IPDTDR(1,I),ISTAT)
         IF (IAMORD.EQ.1) CALL UREADT (IUNIT,IREC,JPDTDR(1,I),ISTAT)
         IF (ISTAT.NE.0) GO TO 30
         IREC=IREC+1
         IF (IAMORD.EQ.0) CALL UREADT (IUNIT,IREC,IPDTDR(5,I),ISTAT)
         IF (IAMORD.EQ.1) CALL UREADT (IUNIT,IREC,JPDTDR(5,I),ISTAT)
         IF (ISTAT.NE.0) GO TO 30
10       CONTINUE
C
C  READ CONTROL RECORDS FROM PARAMETER FILES
      IF (IAMORD.EQ.0) NFILE=NMPFIL
      IF (IAMORD.EQ.1) NFILE=NUMPFL
      IREC=1
      CALL UMEMST (0,IARR,LRECPP)
      DO 20 I=1,NFILE
         IF (IAMORD.EQ.0) IUNIT=KPPRMU(I)
         IF (IAMORD.EQ.1) IUNIT=KUPRMI(I)
         CALL UREADT (IUNIT,IREC,IARR,ISTAT)
         IF (ISTAT.NE.0) GO TO 30
         IF (IAMORD.EQ.0) CALL UMEMOV (IARR,IPMCTL(1,I),8)
         IF (IAMORD.EQ.1) CALL UMEMOV (IARR,JPMCTL(1,I),8)
20       CONTINUE
      GO TO 40
C
C  ERROR
30    WRITE (LPE,60)
C
40    IF (IPPTR.GT.0) WRITE (IOGDB,70) ISTAT
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
50    FORMAT (' *** ENTER RPPPCO')
60    FORMAT ('0*** ERROR - IN RPPPCO - DAIO ERROR READING RECORD ',I6,
     *   'FROM UNIT ',I2,'.')
70    FORMAT (' *** EXIT RPPPCO : ISTAT=',I2)
C
      END
