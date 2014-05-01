C MEMBER EPRHD
C  (from old member EEPRHD)
C
      SUBROUTINE EPRHD
C
C ......................................................................
C
C               PRINT HEADER SUBROUTINE
C
C     THIS SUBROUTINE IS PART OF THE ESP INITILIZATION PROGRAM
C       AND PRINT SEGMENT ROUTINE.
C     THIS ROUTINE WILL PRINT HEADER INFORMATION FOR DESCRIBING
C       A SEGMENT
C
C ......................................................................
C     ORIGINALLY WRITTEN BY
C       ED VANBLARGAN - HRL - MAY,1981
C ......................................................................
C
      DIMENSION SBNAME(2),OLDOPN(2)
C
      INCLUDE 'common/espseg'
      INCLUDE 'common/fdbug'
      INCLUDE 'common/ionum'
      INCLUDE 'common/where'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/espinit/RCS/eprhd.f,v $
     . $',                                                             '
     .$Id: eprhd.f,v 1.1 1995/09/17 18:46:29 dws Exp $
     . $' /
C    ===================================================================
C
C
      DATA NAME,SBNAME/4HEPRT,4HEPRH,4HD   /
C
C SET ERROR TRACES IN CB/WHERE/
C
      IOLDOP=IOPNUM
      IOPNUM=0
      DO 10 I=1,2
      OLDOPN(I)=OPNAME(I)
10    OPNAME(I)=SBNAME(I)
C
C     GET TRACE LEVEL AND DEBUG
C
      IF (ITRACE.GE.1) WRITE(IODBUG,100)
100   FORMAT(1H0,13HEPRHD ENTERED)
      IBUG=IFBUG(NAME)
C
C     PRINT HEADER AND SEGMENT INFORMATION
C
      WRITE (IPR,200) ID,(IECRDT(I),I=1,3),(IECKDT(I),I=1,3)
200   FORMAT(////// 11X,37HFOLLOWING IS INFORMATION FOR SEGMENT:
     *// 21X,2A4 / 20X,10H**********
     *// 11X,I2,1H-,I2,1H-,I4,28H = DATE SEGMENT WAS CREATED.
     */ 11X,I2,1H-,I2,1H-,I4,78H = LAST DATE THAT ESP TIME SERIES WAS CO
     *MPARED WITH FORECAST PROG TIME SERIES.)
C
      IF(IBUG.NE.0) WRITE (IODBUG,300) NSREC,LTSESP,LPESP
300   FORMAT(11X,I8,49H = ESP FILE RECORD NUMBER CONTAINING NEXT SEGMENT
     */ 11X,I8,26H = LENGTH OF ESP TS ARRAY.
     */ 11X,I8,26H = LENGTH OF ESP  P ARRAY.)
C
C RESET ERROR TRACES IN CB /WHERE/
C
      IOPNUM=IOLDOP
      OPNAME(1)=OLDOPN(1)
      OPNAME(2)=OLDOPN(2)
C
      RETURN
      END
