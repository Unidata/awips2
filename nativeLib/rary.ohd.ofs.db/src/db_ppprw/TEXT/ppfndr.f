C MEMBER PPFNDR
C  (from old member PPPPFNDR)
C-----------------------------------------------------------------------
C
       SUBROUTINE PPFNDR (ID,ITYPE,IFIND,IXBUF,IFREE,ISTAT)
C
C          SUBROUTINE:  PPFNDR
C
C             VERSION:  1.0.0
C
C                DATE:  12-2-82
C
C              AUTHOR:  SONJA R SIEGEL
C                       DATA SCIENCES INC
C                       8555 16TH ST, SILVER SPRING, MD 587-3700
C***********************************************************************
C
C          DESCRIPTION:
C
C    THIS ROUTINE WILL SEARCH THE PARAMETER INDEX FILE FOR A
C    STATION ID AND TYPE OR FOR A FREE SLOT IF THE ID IS NOT
C    FOUND.  IT USES A HASHING ALGORITHM TO COMPUTE THE RECORD
C    NUMBER.  IFREE=RECORD NUMBER OF FREE SLOT IT NOT FOUND
C    IFIND=RECORD NUMBER OF INDEX IF FOUND, AND IXBUF IS THE RECORD
C
C***********************************************************************
C
C          ARGUMENT LIST:
C
C         NAME    TYPE  I/O   DIM   DESCRIPTION
C
C       ID         A8    I     2   STATION ID
C       ITYPE      A4    I     1   PARAMETER TYPE
C       IFIND      I     I     1   0 = FIND FIRST FREE SLOT IF RECORD
C                                      NOT FOUND
C                                  1 = FIND RECORD
C       IFIND      I     O     1   INDEX RECORD NUMBER IF FOUND, OR 0
C       IXBUF      I     O     3   INDEX RECORD IF FOUND,OR UNDEF
C       IFREE      I     O     1   FREE SLOT IF NOT FOUND, OR 0
C       ISTAT      I     O     1   STATUS CODE
C                                    0=OK
C                                    1=DAIO READ ERROR
C                                    2=FILE FULL
C***********************************************************************
C
C          COMMON:
C
      INCLUDE 'uio'
      INCLUDE 'udebug'
      INCLUDE 'pppcommon/ppunts'
      INCLUDE 'pppcommon/ppxctl'
      INCLUDE 'ucommon/uordrx'
      INCLUDE 'urcommon/urunts'
      INCLUDE 'urcommon/urxctl'
C
C***********************************************************************
C
C          DIMENSION AND TYPE DECLARATIONS:
C
      DIMENSION ID(2),IXBUF(4),IRAY(3)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/db_ppprw/RCS/ppfndr.f,v $
     . $',                                                             '
     .$Id: ppfndr.f,v 1.1 1995/09/17 18:45:08 dws Exp $
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
C  CHECK IF REORDER RUN
      IF (IAMORD.EQ.1) GO TO 10
C
C  NOT REORDER RUN
      MXP=MXPXRC
      IXP=IPXRC1
      LUINDX=KPPIDX
      GO TO 20
C
C  REORDER RUN
10    MXP=MAXPXR
      IXP=IXPRC1
      LUINDX=KURIDX
C
20    IF (IPPTR.GT.0) WRITE (IOGDB,2003) IFIND
      IXFIND=IFIND
      IPRIME=17
      ITRY=0
      ISAVE=0
      IFREE=0
      IFIND=0
C
C  MAX2 IS NUMBER OF RECORDS AVAILABLE FOR INDEX
      MAX2=MXP-IXP+1
      IOVFL=MAX2*.87225+IXP
      IHOLD=IOVFL-IXP
      IRETRY=IPRIME/2+1
C
C  HASH ON STATION TYPE AND PARAMETER TYPE THEN ADD OFFSET TO GET TO
C  INDEX RECORDS (IXP THROUGH MXP)
      CALL UMEMOV (ID,IRAY,2)
      IRAY(3)=ITYPE
      CALL PPHASH (IHOLD,IRAY,IHASH)
      IHASH=IHASH+IXP-1
C
C  CHECK PRIMARY INDEX RECORDS
75    DO 100 I=1,IRETRY
         IXREC=IHASH
         IF (IPPDB.GT.0) WRITE (IOGDB,2000) IXREC
         CALL UREADT (LUINDX,IXREC,IXBUF,ISTAT)
         IF (ISTAT.NE.0) GO TO 900
         IF (IXFIND.EQ.1) GO TO 77
C       CHECK IF DELETED
         IF (IXBUF(1).EQ.-1) GO TO 80
C          CHECK IF UNUSED
            IF (IXBUF(1).EQ.0) GO TO 200
               GO TO 76
77          IF (IXBUF(1).EQ.0) GO TO 999
C          COMPARE THE NAME
76          CALL UCMPAR (IRAY,IXBUF,3,IMATCH)
            IF (IMATCH.EQ.0) GO TO 300
C          THAT WASNT IT, TRY ANOTHER
            GO TO 90
C       FOUND A DELETED SLOT, SAVE IT
80        IF (ISAVE.EQ.0) ISAVE=IXREC
C       ADD PRIME NUMBER AND TRY AGAIN
90       ITRY=ITRY+1
         IHASH=IHASH+IPRIME
         IF (IHASH.LE.IHOLD+IXP-1) GO TO 100
         IHASH=IHASH-IHOLD
100      CONTINUE
C
      IF (IPPDB.GT.0) WRITE (IOGDB,2010)
C
C NOT FOUND IN LOOP, CHECK OVERFLOW AREA
      DO 150 IXREC=IOVFL,MXP
         IF (IPPDB.GT.0) WRITE (IOGDB,2000) IXREC
         CALL UREADT (LUINDX,IXREC,IXBUF,ISTAT)
         IF (ISTAT.NE.0) GO TO 900
         IF (IXFIND.EQ.1) GO TO 154
            IF (IXBUF(1).EQ.-1) GO TO 156
                IF (IXBUF(1).EQ.0) GO TO 200
                GO TO 155
154      IF (IXBUF(1).EQ.0) GO TO 999
155      CALL UCMPAR (IXBUF,IRAY,3,IMATCH)
         IF (IMATCH.EQ.0) GO TO 300
            GO TO 150
156         IF (ISAVE.EQ.0) ISAVE=IXREC
150      CONTINUE
C
      IF (IXFIND.EQ.1) GO TO 999
C
C  NOT FOUND AT ALL BUT ALSO INDEX OVERFLOW IS FULL
      IF (ISAVE.NE.0) GO TO 210
      IF (IPPDB.GT.0) WRITE (IOGDB,2004)
      ISTAT=2
      GO TO 999
C
C  FOUND EMPTY SLOT AND NO ID
200   IFREE=IXREC
C
C  SEE IF WE WERE SUPPOSED TO REUSE
210   IF (ISAVE.NE.0) IFREE=ISAVE
      GO TO 999
C
C  FOUND ID
300   IFIND=IXREC
      GO TO 999
C
C  READ ERROR FROM INDEX FILE
900   IF (IPPDB.GT.0) WRITE (IOGDB,2001) ISTAT
      ISTAT=1
C
999   IF (IPPTR.GT.0) WRITE (IOGDB,2002) IRAY,IFREE,IFIND,ISTAT
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
2003  FORMAT (' *** ENTER PPFNDR - IFIND=',I2)
2000  FORMAT (' IXREC=',I6)
2010  FORMAT (' CHECK OVERFLOW AREA')
2004  FORMAT (' **ERROR** IN PPFNDR - PPPINDEX IS FULL')
2001  FORMAT (' **ERROR** IN PPFNDR - READ ERROR FROM PPPINDEX ',I6)
2002  FORMAT (' *** EXIT PPFNDR - IRAY=',3A4,3X,'IFREE=',I6,3X,
     *   'IFIND=',I6,3X,'ISTAT=',I2)
C
      END
