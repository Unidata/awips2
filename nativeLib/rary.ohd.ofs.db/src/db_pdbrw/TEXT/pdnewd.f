C MEMBER PDNEWD
C  (from old member PDWPD1S)
C-----------------------------------------------------------------------
C
C                             LAST UPDATE: 09/28/95.16:01:30 BY $WC20SV
C
C @PROCESS LVL(77)
C
      SUBROUTINE PDNEWD (IFDAY,ISTAT)
C
C  THIS ROUTINE CREATES A NEW DAY IN THE DAILY DATA BASE
C  FOR ALL DATA TYPE DEFINED EXCEPT TF24.
C
      CHARACTER*4 DTYPE
C
      INCLUDE 'uio'
      INCLUDE 'udebug'
      INCLUDE 'hclcommon/hdflts'
      INCLUDE 'pdbcommon/pdsifc'
      INCLUDE 'pdbcommon/pddtdr'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/db_pdbrw/RCS/pdnewd.f,v $
     . $',                                                             '
     .$Id: pdnewd.f,v 1.2 1996/01/16 22:53:47 page Exp $
     . $' /
C    ===================================================================
C
C
C
      IF (IPDTR.GT.0) WRITE (IOGDB,10)
10    FORMAT (' *** ENTER PDNEWD')
C
      IF (IPDDB.GT.0) WRITE (IOGDB,*) 'IFDAY=',IFDAY
C
C  PROCESS EACH TYBE
      DO 60 IX=1,NMDTYP
         CALL UMEMOV (IDDTDR(2,IX),DTYPE,1)
         IF (DTYPE.EQ.'TF24') GO TO 60
         IF (IDDTDR(7,IX).LE.0) GO TO 60
C     COMPUTE THE NEEDED VALUES
         CALL PDVALS (IX,NNRC1D,NNRCAL,LLSTRC)
         IFILEL=IDDTDR(4,IX)
C     CHECK WHERE NEW DAY WILL GO
         IF (IDDTDR(13,IX).EQ.LLSTRC) GO TO 20
C        NEW DAY GOES IN NEXT RECORD - NO WRAP-AROUND
            IDDTDR(13,IX)=IDDTDR(13,IX)+NNRC1D
            GO TO 30
C     NEW DAY MUST WRAP-AROUND TO BEGINNING
20       IDDTDR(13,IX)=IDDTDR(15,IX)
C     UPDATE THE DATE
30       CALL UMEMOV (IFDAY,IDDTDR(11,IX),1)
         IF (IDDTDR(10,IX).NE.IDDTDR(13,IX)) GO TO 60
C     INITIALIZE DATA RECORDS
         IZREC=IDDTDR(10,IX)
         CALL PDSET0 (DTYPE,IZREC,NNRC1D,IFILEL,ISTAT)
         IF (ISTAT.NE.0) GO TO 80
C     MOVE UP THE POINTER OF THE OLDEST DATE
         IF (IDDTDR(10,IX).EQ.LLSTRC) GO TO 40
            IDDTDR(10,IX)=IDDTDR(10,IX)+NNRC1D
            GO TO 50
C     NOT ONLY LOST THE EARLIEST DATE, BUT MUST WRAP-AROUND
40       IDDTDR(10,IX)=IDDTDR(15,IX)
C     UPDATE DATE OF LAST DAY OF DATA
50       CALL UMEMOV (IDDTDR(8,IX),IE,1)
         IE=IE+1
         CALL UMEMOV (IE,IDDTDR(8,IX),1)
60       CONTINUE
C
      CALL MDYH2 (IFDAY,0,IFMO,IFDA,IFYR,IFHR,ITZ,IDSAV,TIME(3))
      IFYR=MOD(IFYR,100)
      IF (IPDDB.GT.0) WRITE (LP,70) IFMO,IFDA,IFYR
70    FORMAT ('0**NOTE** IN PDNEWD - NEW DAY OF DATA FOR DATE ',
     *   I2.2,'/',I2.2,'/',I2.2,
     *   ' CREATED.')
C
80    IF (IPDTR.GT.0) WRITE (IOGDB,90) ISTAT
90    FORMAT (' *** EXIT PDNEWD - ISTAT=',I2)
C
      RETURN
C
      END
