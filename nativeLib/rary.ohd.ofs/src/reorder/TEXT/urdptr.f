C MODULE URDPTR
C-----------------------------------------------------------------------
C
       SUBROUTINE URDPTR (ITYPE,LPARRY,IPARRY,LUSED,ISTAT)
C
C  THIS ROUTINE READS THE POINTER RECORDS FOR THE SPECIFIED DATA TYPE
C  FROM THE PREPROCESSOR DATA BASE.
C
C  ARGUMENT LIST:
C
C     NAME    TYPE  I/O   DIM   DESCRIPTION
C     ------  ----  ---   ---   -----------
C     ITYPE    A4    I     1    DATA TYPE
C     LPARRY   I*4   I     1    LENGTH OF IPARRY
C     IPARRY   I*2   O     ?    POINTER ARRAY
C     ISTAT    I*4   O     1    STATUS CODE:
C                                 0=NORMAL RETURN
C                                 1=DATA TYPE NOT FOUND
C                                 2=SYSTEM READ ERROR
C
      INTEGER*2 IPARRY(LPARRY)
C
      INCLUDE 'uiox'
      INCLUDE 'udebug'
      INCLUDE 'ucommon/uordrx'
      INCLUDE 'urcommon/urpddt'
      INCLUDE 'urcommon/urcdta'
      INCLUDE 'urcommon/ursifc'
      INCLUDE 'urcommon/urunts'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/reorder/RCS/urdptr.f,v $
     . $',                                                             '
     .$Id: urdptr.f,v 1.2 2002/02/11 21:11:46 dws Exp $
     . $' /
C    ===================================================================
C
C
      IF (IPDTR.GT.0) THEN
         WRITE (IOGDB,*) 'ENTER URDPTR'
         CALL SULINE (IOGDB,1)
         ENDIF
C
      ISTAT=0
C
      IAMORD=1
C
C  FIND TYPE IN DATA TYPE DIRECTORY
      IDX=IPDCKD(ITYPE)
      IF (IDX.GT.0) GO TO 10
         WRITE (LP,70) ITYPE
         CALL SUERRS (LP,2,-1)
         ISTAT=1
         GO TO 30
C
C  GET FILE NUMBER
10    IFILE=JDDTDR(4,IDX)
C
C  READ POINTER RECORDS
      LUSED=JDDTDR(18,IDX)
      LRL=LRLURD*2
      NPTRC=IUNRCD(LUSED,LRL)
      NWDS=NPTRC*LRL
      IF (NWDS.LE.LPARRY) GO TO 20
         WRITE (LP,60) ITYPE,LPARRY,NWDS
         CALL SUERRS (LP,2,-1)
         ISTAT=2
         GO TO 30
20    LRCNUM=JDDTDR(14,IDX)
      CALL RVLRCD (KURDDF(IFILE),LRCNUM,NPTRC,IPARRY,LRLURD,ISTAT)
      IF (ISTAT.EQ.0) GO TO 40
      WRITE (LP,80) ISTAT
      CALL SUERRS (LP,2,-1)
      ISTAT=2
C
C  SET ERROR FLAG
30    IWURFL=1
C
40    IF (IPDTR.GT.0) THEN
         WRITE (IOGDB,*) 'EXIT URDPTR - ISTAT=',ISTAT
         CALL SULINE (IOGDB,1)
         ENDIF
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
60    FORMAT ('0*** ERROR - IN URDPTR - ARRAY TO HOLD POINTERS FOR ',
     *   'DATA TYPE ',A4,' TOO SMALL. ',I6,' AVAILABLE. ',I6,' NEEDED.')
70    FORMAT ('0*** ERROR - IN URDPTR - DATA TYPE ',A4,' NOT DEFINED ',
     *   'ON THE NEW PREPROCESSOR DATA BASE.')
80    FORMAT ('0*** ERROR - IN URDPTR - SYSTEM ERROR. RVLRCD STATUS ',
     *   'CODE=',I2,'.')
C
      END
