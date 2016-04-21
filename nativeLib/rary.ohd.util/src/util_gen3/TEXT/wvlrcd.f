C MEMBER WVLRCD
C-----------------------------------------------------------------------
C
      SUBROUTINE WVLRCD (IUNIT,ISTRT,NREC,ARRAY,LRECL,ISTAT)
C
C  THIS ROUTINE WRITES A SPECIFIED NUMBER OF RECORDS FROM AN ARRAY TO A
C  DATASET USING DAIO.
C
C     ARGUMENT LIST:
C
C         NAME    TYPE  I/O   DIM   DESCRIPTION
C       IUNIT     I   INPUT   1     LOGICAL UNIT OF OUTPUT DATA SET
C       ISTRT     I   INPUT   1     FIRST RECORD NUMBER TO WRITE
C       NREC      I   INPUT   1     NUMBER OF RECORDS TO WRITE
C       ARRAY     I   INPUT   ?     ARRAY TO WRITE FROM
C       LRECL     I   INPUT   1     LOGICAL RECORD LENGTH OF DATA SET
C       ISTAT     I   OUTPUT  1      STATUS INDICATOR
C                                        0=RECORDS READ
C                                        1=RECORDS NOT READ
C
      INTEGER ARRAY(1)
C
      INCLUDE 'ucmdbx'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/util/src/util_gen3/RCS/wvlrcd.f,v $
     . $',                                                             '
     .$Id: wvlrcd.f,v 1.1 1995/09/17 19:04:16 dws Exp $
     . $' /
C    ===================================================================
C
C
C
      IF (ICMTRC.GT.1) WRITE (ICMPRU,20)
C
      IREC=ISTRT
      IPOS=1
C
      DO 10 I=1,NREC
         CALL UWRITT (IUNIT,IREC,ARRAY(IPOS),ISTAT)
         IREC=IREC+1
         IPOS=IPOS+LRECL
10       CONTINUE
C
      IF (ICMTRC.GT.1) WRITE (ICMPRU,30) ISTAT
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
20    FORMAT (' *** ENTER WVLRCD')
30    FORMAT (' *** EXIT WVLRCD - ISTAT=',I2)
C
      END
