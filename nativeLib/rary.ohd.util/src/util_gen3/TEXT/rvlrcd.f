C MEMBER RVLRCD
C-----------------------------------------------------------------------
C
      SUBROUTINE RVLRCD (IUNIT,ISTRT,NREC,ARRAY,LRECL,ISTAT)
C
C  THIE ROUTINE READS A SPECIFIED NUMBER OF RECORDS FROM A DATASET INTO
C  AN ARRAY USING DAIO.
C
C    ARGUMENT LIST:
C
C         NAME    TYPE  I/O   DIM   DESCRIPTION
C       IUNIT     I   INPUT    1    LOGICAL UNIT OF FILE TO READ
C       ISTRT     I   INPUT    1    FIRST RECORD TO READ
C       NREC      I   INPUT    1    NUMBER OF RECORDS TO READ
C       ARRAY     I   OUTPUT   ?    ARRAY TO RECEIVE RECORDS
C       LRECL     I   INPUT    1    LOGICAL RECORD LENGTH OF FILE
C       ISTAT     I   OUTPUT   1    STATUS INDICATOR
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
     .$Source: /fs/hseb/ob72/rfc/util/src/util_gen3/RCS/rvlrcd.f,v $
     . $',                                                             '
     .$Id: rvlrcd.f,v 1.1 1995/09/17 19:02:54 dws Exp $
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
         CALL UREADT (IUNIT,IREC,ARRAY(IPOS),ISTAT)
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
20    FORMAT (' *** ENTER RVLRCD')
30    FORMAT (' *** EXIT RVLRCD - ISTAT=',I2)
C
      END
