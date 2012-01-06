C MODULE PDCNVT
C-----------------------------------------------------------------------
C
       SUBROUTINE PDCNVT (CFACT,TFACT,NDVALS,NVLPOB,VMISS,DVALS)
C
C  THIS ROUTINE CONVERTS VALUES FROM ONE UNITS TO ANOTHER.
C
C  ARGUMENT LIST:
C
C       NAME      TYPE  I/O   DIM   DESCRIPTION
C       ------    ----  ---   ---   -----------
C       CFACT      R     I     1    CONVERSION FACTOR
C       TFACT      R     I     1    TEMPERATURE CONVERSION FACTOR
C       NDVALS     I     I     1    NUMBER OF DATA VALUES TO CONVERT
C       NVLPOB     I     I     1    NUMBER OF VALUES PER OBSERVATION
C       VMISS      R     I     1    VALUE FOR MISSING DATA
C       DVALS      R    I/O    ?    ARRAY CONTAINING DATA
C
      INCLUDE 'udebug'
C
      DIMENSION DVALS(*)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/db_pdbrw/RCS/pdcnvt.f,v $
     . $',                                                             '
     .$Id: pdcnvt.f,v 1.2 2002/02/11 20:49:20 dws Exp $
     . $' /
C    ===================================================================
C
C
      IF (IPDTR.GT.1) WRITE (IOGDB,*) 'ENTER PDCNVT'
C
      DO 10 I=1,NDVALS,NVLPOB
         IF(DVALS(I).EQ.VMISS) GO TO 10
         DVALS(I)=DVALS(I)*CFACT+TFACT
10       CONTINUE
C
      IF (IPDTR.GT.1) WRITE (IOGDB,*) 'EXIT PDCNVT'
C
      RETURN
C
      END
