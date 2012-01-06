C MEMBER PDCKST
C  (from old member PDPDSTAR)
C-----------------------------------------------------------------------
C
      SUBROUTINE PDCKST (ILS,ISKIP,IARR,IBUF)
C
C          ROUTINE:  PDCKST
C
C             VERSION:  1.0.0
C
C                DATE:  2-4-83
C
C              AUTHOR:  JIM ERLANDSON
C                       DATA SCIENCES INC
C                       8555 16TH ST, SILVER SPRING, MD 587-3700
C***********************************************************************
C
C          DESCRIPTION:
C
C    THIS ROUTINE CHECKS A VALUE TO SEE IF IT IS THE LARGEST,
C    SECOND LARGEST, OR THE SMALLEST, SECOND SMALLEST VALUE IN AN
C    RRS RECORD (INCLUDING FREE POOL RECORD.
C
C***********************************************************************
C
C          ARGUMENT LIST:
C
C         NAME    TYPE  I/O   DIM   DESCRIPTION
C
C       ILS        I     I     1    LARGE/SMALL FLAG
C                                    1=LARGE
C                                    2=SMALL
C       ISKIP      I     I     1    NUMBER OF VALUES PER OBSERVATION
C       IARR       I    I/O    ?    ARRAY TO HOLD LARGE/SMALL VALUES
C       IBUF       I     I     ?    ARRAY HOLDING VALUE TO CHECK
C
C***********************************************************************
C
C          COMMON:
C
      INCLUDE 'uio'
      INCLUDE 'udebug'
C
C***********************************************************************
C
C          DIMENSION AND TYPE DECLARATIONS:
C
      INTEGER IARR(1),IBUF(1)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/db_pdbrw/RCS/pdckst.f,v $
     . $',                                                             '
     .$Id: pdckst.f,v 1.1 1995/09/17 18:43:30 dws Exp $
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
C
      IF (IPDTR.GT.1) WRITE (IOGDB,10)
10    FORMAT (' *** ENTER PDCKST')
C
      GO TO (20,50),ILS
C
C  CHECK FOR LARGSET VALUE
20    IF (IPDGRT(IBUF(2),IARR(1)).NE.1) GO TO 40
      IF (IARR(2).EQ.0) GO TO 30
         IARR(3)=IARR(1)
         IARR(4)=IARR(2)
30    IARR(1)=IBUF(2)
      IARR(2)=JULMIN(ISKIP,IBUF(1))
      GO TO 90
C
C  CHECK SECOND LARGEST VALUE
40    IF (IPDGRT(IBUF(2),IARR(3)).EQ.-1) GO TO 90
      IARR(3)=IBUF(2)
      IARR(4)=JULMIN(ISKIP,IBUF(1))
      GO TO 90
C
C  CHECK FOR SMALLEST VALUE
50    IF (IARR(2).EQ.0) GO TO 60
         IF (IPDGRT(IBUF(2),IARR(1)).NE.-1) GO TO 70
         IARR(3)=IARR(1)
         IARR(4)=IARR(2)
60    IARR(1)=IBUF(2)
      IARR(2)=JULMIN(ISKIP,IBUF(1))
      GO TO 90
C
C  CHECK SECOND SMALLEST VALUE
70    IF (IARR(4).EQ.0) GO TO 80
         IF (IPDGRT(IBUF(2),IARR(3)).EQ.1) GO TO 90
80    IARR(3)=IBUF(2)
      IARR(4)=JULMIN(ISKIP,IBUF(1))
C
90    IF (IPDTR.GT.1) WRITE (IOGDB,100)
100   FORMAT (' *** EXIT PDCKST')
C
      RETURN
C
      END
