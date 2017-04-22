C MODULE PCMPBT
C-----------------------------------------------------------------------
C   10/27/87 SRS UPDATE DEBUG TO USE COMMON/UDEBUG/
C
C             VERSION:  1.0.0
C              AUTHOR:  JIM ERLANDSON
C                       DATA SCIENCES INC
C                       8555 16TH ST, SILVER SPRING, MD 587-3700
C***********************************************************************
C    ROUTINE COMPACTS THE HEADER OF A TIME SERIES RECORD. THE FIRST 7
C    WORDS ARE CHANGED FROM I*4 INTEGER WORDS TO ONE WORD OF
C    I*1 VALUES AND 2 OF I*2..  THE REMAINING BYTE IS ZERO FILLED.
C    THE HEADER IS THUS COMPACTED TO DIMENSION (18).  THE FIRST
C    3 WORDS ARE CHECK TO BE LESS THAN INTEGER VALUE 256.
C    IF FIRST WORD IS > 255, IT IS SET TO 0
C***********************************************************************
C          ARGUMENT LIST:
C
C         NAME    TYPE  I/O   DIM   DESCRIPTION
C
C       IHEADI     I     I    22    INPUT ARRAY CONTAINING EXPANDED
C                                   HEADER
C
C       IHEADO     I     O    18    OUTPUT ARRAY CONTAINING COMPACTED
C                                   HEADER
C
C       ISTAT      I     O     1    STATUS INDICATOR
C                                      0=NO VALUE GT 255
C                                      1=VALUE GT 255
C***********************************************************************
      SUBROUTINE PCMPBT (IHEADI,IHEADO,ISTAT)

      INCLUDE 'udebug'

      INTEGER    IHEADI(22),IHEADO(18),II
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/db_prdrw_lx/RCS/pcmpbt.f,v $
     . $',                                                             '
     .$Id: pcmpbt.f,v 1.2 2002/02/21 20:26:42 dws Exp $
     . $' /
C    ===================================================================
C

        ISTAT=0

C  ERROR:  Value >   255 in input numbers 2 thru 3
C          Value > 65535 in input numbers 4 thru 7
C          Value <     0 in input numbers 1 thru 7

        IF (IHEADI(1) .GT. 255) IHEADI(1)=0

        DO 10 II=1,3
            IF (IHEADI(II) .GT.   255) ISTAT = 1
            IF (IHEADI(II) .LT.     0) ISTAT = 1
   10   CONTINUE

        DO 20 II=4,7
            IF (IHEADI(II) .GT. 65535) ISTAT = 1
            IF (IHEADI(II) .LT.     0) ISTAT = 1
   20   CONTINUE

C The following was changed by Hank Herr (8/16/2001).
C I needed to reverse the order of the bytes, I commented out the
C second IHEADO(1) line below and make IHEADI(1) to be (3) and (3)
C to be (1).  I then exchanged IHEADI(5) for (4) and (7) for (6).

        IF (ISTAT .EQ. 0) THEN
            IHEADO(1) = 65536*IHEADI(3) + 256*IHEADI(2) + IHEADI(1)
CC          IHEADO(1) = 256*IHEADO(1)
            IHEADO(2) = 65536*IHEADI(5) + IHEADI(4)
            IHEADO(3) = 65536*IHEADI(7) + IHEADI(6)

            DO 30 II=4,18
                IHEADO(II) = IHEADI(II+4)
   30       CONTINUE
        ENDIF


      IF (IPRTR.GT.0) WRITE (IOGDB,50) ISTAT
   50 FORMAT (' *** EXIT PCMPBT : ISTAT=',I2)

      RETURN
      END
