C MODULE PDCKCV
C-----------------------------------------------------------------------
C
       SUBROUTINE PDCKCV (IUNITS,ITYPE,IFLAG,ICONVT,FACTOR,TFACT,ISTAT)
C
C  THIS ROUTINE GETS THE CONVERSION FACTORS FOR A UNITS CONVERSION.
C  THE CURRENT UNITS (FOR A WRITE) OR THE REQUESTED UNITS (FOR A READ)
C  ARE CHECKED AGAINST THE STORED UNITS TO SEE IF A CONVERSION IS
C  NECESSARY.
C
C  ARGUMENT LIST:
C
C       NAME      TYPE  I/O   DIM   DESCRIPTION
C       ------    ----  ---   ---   -----------
C       IUNITS     A4    I     1    UNITS CODE
C       ITYPE      A4    I     1    DATA TYPE CODE
C       IFLAG      I     I     1    READ/WRITE INDICATOR:
C                                    1=IUNITS IS CURRENT UNITS
C                                    2=IUNITS IS REQUESTED UNITS
C       ICONVT     I     O     1    CONVERT INDICATOR:
C                                    0=DO NOT CONVERT
C                                    1=CONVERT
C       FACTOR     R     O     1    CONVERSION FACTOR
C       TFACT      R     O     1    TEMPERATURE CONVERSION FACTOR
C       ISTAT      I     O     1    STATUS INDICATOR:
C                                    0=NORMAL RETURN
C                                    OTHER=INVALID CONVERSION
C
      INCLUDE 'uiox'
      INCLUDE 'udebug'
      INCLUDE 'pdbcommon/pdtrrx'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/db_pdbrw/RCS/pdckcv.f,v $
     . $',                                                             '
     .$Id: pdckcv.f,v 1.2 2002/02/11 19:52:28 dws Exp $
     . $' /
C    ===================================================================
C
C
      IF(IPDTR.GT.1) WRITE (IOGDB,*) 'ENTER PDCKCV '
C
      ISTAT=0
C
C  CHECK IF VALID TYPE
      DO 10 I=1,NTYPE
         IF (ITYPE.EQ.IRTYPE(I)) GO TO 30
10       CONTINUE
      ISTAT=1
      WRITE (LP,20) ITYPE
20    FORMAT ('0**ERROR** DATA TYPE ',A4,' NOT DEFINED AS AN RRS DATA ',
     *   'TYPE')
      GO TO 40
C
C  SET UNITS IN WHICH DATA IS STORED
30    IBASUN=IRSUNT(I)
      ICONVT=0
C
C  CHECK IF ALREADY HAVE BASE UNITS
      IF (IBASUN.EQ.IUNITS) GO TO 40
C
C  GET CONVERSION FACTORS
      ICONVT=1
      IF (IFLAG.EQ.1) CALL UDUCNV (IUNITS,IBASUN,2,1,FACTOR,TFACT,ISTAT)
      IF (IFLAG.EQ.2) CALL UDUCNV (IBASUN,IUNITS,2,1,FACTOR,TFACT,ISTAT)
C
40    IF (IPDTR.GT.1) WRITE (IOGDB,*) 'EXIT PDCKCV'
C
      RETURN
C
      END
