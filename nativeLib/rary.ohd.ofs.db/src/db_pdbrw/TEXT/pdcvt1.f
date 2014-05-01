C MEMBER PDCVT1
C  (from old member PDBUNCVT)
C
C                             LAST UPDATE: 02/06/95.17:21:45 BY $WC21DT
C
       SUBROUTINE PDCVT1(FACTOR,TFACT,VMISS,VAL)
C***********************************************************************
C                                                                      *
C          SUBROUTINE:  PDCVT1                                         *
C                                                                      *
C             VERSION:  1.0.0                                          *
C                                                                      *
C                DATE:  1-14-83                                        *
C                                                                      *
C              AUTHOR:  JIM ERLANDSON                                  *
C                       DATA SCIENCES INC                              *
C                       8555 16TH ST, SILVER SPRING, MD 587-3700       *
C***********************************************************************
C                                                                      *
C          DESCRIPTION:                                                *
C                                                                      *
C    THIS SUBROUTINE CONVERTS ONE VALUE FROM UNITS TO ANOTHER.         *
C    THE VALUE OF MISSING DATA IS NOT CONVERTED.                       *
C                                                                      *
C***********************************************************************
C                                                                      *
C          ARGUMENT LIST:                                              *
C                                                                      *
C         NAME    TYPE  I/O   DIM   DESCRIPTION                        *
C                                                                      *
C       FACTOR     R     I     1    CONVERSION FACTOR                  *
C                                                                      *
C       TFACT      R     I     1    TEMPERATURE CONVERSION FACTOR      *
C                                                                      *
C       VMISS      R     I     1    VALUE FOR MISSING DATA             *
C                                                                      *
C       VAL        R    I/O    1    VALUE TO CONVERT                   *
C                                                                      *
C***********************************************************************
C                                                                      *
C          COMMON:                                                     *
C                                                                      *
      INCLUDE 'udebug'
      INCLUDE 'uio'
      INCLUDE 'udsi'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/db_pdbrw/RCS/pdcvt1.f,v $
     . $',                                                             '
     .$Id: pdcvt1.f,v 1.1 1995/09/17 18:43:38 dws Exp $
     . $' /
C    ===================================================================
C
C                                                                      *
C***********************************************************************
C                                                                      *
C          DIMENSION AND TYPE DECLARATIONS:                            *
C                                                                      *
C                                                                      *
C***********************************************************************
C                                                                      *
C          DATA:                                                       *
C                                                                      *
C                                                                      *
C***********************************************************************
C
C
      IF(VAL.EQ.VMISS) GO TO 999
      VAL = VAL * FACTOR + TFACT
 999  CONTINUE
      IF(IPDTR.GT.1) WRITE(IOGDB,1000)
 1000 FORMAT(' *** EXIT PDCVT1')
      RETURN
      END
