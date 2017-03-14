C MEMBER RPDGRD
C  (from old member PDRPDGRD)
C-----------------------------------------------------------------------
C
       SUBROUTINE RPDGRD (ITYPE,MAXGRD,ISTAT)
C
C          SUBROUTINE:  RPDGRD
C
C             VERSION:  1.0.0
C
C                DATE: 12-12-86
C
C              AUTHOR:  SONJA R. SIEGEL
C                       DATA SCIENCES INC
C                       8555 16TH ST, SILVER SPRING, MD 587-3700
C***********************************************************************
C
C          DESCRIPTION:
C
C    THIS SUBROUTINE RETURNS THE MAXIMUM NUMBER OF VALUES THAT CAN
C    BE STORED ON THE PPDB FOR GRID-POINT RELATED DATA TYPES PG24 AND
C    APIG.
C***********************************************************************
C
C          ARGUMENT LIST:
C
C         NAME    TYPE  I/O   DIM   DESCRIPTION
C
C         ITYPE    A4    I     1     DATA TYPE 'PG24' OR 'APIG'
C         MAXGRD  I*4    O     1     MAX NUMBER OF VALUES CAN BE STORED*
C         ISTAT   I      O     1     STATUS INDICATOR
C                                       0 = OK, 1-ILLEGAL DATA TYPE
C                                       ZERO RETURNED FOR MAXGRD
C                                       2 = TYPE NOT DEFINED ON PPDB
C
C
C***********************************************************************
C
C          COMMON:
C
      INCLUDE 'uio'
      INCLUDE 'udsi'
      INCLUDE 'udebug'
      INCLUDE 'pdbcommon/pddtdr'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/db_pdbrw/RCS/rpdgrd.f,v $
     . $',                                                             '
     .$Id: rpdgrd.f,v 1.1 1995/09/17 18:44:38 dws Exp $
     . $' /
C    ===================================================================
C
C
C***********************************************************************
C
C          DIMENSION AND TYPE DECLARATIONS:
C
C
C***********************************************************************
C
C          DATA:
C
      DATA LPG24/4hPG24/,LAPIG/4hAPIG/
C
C***********************************************************************
C
C
      ISTAT = 0
      MAXGRD = 0
C
C  DEBUG
C
      IF (IPDTR.GT.0) WRITE (IOGDB,100)
  100 FORMAT(' SUBROUTINE RPDGRD ENTERED')
C
C  CHECK IF TYPE DEFINED AND GET INDEX
C
      IF (ITYPE.NE.LPG24 .AND. ITYPE.NE.LAPIG) GO TO 200
      IX = IPDCKD(ITYPE)
      IF (IX.NE.0.AND.IDDTDR(16,IX).GT.0) GO TO 300
  200 CONTINUE
      IF (IPDDB.GT.0) WRITE (LPE,201) ITYPE
  201 FORMAT(' **ERR0R** ',A4,' IS NOT DEFINED ON PPDB')
      ISTAT = 1
      GO TO 900
  300 CONTINUE
C
C  OBTAIN MAX NUMBER OF VALUES IN WORD 16 OF INDEX
C
      IF (IDDTDR(16,IX) .GT. 0) GO TO 400
      ISTAT = 2
      GO TO 900
400   CONTINUE
      MAXGRD = IDDTDR(16,IX) - 1
  900 CONTINUE
C
C  DEBUG AND RETURN
C
      IF (IPDTR.GT.0.OR.IPDDB.GT.0) WRITE (IOGDB,910) MAXGRD
  910 FORMAT(' RPDGRD EXECUTED. MAXGRD=',I7)
  999 CONTINUE
C
      RETURN
C
      END
