C MEMBER RPDLRS
C  (from old member PDBUTILS)
C-----------------------------------------------------------------------
C
C                             LAST UPDATE: 10/20/94.16:16:57 BY $WC21DT
C
C @PROCESS LVL(77)
C
      SUBROUTINE RPDLRS (LWNEED,ISTAT)
C
C***********************************************************************
C
C          DESCRIPTION:
C
C    THIS ROUTINE RETURNS THE LENGTH OF WORK SPACE NEEDED TO READ
C    OR WRITE ANY RRS(RIVER,RESERVOIR,OR SNOW) DATA TYPE.
C***********************************************************************
C
C          ARGUMENT LIST:
C
C         NAME    TYPE  I/O   DIM   DESCRIPTION
C
C       LWNEED     I     O     1     LENGTH OF ARRAY
C       ISTAT      I     O     1     STATUS INDICATOR
C                                      0=OK,NORMAL RETURN
C                                      1=NO RRS DATA TYPES DEFINED
C
C***********************************************************************
C
C          COMMON:
C
      INCLUDE 'uio'
      INCLUDE 'udebug'
      INCLUDE 'pdbcommon/pdrrsc'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/db_pdbrw/RCS/rpdlrs.f,v $
     . $',                                                             '
     .$Id: rpdlrs.f,v 1.1 1995/09/17 18:44:42 dws Exp $
     . $' /
C    ===================================================================
C
C
C***********************************************************************
C
C
      ISTAT=0
C
      IF (IPDTR.GT.0) WRITE (IOGDB,30)
C
C  CALCULATE LENGTH OF ARRAY
      IF (MAXRSZ.EQ.0) GO TO 10
      LWNEED=MAXRSZ+2*IFREEL
      GO TO 20
C
C  NO RRS TYPES DEFINED
10    ISTAT=1
C
20    IF (IPDTR.GT.0) WRITE (IOGDB,40) ISTAT
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
30    FORMAT (' *** ENTER RPDLRS')
40    FORMAT (' *** EXIT RPDLRS - ISTAT=',I2)
C
      END
