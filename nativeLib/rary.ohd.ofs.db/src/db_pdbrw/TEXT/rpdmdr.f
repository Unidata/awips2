C MEMBER RPDMDR
C  (from old member PDRPDMDR)
C***********************************************************************
C                                                                      *
C         MEMBER PDRPDMDR                                              *
C                                                                      *
C***********************************************************************
       SUBROUTINE RPDMDR(MAXMDR,ISTAT)
C***********************************************************************
C                                                                      *
C          SUBROUTINE:  RPDMDR                                         *
C                                                                      *
C             VERSION:  1.0.0                                          *
C                                                                      *
C                DATE:  10-31-83                                       *
C                                                                      *
C              AUTHOR:  JANINE FRANZOI                                 *
C                       DATA SCIENCES INC                              *
C                       8555 16TH ST, SILVER SPRING, MD 587-3700       *
C***********************************************************************
C                                                                      *
C          DESCRIPTION:                                                *
C                                                                      *
C    THIS SUBROUTINE RETURNS THE MAXIMUM NUMBER OF 6-HOUR MDR VALUES   *
C    THAT CAN BE STORED IN THE PREPROCESSOR DATA BASE.                 *
C    (THIS VALUE IS ALSO THE MAXIMUM NUMBER OF STATIONS)             *
C                                                                      *
C***********************************************************************
C                                                                      *
C          ARGUMENT LIST:                                              *
C                                                                      *
C         NAME    TYPE  I/O   DIM   DESCRIPTION                        *
C                                                                      *
C       MAXMDR     I    O      1     MAX # VALUES STORED               *
C       ISTAT      I    O      1     STATUS CODE                       *
C                                     0 = NORMAL RETURN                *
C                                     1 = TYPE UNDEFINED;MAXMDR = 0    *
C                                                                      *
C***********************************************************************
C                                                                      *
C          COMMON:                                                     *
C                                                                      *
      INCLUDE 'uio'
      INCLUDE 'udsi'
      INCLUDE 'udebug'
      INCLUDE 'pdbcommon/pddtdr'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/db_pdbrw/RCS/rpdmdr.f,v $
     . $',                                                             '
     .$Id: rpdmdr.f,v 1.1 1995/09/17 18:44:43 dws Exp $
     . $' /
C    ===================================================================
C
C                                                                      *
C***********************************************************************
C                                                                      *
C          DATA:                                                       *
C                                                                      *
      DATA MDR6/4hMDR6/
C                                                                      *
C***********************************************************************
C
C  DEBUG
C
      IF(IPDTR.EQ.1) WRITE(IOGDB,50)
   50 FORMAT(' ENTER RPDMDR')
C
      ISTAT = 0
      MAXMDR = 0
C
C  FIND TYPE IN DIRECTORY
C
      IDX = IPDCKD(MDR6)
      IF(IDX.EQ.0) GO TO 100
C
C  FIND MAXIMUM NUMBER OF VALUES(STATIONS)
C
      MAXMDR = IDDTDR(16,IDX)
      IF(MAXMDR.NE.0) GO TO 200
C
C  MDR NOT DEFINED ON PPDB
C
  100 ISTAT = 1
C
  200 CONTINUE
C
C  DEBUG
C
      IF(IPDTR.EQ.1.OR.IPDDB.EQ.1) WRITE(IOGDB,250)
  250 FORMAT(' EXIT RPDMDR')
  999 CONTINUE
      RETURN
      END
