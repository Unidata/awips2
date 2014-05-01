C MEMBER PDCHSP
C  (from old member PDCHGSPC)
C***********************************************************************
C                                                                      *
C         MEMBER PDCHGSPC                                              *
C                                                                      *
C***********************************************************************
       SUBROUTINE PDCHSP(ISTAFL,IFLAG,ISTAT)
C***********************************************************************
C                                                                      *
C          SUBROUTINE:  PDCHSP                                         *
C                                                                      *
C             VERSION:  1.0.0                                          *
C                                                                      *
C                DATE:  6-29-83                                        *
C                                                                      *
C              AUTHOR:  JANINE FRANZOI                                 *
C                       DATA SCIENCES INC                              *
C                       8555 16TH ST, SILVER SPRING, MD 587-3700       *
C***********************************************************************
C                                                                      *
C          DESCRIPTION:                                                *
C                                                                      *
C    THIS DRIVER SUBROUTINE IS CALLED BY THE MAIN PPDB UTILITY DRIVER  *
C    TO READ THE COMMAND CARDS FOR THE CHNGSPEC COMMAND.  IN TURN      *
C    THE APPROPRIATE SUBROUTINE IS CALLED.  THE SPECIFICATIONS THAT    *
C    CAN BE CHANGED ARE: 1. MAXIMUM NUMBER OF DAYS FOR A DAILY DATA    *
C    TYPE, 2. THE FILE ON WHICH A DAILY DATA TYPE IS HELD, AND 3.      *
C    THE FILE ON WHICH THE RRS FREE POOL DATA ARE HELD.  WHEN THE      *
C    END CARD IS REACHED THIS DRIVER RETURNS TO THE MAIN DRIVER.       *
C                                                                      *
C***********************************************************************
C                                                                      *
C          ARGUMENT LIST:                                              *
C                                                                      *
C         NAME    TYPE  I/O   DIM   DESCRIPTION                        *
C                                                                      *
C       ISTAFL     I     I     1     STATION FLAG                      *
C                                     0 = 8-CHAR ID                    *
C                                     1 = STATION NUMBER               *
C       IFLAG      I     O     1     WRITE FLAG                        *
C                                     0 = DO NOT REWRITE CONTROLS      *
C                                     1 = RE-WRITE CONTROLS            *
C       ISTAT      I     O     1     STATUS CODE                       *
C                                     0 = NORMAL RETURN                *
C                                     OTHER = ERROR                    *
C                                                                      *
C***********************************************************************
C                                                                      *
C          COMMON:                                                     *
C                                                                      *
      INCLUDE 'uio'
      INCLUDE 'udsi'
      INCLUDE 'udebug'
      INCLUDE 'ufreei'
C                                                                      *
C***********************************************************************
C                                                                      *
C          DIMENSION AND TYPE DECLARATIONS:                            *
C                                                                      *
      DIMENSION LCMD(2,5)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/ppdutil/RCS/pdchsp.f,v $
     . $',                                                             '
     .$Id: pdchsp.f,v 1.1 1995/09/17 19:09:17 dws Exp $
     . $' /
C    ===================================================================
C
C                                                                      *
C***********************************************************************
C                                                                      *
C          DATA:                                                       *
C                                                                      *
      DATA NCMD/5/
      DATA LCMD/4hMAXD,4hAY  ,4hMIND,4hAY  ,4hTYPO,4hBS  ,
     1          4hDLYF,4hILE ,4hEND ,4h    /
C                                                                      *
C***********************************************************************
C
C
      ISTAT = 0
C
C  DEBUG
C
      IF(IPDTR.EQ.1) WRITE(IOGDB,10)
   10 FORMAT(' *** ENTER PDCHSP ')
C
   25 CONTINUE
C
C  READ A COMMAND CARD - CHECK FOR VALID COMMAND
C
      CALL RCMND2(LCMD,NCMD,INDX)
      IF(INDX.EQ.0) GO TO 30
C
C  EXECUTE THE COMMAND
C
      GO TO (100,200,300,400,500) , INDX
   30 CONTINUE
C
C  NOT A COMMAND
C
      WRITE(LPE,35)
   35 FORMAT(' ***ERROR*** IN PDCHSP. NOT A VALID COMMAND. CARD ',
     1       'IGNORED.')
      GO TO 25
C
  100 CONTINUE
C
C  CHANGE MAX # OF DAYS FOR A DAILY DATA TYPE
C
      CALL PDCMXD(IFLAG,ISTAT)
      IF(ISTAT.NE.0) GO TO 900
      GO TO 25
C
  200 CONTINUE
C
C  CHANGE MIN # OF DAYS FOR RRS STATION AND TYPE
C
      CALL PDCMND(ISTAFL,ISTAT)
      IF(ISTAT.NE.0) GO TO 900
      GO TO 25
C
  300 CONTINUE
C
C  CHANGE # OF OBSERVATIONS FOR RRS STATION AND RRS DATA TYPE
C
      CALL PDCHOB(ISTAFL,ISTAT)
      IF(ISTAT.NE.0) GO TO 900
      GO TO 25
C
  400 CONTINUE
C
C  CHANGE FILE FOR SPECIFIED DAILY DATA TYPE
C
      CALL PDLYFL(IFLAG,ISTAT)
      IF(ISTAT.NE.0) GO TO 900
      GO TO 25
C
  500 CONTINUE
C
C  RETURN TO MAIN DRIVER
C
      IF(IPDTR.EQ.1.OR.IPDDB.EQ.1) WRITE(IOGDB,550)
  550 FORMAT(' *** EXIT PDCHSP ')
      GO TO 999
  900 CONTINUE
C
C  SYSTEM ERROR
C
      WRITE(LPE,950) ISTAT
  950 FORMAT(' ***ERROR*** IN PDCHSP. STATUS = ',I2)
  999 CONTINUE
      RETURN
      END
