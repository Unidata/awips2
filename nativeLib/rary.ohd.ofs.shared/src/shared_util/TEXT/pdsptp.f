C MEMBER PDSPTP
C  (from old member PDSPTYPE)
C***********************************************************************
C                                                                      *
C         MEMBER PDSPTYPE                                              *
C                                                                      *
C***********************************************************************
       SUBROUTINE PDSPTP(LDTYPE,MAXSTA,IX,MXDP,MXDT,IFLP,IFLT,NWORDP,
     1                   NWORDT,IERFLG)
C***********************************************************************
C                                                                      *
C          SUBROUTINE:  PDSPTP                                         *
C                                                                      *
C             VERSION:  1.0.0                                          *
C                                                                      *
C                DATE:  1-18-83                                        *
C                                                                      *
C              AUTHOR:  JANINE FRANZOI                                 *
C                       DATA SCIENCES INC                              *
C                       8555 16TH ST, SILVER SPRING, MD 587-3700       *
C***********************************************************************
C                                                                      *
C          DESCRIPTION:                                                *
C                                                                      *
C    THIS SUBROUTINE IS CALLED BY THE MAIN PROGRAMS PPDBINIT AND       *
C    PDBFSIZE TO CALCULATE THE NUMBER OF DATA WORDS FOR THE            *
C    PRECIPITATION AND TEMPERATURE DATA TYPES, USING THE DELTA T       *
C    VALUES FOR EACH RESPECTIVE TYPE. THE TOTAL DATA WORDS ARE         *
C    RETURNED TO THE CALLING PROGRAMS TO CALCULATE THE DAILY DATA      *
C    RECORDS FOR EACH TYPE.                                            *
C                                                                      *
C***********************************************************************
C                                                                      *
C          ARGUMENT LIST:                                              *
C                                                                      *
C         NAME    TYPE  I/O   DIM   DESCRIPTION                        *
C                                                                      *
C        LDTYPE     A     I     1     DAILY DATA TYPE                  *
C        MAXSTA     I     I     1     MAX # OF STATIONS                *
C        IX         I     I     1     SUBSCRIPT OF TYPE IN DIRECTORY   *
C        MXDP       I     I     1     # DAYS FOR PRCP TYPE             *
C        MXDT       I     I     1     # DAYS FOR TEMP TYPE             *
C        IFLP       I     I     1     FILE # OF PRCP DATA              *
C        IFLT       I     I     1     FILE # OF TEMP DATA              *
C        NWORDP     I     O     1     # DATA WRDS FOR PRCP TYPE        *
C        NWORDT     I     O     1     # DATA WRDS FOR TEMP TYPE        *
C        IERFLG     I     O     1     ERROR FLAG                       *
C                                        0 = NORMAL RETURN             *
C                                        1 = ERROR                     *
C                                                                      *
C***********************************************************************
C                                                                      *
C          COMMON:                                                     *
C                                                                      *
      INCLUDE 'uio'
      INCLUDE 'udebug'
      INCLUDE 'pdbcommon/pddtdr'
C                                                                      *
C***********************************************************************
C                                                                      *
C          DIMENSION AND TYPE DECLARATIONS:                            *
C                                                                      *
      DIMENSION IPP(6),IDLT(6)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shared_util/RCS/pdsptp.f,v $
     . $',                                                             '
     .$Id: pdsptp.f,v 1.1 1995/09/17 19:24:24 dws Exp $
     . $' /
C    ===================================================================
C
C                                                                      *
C***********************************************************************
C                                                                      *
C          DATA:                                                       *
C                                                                      *
      DATA IPP/4hPP01,4hPP03,4hPP06,4hTA01,4hTA03,4hTA06/,
     1     IDLT/24,8,4,24,8,4/
C***********************************************************************
C  DEBUG
C
      IF(IPPTR.EQ.1) WRITE(IOGDB,50)
  50  FORMAT(' SUBROUTINE PDSPTP ENTERED')
C
C CHECK FOR DATA TYPE
C
      DO 100 I = 1,6
        IF(LDTYPE.EQ.IPP(I)) GO TO 200
  100 CONTINUE
      GO TO 205
  200 CONTINUE
C
C  CHECK IF TEMP OR PRECIP TYPE
C
      IF(I.GE.1.AND.I.LE.3) GO TO 300
      IF(I.GE.4.AND.I.LE.6) GO TO 350
C
C  ERROR
C
  205 WRITE(LPE,210)
  210 FORMAT(' **ERROR** FROM PDSPTP. INVALID DATA TYPE')
      IERFLG = 1
      GO TO 999
  300 CONTINUE
C
C  PROCESS PRECIPITATION DATA TYPE- CHECK IF FIRST TIME THROUGH
C
      IF(MXDP.EQ.0) MXDP = IDDTDR(7,IX)
      IF(IFLP.EQ.0) IFLP = IDDTDR(4,IX)
C
C  CHECK # OF DAYS SAME FOR ALL TYPES
C
      IF(MXDP.EQ.IDDTDR(7,IX)) GO TO 320
      WRITE(LPE,310) IPP(I)
  310 FORMAT(' **ERROR** FROM PDSPTP. INVALID # OF DAYS FOR DATA',
     1       ' TYPE ',A4)
      IERFLG = 1
      GO TO 999
  320 CONTINUE
C
C  CHECK IF FILE THE SAME FOR ALL TYPES
C
      IF(IFLP.EQ.IDDTDR(4,IX)) GO TO 340
      WRITE(LPE,330) IPP(I)
  330 FORMAT(' **ERROR** FROM PDSPTP. INVALID FILE FOR DATA',
     1      ' TYPE ',A4)
      IERFLG = 1
      GO TO 999
  340 CONTINUE
C
C  OBTAIN DELT T VALUE FOR CALCULATION
C
      NWORDP = NWORDP + MAXSTA * IDLT(I)
      GO TO 999
  350 CONTINUE
C
C  PROCESS TEMPERATURE DATA - CHECK IF FIRST TIME THROUGH
C
      IF(MXDT.EQ.0) MXDT = IDDTDR(7,IX)
      IF(IFLT.EQ.0) IFLT = IDDTDR(4,IX)
C
C  CHECK IF # OF DAYS SAME FOR ALL TYPES
C
      IF(MXDT.EQ.IDDTDR(7,IX)) GO TO 360
      WRITE(LPE,310) IPP(I)
      IERFLG = 1
      GO TO 999
  360 CONTINUE
C
C  CHECK IF FILE IS THE SAME FOR ALL TYPES
C
      IF(IFLT.EQ.IDDTDR(4,IX)) GO TO 370
      WRITE(LPE,330) IPP(I)
      IERFLG = 1
      GO TO 999
  370 CONTINUE
C
C  CALCULATE # OF DATA WORDS FOR DATA RECORDS
C
      NWORDT = NWORDT + MAXSTA * IDLT(I)
C
C  DEBUG AND RETURN
C
      IF(IPPTR.EQ.1.OR.IPPDB.EQ.1) WRITE(IOGDB,400)
  400 FORMAT(' PDSPTP EXECUTED. PRECIP AND TEMP TYPES PROCESSED.')
  999 CONTINUE
      RETURN
      END
