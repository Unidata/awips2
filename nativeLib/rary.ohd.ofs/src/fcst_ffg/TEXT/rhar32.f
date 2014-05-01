C MEMBER RHAR32
C  (from old member FCEX32)
C VERSION 1.10
C######################################################################
C @PROCESS LVL(77)
      SUBROUTINE RHAR32(PR,CR,SFI,SRAIM,SRO,RFCTR)
C.......................................................................
C  THIS ROUTINE RETRIEVES THE INITIAL DATA FROM THE API-HAR MODEL
C                   WHICH IS OPERATION NUMBER 35
C.......................................................................
C  INITIALY WRITTEN BY
C         JANICE LEWIS, HYDROLOGIC RESEARCH LAB, DEC 1991
C.......................................................................
C  LIST OF VARIABLES:
C
C  NAME    DEFINITION
C  ______  _________________________________________
C  RFCTR   RUNOFF ADJUSTMENT FACTOR
C  SFI     CURRENT STORM FI VALUE
C  SRAIM   CURRENT STORM RAIN/MELT
C  SRO     CURRENT STORM RUNOFF
C.......................................................................
      DIMENSION PR(*),CR(*),SUBNAM(2)
C.......................................................................
      INCLUDE 'common/fdbug'
      INCLUDE 'common/ionum'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_ffg/RCS/rhar32.f,v $
     . $',                                                             '
     .$Id: rhar32.f,v 1.2 1995/11/14 20:29:36 erb Exp $
     . $' /
C    ===================================================================
C
C.......................................................................
      DATA SUBNAM /4HRHAR,4H32  /,NOP/32/
C.......................................................................
C
C  CALL DEBUG CHECK ROUTINE
C
      CALL FPRBUG(SUBNAM,1,NOP,IBUG)
C.......................................................................
C  SELECT REQUIRED VARIABLES FROM THE PR AND CR ARRAYS.
      RFCTR=PR(12)
      SFI=CR(6)
      SRAIM=CR(7)
      SRO=CR(8)
      RETURN
      END
