C MEMBER RHFD32
C  (from old member FCEX32)
C VERSION 1.10
C-----------------------------------------------------------------------
C
C                             LAST UPDATE: 06/08/95.14:08:22 BY $WC20SV
C
C######################################################################
C######################################################################
C @PROCESS LVL(77)
C
      SUBROUTINE RHFD32(PR,CR,SFI,SRAIM,SRO,RFCTR,NREL)
C.......................................................................
C  THIS ROUTINE RETRIEVES THE INITIAL DATA FROM THE API-HFD MODEL
C                   WHICH IS OPERATION NUMBER 43
C.......................................................................
C  INITIALY WRITTEN BY
C         TIM SWEENEY, HYDROLOGIC RESEARCH LAB,         FEB 1995
c
c  Updated parameters to match API-HFD operation
c         Tim Sweeney  -  HRL                           Oct 1995
C.......................................................................
C  LIST OF VARIABLES:
C
C  NAME    DEFINITION
C  ______  _________________________________________
C  NREL    GEOGRAPHICAL RELATIONSHIP NUMBER
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
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_ffg/RCS/rhfd32.f,v $
     . $',                                                             '
     .$Id: rhfd32.f,v 1.3 1996/03/21 15:52:56 page Exp $
     . $' /
C    ===================================================================
C
C.......................................................................
      DATA SUBNAM /4HRHFD,4H43  /,NOP/43/
C.......................................................................
C
C  CALL DEBUG CHECK ROUTINE
C
      CALL FPRBUG(SUBNAM,1,NOP,IBUG)
C.......................................................................
C  SELECT REQUIRED VARIABLES FROM THE PR AND CR ARRAYS.
      NREL  = PR(23)
      RFCTR = PR(12)
      SFI   = CR(6)
      SRAIM = CR(7)
      SRO   = CR(8)
      RETURN
      END
