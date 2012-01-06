C MEMBER RMKC32
C  (from old member FCEX32)
C VERSION 1.10
C######################################################################
C######################################################################
C @PROCESS LVL(77)
      SUBROUTINE RMKC32(PR,CR,IDT,FI,STRA,STRO)
C
C.......................................................................
C  THIS ROUTINE RETRIEVES THE INITIAL DATA FROM THE API-MKC MODEL
C                   WHICH IS OPERATION NUMBER 29
C.......................................................................
C  INITIALY WRITTEN BY
C         JANICE LEWIS, HYDROLOGIC RESEARCH LAB, DEC 1991
C
C         TIM SWEENEY, HRL -                     DEC 1994
C            ADDED DEBUG AND TRACE
C.......................................................................
C  LIST OF VARIABLES:
C
C  NAME        DEFINITION
C  ______      _____________________________________
C  AI          CURRENT AI VALUE
C  CR          CARRYOVER ARRAY
C  FI          ADJUSTED STORM AI VALUE
C  FIADJ       DURATION ADJUSTMENT FOR FI
C  IAIADJ      AI ADJUSTMENT FACTOR
C  IAICO       STORM AI VALUE
C  IDT/FIDT    TIME STEP
C  ISTRA/STRA  STORM RAINFALL VALUE
C  ISTRO/STRO  STORM RUNOFF VALUE
C  PR          PARAMETER ARRAY
C.......................................................................
      DIMENSION PR(*),CR(*),SUBNAM(2)
C.......................................................................
      INCLUDE 'common/fdbug'
      INCLUDE 'common/ionum'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_ffg/RCS/rmkc32.f,v $
     . $',                                                             '
     .$Id: rmkc32.f,v 1.2 1995/11/14 20:30:20 erb Exp $
     . $' /
C    ===================================================================
C
C.......................................................................
      DATA SUBNAM /4HRMKC,4H32  /,NOP/32/
C.......................................................................
C
C  CALL DEBUG CHECK ROUTINE
C
      CALL FPRBUG(SUBNAM,1,NOP,IBUG)
C.......................................................................
C  GET INITIAL DATA FROM PARAMETER AND CARRYOVER ARRAYS
      FIDT=IDT
      ISTRA=CR(2)
      STRA = ISTRA*0.01
      IAICO=CR(3)
      ISTRO=CR(4)
      STRO = ISTRO*0.01
      IAIADJ= PR(12)
C.......................................................................
C  COMPUTE THE ADJUSTMENT FACTOR DUE TO THE CHANGE IN TIME STEP FOR FI
      FIADJ=0.05*(FIDT/6.)
C.......................................................................
C  COMPUTE FI
      IAICO = IAICO + IAIADJ
      IF(IAICO.LE.10) IAICO=11
      AI=IAICO
      FI=AI/10. + FIADJ
      RETURN
      END
