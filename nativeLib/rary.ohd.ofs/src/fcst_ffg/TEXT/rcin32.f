C MEMBER RCIN32
C  (from old member FCEX32)
C VERSION 1.10
C#######################################################################
C @PROCESS LVL(77)
      SUBROUTINE RCIN32(CR,FI,STRA,STRO)
C
C.......................................................................
C  THIS ROUTINE RETRIEVES THE INITIAL DATA FROM THE API-CIN MODEL
C                      WHICH IS OPERATION NUMBER 33
C.......................................................................
C  INITIALY WRITTEN BY
C        JANICE LEWIS, HYDROLOGIC RESEARCH LAB, DEC 1991
C
C        TIM SWEENEY, HRL                       DEC 1994
C           ADDED DEBUG AND TRACE
C.......................................................................
C  LIST OF VARIABLES:
C
C  NAME        DEFINITION
C  ______      _____________________________________
C  AI          CURRENT AI VALUE
C  CR          CARRYOVER ARRAY
C  FI          ADJUSTED STORM AI VALUE
C  PR          PARAMETER ARRAY
C  STRA        STORM RAINFALL VALUE
C  STRO        STORM RUNOFF VALUE
C.......................................................................
      DIMENSION CR(*),SUBNAM(2)
C.......................................................................
      INCLUDE 'common/fdbug'
      INCLUDE 'common/ionum'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_ffg/RCS/rcin32.f,v $
     . $',                                                             '
     .$Id: rcin32.f,v 1.2 1995/11/14 20:28:55 erb Exp $
     . $' /
C    ===================================================================
C
C.......................................................................
      DATA SUBNAM /4HRCIN,4H32  /,NOP/32/
C.......................................................................
C
C  CALL DEBUG CHECK ROUTINE
C
      CALL FPRBUG(SUBNAM,1,NOP,IBUG)
C.......................................................................
C
C  SELECT REQUIRED VARIABLES FROM THE CR ARRAY.
      STRA = CR(2)
      STRO = CR(4)
      AI   = CR(3)
      FI   = AI
C.......................................................................
C  NOTE THAT THE ADJUSTMENT PARAMETER IN ROUTINE EX33 (AIADJ)
C  IS INCLUDED IN AI.
C.......................................................................
      RETURN
      END
