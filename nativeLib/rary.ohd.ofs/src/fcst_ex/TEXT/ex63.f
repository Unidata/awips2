C MEMBER EX63
C
      SUBROUTINE EX63(PO,DD,NUM)
C.......................................
C     THIS SUBROUTINE EXECUTES THE SET-TS OPERATION.
C.......................................
C     SUBROUTINE INITIALLY WRITTEN BY. . .
C            JASON SPERFSLAGE - HRC     FEBRUARY 2001 
C.......................................
      DIMENSION PO(1)
      DIMENSION DD(1)
      COMMON/FDBUG/IODBUG,ITRACE,IDBALL,NDEBUG,IDEBUG(20)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_ex/RCS/ex63.f,v $
     . $',                                                             '
     .$Id: ex63.f,v 1.1 2002/05/16 13:04:41 dws Exp $
     . $' /
C    ===================================================================
C
C.......................................
C     TRACE LEVEL FOR THIS SUBROUTINE=1.
      IF (ITRACE.GE.1) WRITE(IODBUG,900)
  900 FORMAT (1H0,15H** EX63 ENTERED)
C     NO DEBUG OUTPUT.
C.......................................
C     SET USER-DEFINED CONSTANT VALUE FROM P().
      VAL=PO(6)
C.......................................
C     SET ENTIRE DATA ARRAY TO USER-DEFINED CONSTANT.
      DO 100 I=1,NUM
      DD(I)=VAL
  100 CONTINUE
C.......................................
      IF (ITRACE.GE.1) WRITE(IODBUG,901)
  901 FORMAT (1H0,12H** EXIT EX63)
C.......................................
      RETURN
      END
