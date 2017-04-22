C MEMBER PRP63
C
      SUBROUTINE PRP63(TSC)
C.......................................
C THIS SUBROUTINE PRINTS THE INFORMATION ASSOCIATED
C    WITH THE SET-TS OPERATION.
C.......................................
C SUBROUTINE INITIALLY WRITTEN BY. . .
C        JASON SPERFSLAGE  HRC - FEBRUARY 2001
C.......................................
      DIMENSION TSC(1)
      COMMON/FDBUG/IODBUG,ITRACE,IDBALL,NDEBUG,IDEBUG(20)
      COMMON/IONUM/IN,IPR,IPU
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_prpc/RCS/prp63.f,v $
     . $',                                                             '
     .$Id: prp63.f,v 1.1 2002/05/15 13:41:22 hank Exp $
     . $' /
C    ===================================================================
C
C.......................................
C TRACE LEVEL FOR THIS SUBROUTINE=1.
      IF (ITRACE.GE.1) WRITE(IODBUG,900)
  900 FORMAT (1H0,16H** PRP63 ENTERED)
C     NO DEBUG OUTPUT.
C.......................................
C PRINT INFORMATION ON TIME SERIES THAT IS TO
C BE INITIALIZED.
      IDT = TSC(5)
      VAL = TSC(6)
      WRITE (IPR,901) (TSC(I),I=2,4),IDT,VAL
  901 FORMAT (1H0,10X,38HTHE FOLLOWING TIME SERIES WILL BE SET ,
     120HTO A CONSTANT VALUE.,//16X,5HI.D.=,2A4,3X,5HTYPE=,A4,3X,
     214HTIME INTERVAL=,I2,1X,5HHOURS,3X,6HVALUE=,E10.3)
C.......................................
      RETURN
      END
