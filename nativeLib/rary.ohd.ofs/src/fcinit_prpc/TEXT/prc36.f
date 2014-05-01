C MEMBER PRC36
C  (from old member FCPRC36)
C
      SUBROUTINE PRC36 (PL,CL)
C.......................................
C     THIS SUBROUTINE PRINTS THE STATE VARIABLES (CARRYOVER VALUES)
C        FOR THE XINANJINAG BASIN MODEL OPERATION.
C.......................................
C     SUBROUTINE INITIALLY WRITTEN BY. . .
C            QINGPING ZHU  - YRCC CHINA JUNE 1988   VERSION 1
C.......................................
      DIMENSION PL(1),CL(1)
C
C     COMMON BLOCKS.
      COMMON/FDBUG/IODBUG,ITRACE,IDBALL,NDEBUG,IDEBUG(20)
      COMMON/IONUM/IN,IPR,IPU
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_prpc/RCS/prc36.f,v $
     . $',                                                             '
     .$Id: prc36.f,v 1.1 1995/09/17 18:49:45 dws Exp $
     . $' /
C    ===================================================================
C
C.......................................
C     CHECK TRACE LEVEL -- TRACE LEVEL FOR THIS SUBROUTINE=1.
      IF (ITRACE.GE.1) WRITE (IODBUG,900)
  900 FORMAT (1H0,16H** PRC36 ENTERED)
C     NO DEBUG OUTPUT FOR THIS SUBROUTINE.
C.......................................
C     PRINT TITLE
      WRITE (IPR,901) (PL(I),I=4,8)
  901 FORMAT (1H0,10X,40HSOIL-MOISTURE AND DISCHARGE CONTENTS FOR,
     11X,5A4)
C
C     PRINT CARRYOVERS
      WRITE (IPR,902)
  902 FORMAT (1H0,12X,5HUNIT ,8X ,5H WUC ,3X,5H WLC ,3X,5H WDC ,3X,
     15H SC  ,3X,5H QIC ,3X,5H QGC ,3X,5H FRC )
      NUNIT=PL(3)
      J=0
      DO 114 I=1,NUNIT
      WRITE (IPR,903) I,(CL(K+J),K=1,7)
  903 FORMAT( 1H0,15X,I2,4X,4F8.1,2F8.2,F8.2)
      J=J+7
  114 CONTINUE
C.......................................
      RETURN
      END
