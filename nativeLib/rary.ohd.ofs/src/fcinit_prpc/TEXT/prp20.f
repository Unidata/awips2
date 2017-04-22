C MEMBER PRP20
C  (from old member FCPRP20)
C
      SUBROUTINE PRP20(PO)
C ...................................................................
C
C     THIS IS THE PRINT PARAMETER SUBROUTINE FOR THE CHANGE
C       TIME INTERVAL OF TIME SERIES OPERATION
C ...................................................................
C
C        SUBROUTINE ORIGINALLY WRITTEN BY
C           ED VANBLARGAN - HRL  APRIL, 1981
C ...................................................................
      DIMENSION PO(1),ISUBN(2)
C
      COMMON/IONUM/IN,IPR,IPU
      COMMON/FDBUG/IODBUG,ITRACE,IDBALL,NDEBUG,IDEBUG(20)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_prpc/RCS/prp20.f,v $
     . $',                                                             '
     .$Id: prp20.f,v 1.1 1995/09/17 18:50:05 dws Exp $
     . $' /
C    ===================================================================
C
C
      DATA ISUBN,LTRACE,NOP/4HPRP2,4H0   ,1,20/
      DATA IYES,INO/4H YES,4H  NO/
C
C     UTILITY SUBROUTINE TO CHECK DEBUG AND TRACE LEVEL
C
      CALL FPRBUG(ISUBN,LTRACE,NOP,IBUG)
      NEEDP=14
C ...................................................................
C
C     DEBUG OUTPUT
C
      IF (IBUG.GE.1) WRITE (IODBUG,100) (PO(I),I=1,NEEDP)
100   FORMAT(1H0,10HPO ARRAY= ,
     *F4.0,1X,2A4,1X,A4,F4.0,1X,2A4,1X,A4,4F4.0,A4,1X,A4)
C ...................................................................
C
C ....SET VARIABLES
C
      ITA=PO(5)
      ITB=PO(9)
      ICASE=PO(12)
      NEEDC=PO(10)
      ICOPR=IYES
      IF (NEEDC.EQ.0) ICOPR=INO
C
C ....PRINT INFORMATION ON THE TIME SERIES
C
      WRITE(IPR,500) (PO(I),I=2,4),PO(13),ITA,(PO(I),I=6,8),PO(14),ITB
500   FORMAT(1H0,10X,60HFOLLOWING IS INFORMATION USED BY THE OPERATION
     $FOR CHANGING / 11X,35HTHE TIME INTERVAL OF A TIME SERIES.
     $// 11X,4HTIME,14X,4HDATA,4X,4HTIME,4X,4HTIME
     $/ 11X,6HSERIES,4X,4HI.D.,4X,4HTYPE,4X,5HSCALE,3X,14HINTERVAL(HRS.)
     $/ 11X,6H------,4X,4H----,4X,4H----,4X,5H-----,3X,8H--------
     $// 11X,3H1.),4X,2A4,3X,A4,5X,A4,4X,I4
     $ / 11X,3H2.),4X,2A4,3X,A4,5X,A4,4X,I4)
C
C CHECK NTRP OPTION
      IF (IABS(ICASE).NE.5) GO TO 590
      NOPT=IYES
      IF (ICASE.LT.0) NOPT=INO
      WRITE(IPR,550) NOPT
550   FORMAT(/ 11X,28HWILL THERE BE INTERPOLATION?,A4)
C CARRYOVER
590   WRITE (IPR,600) ICOPR
600   FORMAT(/ 11X,17HCARRYOVER NEEDED?,A4)
      RETURN
      END
