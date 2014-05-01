C MEMBER PUC48
C-----------------------------------------------------------------------
C
C MULT/DIV OPERATION PUNCH ROUTINE
C
C OHIO RIVER FORECAST CENTER/DAN MULLEN 6/7/94 VER 1
C
C THIS PROGRAM PUNCHES THE PO ARRAY.
C
        SUBROUTINE PUC48 (PO)
C
C DECLARE VARIABLES AND COMMON BLOCKS
C
        COMMON /FDBUG/IODBUG,ITRACE,IDBALL,NDEBUG,IDEBUG(20)
        COMMON /IONUM/IN,IPR,IPU
        DIMENSION PO(*),SNAME(2)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_puc/RCS/puc48.f,v $
     . $',                                                             '
     .$Id: puc48.f,v 1.1 1996/07/11 20:42:04 dws Exp $
     . $' /
C    ===================================================================
C
        DATA SNAME/4hPRP4,4h8   /
C
C DESIGNATE TRACE LEVELS
C
        CALL FPRBUG(SNAME,1,48,IBUG)
        ITH=PO(5)
        IMULDIV=PO(12)
C
C PUNCH CARD INPUT
C
        WRITE(IPU,1001)(PO(I),I=2,4),ITH,(PO(I),I=6,8),ITH,
     &  (PO(I),I=9,11),IMULDIV
1001    FORMAT(2X,2A4,1X,A4,3X,I2,2X,2A4,1X,A4,3X,I2,/,2X,2A4,
     &  1X,A4,2X,I2)
        RETURN
        END
