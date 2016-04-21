C MEMBER PRP48
C-----------------------------------------------------------------------
C
C MULT/DIV OPERATION PRINT ROUTINE
C
C OHIO RIVER FORECAST CENTER/DAN MULLEN 6/7/94 VER 1
C
C THIS PROGRAM PRINTS THE PO ARRAY
C
        SUBROUTINE PRP48 (PO)
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
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_prpc/RCS/prp48.f,v $
     . $',                                                             '
     .$Id: prp48.f,v 1.1 1996/07/11 20:41:43 dws Exp $
     . $' /
C    ===================================================================
C
        DATA SNAME/4hPRP4,4h8   /
C
C DESIGNATE TRACE LEVELS
C
        CALL FPRBUG(SNAME,1,48,IBUG)
        ITH=PO(5)
C
C PRINT OUTPUT
C
        IF(PO(12).EQ.0.01) GO TO 10
        WRITE(IPR,1001)(PO(I),I=2,4),ITH,(PO(I),I=6,8),ITH,
     &  (PO(I),I=9,11)
1001    FORMAT(1X,'TIME SERIES #1 I.D.=',2A4,3X,'TYPE=',A4,3X,
     &  'TIME INTERVAL=',I2,1X,'HOURS',
     &  /1X,'MULTIPLIED BY',/1X,'TIME SERIES #2 I.D.=',2A4,3X,
     &  'TYPE=',A4,3X,'TIME INTERVAL=',I2,1X,'HOURS',
     &  /1X,'EQUALS OUTPUT TIME SERIES I.D.=',2A4,3X,'TYPE=',A4)
        GO TO 20
10      WRITE(IPR,1002)(PO(I),I=2,4),ITH,(PO(I),I=6,8),ITH,
     &  (PO(I),I=9,11)
1002    FORMAT(1X,'TIME SERIES #1 I.D.=',2A4,3X,'TYPE=',A4,3X,
     &  'TIME INTERVAL=',I2,1X,'HOURS',
     &  /1X,'DIVIDED BY',/1X,'TIME SERIES #2 I.D.=',2A4,3X,
     &  'TYPE=',A4,3X,'TIME INTERVAL=',I2,1X,'HOURS',
     &  /1X,'EQUALS OUTPUT TIME SERIES I.D.=',2A4,3X,'TYPE=',A4)
20      RETURN
        END
