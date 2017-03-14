C MEMBER SUGMDR
C-----------------------------------------------------------------------
C
      SUBROUTINE SUGMDR (ICOL,NCOL,LCOL,IROW,NROW,LROW,NMDRGP,IUNIT)
C
      INTEGER*2 NMDRGP(42,89),I2ARAY(42)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/ppinit_util/RCS/sugmdr.f,v $
     . $',                                                             '
     .$Id: sugmdr.f,v 1.1 1995/09/17 19:15:29 dws Exp $
     . $' /
C    ===================================================================
C
C
C
C  PRINT ROW AND COLUMN INFORMATION
      WRITE (IUNIT,50) ICOL,NCOL,LCOL,IROW,NROW,LROW
      CALL SULINE (IUNIT,1)
C
C  PRINT GRID BOX NUMBERS
      WRITE (IUNIT,60)
      CALL SULINE (IUNIT,1)
      WRITE (IUNIT,70) (I,I=ICOL,LCOL)
      CALL SULINE (IUNIT,1)
      WRITE (IUNIT,80)
      CALL SULINE (IUNIT,1)
      DO 20 IR=IROW,LROW
         DO 10 IC=ICOL,LCOL
            I2ARAY(IC-ICOL+1)=NMDRGP(IC-ICOL+1,IR)/100
10          CONTINUE
         WRITE (IUNIT,100) IR,(I2ARAY(N),N=1,NCOL)
         CALL SULINE (IUNIT,1)
20       CONTINUE
C
C  PRINT DEGREES FROM SE CORNER
      WRITE (IUNIT,90)
      CALL SULINE (IUNIT,1)
      WRITE (IUNIT,70) (I,I=ICOL,LCOL)
      CALL SULINE (IUNIT,1)
      WRITE (IUNIT,80)
      CALL SULINE (IUNIT,1)
      DO 40 IR=IROW,LROW
         DO 30 IC=ICOL,LCOL
            IVAL=NMDRGP(IC-ICOL+1,IR)
            IVAL=IABS(IVAL)
            I2ARAY(IC-ICOL+1)=IVAL-(IVAL/100*100)
30          CONTINUE
         WRITE (IUNIT,100) IR,(I2ARAY(N),N=1,NCOL)
         CALL SULINE (IUNIT,1)
40       CONTINUE
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
50    FORMAT (' ICOL=',I3,3X,'NCOL=',I3,3X,'LCOL=',I3,3X,
     *   'IROW=',I3,3X,'NCOL=',I3,3X,'LROW=',I3)
60    FORMAT (' GRID BOX NUMBERS')
70    FORMAT (6X,42I3)
80    FORMAT (6X,42('--+'))
90    FORMAT (' TENTHS OF DEGREE NORTH OF SE CORNER AND ',
     *   'AND WEST OF SE CORNER')
100   FORMAT (1H ,I3,': ',42I3)
C
      END
