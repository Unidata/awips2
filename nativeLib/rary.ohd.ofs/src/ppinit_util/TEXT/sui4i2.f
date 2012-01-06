C MEMBER SUI4I2
C-----------------------------------------------------------------------
C
C  ROUTINE SUI4I2 CONVERTS VALUES FROM INTEGER*4 TO INTEGER*2
C
      SUBROUTINE SUI4I2 (INT4,NPOS4,INT2,NPOS2,LDEBUG)
C
      INTEGER*2 INT2(1)
      INTEGER*4 INT4(1)
C
      INCLUDE 'scommon/sudbgx'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/ppinit_util/RCS/sui4i2.f,v $
     . $',                                                             '
     .$Id: sui4i2.f,v 1.1 1995/09/17 19:15:40 dws Exp $
     . $' /
C    ===================================================================
C
C
C
      INT2(NPOS2)=INT4(NPOS4)
      IF (LDEBUG.GT.1) WRITE (IOSDBG,20) NPOS4,INT4(NPOS4),
     *   NPOS2,INT2(NPOS2)
      IF (LDEBUG.GT.1) CALL SULINE (IOSDBG,1)
C
      RETURN
C
20    FORMAT (' NPOS4=',I5,3X,'INT4(NPOS4)=',I6,3X,
     *   'NPOS2=',I5,3X,'INT2(NPOS2)=',I6)
C
      END
