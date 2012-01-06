C MODULE MF27CB
C-----------------------------------------------------------------------
C
C  THIS ROUTINE FILLS THE MOD127 COMMON BLOCK WHICH IS USED IN THE 
C  LIST-FTW OPERATION (NUMBER 27)
C
      SUBROUTINE MF27CB (TSID,DTYPE,IDT,ISTART,NVALS,IBUG)
C
      INCLUDE 'common/mod127'
      INCLUDE 'common/ionum'
      INCLUDE 'common/fdbug'
C
      DIMENSION TSID(2)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_mods/RCS/mf27cb.f,v $
     . $',                                                             '
     .$Id: mf27cb.f,v 1.2 2001/06/13 12:14:54 mgm Exp $
     . $' /
C    ===================================================================
C
C
      IF(NTS27.LT.10)GO TO 10
C
C  NO SPACE IN MOD127 COMMON BLOCK
C
      WRITE(IPR,600)TSID,DTYPE,IDT,ISTART,NVALS
 600  FORMAT('0**WARNING** THE MOD127 COMMON BLOCK IS FULL.',
     1 '  THE ENTRY FOR TIME SERIES ',2A4,1X,A4,I3/11X,
     2 'STARTING AT HOUR ',I7,' FOR ',I3,' PERIODS CANNOT BE MADE.')
      CALL WARN
      GO TO 999
C
C  THERE IS ROOM - MAKE ENTRY IN COMMON
C
 10   NTS27=NTS27+1
      TSID27(1,NTS27)=TSID(1)
      TSID27(2,NTS27)=TSID(2)
      DTYP27(NTS27)=DTYPE
      IDT27(NTS27)=IDT
      ISTR27(NTS27)=ISTART
      NVAL27(NTS27)=NVALS
C
      IF(IBUG.GT.0)WRITE(IODBUG,900)NTS27,(TSID27(1,I),TSID27(2,I),
     1 DTYP27(I),IDT27(I),ISTR27(I),NVAL27(I),I=1,NTS27)
 900  FORMAT(11X,'IN SUBROUTINE MF27CB - ENTRY ',I2,' MADE TO COMMON ',
     1 'MOD127, THE COMMON BLOCK NOW CONTAINS'/(11X,2A4,1X,A4,I3,I8,I4))
C
 999  RETURN
      END
