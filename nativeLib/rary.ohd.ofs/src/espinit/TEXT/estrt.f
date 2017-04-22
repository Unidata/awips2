C MEMBER ESTRT
C
C   THIS SUBROUTINE FILLS THE COMMON BLOCK ESPFLE
C   AND THE COMMON BLOCK FPROG.
C   IT ALSO CALLS RPDBCI TO INITIALIZE
C   COMMON BLOCKS FOR PROCESSED DATA BASE.
C
C   THIS SUBROUTINE WAS WRITTEN BY GERALD N DAY.
C
      SUBROUTINE ESTRT()
C
      INCLUDE 'common/espfle'
      INCLUDE 'common/esprec'
      INCLUDE 'common/eunit'
      INCLUDE 'common/where'
      INCLUDE 'common/fdbug'
      INCLUDE 'common/ionum'
      INCLUDE 'common/fprog'
C
      DIMENSION PNAMEX(5),OLDOPN(2),SBNAME(2)
      INTEGER   ISTAT
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/espinit/RCS/estrt.f,v $
     . $',                                                             '
     .$Id: estrt.f,v 1.2 1999/01/19 20:18:13 page Exp $
     . $' /
C    ===================================================================
C
C
      DATA SBNAME/4HESTR,4HT   /
      DATA SVDAT1/4H04/0/,SVDAT2/4H7/81/
      DATA PNAMEX/4HMAIN,4H PRO,4HGRAM,4H EIN,4HIT  /
C
      IOLDOP=IOPNUM
      IOPNUM=0
      OLDOPN(1) = OPNAME(1)
      OLDOPN(2) = OPNAME(2)
      OPNAME(1) = SBNAME(1)
      OPNAME(2) = SBNAME(2)
C
      IF(ITRACE.GE.1) WRITE(IODBUG,'('' ** ESTRT ENTERED'')')
C
C   READ FIRST RECORD OF ESP PARAMETER FILE INTO CB/ESPREC/
C   AND THEN TRANSFER INTO CB/ESPFLE/
C
      CALL UREADT(KEPARM,1,ESPDAT,ISTAT)
C
      IF (ISTAT .NE. 0) THEN
        WRITE(IPR,'(''0**ERROR** IN ESTRT FROM UREADT'')')
        CALL ERROR()
      ELSE
        MXREC=ESPDAT(1)
        NXREC=ESPDAT(2)
        LRECL=ESPDAT(3)
        MAINUM=2
        VERS=3.000
        VDATE(1)=SVDAT1
        VDATE(2)=SVDAT2
        PNAME(1) = PNAMEX(1)
        PNAME(2) = PNAMEX(2)
        PNAME(3) = PNAMEX(3)
        PNAME(4) = PNAMEX(4)
        PNAME(5) = PNAMEX(5)
        NDD=31
C
C   PROCESSED DATA BASE INITIALIZATION
C
        CALL RPDBCI(ISTAT)
        IF (ISTAT .NE. 0) THEN
          WRITE(IPR,'(''0**ERROR** FROM RPDBCI, ISTAT= '',I5)') ISTAT
          CALL ERROR()
        ENDIF
      ENDIF
C
      IOPNUM=IOLDOP
      OPNAME(1) = OLDOPN(1)
      OPNAME(2) = OLDOPN(2)
C
      RETURN
      END
