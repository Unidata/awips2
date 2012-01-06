C MODULE PUC46
C-----------------------------------------------------------------------
C
      SUBROUTINE PUC46 (PO,CO)

C  THIS IS THE CARD PUNCH ROUTINE FOR THE NOMSNG OPERATION.
C
C   WRITTEN BY MIKE SMITH - 7/1995
C
      CHARACTER*8 RTNNAM,OLDNAM
      CHARACTER*10 CHAR
      DIMENSION PO(*),CO(*)
C
      INCLUDE 'common/ionum'
      INCLUDE 'common/fdbug'
      INCLUDE 'common/pudflt'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_puc/RCS/puc46.f,v $
     . $',                                                             '
     .$Id: puc46.f,v 1.5 2004/05/13 14:29:12 gzhou Exp $
     . $' /
C    ===================================================================
C
C
      RTNNAM='PUC46'
      IOPNUM=0
      CALL FSTWHR (RTNNAM,IOPNUM,OLDNAM,IOLDOP)
C
      LTRACE=1
      NUMOP=46
      CALL FPRBUG (RTNNAM,LTRACE,NUMOP,IBUG)

C  GET CONTROL VARIABLES
      ITH=PO(5)
      INTERP=PO(10)
      EXTRAP=PO(11)

      IF (IPDFLT.EQ.0) GO TO 30
C
C  PUNCH DEFAULT CARRYOVER
10    WRITE (IPU,20) (PO(I),I=2,4),ITH,(PO(I),I=6,8),INTERP,EXTRAP
20    FORMAT (2X,2A4,1X,A4,3X,I2,2X,2A4,1X,A4,4X,I1,1X,F4.2,4X,I1)
      GO TO 50
C
C  PUNCH ACTUAL CARRYOVER
30    IF ((CO(1).LT.-998.99).AND.(CO(1).GT.-999.01)) GO TO 10
C
C  PUNCH ACTUAL CARRYOVER
      NDEC=2
      IPRERR=1
      IPUNIT=IPR
      CALL UFF2A (CO(1),CHAR,1,LEN(CHAR),NDEC,IPRERR,IPUNIT,IERR)
      IF (IERR.NE.0) THEN
         WRITE (IPR,35) CO(1)
35    FORMAT ('0**ERROR** CANNOT CONVERT CARRYOVER VALUE ',F10.2,
     *   ' TO A CHARACTER STRING.')
         CALL ERROR
         ENDIF
      WRITE (IPU,20) (PO(I),I=2,4),ITH,(PO(I),I=6,8),INTERP,EXTRAP,1
c jgg  Added ITEMP=CO2 and changed WRITE, because mismatch between real 
c      variable and I format was causing an i/o error in Linux. (R21 5/7/02)      
      ITEMP=CO(2)
c jgg      WRITE (IPU,40) CHAR,CO(2)
      WRITE (IPU,40) CHAR, ITEMP
c gz   for bug r24-47 02/2004
c40    FORMAT (2X,A,1X,I2)
40    FORMAT (2X,A,1X,I4)
C
50    CALL FSTWHR (OLDNAM,IOLDOP,OLDNAM,IOLDOP)
C
      RETURN
C
      END
