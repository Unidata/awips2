C MEMBER PDADJB
C  (from old member PDWPD1S)
C-----------------------------------------------------------------------
C
C                             LAST UPDATE: 07/11/95.07:36:37 BY $WC20SV
C
C @PROCESS LVL(77)
C
      SUBROUTINE PDADJB (IFHPDB,NDTWDS,IEDATE,JDATA,NINWDS,IFDAY,IHOFST,
     *   INCR)
C
C          ROUTINE:  PDADJB
C
C             VERSION:  1.0.0
C
C                DATE:  03-11-85
C
C              AUTHOR:  SONJA R SIEGEL
C                       DATA SCIENCES INC
C                       8555 16TH ST, SILVER SPRING, MD 587-3700
C***********************************************************************
C
C          DESCRIPTION:
C
C
C  THIS ROUTINE ADJUSTS THE POINTER FOR DATA WHEN THE FIRST
C  DAY TO BE WRITTEN IS EARLIER THAN THE FIRST DAY ON FILE, BUT
C  SOME OF THE DATA IS OKAY.
C
C***********************************************************************
C
C          ARGUMENT LIST:
C
C         NAME    TYPE  I/O   DIM   DESCRIPTION
C
C        IFHPDB    I    I     1     FIRST HOUR OF DATA IN FILE TIME
C        NDTWDS    I    I     1     NUMBER OF WORDS OF DATA FOR 1 DAY
C        IEDATE    I    I     1     FIRST DAY OF DATA ON FILE
C        JDATA     I   I/O    1     POINTER TO DATA IN INPUT ARRAY
C        NINWDS    I   I/O    1     NUMBER OF WORDS OF DATA FOR THIS
C                                       TYPE
C        IFDAY     I   I/O    1     JULIAN DATE OF FIRST DATA
C        IHOFST    I   I/O    1     OFFSET OF FIRST HOUR OF DATA & 12Z
C        INCR      I    I     1     INCR FOR WORDS OF DATA,1 FOR ALL
C                                                    EXCEPT 2 FOR TM24
C
C*********************************************************************
C
      INCLUDE 'udebug'
      INCLUDE 'hclcommon/hdflts'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/db_pdbrw/RCS/pdadjb.f,v $
     . $',                                                             '
     .$Id: pdadjb.f,v 1.1 1995/09/17 18:43:27 dws Exp $
     . $' /
C    ===================================================================
C
C
C
      IF (IPDTR.GT.0) WRITE (IOGDB,10)
10    FORMAT (' *** ENTER PDADJB')
C
      NVALS=0
      IDELTA=24/NDTWDS*INCR
C
20    NVALS=NVALS+INCR
      IFHPDB=IFHPDB+IDELTA
      IFDAY=((IFHPDB+23)/24)+1
      IF (IFDAY.LT.IEDATE) GO TO 20
C
      JDATA=JDATA+NVALS
      NINWDS=NINWDS-NVALS
      IHOFST=MOD(IFHPDB,24)
C
      IF (IPDDB.GT.0) WRITE (IOGDB,30) IFHPDB,NDTWDS,IEDATE,JDATA,
     *      NINWDS,IFDAY,IHOFST,NVALS,INCR,IDELTA
30    FORMAT (' PDADJB,IFHOUR,NDTWDS,IEDATE,JDATA,NINWDS,IFDAY,',
     *       'IHOFST,NVALS,INCR,IDELTA='/1X,10I6)
C
      IF (IPDTR.GT.0) WRITE (IOGDB,40)
40    FORMAT (' *** EXIT PDADJB')
C
      RETURN
C
      END
