C MEMBER EPRG01
C  (from old member EEPRG01)
C
      SUBROUTINE EPRG01(PEINFO)
C
C ......................................................................
C
C          PRINT GENERATE 01 INFO
C
C THIS ROUTINE WILL PRINT OUT THE EXTERNAL LOCATION
C INFORMATION STORED IN THE TSESP ARRAY FOR A PE
C TIME SERIES WHICH WILL GENERATE DAILY PE VALUES
C FROM THE 1 DAILY(PER MONTH) VALUE STORED.
C
C
C
C SUBROUTINE ORIGINALLY BY :
C   ED VANBLARGAN - HRL - OCT, 1981
C
C ......................................................................
C
C
      DIMENSION PEINFO(1),SBNAME(2),OLDOPN(2)
C
      INCLUDE 'common/fdbug'
      INCLUDE 'common/ionum'
      INCLUDE 'common/where'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/espinit/RCS/eprg01.f,v $
     . $',                                                             '
     .$Id: eprg01.f,v 1.1 1995/09/17 18:46:27 dws Exp $
     . $' /
C    ===================================================================
C
C
      DATA NAME,SBNAME/4HEPRT,4HEPRG,4H01  /
C
C SET UP ERROR TRACE FOR THIS SUBROUTINE IN COMMON BLK/WHERE/
C
      IOLDOP=IOPNUM
      IOPNUM=0
      DO 10 I=1,2
        OLDOPN(I)=OPNAME(I)
        OPNAME(I)=SBNAME(I)
10    CONTINUE
C
C GET TRACE LEVEL AND DEBUG
C
      IF (ITRACE.GE.1) WRITE(IODBUG,50)
50    FORMAT(1H0,14HEPRG01 ENTERED)
      IBUG=IFBUG(NAME)
C
C PRINT OUT PE VALUE FOR EACH MONTH
C PEINFO IS ARRAY WHICH OCCUPIES LOCATIONS OF THE TSESP ARRAY
C STARTING WITH THE 3RD LOCATION OF EXTERNAL INFO LOCATIONS
C IN TSESP
C
      WRITE(IPR,200) (PEINFO(I),I=1,12)
200   FORMAT(/ 11X,36HDAILY PE FOR 16TH DAY OF EACH MONTH:
     * / 12X,45HJAN   FEB   MAR   APR   MAY  JUNE  JULY   AUG,
     * 24H   SEP   OCT   NOV   DEC
     * / 9X,12(2X,F4.1))
C
C SET ERROR TRACE IN CB/WHERE/ BACK THE WAY THEY WERE
C
      IOPNUM=IOLDOP
      OPNAME(1)=OLDOPN(1)
      OPNAME(2)=OLDOPN(2)
C
      RETURN
      END
