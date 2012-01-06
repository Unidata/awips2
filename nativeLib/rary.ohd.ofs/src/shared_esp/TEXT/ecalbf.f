C MODULE EECALBF
C-----------------------------------------------------------------------
C
      SUBROUTINE ECALBF(D,LD,TSESP,LOC,NPDT,IDLOOP,LJDCON,KNTYR,NYRS,
     1 IER)
C
C   THIS RROUTINE READS DATA FROM THE CALIBRATION FILES
C
C   THIS ROUTINE WAS ORIGINALLY WRITTEN BY GERALD N. DAY
C
cew stubbed outon workstations
C
C
      LOGICAL LBUG
C
      INCLUDE 'common/ionum'
      INCLUDE 'common/fdbug'
      INCLUDE 'common/where'
      INCLUDE 'clbcommon/crwctl'
      INCLUDE 'common/fctime'
      INCLUDE 'common/eswtch'
      INCLUDE 'common/esprun'
      COMMON/BHTIME/IBHREC,CMONTH,CDAY,CYEAR,CHRMN,CSCNDS,LTSHDR
C
      INTEGER CMONTH,CDAY,CYEAR,CHRMN,CSCNDS
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shared_esp/RCS/ecalbf.f,v $
     . $',                                                             '
     .$Id: ecalbf.f,v 1.2 2002/02/11 20:31:21 dws Exp $
     . $' /
C    ===================================================================
C
C
	write(IPR,*) 'Entered ECALBF which is stubbed off'

      RETURN
      END
