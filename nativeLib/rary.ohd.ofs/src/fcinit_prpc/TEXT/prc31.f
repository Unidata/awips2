C     MODULE PRC31
C.......................................
C     FOR OPERATION 31 (SNOW-43)
C     THIS IS THE PRINT CARRYOVER ROUTINE FOR THE 'SNOW-43 '
C     OPERATION.
C.......................................
C     ROUTINE INITIALLY WRITTEN BY...
C        ERIC ANDERSON - HRL   MAY 1980 for the 'SNOW-17' operation.  
C     Modified by...
C        Mark Woodbury - RTi, May 1995 for the 'SNOW-43' operation.
C     Modified by...
C        dws - took out non-standard variable digit format stuff
C              for "format(16x,f8.<idig(1)>, f8.<idig(2)> ... "
C.......................................
C    Argument       I/O         Description
C      PS            I          P, Parameter array
C      CS            I          Carry over array
C.......................................
      SUBROUTINE PRC31(PS,CS)

      REAL          PS(*),CS(*)
      INTEGER       idig(5)

      EXTERNAL      FMTDIG
      CHARACTER*1   FMTDIG
      CHARACTER*30  FORMT

C----- C O M M O N  B L O C K S -------------------------
C           what's in the common:
C             COMMON/IONUM/IN,IPR,IPU
C             COMMON/FDBUG/IODBUG,ITRACE,IDBALL,NDEBUG,IDEBUG(20)
C             COMMON/FCONIT/IVALUE
C
      INCLUDE 'common/ionum'
      INCLUDE 'common/fdbug'
      INCLUDE 'common/fconit'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_prpc/RCS/prc31.f,v $
     . $',                                                             '
     .$Id: prc31.f,v 1.2 2000/12/19 15:00:14 jgofus Exp $
     . $' /
C    ===================================================================
C
      DATA    FORMT  / '(16X,F8.0,F8.0,F8.0,F8.0,F8.0)' /

C.......................................
C     TRACE LEVEL=1, NO DEBUG OUTPUT
      IF(ITRACE.GE.1) WRITE(IODBUG,'(''0'',''** PRC31 ENTERED'')')
C.......................................
C     PRINT TITLE.
      WRITE(IPR,901) (PS(I),I=2,6)
  901 FORMAT('0',10X,'SNOW COVER CONDITIONS FOR ',5A4)
C.......................................
C     DETERMINE IF A SNOW COVER EXISTS.
      WE=CS(1)
      IF(WE.GT.0.0) GO TO 100

C     NO SNOW COVER
      WRITE(IPR,902)
  902 FORMAT('0',15X,'NO SNOW COVER EXISTS')
      RETURN

C     SNOW COVER EXISTS.
  100 ITPX=PS(10)
      NEXLAG=5/ITPX+2
      L=10+NEXLAG
      IF(IVALUE.EQ.0) GO TO 110
      IRDCO=PS(15)
      IF(IRDCO.EQ.2) GO TO 110

C     PRINT FIRST 6 VARIABLES ONLY.
      WRITE(IPR,903)
  903 FORMAT('0',20X,'WE  NEGHS   LIQW TINDEX ACCMAX AESC')
      WRITE(IPR,904) (CS(I),I=1,5), cs(l+1)
  904 FORMAT(' ',15X,F7.0,3F7.1,F7.0,f5.2,f7.0,F7.2,F7.0,F7.1,F7.0,
     1 7F5.1)
      RETURN

C     PRINT ALL VARIABLES
  110 WRITE(IPR,905)
  905 FORMAT('0',20X,'WE  NEGHS   LIQW TINDEX ACCMAX AESC     SB ',
     $       'SBAESC   SBWS STORGE  AEADJ     EXLAG')
      WRITE(IPR,904) (CS(I),I=1,5), cs(l+1), (cs(i), i=6,l)
      lkfpm = ps(33)
      if (lkfpm .eq. 0) go to 120

      write(ipr,906)
 906  FORMAT ('0', 20X, 'STATE ERROR COVARIANCE MATRIX (P)')

      do 115 i = 1, 5

         indx = 4
         do 112 j = 1,5
            indx = indx+5
            FORMT(indx:indx) = FMTDIG( cs(l+1+i+(j-1)*5), 8 )
 112     continue

         write(ipr,FORMT) (cs(l+1+i+(j-1)*5), j=1,5)
 115  continue

C.......................................
      IF(ITRACE.GE.1) WRITE(IODBUG,'(''0'',''**EXIT PRC31'')')

 120  RETURN
      END
