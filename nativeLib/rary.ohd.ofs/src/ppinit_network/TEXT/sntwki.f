C MODULE SNTWKI
C-----------------------------------------------------------------------
C
C  ROUTINE TO SET NETWORK INDICATORS.
C
      SUBROUTINE SNTWKI (IPCPN,ITPPVR,IPTWGTI,IPSORT,
     *   ITEMP,ITTAVR,ITFMM,ITSORT,
     *   IPE,IESORT,
     *   IRRS,
     *   NUGPA,
     *   SORTBY,
     *   IAREA,
     *   ISORTBY,
     *   INTWKI,ISTAT)
C
      CHARACTER*4 SORTBY
C
      INCLUDE 'uio'
      INCLUDE 'scommon/sudbgx'
      INCLUDE 'scommon/sntwfx'
      INCLUDE 'scommon/sntwkx'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/ppinit_network/RCS/sntwki.f,v $
     . $',                                                             '
     .$Id: sntwki.f,v 1.2 2000/12/18 23:01:09 dws Exp $
     . $' /
C    ===================================================================
C
C
      IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,*) 'ENTER SNTWKI'
         CALL SULINE (IOSDBG,1)
         ENDIF
C
C  SET DEBUG LEVEL
      LDEBUG=ISBUG('DEFN')
C
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,*)
     *      ' IPCPN=',IPCPN,
     *      ' ITEMP=',ITEMP,
     *      ' IPE=',IPE,
     *      ' IRRS=',IRRS,
     *      ' NUGPA=',NUGPA,
     *      ' SORTBY=',SORTBY,
     *      ' IAREA=',IAREA,
     *      ' ISORTBY=',ISORTBY,
     *      ' '
         CALL SULINE (IOSDBG,1)
         WRITE (IOSDBG,*)
     *      ' ITPPVR=',ITPPVR,
     *      ' IPTWGTI=',IPTWGTI,
     *      ' IPSORT=',IPSORT,
     *      ' ITTAVR=',ITTAVR,
     *      ' ITFMM=',ITFMM,
     *      ' ITSORT=',ITSORT,
     *      ' IESORT=',IESORT,
     *      ' '
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      ISTAT=0
C
      INTWKI=0
C
C  CHECK IF SORT BY DESCRIPTION SPECIFIED
      IF (SORTBY.EQ.'DESC') THEN
         IF (IPCPN.EQ.1) THEN
            INWFLG(12)=1
            INTWKI=1
            ENDIF
         IF (IPCPN.EQ.1.AND.ITPPVR.GT.0) THEN
            INWFLG(13)=1
            INTWKI=1
            ENDIF
         IF (ITEMP.EQ.1) THEN
            INWFLG(14)=1
            INTWKI=1
            ENDIF
         IF (IPE.EQ.1) THEN
            INWFLG(15)=1
            INTWKI=1
            ENDIF
         IF (IRRS.EQ.1) THEN
            INWFLG(16)=1
            INTWKI=1
            ENDIF
         IF (NUGPA.GT.0) THEN
            INWFLG(17)=1
            INTWKI=1
            ENDIF
         GO TO 10
         ENDIF
C
C  CHECK IF STATION HAS PCPN PARAMETERS
      IF (IPCPN.EQ.1) THEN
         INWFLG(1)=1
         IF (ITPPVR.GT.0) INWFLG(2)=1
         IF (ITPPVR.GT.0) INWFLG(6)=1          
         IF (IPTWGTI.EQ.1) INWFLG(7)=1
         IF (IPSORT.EQ.1) THEN
            INWFLG(12)=1
            IF (ITPPVR.GT.0) INWFLG(13)=1
            ENDIF
         INTWKI=1
         ENDIF
C
C  CHECK IF STATION HAS TEMP PARAMETERS
      IF (ITEMP.EQ.1) THEN
         INWFLG(3)=1
         IF (ITTAVR.GT.0.AND.ITTAVR.LT.24) INWFLG(4)=1
         IF (ITFMM.EQ.1) INWFLG(5)=1
         INWFLG(8)=1
         IF (ITSORT.EQ.1) INWFLG(14)=1
         INTWKI=1
         ENDIF
C
C  CHECK IF STATION HAS PE PARAMETERS
      IF (IPE.EQ.1) THEN
         INWFLG(11)=1
         IF (IESORT.EQ.1) INWFLG(15)=1
         INTWKI=1
         ENDIF
C
C  CHECK IF STATION HAS RRS PARAMETERS
      IF (IRRS.EQ.1) THEN
         INWFLG(16)=1
         INTWKI=1
         ENDIF
C
C  CHECK IF STATION HAS GRID POINT ADDRESS
      IF (NUGPA.GT.0) THEN
         INWFLG(17)=1
         INTWKI=1
         ENDIF
C
C  CHECK IF TO SET AREA INDICATORS
      IF (IAREA.EQ.1) THEN
         INWFLG(6)=1
         INWFLG(7)=1
         INWFLG(8)=1
         INWFLG(11)=1
         INTWKI=1
         ENDIF
C
C  CHECK IF TO SET ALPHABETICAL ORDER INDICATORS
      IF (ISORTBY.EQ.1) THEN
         INWFLG(12)=1
         INWFLG(13)=1
         INWFLG(14)=1
         INWFLG(15)=1
         INWFLG(16)=1
         INWFLG(17)=1
         INTWKI=1
         ENDIF
C
C  SET INDICATOR THAT NETWORK COMMON BLOCK HAS BEEN FILLED
10    IF (INTWKI.EQ.1) THEN
         INWFIL=0
         ENDIF
C
      IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,*) 'EXIT SNTWKI'
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      RETURN
C
      END
