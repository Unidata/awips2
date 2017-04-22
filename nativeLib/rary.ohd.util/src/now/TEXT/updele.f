C MODULE UPDELE
C  =====================================================================
C  pgm: UPDELE(IUNIT,FILNM,IC) .. delete any file
C
C   in: IUNIT ...... desired logical unit number for file, else 0 - INT
C   in: FILNM ...... pathname of file for inquiry, else blank - CHAR*(*)
C  out: IC ......... status (-1=not exists, 0=deleted, pos=error) - INT
C   in: (common) ... block common UPDAIO contains unit numbers for
C   in:              i/o routine messages (see subrtn UPRIMO)
C
C  rqd: common:  UPDAIO
C
C  cmt:  WARNING ... this routine requires a free unit number (1-99).
C  =====================================================================
      SUBROUTINE UPDELE(IUNIT,FILNM,IC)


      INTEGER        IUNIT,IC,LS,LU,UNFIX,LL
      LOGICAL        L1,L2,L3,L4
      CHARACTER*(*)  FILNM
      CHARACTER*128  LN

      INCLUDE 'updaio'

C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/util/src/now/RCS/updele.f,v $
     . $',                                                             '
     .$Id: updele.f,v 1.2 2001/06/12 19:11:06 dws Exp $
     . $' /
C    ===================================================================
C


        UNFIX = 99
   90   IF( UNFIX .GE. 1 ) GO TO 94
          IC = 99
          IF( UE.GE.0 ) WRITE(UE,1110)
 1110     FORMAT(' un-del    ','  ** ERROR - no units left')
            GO TO 900
   94   IF( UPRECL(UNFIX) .EQ. -1 ) GO TO 96
          UNFIX = UNFIX-1
            GO TO 90
   96   CONTINUE
 
        IC = -1
        LS = -1
        LU = IUNIT
        LN = FILNM
        L1 = .FALSE.
        L2 = .FALSE.
        L3 = .FALSE.
        L4 = .FALSE.
        IF(               LN.NE.'  ' ) L1 = .TRUE.
        IF( LU.GT.0 .AND. LN.EQ.'  ' ) L2 = .TRUE.

        IF(L1         ) INQUIRE(FILE=LN,ERR=100,IOSTAT=LS,EXIST=L3)
        IF(L1 .AND. L3) INQUIRE(FILE=LN,ERR=100,IOSTAT=LS,NAME=LN)
        IF(L1 .AND. L3) INQUIRE(FILE=LN,ERR=100,IOSTAT=LS,OPENED=L4)
        IF(L1 .AND. L4) INQUIRE(FILE=LN,ERR=100,IOSTAT=LS,NUMBER=LU)
        IF(L1 .AND. L4) CLOSE  (UNIT=LU,ERR=100,IOSTAT=LS)
        IF(L2         ) INQUIRE(UNIT=LU,ERR=100,IOSTAT=LS,OPENED=L4)
        IF(L2 .AND. L4)  L3 = .TRUE.
        IF(L2 .AND. L4) INQUIRE(UNIT=LU,ERR=100,IOSTAT=LS,NAME=LN)
        IF(L2 .AND. L4) CLOSE  (UNIT=LU,ERR=100,IOSTAT=LS)
        IF(L2 .AND. L4)  UNFIX = LU

        IF(L3) OPEN (UNIT=UNFIX,ERR=100,IOSTAT=LS,FILE=LN)
        IF(L3) CLOSE(UNIT=UNFIX,ERR=100,IOSTAT=LS,STATUS='DELETE')
        IF(L3)  IC = 0

  100   IF( LS.GT.0 ) IC = LS

        IF( UU.GE.0 .OR. UE.GE.0 ) LL=LENSTR(LN)
 
        IF(UU.GE.0 .AND. L1 .AND. IC.LT.0) WRITE(UU,1111) LN(1:LL)
        IF(UU.GE.0 .AND. L1 .AND. L4     ) WRITE(UU,1112) LU,LN(1:LL)
        IF(UU.GE.0 .AND. L1 .AND. L3     ) WRITE(UU,1122) UNFIX,LN(1:LL)
        IF(UU.GE.0 .AND. L1 .AND. IC.EQ.0) WRITE(UU,1113) UNFIX,LN(1:LL)
        IF(UE.GE.0 .AND. L1 .AND. IC.GT.0) WRITE(UE,1114) IC,LN(1:LL)
        IF(UU.GE.0 .AND. L2 .AND. IC.LT.0) WRITE(UU,1115) LU
        IF(UU.GE.0 .AND. L2 .AND. L4     ) WRITE(UU,1116) LU,LN(1:LL)
        IF(UU.GE.0 .AND. L2 .AND. L3     ) WRITE(UU,1126) UNFIX,LN(1:LL)
        IF(UU.GE.0 .AND. L2 .AND. IC.EQ.0) WRITE(UU,1117) UNFIX,LN(1:LL)
        IF(UE.GE.0 .AND. L2 .AND. IC.GT.0) WRITE(UE,1118) LU,IC

 1111   FORMAT(' un-del',4X,'     does not exist    ',A)
 1112   FORMAT(' un-del',I4,'     closed            ',A)
 1122   FORMAT(' un-del',I4,'     opened for delete ',A)
 1113   FORMAT(' un-del',I4,'     deleted           ',A)
 1114   FORMAT(' un-del',4X,'  ** ERROR is',I5,'     ',A)
 1115   FORMAT(' un-del',I4,'     does not exist    ')
 1116   FORMAT(' un-del',I4,'     closed            ',A)
 1126   FORMAT(' un-del',I4,'     opened for delete ',A)
 1117   FORMAT(' un-del',I4,'     deleted           ',A)
 1118   FORMAT(' un-del',I4,'  ** ERROR is',I5,'     ')

        IF( L1 .AND. L4 ) UPRECL(LU) = -1
        IF( L2 .AND. L4 ) UPRECL(LU) = -1

  900   CONTINUE
 
 
      RETURN
      END
