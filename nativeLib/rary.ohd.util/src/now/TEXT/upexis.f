C MODULE UPEXIS
C  =====================================================================
C  pgm: UPEXIS(IUNIT,FILNM,IC) .. inquire file existance
C
C   in: IUNIT ...... desired logical unit number for file, else 0 - INT
C   in: FILNM ...... pathname of file for inquiry, else blank - CHAR*(*)
C  out: IC ......... status (-1=not exists, 0=exists, pos=error) - INT
C   in: (common) ... block common UPDAIO contains unit numbers for
C   in:              i/o routine messages (see subrtn UPRIMO)
C
C  rqd: common:  UPDAIO
C  =====================================================================
      SUBROUTINE UPEXIS(IUNIT,FILNM,IC)


      INTEGER        IUNIT,IC,LS,LU,LL
      LOGICAL        L1,L2,L3
      CHARACTER*(*)  FILNM
      CHARACTER*128  LN

      INCLUDE 'updaio'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/util/src/now/RCS/upexis.f,v $
     . $',                                                             '
     .$Id: upexis.f,v 1.3 2004/05/03 21:37:55 hank Exp $
     . $' /
C    ===================================================================
C
 

        IC = -1
        LS = -1
        LU = IUNIT
        LN = FILNM
        L1 = .FALSE.
        L2 = .FALSE.
        L3 = .FALSE.

C         Added April 2004 by hank to make the filename include
C         a process id, if the file is a 'TEMP.' file.
C         This assumes the file is to be placed in the /tmp dir.

        CALL UPNOFI(LN,1)
      
        IF(               LN.NE.'  ' ) L1 = .TRUE.
        IF( LU.GT.0 .AND. LN.EQ.'  ' ) L2 = .TRUE.

        IF(L1         ) INQUIRE(FILE=LN,ERR=100,IOSTAT=LS,EXIST=L3)
        IF(L1 .AND. L3) INQUIRE(FILE=LN,ERR=100,IOSTAT=LS,NAME=LN)
        IF(L2         ) INQUIRE(UNIT=LU,ERR=100,IOSTAT=LS,OPENED=L3)
        IF(L2 .AND. L3) INQUIRE(UNIT=LU,ERR=100,IOSTAT=LS,NAME=LN)
        IF(L3         )  IC = 0

  100   IF( LS.GT.0 ) IC = LS

        IF( UU.GE.0 .OR. UE.GE.0 ) LL = LENSTR(LN)
 
        IF( UU.GE.0 .AND. L1 .AND. IC.LT.0 ) WRITE(UU,1111) LN(1:LL)
        IF( UU.GE.0 .AND. L1 .AND. IC.EQ.0 ) WRITE(UU,1112) LN(1:LL)
        IF( UE.GE.0 .AND. L1 .AND. IC.GT.0 ) WRITE(UE,1113) IC,LN(1:LL)
        IF( UU.GE.0 .AND. L2 .AND. IC.LT.0 ) WRITE(UU,1114) LU
        IF( UU.GE.0 .AND. L2 .AND. IC.EQ.0 ) WRITE(UU,1115) LU,LN(1:LL)
        IF( UE.GE.0 .AND. L2 .AND. IC.GT.0 ) WRITE(UE,1116) LU,IC

 1111   FORMAT(' un-exi',4X,'     does not exist    ',A)
 1112   FORMAT(' un-exi',4X,'     exists            ',A)
 1113   FORMAT(' un-exi',4X,'  ** ERROR is',I5,'     ',A)
 1114   FORMAT(' un-exi',I4,'     does not exist    ')
 1115   FORMAT(' un-exi',I4,'     exists            ',A)
 1116   FORMAT(' un-exi',I4,'  ** ERROR is',I5,'     ')


      RETURN
      END
