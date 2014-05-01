C MODULE UPCLOS
C  =====================================================================
C  pgm: UPCLOS(IUNIT,FILNM,IC) .. close any file
C
C   in: IUNIT ...... desired logical unit number for file, else 0 - INT
C   in: FILNM ...... pathname of file to be closed, else blank - CHAR*(*)
C  out: IC ......... status (-1=not open, 0=closed, pos=error) - INT
C   in: (common) ... block common UPDAIO contains unit numbers for
C   in:              i/o routine messages (see subrtn UPRIMO)
C
C  rqd: common:  UPDAIO
C  =====================================================================
      SUBROUTINE UPCLOS(IUNIT,FILNM,IC)


      INTEGER        IUNIT,J,IC,LS,LU,LL
      LOGICAL        L1,L2,L3
      CHARACTER*(*)  FILNM
      CHARACTER*128  LN

      INCLUDE 'updaio'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/util/src/now/RCS/upclos.f,v $
     . $',                                                             '
     .$Id: upclos.f,v 1.5 2002/02/11 16:52:23 dws Exp $
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
        IF(LU.LE.0 .AND. LN.NE.'  ') L1 = .TRUE.
        IF(LU.GT.0                 ) L2 = .TRUE.

        IF(L1) INQUIRE(FILE=LN,ERR=100,IOSTAT=LS,OPENED=L3,NUMBER=LU)
        IF(L2) INQUIRE(UNIT=LU,ERR=100,IOSTAT=LS,OPENED=L3)
        IF(L3) INQUIRE(UNIT=LU,ERR=100,IOSTAT=LS,NAME=LN)
        IF(L3 .AND. LU.NE.UR
     $        .AND. LU.NE.UW
     $        .AND. LU.NE.UTR
     $        .AND. LU.NE.UTW) CLOSE(UNIT=LU,ERR=100,IOSTAT=LS)
        IF(L3)  IC = LS

  100   IF(LS.GT.0) IC = LS
 
        IF(IC.EQ.0) THEN
          UPRECL(LU) = -1
          IF(UU .EQ. LU) UU = -1
          IF(UE .EQ. LU) UE = -1
          ENDIF

        IF(UU.GE.0 .OR. UE.GE.0) LL=LENSTR(LN)
 
        IF(UU.GE.0 .AND. IC.EQ.0) WRITE(UU,1111,IOSTAT=J) LU,LN(1:LL)
        IF(UE.GE.0 .AND. IC.GT.0) WRITE(UE,1112,IOSTAT=J) LU,IC,LN(1:LL)

 1111     FORMAT(' un-clo',I4,'     closed            ',A)
 1112     FORMAT(' un-clo',I4,' **ERROR** status is ',I4,'     ',A)

      RETURN
C
      END
