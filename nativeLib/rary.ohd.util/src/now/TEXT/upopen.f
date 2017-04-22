C MODULE UPOPEN
C  =====================================================================
C  pgm: UPOPEN(IUNIT,FILNM,IRECL,FMT,IC) .. open (create) a file
C
C   in: IUNIT ...... desired logical unit number for file - INT
C   in:                (note, a unit number must be given)
C   in: FILNM ...... pathname of file to be opened - CHAR*(*)
C   in:                (if blank, then a temporary file is opened)
C   in: IRECL ...... record length in long-wrds - INT
C   in:                 if pos then open a direct access file
C   in:                 if   0 then open formatted sequential file
C   in:                 if neg then open unformatted sequential file
C   in: FMT ........ file format ('U' unformatted, 'F' formatted,
C   in:              else use unfmt for direct, fmt for seq) - CHAR*1
C  out: IC ......... status (0=no error, -1=no action, pos=error) - INT
C   in: (common) ... block common UPDAIO contains unit numbers for
C   in:              i/o routine messages (see subrtn UPRIMO)
C
C  rqd: common:  UPDAIO
C  rqd: subrtn:  UPNOFI,LENSTR
C
C  cmt: Commented statements switches between accepting already open
C  cmt:   units and just closing them verses giving an error.
C  =====================================================================
      SUBROUTINE UPOPEN(IUNIT,FILNM,IRECL,FMT,IC)


      INTEGER        IUNIT,IRECL,IC,I,J,L,E,R,LL,LL2
      LOGICAL        L1,L2,L3,L4,L5,L6,L7,L8
      CHARACTER*(*)  FILNM
      CHARACTER*1    FMT
      CHARACTER*128  LN,LX

      INCLUDE 'updaio'

      CHARACTER*7    X
      CHARACTER*6    D
      CHARACTER*10   S
      CHARACTER*11   U,W
      CHARACTER*9    F
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/util/src/now/RCS/upopen.f,v $
     . $',                                                             '
     .$Id: upopen.f,v 1.4 2004/05/03 21:38:55 hank Exp $
     . $' /
C    ===================================================================
C
      DATA           X / 'SCRATCH'     /
      DATA           D / 'DIRECT'      /
      DATA           S / 'SEQUENTIAL'  /
      DATA           U / 'UNFORMATTED' /
      DATA           F / 'FORMATTED'   /


       LBUG=0
        
      IC = -1
      E  = -1
      I  = IUNIT
      LN = FILNM
      LX = ' '
      R  = IRECL
      L  = 4*R

C         Added April 2004 by dws to make the filename include
C         a process id, if the file is a 'TEMP.' file.  Move the file
C         To the /tmp directory if it is a TEMP.* file (pass in 1).

      CALL UPNOFI(LN,1)
      
      L1 = .FALSE.
      L2 = .FALSE.
      L3 = .FALSE.
      L4 = .FALSE.
      L5 = .FALSE.
      L6 = .FALSE.
      L7 = .FALSE.
      L8 = .FALSE.
 
      IF (R.LE.0) W = F
      IF (R.GT.0) W = U
      IF (FMT.EQ.'F' .OR. FMT.EQ.'f') W = F
      IF (FMT.EQ.'U' .OR. FMT.EQ.'u') W = U

      IF (I.GT.0 .AND. I.LE.MUPRECL) THEN
CCC         IF (I.GT.0) INQUIRE(I,ERR=100,IOSTAT=E,OPENED=L5)
         IF (I.GT.0) INQUIRE(I,ERR=100,IOSTAT=E,OPENED=L6,NAME=LX)

         IF (.NOT.L5 .AND. LN.NE.'  ' .AND. L.LE.0) L1 = .TRUE.
         IF (.NOT.L5 .AND. LN.NE.'  ' .AND. L.GT.0) L2 = .TRUE.
         IF (.NOT.L5 .AND. LN.EQ.'  ' .AND. L.LE.0) L3 = .TRUE.
         IF (.NOT.L5 .AND. LN.EQ.'  ' .AND. L.GT.0) L4 = .TRUE.

         IF (L1) INQUIRE(FILE=LN,ERR=100,IOSTAT=E,EXIST=L7)
         IF (L2) INQUIRE(FILE=LN,ERR=100,IOSTAT=E,EXIST=L8)

         IF (L1) THEN
            OPEN(I,ERR=100,IOSTAT=E,ACCESS=S,FORM=W,FILE=LN)
            IF (LBUG.GT.0) WRITE (UU,*) 'UNIT=',I,' IOSTAT=',E,
     *         ' ACCESS=',S,' FORM=',W,' FILE=',LN
            ENDIF
         IF (L2) THEN
            OPEN(I,ERR=100,IOSTAT=E,ACCESS=D,FORM=W,FILE=LN,RECL=L)
            IF (LBUG.GT.0) WRITE (UU,*) 'UNIT=',I,' IOSTAT=',E,
     *         ' ACCESS=',D,' FORM=',W,' FILE=',LN,' RECL=',L
            ENDIF
         IF (L3) THEN
            OPEN(I,ERR=100,IOSTAT=E,ACCESS=S,FORM=W,STATUS=X)
            IF (LBUG.GT.0) WRITE (UU,*) 'UNIT=',I,' IOSTAT=',E,
     *         ' ACCESS=',S,' FORM=',W,' STATUS=',X
            ENDIF
         IF (L4) THEN
            OPEN(I,ERR=100,IOSTAT=E,ACCESS=D,FORM=W,STATUS=X,RECL=L)
            IF (LBUG.GT.0) WRITE (UU,*) 'UNIT=',I,' IOSTAT=',E,
     *         ' ACCESS=',D,' FORM=',W,' STATUS=',X,' RECL=',L
            ENDIF

         IF (.NOT.L5 .AND. E.EQ.0) INQUIRE(I,ERR=100,IOSTAT=E,NAME=LN)
  100    IF (L1 .OR. L2 .OR. L3 .OR. L4 .OR. E.GT.0) IC = E
      ENDIF

      IF (LN .EQ. ' ') LN = FILNM

      LL=LENSTR(LN)
      LL2=LENSTR(LX)
 
      IF (UU.GE.0 .AND. IC.EQ.0) THEN
         IF (L6) WRITE (UU,1110,IOSTAT=J) I,LX(1:LL2)
         IF (L1 .AND. .NOT.L7) WRITE (UU,1111,IOSTAT=J) LN(1:LL)
         IF (L2 .AND. .NOT.L8) WRITE (UU,1112,IOSTAT=J) R,LN(1:LL)
         IF (L1) WRITE (UU,1113,IOSTAT=J) I,LN(1:LL)
         IF (L2) WRITE (UU,1114,IOSTAT=J) I,R,LN(1:LL)
         IF (L3) WRITE (UU,1115,IOSTAT=J) I,LN(1:LL)
         IF (L4) WRITE (UU,1116,IOSTAT=J) I,R,LN(1:LL)
         ENDIF

      IF (UE.GE.0 .AND. IC.GT.0) THEN
         IF (L1) WRITE (UE,1117,IOSTAT=J) I,E,LN(1:LL)
         IF (L2) WRITE (UE,1118,IOSTAT=J) I,E,LN(1:LL)
         IF (L3) WRITE (UE,1119,IOSTAT=J) I,E,LN(1:LL)
         IF (L4) WRITE (UE,1120,IOSTAT=J) I,E,LN(1:LL)
         ENDIF

      IF (L5 .AND. UE.GE.0) WRITE (UE,1121,IOSTAT=J) I,LN(1:LL)

      IF (IC.EQ.0) UPRECL(I) = R

 1110 FORMAT (' un-opn',I4,'     closed old file   ',A)
 1111 FORMAT (' un-opn',4X,'     create seq        ',A)
 1112 FORMAT (' un-opn',4X,'     create dam r=',I4,' ',A)
 1113 FORMAT (' un-opn',I4,'     seq on',4X,'        ',A)
 1114 FORMAT (' un-opn',I4,'     dam on',4X,' r=',I4,' ',A)
 1115 FORMAT (' un-opn',I4,'     tseq -',4X,'        ',A)
 1116 FORMAT (' un-opn',I4,'     tdam -',4X,' r=',I4,' ',A)
 1117 FORMAT (' un-opn',I4,'  **ERROR** is',I5,' sam ',A)
 1118 FORMAT (' un-opn',I4,'  **ERROR** is',I5,' dam ',A)
 1119 FORMAT (' un-opn',I4,'  **ERROR** is',I5,' tsm ',A)
 1120 FORMAT (' un-opn',I4,'  **ERROR** is',I5,' tdm ',A)
 1121 FORMAT (' un-opn',I4,'  **ERROR** is unt open ',A)

      RETURN

      END
