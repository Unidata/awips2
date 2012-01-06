C MODULE UDOPEN
C  =====================================================================
C  pgm: UDOPEN(UNT,KOD,NEWNAM,LRECL,ISTAT) .. open a DAIO file if needed
C
C   in: UNT ..... unit number identifying the desired file
C   in: KOD ..... summation of two part code:
C   in:            part one (manditory) indicates operation:
C   in:             1 - open or create, else ISTAT = 1
C   in:                 (else skip rtn if already open)
C   in:             2 - open existing, else ISTAT = 2
C   in:                 (else skip rtn if already open)
C   in:             3 - create new, else if exists then ISTAT = 3
C   in:             4 - create new, del old if exists, else ISTAT = 4
C   in:             5 - inquire only, ISTAT=0 exsts,=-1 no exst, else=5
C   in:             6 - get filnam and LRECL for gvn UNT, else ISTAT=6
C   in:            second optional part of key forces formatted or
C   in:            unformatted open, else default is used:
C   in:             0 - default (formatted for seq, unform for dir ac)
C   in:             8 - force unformatted file if created
C   in:            16 - force formatted file if created
C  out: NEWNAM .. filename for given unit numbr (128 characters long)
C  out: LRECL ... number of words (4 bytes) in each record
C  out: ISTAT ... status code, 0 for exists or file opened, else=KOD
C
C  cmt: This rtn belongs on the level of DREAD, DWRITE, DCLOSE.
C  =====================================================================
      SUBROUTINE UDOPEN(UNT,KOD,NEWNAM,LRECL,ISTAT)


      EXTERNAL      UPFNCU,UPOPEN,UPCLOS,UPEXIS,UPDELE,UDOE

      INTEGER       UNT,KOD,LRECL,ISTAT,IFUN,LBLCK,IC,JF,COD,II
      CHARACTER*1   DFL,UFM,FM,FORMT
      CHARACTER*3   JS
      CHARACTER*32  FILNAM
      CHARACTER*128 NEWNAM
      DATA          DFL,UFM,FM / ' ','U','F' /

      INCLUDE 'updaio'
 
      INCLUDE 'ufiles'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/util/src/now/RCS/udopen.f,v $
     . $',                                                             '
     .$Id: udopen.f,v 1.2 2001/06/12 19:09:15 dws Exp $
     . $' /
C    ===================================================================
C


C                Decode "KOD" input control variable

      II  = KOD/8
      COD = KOD-(8*II)
      FORMT = DFL
      IF( II .EQ. 1 ) FORMT = UFM
      IF( II .EQ. 2 ) FORMT = FM

C                Get record length and filename for given unit number
C                  (Make LRECL the number of 4-byte words, status ok)
C                  (JF=-1 file not open, JF=+ DAM open, JF=0 SAM open)


      FILNAM = ' '
      CALL UPFNCU('    ',FILNAM,UNT,NEWNAM,IFUN,LRECL,LBLCK,IC)
      LRECL = (LRECL+3)/4
      ISTAT = 0
      JF = UPRECL(UNT)

C                If open, no UPFNCU err; have LRECL,ISTAT,NEWNAM; scram
C                  (Continue if trying to create new file, COD=3 or 4)


      IF(JF.GE.0 .AND. IC.EQ.0 .AND. COD.NE.3 .AND. COD.NE.4 ) GO TO 900


C                Check for valid data for given unit number, if error
C                  then set ISTAT = COD (pos number) and scram


      IF( IC.EQ.1 ) CALL UDOE(UNT,IC,'unit number not found   ')
      IF( IC.GT.1 ) CALL UDOE(UNT,IC,'cannot get file informtn')
      IF( IC.LT.0 ) CALL UDOE(UNT,IC,'bad record length       ')
      IF( IC.NE.0 ) ISTAT = COD
      IF( IC.NE.0 ) GO TO 900


C                If OD = 6 then have name and LRECL so scram


      IF( COD .EQ. 6 ) GO TO 900


C                Then check for existance of file for COD = 2, 3, 4, 5
C                  (Note, doesn't hurt to get JS for COD = 1)
C                  (If undesired result, set ISTAT = COD and leave)

C                Set situation code: JS='ERR' for program error
C                                    JS='OLD' for file already exists
C                                    JS='NEW' for creating a new file
C                                    JS='NOF' for error since no file
C                                    JS='YSF' for error since file exsts
C                                    JS='CLD' for close cur file and del
C                                    JS='DEL' for delete old exstg file


      CALL UPEXIS(UNT,NEWNAM,IC)
        IF( IC .LT. 0 ) JS = 'NEW'
        IF( IC .EQ. 0 ) JS = 'OLD'
        IF( IC .GT. 0 ) JS = 'ERR'

        IF( COD.EQ.2 .AND. IC.LT.0               ) JS = 'NOF'
        IF( COD.EQ.3 .AND. IC.EQ.0               ) JS = 'YSF'
        IF( COD.EQ.4 .AND. IC.EQ.0 .AND. JF.GE.0 ) JS = 'CLD'
        IF( COD.EQ.4 .AND. IC.EQ.0 .AND. JF.LT.0 ) JS = 'DEL'

      IF( JS.EQ.'ERR' ) CALL UDOE(UNT,IC,'cannot check for exstnce')
      IF( JS.EQ.'NOF' ) CALL UDOE(UNT,IC,'cannot find file to open')
      IF( JS.EQ.'YSF' ) CALL UDOE(UNT,IC,'file already exists     ')

      IF(                JS.EQ.'ERR' ) ISTAT = COD
      IF(                JS.EQ.'NOF' ) ISTAT = COD
      IF(                JS.EQ.'YSF' ) ISTAT = COD
      IF( COD.EQ.5 .AND. JS.EQ.'NEW' ) ISTAT =  0
      IF( COD.EQ.5 .AND. JS.EQ.'OLD' ) ISTAT = -1

      IF( COD   .EQ. 5 ) GO TO 900
      IF( ISTAT .NE. 0 ) GO TO 900


C                If old file needs deleting, then do it!


      IF( JS.EQ.'CLD' ) CALL UPCLOS(UNT,NEWNAM,IC)
      IF( JS.EQ.'CLD' ) CALL UPDELE(UNT,NEWNAM,IC)
      IF( JS.EQ.'DEL' ) CALL UPDELE(UNT,NEWNAM,IC)
      IF( IC.GT.0 ) ISTAT = COD
      IF( IC.GT.0 ) GO TO 900


C                Open or create file if no errors have been found
C                  (If LRECL .GT. 0, set parameters for DAM file)


      CALL UPOPEN(UNT,NEWNAM,LRECL,FORMT,IC)
        IF( IC.EQ.0 .AND. LRECL.GT.0 ) IFILES(UNT) = 1
        IF( IC.EQ.0 .AND. LRECL.LE.0 ) IFILES(UNT) = 2
        IF( IC.NE.0 ) ISTAT = COD


  900 CONTINUE


      RETURN
      END
