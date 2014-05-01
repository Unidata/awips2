C MEMBER UDDNST
C-----------------------------------------------------------------------
C  ROUTINE TO CHECK STATUS OF DDNAME.
C
C     ARGUMENT   TYPE   I/O   DIM   CONTENTS
C     --------   ----   ---   ---   --------
C     DDNAME     A8     I/O   1     DD NAME
C     NUNIT      I4     I     1     UNIT NUMBER
C                                    0=USE DDNAME
C                                   >0=USE UNIT NUMBER TO SET DDNAME
C     IPRERR     I*4    I     1     NOT USED ANYMORE
C     ISTAT      I*4    O     1     STATUS CODE
C                                    0=NORMAL RETURN
C                                    1=BAD DDNAME/UNIT NUMBR COMBINATION
C                                    2=INVALID UNIT NUMBER (<0 or >99)
C                                    4=CANNOT CONVERT UNIT NUMBER
C-----------------------------------------------------------------------
      SUBROUTINE UDDNST (DDNAME,NUNIT,IPRERR,ISTAT)

      EXTERNAL       UEROR

      CHARACTER*(*)  DDNAME
      CHARACTER*25   ERH
      CHARACTER*8    XFT,DDTEMP
      INTEGER        NUNIT,IPRERR,ISTAT,IERR

      INCLUDE 'uiox'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/util/src/util_gen3/RCS/uddnst.f,v $
     . $',                                                             '
     .$Id: uddnst.f,v 1.3 1999/04/22 13:56:10 page Exp $
     . $' /
C    ===================================================================
C

      DATA  ERH / '+*** ERROR - IN UDDNST - ' /

      DDTEMP=DDNAME
      ISTAT=0

C               If unit num is 0; if DDNAME is not 'NONE' use it,
C                                 else have error ISTAT=1
C               Else if unit num is bad (not 1-99) have error ISTAT=2
C               Else if DDNAME is not 'NONE', set to 'FT<unit-num>F001'

      IF (NUNIT.EQ.0) THEN
         IF (DDTEMP(1:4).EQ.'NONE') ISTAT=1
      ELSEIF (NUNIT.LT.1.OR.NUNIT.GT.99) THEN
         ISTAT=2
      ELSEIF (DDTEMP(1:4).NE.'NONE') THEN
         XFT='FT00F001'
         WRITE(XFT(3:4),'(I2.2)',IOSTAT=IERR) NUNIT
         DDNAME=XFT
         IF (IERR.GT.0) ISTAT=4
      ENDIF

      IF (ISTAT.NE.0) CALL UEROR (LP,1,-1)
      IF (ISTAT.EQ.1) WRITE (LP,70) DDTEMP,NUNIT
      IF (ISTAT.EQ.2) WRITE (LP,80) NUNIT
      IF (ISTAT.EQ.4) WRITE (LP,90)

      RETURN

C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

70    FORMAT (A,'DDNAME IS ',A,' AND ','UNIT NUMBER IS ',I2,'.')
80    FORMAT (A,I3,' IS AN INVALID UNIT NUMBER.')
90    FORMAT (A,'CANNOT CONVERT NUMBER TO TEXT IN WRITE STATEMENT.')

      END
