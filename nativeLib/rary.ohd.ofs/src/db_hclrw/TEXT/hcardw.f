C MODULE HCARDW
C-----------------------------------------------------------------------
C
      SUBROUTINE HCARDW (NNCARD,ISTAT)
C
C  THIS ROUTINE WRITES A CARD IMAGE TO THE HCL CARD IMAGE FILE.
C  THE FILE WILL CONTAIN THE COMPLETE SET OF CARDS FOR AN HCL
C  COMMAND.
C
C  ARGUMENT LIST:
C
C       NAME      TYPE  I/O   DIM   DESCRIPTION
C       ------    ----  ---   ---   -----------
C       NNCARD     I     I     1    CARD NUMBER (ALSO RECORD NUMBER)
C       ISTAT      I     O     1    STATUS:
C                                     0=OKAY
C                                     OTHER=ERROR
C
      INCLUDE 'uio'
      INCLUDE 'udebug'
      INCLUDE 'ufreei'
      INCLUDE 'hclcommon/hunits'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/db_hclrw/RCS/hcardw.f,v $
     . $',                                                             '
     .$Id: hcardw.f,v 1.2 1999/04/23 20:00:58 page Exp $
     . $' /
C    ===================================================================
C
C
      IF (IHCLTR.GT.0) WRITE (IOGDB,*) 'ENTER HCARDW'
C
      CALL UWRITT (KHCARD,NNCARD,IBUF,ISTAT)
C
      IF (IHCLTR.GT.0) WRITE (IOGDB,*) 'EXIT HCARDW'
C
      RETURN
C
      END
