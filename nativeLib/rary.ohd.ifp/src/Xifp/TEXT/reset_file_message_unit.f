c  Subroutine originally written by G. Smith, 11/19/92
c
      Subroutine reset_file_message_unit()
c
      INTEGER          UR,UW,UTR,UTW,UE,UU,UPRECL(99)
      COMMON  /UPDAIO/ UR,UW,UTR,UTW,UE,UU,UPRECL
C
C  =================================== RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ifp/src/Xifp/RCS/reset_file_message_unit.f,v $
     . $',                                                             '
     .$Id: reset_file_message_unit.f,v 1.1 1995/09/08 15:01:18 page Exp $
     . $' /
C  =====================================================================
C
c
c Reset variable UU to -1.  This will suppress informational
c  messages about open, close, deletion, etc. for OFS files.
c
      UU = -1
c
      Return
      End
