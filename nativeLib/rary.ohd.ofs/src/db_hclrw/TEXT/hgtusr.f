C MODULE HGTUSR
C-----------------------------------------------------------------------
C
C  ROUTINE TO GET USER NAME.
C
      SUBROUTINE HGTUSR (USERID,ISTAT)
C
      CHARACTER*8 USERID
C
      INCLUDE 'hclcommon/hdflts'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/db_hclrw/RCS/hgtusr.f,v $
     . $',                                                             '
     .$Id: hgtusr.f,v 1.2 2001/06/13 13:08:41 dws Exp $
     . $' /
C    ===================================================================
C
C
C  READ USER PARAMETER FILE
      CALL HGETPM (ISTAT)
      IF (ISTAT.EQ.0) THEN
         CALL SUBSTR (HNAMRF,1,LEN(USERID),USERID,1)
         ENDIF
C
      RETURN
C
      END
