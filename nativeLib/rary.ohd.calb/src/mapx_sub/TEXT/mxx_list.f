C$PRAGMA C (GET_PWD)
C MODULE MXX_LIST
C  =====================================================================
C  pgm: MXX_LIST .. Output the values of various pathname
C
C  rqd: WLIN
C  =====================================================================
      SUBROUTINE MXX_LIST(DIRGRD,MISLOG,DIRMAP)

      EXTERNAL       WLIN

      CHARACTER*200  LIN
      INTEGER        JE
      CHARACTER*(*)  DIRGRD,MISLOG,DIRMAP
      CHARACTER*150  STRNG
      CHARACTER*256  PWD
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/calb/src/mapx_sub/RCS/mxx_list.f,v $
     . $',                                                             '
     .$Id: mxx_list.f,v 1.2 2002/02/15 20:08:24 dws Exp $
     . $' /
C    ===================================================================
C

        CALL WLIN('B',' ')
        
        CALL GET_PWD (PWD,LPWD)

        LIN = ' '
        WRITE(LIN,'(A,A)',IOSTAT=JE) ' GRID DIRECTORY:     ',DIRGRD
        IF(JE.EQ.0) CALL WLIN('M',LIN)

        LIN = ' '
        STRNG=DIRMAP
        IF (DIRMAP(1:1).NE.'/') STRNG=PWD(1:LPWD)//'/'//DIRMAP
        WRITE(LIN,'(A,A)',IOSTAT=JE) ' MAPX DIRECTORY:     ',STRNG
        IF(JE.EQ.0) CALL WLIN('M',LIN)

        LIN = ' '
        STRNG=MISLOG
        IF (MISLOG(1:1).NE.'/') STRNG=PWD(1:LPWD)//'/'//MISLOG
        WRITE(LIN,'(A,A)',IOSTAT=JE) ' MISSED LOG FILE:    ',STRNG
        IF(JE.EQ.0) CALL WLIN('M',LIN)

        CALL WLIN('B',' ')

      RETURN
      END
