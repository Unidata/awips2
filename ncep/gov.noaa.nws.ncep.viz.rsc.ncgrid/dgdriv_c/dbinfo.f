        SUBROUTINE DB_INFO  ( dbinfo, iret )
C************************************************************************
C* DB_INFO                                                              *
C*                                                                      *
C* This subroutine puts DB info into common.                            *
C*                                                                      *
C* DB_INFO  ( DBINFO, IRET )                                            *
C*                                                                      *
C* Input parameters:                                                    *
C*      DBINFO          CHAR*          DB info                          *
C*                                                                      *
C* Output parameters:                                                   *
C*      IRET            INTEGER         Return code                     *
C*                                        0 = normal return             *
C*                                      -14 = could not set db def parms*
C**                                                                     *
C* Log:                                                                 *
C* m.gamazaychikov/CWS	07/09                                           *
C* m.gamazaychikov/CWS  02/11   Removed setting the dbhost              *
C************************************************************************
        INCLUDE         'GEMPRM.PRM'
        INCLUDE         'dbcmn.cmn'
C*
        CHARACTER*(*)   dbinfo
C*
        CHARACTER       sourc*6, parms*5, pfile*4
        CHARACTER       carr(5)*128, carr2(2)*80, sprtr*2
        DATA            sourc / 'SOURCE' /
        DATA            parms / 'PARMS' /
        DATA            pfile / 'FILE' /
        DATA            sprtr / '--' /
        LOGICAL         issource, isparms, isfile
C------------------------------------------------------------------------
        DO ii = 1, 2
          carr(ii) = ' '
          carr2(ii) = ' '
        END DO
        dbdatasrc = ' ' 
        dbparms   = ' ' 
        dbprmfile = ' ' 
        issource = .false.
        isparms = .false.
        isfile = .false.
        CALL ST_CLS2 ( dbinfo, '|', '--', 5, carr, num, ier )
C
C*      Match alias with its attribute list.
C
        DO icarr = 2, num
           IF ( carr(icarr) .ne. sprtr ) THEN  
              CALL ST_CLS2 ( carr(icarr), ':', ' ', 2, carr2, 
     +                       num2, ier )
              IF ( num2 .eq. 2 ) THEN
                 CALL ST_LSTR ( carr2(1), ilstr1, ier )
                 CALL ST_LSTR ( carr2(2), ilstr2, ier )
                 IF (carr2(1)(:ilstr1) .eq. sourc) THEN
                   dbdatasrc = carr2(2)(1:ilstr2)
                   issource = .true.
                 ELSE IF (carr2(1)(:ilstr1) .eq. parms) THEN
                   dbparms   = carr2(2)(1:ilstr2)
                   isparms = .true.
                 ELSE IF (carr2(1)(:ilstr1) .eq. pfile) THEN
                   dbprmfile = carr2(2)(1:ilstr2)
                   isfile = .true.
                 END IF
              END IF
           END IF
        END DO
        
        IF ( .not. issource .or. .not. isparms .or. .not. isfile ) THEN
           iret = -14
           CALL ER_WMSG  ( 'DB', iret, ' ', ier )
           RETURN
        END IF
C*
        RETURN
        END
