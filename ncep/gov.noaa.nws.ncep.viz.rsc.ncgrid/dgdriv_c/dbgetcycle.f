        SUBROUTINE DB_GETCYCLE  ( cycle, iret )
C************************************************************************
C* DB_GETCYCLE                                                          *
C*                                                                      *
C* This subroutine gets dbtime from  common.                            *
C*                                                                      *
C* DB_GETCYCLE  ( CYCLE, IRET )                                         *
C*                                                                      *
C* Input parameters:                                                    *
C*      CYCLE           CHAR*           DB info                         *
C*                                                                      *
C* Output parameters:                                                   *
C*      IRET            INTEGER         Return code                     *
C*                                        0 = normal return             *
C**                                                                     *
C* Log:                                                                 *
C* m.gamazaychikov/CWS	07/09                                           *
C************************************************************************
        INCLUDE         'GEMPRM.PRM'
        INCLUDE         'dbcmn.cmn'
C*
        CHARACTER*(*)   cycle
C*
C------------------------------------------------------------------------
C*
c        CALL ST_LSTR ( dbtime, ldbtime, ier )
        CALL ST_LSTR ( navtime, ldbtime, ier )
        IF ( ldbtime .eq. 0) THEN 
           iret = -1
        ELSE
c           cycle = dbtime ( :ldbtime)
           cycle = navtime ( :ldbtime)
           iret = 0   
        END IF
        CALL ST_NULL (cycle, cycle, lstr, ier)
        RETURN
        END
