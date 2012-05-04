        SUBROUTINE DB_ENSM1  ( iret )
C************************************************************************
C* DB_ENSM1                                                             *
C*                                                                      *
C* This subroutine gets evtname from  common.                           *
C*                                                                      *
C* DB_ENSM1  ( IRET )                                                   *
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
        CHARACTER       message*720, funcnm*20, loglevel*6
C*
C------------------------------------------------------------------------
C*
        iret = 0
        loglevel = "debug"
        funcnm="DB_ENSM1"
        CALL ST_NULL ( funcnm,   funcnm,   lenq, ier )
        CALL ST_NULL ( loglevel, loglevel, lenq, ier )

        IF ( isensnames ) THEN
           message = "decrementing ensemble member counter" 
           CALL ST_NULL ( message,  message,  lenq, ier )
           CALL DB_MSGCAVE ( funcnm, loglevel, message, ier )
           WRITE (message, 1001 ) iensmem
 1001      FORMAT ("before decrementing iensmem=", I5 )
           CALL ST_NULL ( message,  message,  lenq, ier )
           CALL DB_MSGCAVE ( funcnm, loglevel, message, ier )
           IF ( iensmem .le. iens .and. iensmem .ge. 2 ) THEN
              iensmem = iensmem - 1
           END IF
           WRITE (message, 1003 ) iensmem
 1003      FORMAT ("after decrementing iensmem=", I5 )
           CALL ST_NULL ( message,  message,  lenq, ier )
           CALL DB_MSGCAVE ( funcnm, loglevel, message, ier )
        END IF

        RETURN
        END
