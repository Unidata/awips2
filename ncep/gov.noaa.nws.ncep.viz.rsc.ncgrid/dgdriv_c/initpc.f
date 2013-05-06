        SUBROUTINE INITPC  ( iret )
C************************************************************************
C* PC_INIT                                                              *
C*                                                                      *
C* This subroutine initializes the parameter conversion software.       *
C* Information about the current data set is saved.  It must be the     *
C* first PC subroutine called.                                          *
C*                                                                      *
C* PC_INIT  ( IVERT, NPARM, PARMS, IRET )                               *
C*                                                                      *
C* Input parameters:                                                    *
C*      IVERT           INTEGER         Vertical coordinate type        *
C*                                        0 = NONE                      *
C*                                        1 = PRES                      *
C*                                        2 = THTA                      *
C*                                        3 = HGHT                      *
C*      NPARM           INTEGER         Number of parameters            *
C*      PARMS (NPARM)   CHAR*4          Parameter names                 *
C*                                                                      *
C* Output parameters:                                                   *
C*      IRET            INTEGER         Return code                     *
C*                                         0 = normal return            *
C*                                        -1 = invalid NPARM            *
C*                                        -2 = invalid IVERT            *
C**                                                                     *
C* Log:                                                                 *
C* M. desJardins/GSFC    9/84                                           *
C* M. desJardins/GSFC    9/88   GEMPAK4                                 *
C* M. desJardins/GSFC    7/90   Initialize station parms, ...           *
C* T. Lee/GSC            8/97   Added TVRC to output parameter          *
C************************************************************************
C*
        CHARACTER*4 parms(6)
        ivert =1
        nparm =6
        parms(1) = "PRES"
        parms(2)  = "TEMP"
        parms(3)  = "DWPT"
        parms(4)  = "DRCT"
        parms(5)  = "SPED"
        parms(6)  = "HGHT"
        CALL PC_INIT  ( ivert, nparm, parms, iret )
C*
        RETURN
        END


