// ****************************************************
// +++ test_WorkstationTestMode.C: Main test driver for WorkstationTestMode.C
//
// History
//
// 04-jan-05 davison	Initial
//
// -- **************************************************
#include <stdlib.h>
#include <stdio.h>

#include "WorkstationTestMode.H"


int
main (
    int,
    char * []
) {
    
    WorkstationTestMode::Wstm wsMode = 
        WorkstationTestMode::checkWorkstationTestMode ();
    if (wsMode == WorkstationTestMode::TEST) printf ("WS mode is TEST\n");
    if (wsMode == WorkstationTestMode::PRACTICE) 
        printf ("WS mode is PRACTICE\n");
    if (wsMode == WorkstationTestMode::OPERATIONAL)
        printf ("WS mode is OPERATIONAL\n");
    if (wsMode == WorkstationTestMode::PANIC)
        printf ("WS mode is PANIC\n");
    exit (0);
}

