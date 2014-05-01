// *******************************************************
// +++ WorkstationTestMode.H: Interface to workstation mode
//
//
// Usage:
//
// #include "WorkstationTestMode.H"
//
//    WorkstationTestMode::Wstm wstm = 
//	WorkstationTestMode::checkWorkstationTestMode ();
//
//  
// If WorkstationTestMode::checkWorkstationTestMode returns a
// value of WorkstationTestMode::PANIC, it means the workstation 
// test mode cannot be determined.
//
// The other possible return values correspond to workstation
// modes as described in the header comments in getTestMode.C
// and tmcp.C.
//
// WorkstationTestMode::checkWorkstationTestMode works by
// executing (via system()) $FXA_HOME/bin/getTestMode.
//
// History:
// 23-dec-04 Davison	Initial
//
// --- ***************************************************
#ifndef _WORKSTATIONTESTMODE_H_
#define _WORKSTATIONTESTMODE_H_

    class WorkstationTestMode {
        public:
	    enum Wstm {TEST, PRACTICE, OPERATIONAL, PANIC};
	    static Wstm checkWorkstationTestMode (void);
    };

#endif

