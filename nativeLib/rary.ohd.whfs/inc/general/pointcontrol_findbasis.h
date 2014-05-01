#ifndef POINTCONTROL_FINDBASIS_H
#define POINTCONTROL_FINDBASIS_H

#include "pointcontrol_report.h"

ReportList * load_obsfcst_report ( ReportList             *repPtr ,
                                   ReportList             *inputHead ) ;

ReportList * determine_rs_mofo(ReportList * obsrepHead,
                               ReportList * fcstrepHead);


#endif /* #ifndef POINTCONTROL_FINDBASIS_H */
