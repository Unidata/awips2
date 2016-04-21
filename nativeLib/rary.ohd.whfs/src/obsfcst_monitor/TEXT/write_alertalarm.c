#include "obsfcst_monitor.h"

int write_to_alertalarm(FILE* logfp,
                        Forecast* fcst_ptr,
                        Observation* obs_ptr,
                        int check_diff_ret)
{
  AlertAlarmVal alert_alarmval_rec;
  bool          unique = false;
  int           ret;
  char          time_str1[20];
  char          time_str2[20];
  DbStatus      *dbstatus_struct = NULL;

  memset(& alert_alarmval_rec, 0, sizeof(AlertAlarmVal));

  strcpy(alert_alarmval_rec.lid, fcst_ptr->lid);
  strcpy(alert_alarmval_rec.pe, fcst_ptr->pe);
  alert_alarmval_rec.dur=fcst_ptr->dur;
  strcpy(alert_alarmval_rec.ts, fcst_ptr->ts);
  strcpy(alert_alarmval_rec.extremum, fcst_ptr->extremum);
  alert_alarmval_rec.probability = fcst_ptr->probability;
  alert_alarmval_rec.validtime= fcst_ptr->validtime;
  alert_alarmval_rec.basistime= fcst_ptr->basistime;
  alert_alarmval_rec.value= fcst_ptr->value;
  alert_alarmval_rec.suppl_value= obs_ptr->value;
  strcpy(alert_alarmval_rec.shef_qual_code, fcst_ptr->shef_qual_code);
  alert_alarmval_rec.quality_code = fcst_ptr->quality_code;
  alert_alarmval_rec.revision = fcst_ptr->revision;
  strcpy(alert_alarmval_rec.product_id, fcst_ptr->product_id);
  alert_alarmval_rec.producttime= fcst_ptr->producttime;
  alert_alarmval_rec.postingtime= fcst_ptr->postingtime;

  if(check_diff_ret == 1)
    strcpy(alert_alarmval_rec.aa_categ, ALARM_CATEGSTR);
  else
    strcpy(alert_alarmval_rec.aa_categ, ALERT_CATEGSTR); /** check_diff_ret will be 2 **/

  strcpy(alert_alarmval_rec.aa_check, DIFF_CHECKSTR);


  /* insert the record into the AlertAlarmVal table
      and check its return status.  */

  ret = InsertIfUniqueAlertAlarmVal(&alert_alarmval_rec, &unique);

  dbstatus_struct = (DbStatus*) calloc(1, sizeof(DbStatus));
  memcpy(dbstatus_struct ,GetAlertAlarmValDbStatus(), sizeof(DbStatus));

  yearsec_dt_to_ansi(alert_alarmval_rec.basistime, time_str1);
  yearsec_dt_to_ansi(alert_alarmval_rec.validtime, time_str2);
  fprintf(logfp, "\n%s-", alert_alarmval_rec.aa_categ);
  fprintf(logfp, "%f %f",
                 alert_alarmval_rec.value, alert_alarmval_rec.suppl_value );


  if ( !unique )
  {
     fprintf(logfp, "\nDuplicate Record... ");
  }
  else if (ret < 0)
  {
      fprintf(logfp, "\nPostgreSQL error ret[%d] inserting to AlertAlarmVal", ret);
      fprintf(logfp,"\n sql_state[%s], err_msg[%s] ", 
                        dbstatus_struct->sql_state, dbstatus_struct->error_msg);
      return ERROR;
  }
  else
      fprintf(logfp, "-- Inserted ");
  
  return SUCCESS;
}
