package ohd.hseb.alertalarm;

import java.sql.SQLException;
import java.util.ArrayList;
import java.util.List;

import javax.swing.JOptionPane;

import ohd.hseb.db.Database;
import ohd.hseb.db.DbTable;
import ohd.hseb.ihfsdb.generated.AlertAlarmValRecord;
import ohd.hseb.ihfsdb.generated.AlertAlarmValTable;
import ohd.hseb.util.SessionLogger;

public class AlertAlarmDataManager 
{
    private Database _db = null;
    private SessionLogger _logger = null;
    private String _missingRepresentation; 

    public AlertAlarmDataManager(Database dbHandler, SessionLogger logger, String missingRepresentation)
    {
        super();
        _db = dbHandler;
        _logger = logger;
        _missingRepresentation = missingRepresentation;

        if (_db == null)
        {
            JOptionPane.showMessageDialog(null, "Database should be provided","AlertAlarm Application", JOptionPane.PLAIN_MESSAGE);
            System.exit(0);
        }
    }

    public void logSQLException(SQLException exception)
    {
        _logger.log("SQL ERROR = " +
            exception.getErrorCode() +  " " +
            exception.getMessage());

        exception.printStackTrace(_logger.getPrintWriter());

        _logger.log("End of stack trace");

    }

    public List readDataFromAlertAlarmVal()
    {
        List alertAlarmInfoList = null;
        List dataList = null;
        String header = "AlertAlarmDataManager.readDataFromAlertAlarmVal(): ";
        AlertAlarmValTable alertAlarmValTable = new AlertAlarmValTable(_db);
        AlertAlarmValRecord  alertAlarmValRecord = null;
        String whereClause = null; 
        try
        {
            whereClause = "order by lid";
            alertAlarmInfoList = alertAlarmValTable.select(whereClause);
            _logger.log(header + " Where clause:"+ whereClause);
            if(alertAlarmInfoList != null)
            {
                dataList = new ArrayList();
                for(int i=0;i < alertAlarmInfoList.size(); i++)
                {

                    alertAlarmValRecord = (AlertAlarmValRecord) alertAlarmInfoList.get(i);
                    AlertAlarmJTableRowData rowData = new AlertAlarmJTableRowData(_missingRepresentation);
                    //rowData.setAlertAlarmRecord(alertAlarmValRecord);
                    rowData.setLid(alertAlarmValRecord.getLid());
                    rowData.setPe(alertAlarmValRecord.getPe());
                    rowData.setTs(alertAlarmValRecord.getTs());

                    rowData.setDur(alertAlarmValRecord.getDur());

                    rowData.setExtremum(alertAlarmValRecord.getExtremum());

                    if(alertAlarmValRecord.getProbability() == -1.0)
                    {
                        rowData.setProbability(DbTable.getNullFloat());         
                    }
                    else
                    {
                        rowData.setProbability(alertAlarmValRecord.getProbability());
                    }

                    rowData.setValidTime(alertAlarmValRecord.getValidtime());
                    rowData.setBasisTime(alertAlarmValRecord.getBasistime());

                    rowData.setValue(alertAlarmValRecord.getValue());

                    if(alertAlarmValRecord.getSuppl_value() == -9999.0)
                    {
                        rowData.setSupplValue(DbTable.getNullDouble());       
                    }
                    else
                    {
                        rowData.setSupplValue(alertAlarmValRecord.getSuppl_value());
                    }

                    rowData.setShefQualCode(alertAlarmValRecord.getShef_qual_code());

                    String result = null;
                    if(alertAlarmValRecord.getQuality_code() == DbTable.getNullInt())
                        result = _missingRepresentation;
                    else if(alertAlarmValRecord.getQuality_code() >= 1610612736)
                        result = "Good";
                    else if(alertAlarmValRecord.getQuality_code() >= 1073741824)
                        result = "Quest";
                    else 
                        result = "Bad";

                    rowData.setQualityCode(result);

                    if(alertAlarmValRecord.getRevision() == DbTable.getNullShort())
                    {
                        result = _missingRepresentation;
                    }
                    else if(alertAlarmValRecord.getRevision() == 1)
                    {
                        result = "Y";
                    }
                    else
                    {
                        result = "N";
                    }
                    rowData.setRevision(result);

                    rowData.setProductId(alertAlarmValRecord.getProduct_id());

                    rowData.setProductTime(alertAlarmValRecord.getProducttime());

                    rowData.setPostingTime(alertAlarmValRecord.getPostingtime());

                    rowData.setActionTime(alertAlarmValRecord.getAction_time());

                    rowData.setAaCateg(alertAlarmValRecord.getAa_categ());
                    rowData.setAaCheck(alertAlarmValRecord.getAa_check());
                    rowData.addAllCellsToMap();
                    dataList.add(rowData);
                }
            }
            else
            {
                _logger.log(header+" AlertAlarminfo list is null");
            }
        }
        catch(SQLException e)
        {
            logSQLException(e);
        }
        return dataList;
    }

}
