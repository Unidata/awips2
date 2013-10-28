package ohd.hseb.monitor;

import java.util.HashMap;
import java.util.Map;


/*
 * 
 * if (strcasecmp(str, "change") == 0)
  function_id = CHANGE;
else if (strcasecmp(str, "existence") == 0)
  function_id = EXISTENCE;
else if (strcasecmp(str, "non_existence") == 0)
  function_id = NON_EXISTENCE;
else if (strcasecmp(str, "last_modified") == 0)
  function_id = LAST_MODIFIED_TIME;
else if (strcasecmp(str, "file_count") == 0)
  function_id = FILECOUNT;
else if (strcasecmp(str, "size") == 0)
  function_id = SIZE;
else
  function_id = INVALID_ARG;
 * 
 * 
 * $FILE_MONITOR_BIN_DIR/file_monitor.LX -last_modified 3600 $FILE_MONITOR_DATA_DIR $FILE_MONITOR_LOG_FILE $SHEFLOGFILE $FROM_ID $MAIL_LIST "FILE MONITOR: Old Shef Decoder Log" >>$INIT_ERR_LOG_FILE 2>&1
#$FILE_MONITOR_BIN_DIR/file_monitor.LX -change $FILE_MONITOR_DATA_DIR $FILE_MONITOR_LOG_FILE $GAGEPP_FILTERED_LOG_FILE $FROM_ID $MAIL_LIST "FILE MONITOR: Gage PP Database Errors" >>$INIT_ERR_LOG_FILE 2>&1
$FILE_MONITOR_BIN_DIR/file_monitor.LX -size -gt 2500 $FILE_MONITOR_DATA_DIR $FILE_MONITOR_LOG_FILE $DPA_FILTERED_LOG_FILE $FROM_ID $MAIL_LIST "FILE MONITOR: Possible DPA Ingest Problem" >>$INIT_ERR_LOG_FILE 2>&1
$FILE_MONITOR_BIN_DIR/file_monitor.LX -change $FILE_MONITOR_DATA_DIR $FILE_MONITOR_LOG_FILE $FILTERED_PG_FILE $FROM_ID $MAIL_LIST "FILE MONITOR: Change in /var/log/postgres on $HNAME" >>$INIT_ERR_LOG_FILE 2>&1
$WHFS_BIN_DIR/file_monitor.LX -file_count -gt 10 $LOG_MONITOR_DATA_PATH $LOG_MONITOR_LOG_FILE $SHEF_DATA_DIR $FROM_ID $MAIL_LIST "FILE MONITOR: File Count in Shef Data Directory" >>$INIT_ERR_LOG_FILE 2>&1

Standard Args:
$FILE_MONITOR_DATA_DIR $FILE_MONITOR_LOG_FILE   $FILE_TO_MONITOR  $FROM_ID $MAIL_LIST  $MESSAGE_SUBJECT_LINE

-last_modified expireTimeInSeconds
-change 

-size -operator sizeInBytes
-file_count -operator count
-existence
-non_existence

 */

public class MonitorManager
{
    
    private String[] _argumentArray = null;
    private Monitor _monitor = null;
    private Reporter _reporter = null;
    
    private String _dataDirPath = null; //for monitor's private data
    private String _logFilePath = null; //for monitor's log file
    private String _fileToMonitorPath = null; //the file that monitor is examining
    
    //email stuff
    private String _fromAddressString = null; 
    private String _toAddressString = null;
    private String _subjectString = null;
    
    
    private static Map<String, MonitorType> _monitorTypeMap = new HashMap<String, MonitorType>();
    private static Map<String, MonitorOperator> _operatorMap = new HashMap<String, MonitorOperator>();
    
    
    static
    {
        _monitorTypeMap.put("last_modified", MonitorType.LAST_MODIFIED_TIME);
        _monitorTypeMap.put("change", MonitorType.CHANGE);
        _monitorTypeMap.put("size", MonitorType.SIZE);
        _monitorTypeMap.put("file_count", MonitorType.FILE_COUNT);
        _monitorTypeMap.put("existence", MonitorType.EXISTENCE);
        _monitorTypeMap.put("non_existence", MonitorType.NON_EXISTENCE);
        
        _operatorMap.put("GT", MonitorOperator.GREATER_THAN);
        _operatorMap.put("GTE", MonitorOperator.GREATER_THAN_OR_EQUAL);
        
        _operatorMap.put("LT", MonitorOperator.LESS_THAN);
        _operatorMap.put("LTE", MonitorOperator.LESS_THAN_OR_EQUAL);
        _operatorMap.put("EQ", MonitorOperator.EQUAL);
    }
    
    // ---------------------------------------------------------------------------------------
    public MonitorManager(String[] argumentArray)
    {
        _argumentArray = argumentArray;
        parseArguments();
    }
    //  ---------------------------------------------------------------------------------------
    private long parseLongArg(int argArrayIndex)
    {
        String longValueString = _argumentArray[argArrayIndex];
        long value = Long.valueOf(longValueString);
        
        return value;
    }
    // ---------------------------------------------------------------------------------------
    private String parseStringArgWithDash(int argArrayIndex)
    {
        String argString  = _argumentArray[argArrayIndex];
        String valueString = argString.substring(1);
        
        return valueString;
    }
    // ---------------------------------------------------------------------------------------
    private void parseArguments()
    {
        String header = "MonitorManager.parseArguments(): ";
        
        int i = 0;
        for (String arg : _argumentArray)
        {
            System.out.println("Arg " + i + " = " + arg);
            i++;
        }
        
        
        MonitorType monitorType = getMonitorType();
        
        System.out.println(header + " monitorType = " + getMonitorType());
        
        Monitor monitor = null;

        if (monitorType != null)
        {

            switch(monitorType)
            {
                case LAST_MODIFIED_TIME:
                {
                    long ageInSeconds = parseLongArg(1);

                    readStandardArguments(2);

                    monitor = MonitorFactory.buildLastModifiedTimeFileMonitor(ageInSeconds, _dataDirPath, _logFilePath, _fileToMonitorPath);
                    break;
                }


                case CHANGE:
                {
                    readStandardArguments(1);
                    monitor = MonitorFactory.buildChangeFileMonitor(_dataDirPath, _logFilePath, _fileToMonitorPath);

                    break;
                }
                case SIZE:
                {
                    MonitorOperator operator = getOperator();       
                    long fileSize = parseLongArg(2);


                    readStandardArguments(3);
                    monitor = MonitorFactory.buildSizeFileMonitor(operator, fileSize, _dataDirPath, _logFilePath, _fileToMonitorPath);


                    break;
                }
                case FILE_COUNT:
                {
                    MonitorOperator operator = getOperator();       
                    long fileCount = parseLongArg(2);


                    readStandardArguments(3);
                    monitor = MonitorFactory.buildFileCountFileMonitor(operator, fileCount, _dataDirPath, _logFilePath, _fileToMonitorPath);

                    break;
                }

                case EXISTENCE:
                {

                    readStandardArguments(1);
                    monitor = MonitorFactory.buildExistenceFileMonitor(_dataDirPath, _logFilePath, _fileToMonitorPath);

                    break;
                }

                case NON_EXISTENCE:
                {
                    readStandardArguments(1);
                    monitor = MonitorFactory.buildNonExistenceFileMonitor(_dataDirPath, _logFilePath, _fileToMonitorPath);

                    break;
                }

                default:
                    System.err.println("illegal arguments:");
                break;

            } //end switch

        } //end if 

        _monitor = monitor;
        _reporter = new EmailReporter(_fromAddressString, _toAddressString, _subjectString);

    }
    // ---------------------------------------------------------------------------------------
    private void readStandardArguments(int arrayIndex)
    {
        String header = "MonitorManager.readStandardArguments(): ";
       
        _dataDirPath = _argumentArray[arrayIndex++];
        _logFilePath = _argumentArray[arrayIndex++];
        _fileToMonitorPath = _argumentArray[arrayIndex++];
        _fromAddressString = _argumentArray[arrayIndex++];
        _toAddressString = _argumentArray[arrayIndex++];
        _subjectString = _argumentArray[arrayIndex++];
        
        System.out.println(header + " _subjectString = " + _subjectString);
        
        return;
        
    }
    // ---------------------------------------------------------------------------------------
    private MonitorType getMonitorType()
    {
       
       String monitorTypeString = parseStringArgWithDash(0);
       MonitorType type = _monitorTypeMap.get(monitorTypeString);
       return type;
    }
     
    // ---------------------------------------------------------------------------------------
    
    private MonitorOperator getOperator()
    {
       String header = "MonitorManager.getOperator(): ";
       String operatorString = parseStringArgWithDash(1).toUpperCase();
       System.out.println(header + " operatorString = " + operatorString);
       MonitorOperator operator = _operatorMap.get(operatorString);
       
       if (operator != null)
       {
           System.out.println(header + "operator = " + operator.getSymbol());
       }
       
       return operator;
    }
   
   
    // ---------------------------------------------------------------------------------------
    public Monitor getMonitor()
    {
    
        return _monitor;
    }
    
    // ---------------------------------------------------------------------------------------
    public Reporter getReporter()
    {
      
        return _reporter;
    }
  
    // ---------------------------------------------------------------------------------------
    public static void main(String[] argArray)
    {
         MonitorManager manager = new MonitorManager(argArray);
         
         Monitor monitor = manager.getMonitor();
         
         Reporter reporter = manager.getReporter();
         
         
         Status status = null;
         if (monitor != null)
         {
             status = monitor.monitor();
         }
         
         if (status != null)
         {
             reporter.report(status);
         }
         
         return;
         
    }
    // ---------------------------------------------------------------------------------------
}
