package ohd.hseb.raxdb_sync;

import java.sql.SQLException;
import java.util.List;

import ohd.hseb.db.DbRecord;
import ohd.hseb.db.DbTable;
import ohd.hseb.db.DbTimeHelper;
import ohd.hseb.model.ParamCode;
import ohd.hseb.util.StringHelper;

public abstract class BaseDifferenceMgr implements IDifferenceMgr
{
    protected RaxSyncDataMgr _dm;
    protected int _numberOfIhfsRecords = 0;
    protected int _numberOfSkippedRecords = 0;
    protected int _numberOfNewRecords = 0;
    protected int _numberOfSameRecords = 0;
    protected int _numberOfModRecords = 0;
    protected int _numberOfRaxRecordsInserted = 0;
    protected int _numberOfRaxRecordsUpdated = 0;
    
    protected static int UPDATE = 0;
    protected static int INSERT = 1;

    private String _newRecordsLogFile = null;
    private String _diffRecordsLogFile = null;
    private RaxSyncFileMgr _loggerForNew = null;
    private RaxSyncFileMgr _loggerForDiffs = null;
    private RaxSyncFileMgr _loggerForDebug = null;

    abstract public DiffSet findDifferences();
    abstract public void reportDifferences(RecordDifference oneRecordDifference);
    abstract public int processDifferences(RecordDifference oneRecordDifference);
    abstract protected int update(DbTable dbTable, DbRecord dbRecordOld, DbRecord dbRecordNew) throws SQLException;
    abstract protected int insert(DbTable dbTable, DbRecord dbRecord) throws SQLException;
    abstract protected String formatColumnsForInsertUpdateDisplay(DbRecord dbRecord);
    
    //----------------------------------------------------------------------------------------------
    protected void setDataMgr(RaxSyncDataMgr dm)
    {
        _dm = dm;
    }

    //----------------------------------------------------------------------------------------------
    protected void logApplicationMessage(String message)
    {
        _dm.getMainAppLogger().log(message);
    }

    //----------------------------------------------------------------------------------------------
    protected void logRecordDifferenceMessage(String message)
    {
        _loggerForDiffs.log(message);
    }

    //----------------------------------------------------------------------------------------------
    protected void logNewRecordMessage(String message)
    {
        _loggerForNew.log(message);
    }
    //----------------------------------------------------------------------------------------------
    protected void logDebugMessage(String message)
    {
        if (_dm.isDebugOutputOn())
            _loggerForDebug.log(message);
    }

    //----------------------------------------------------------------------------------------------
    protected void logInsertUpdateMessage(String message)
    {
        _dm.getInsertUpdateLogger().log(message);
    }

    //----------------------------------------------------------------------------------------------
    protected void checkAndAddToFieldDifferenceList(String fieldName, String ihfsString, String raxString, 
            List fieldDifferenceList)
    {
        boolean stringsEqual = areStringsEqual(ihfsString, raxString);
        
        // if the IHFS String (the master) is null or zero length then don't consider it different
        if (ihfsString == null)
            stringsEqual = true;
        else if (ihfsString.length() == 0)
            stringsEqual = true;

        if (! stringsEqual)
        {
            FieldDifference aFieldDifference = new FieldDifference(fieldName, ihfsString, raxString);
            fieldDifferenceList.add(aFieldDifference);
        }
    } // end of method

    //----------------------------------------------------------------------------------------------
    protected void checkAndAddToFieldDifferenceList(String fieldName, int int1, int int2, 
            List fieldDifferenceList)
    {
        if (int1 != int2)
        {
            FieldDifference aFieldDifference = new FieldDifference(fieldName, displayInt(int1), displayInt(int2));
            fieldDifferenceList.add(aFieldDifference);
        }
    } // end of method

    //----------------------------------------------------------------------------------------------
    protected void checkAndAddToFieldDifferenceList(String fieldName, double double1, double double2, 
            List fieldDifferenceList)
    {
        final double smallNumber = 0.1 ;
        boolean valuesEqual = areDoublesEqual(double1, double2, smallNumber);
        if (! valuesEqual)
        {
            FieldDifference aFieldDifference = new FieldDifference(fieldName, displayDouble(double1), displayDouble(double2));
            fieldDifferenceList.add(aFieldDifference);
        }
    } // end of method
    
    //----------------------------------------------------------------------------------------------
    protected void checkAndAddToFieldDifferenceList(String fieldName, double double1, double double2, double decimalPlaces, 
            List fieldDifferenceList)
    {
        FieldDifference aFieldDifference;
        final double smallNumber = Math.pow(0.1, decimalPlaces);
        boolean valuesEqual = areDoublesEqual(double1, double2, smallNumber);
        if (! valuesEqual)
        {
            if (decimalPlaces == 4)
                aFieldDifference = new FieldDifference(fieldName, displayLatLon(double1), displayLatLon(double2));
            else
                aFieldDifference = new FieldDifference(fieldName, displayDouble(double1), displayDouble(double2));
                
            fieldDifferenceList.add(aFieldDifference);
        }
    } // end of method
    
    //----------------------------------------------------------------------------------------------
    private boolean areDoublesEqual(double double1, double double2, double acceptableError)
    {
        boolean result = false;
        
        if ( Math.abs(double1 - double2) <= acceptableError )
        {
            result = true;
        }

        // if both doubles are null values then concider them equal 
        if (DbTable.isNull(double1) && DbTable.isNull(double2))
            result = true;
            
        return result;
    } // end of method
    
    //----------------------------------------------------------------------------------------------
    private boolean areStringsEqual(String string1, String string2)
    {
        return areStringsEqual(string1, string2, false);
    }
   
    //----------------------------------------------------------------------------------------------
    private boolean areStringsEqual(String string1, String string2, boolean caseSensitive)
    {
        boolean result = false;
        if (string1 == null) 
        {
           if (string2 == null)
           {
              result = true;
           }
           else
           {
              result = false;
           }  
        }
        
        else if (string2 == null)
        {
            result = false;
        }
        
        else //neither is null
        {
            if (caseSensitive)
            {
                result = string1.equals(string2);
            }
            else 
            {
                result = string1.equalsIgnoreCase(string2);
            }
        }
        return result;
    } // end of method

    //----------------------------------------------------------------------------------------------
    protected String displayDouble(final double doubleValue)
    {
        String doubleToDisplay = null;

        if (DbTable.isNull(doubleValue))
        {
            doubleToDisplay = "null";
        }
        else
        {
            doubleToDisplay = doubleValue+""; 
        }

        return doubleToDisplay;

    }  // end of displayDouble method
    
    //----------------------------------------------------------------------------------------------
    protected String displayLatLon(final double doubleValue)
    {
        String latlonToDisplay = null;

        if (DbTable.isNull(doubleValue))
        {
            latlonToDisplay = "null";
        }
        else
        {
            latlonToDisplay = _dm.formatLatLon(doubleValue)+""; 
        }

        return latlonToDisplay;

    }  // end of displayLatLon method
    
    //----------------------------------------------------------------------------------------------
    protected String displayInt(final int intValue)
    {
        String intToDisplay = null;

        if (DbTable.isNull(intValue))
        {
            intToDisplay = "null";
        }
        else
        {
            intToDisplay = intValue+""; 
        }

        return intToDisplay;

    }  // end of displayint method
    
    //----------------------------------------------------------------------------------------------
    protected String displayDate(final long dateAsLong)
    {
        String dateToDisplay = null;

        if (DbTable.isNull(dateAsLong))
        {
            dateToDisplay = "null";
        }
        else
        {
            dateToDisplay = DbTimeHelper.getDateStringFromLongTime(dateAsLong); 
        }

        return dateToDisplay;

    }  // end of displayDate method

    //----------------------------------------------------------------------------------------------
    protected String displayDateTime(final long dateAsLong)
    {
        String dateToDisplay = null;

        if (DbTable.isNull(dateAsLong))
        {
            dateToDisplay = "null";
        }
        else
        {
            dateToDisplay = DbTimeHelper.getDateTimeStringFromLongTime(dateAsLong); 
        }

        return dateToDisplay;

    }  // end of displayDate method

    //----------------------------------------------------------------------------------------------
    protected String displayTime(final long timeAsLong)
    {
        String timeToDisplay = null;

        if (DbTable.isNull(timeAsLong))
        {
            timeToDisplay = "null";
        }
        else
        {
            timeToDisplay = DbTimeHelper.getTimeToSecondsStringFromLongTime(timeAsLong); 
        }

        return timeToDisplay;

    }  // end of displayTime method

    //----------------------------------------------------------------------------------------------
    protected String createRaxPeOrTsFromIhfs(String ihfsValue, int subColumn)
    {
        String raxColumnValue = null;
        
        // RAX pe1 or t equals the first character of the pe or ts column in the IHFS db
        // RAX pe2 or s equals the second character of the pe or ts column in the IHFS db
            if (ihfsValue != null)
            {
                if (ihfsValue.length() > 1)
                {
                    if (subColumn == 1)
                        raxColumnValue = ihfsValue.substring(0,1);
                    else
                        raxColumnValue = ihfsValue.substring(1,2);
                }
            }

        return raxColumnValue;

    }// end of createRaxPeOrTsFromIhfs method

    //----------------------------------------------------------------------------------------------
    protected String setRaxDurFromIhfsDur(int ihfsDur)
    {
        String shefDur = "?";
        String raxDur = "V";
        
        // set RAX dur field charater based on IHFS dur field number,
        // if no match found then set it to "V"

        shefDur = ParamCode.getShefDurationCode(ihfsDur);
        
        if (shefDur.equalsIgnoreCase("?"))
            raxDur = "V";
        else
            raxDur = shefDur;
        
        return raxDur;
        
    }// end of setRaxDurFromIhfsDur method

    //----------------------------------------------------------------------------------------------
    protected double ConvertRaxLonFromIhfsLon(double ihfsLon)
    {
        double raxLon = 0.0;
        
        // get RAX lon field by multiplying IHFS lon by -1.0
        // if IHFS is null then leave it null

        if (DbTable.isNull(ihfsLon))       
            raxLon = ihfsLon;
        else
            raxLon = ihfsLon * -1.0;
        
        return raxLon;
        
    }// end of ConvertRaxLonFromIhfsLon method

    //----------------------------------------------------------------------------------------------
    protected long currentTimeMillisTrimmedToSeconds()
    {
        long trimmedToWholeSecond = (System.currentTimeMillis() / 1000) * 1000;
        
        return trimmedToWholeSecond;
        
    }// end of currentTimeMillisTrimmedToSeconds method

    //----------------------------------------------------------------------------------------------
    protected String makeMixedCase(String upperCase)
    {
        String mixedCase = null;
        
        // Call the method in HSEB's StringHelper class to
        // 1) convert the string to ALL lowercase
        // 2) seperate into individual words
        // 3) capitalize the first letter of each word
        // 4) assmble it back into a string
        if (upperCase != null)
        {
            mixedCase = StringHelper.capitalizeFirstLetterOfWords(upperCase);
        }

        return mixedCase;
        
    }// end of makeMixedCase method
    
    //----------------------------------------------------------------------------------------------
    protected void displayEndOfFindDifferencesMessage(String tableName)
    {
        logApplicationMessage(_numberOfIhfsRecords + " = Number of " + tableName + " records found in the IHFS db");

        if (_numberOfSkippedRecords > 0)
            logApplicationMessage(_numberOfSkippedRecords + " = Number of IHFS " + tableName + " records which were NOT compared to those in the RAX db");

        logApplicationMessage(_numberOfSameRecords + " = Number of IHFS " + tableName + " records which are 'The Same' as those in the RAX db");
        logApplicationMessage(_numberOfModRecords + " = Number of IHFS " + tableName + " records which are 'Different' than those in the RAX db");
        logApplicationMessage(_numberOfNewRecords + " = Number of IHFS " + tableName + " records which are NOT found in the RAX db");

        if (_numberOfModRecords > 0)
            logApplicationMessage("Listing of Differences found between " + tableName + " tables are found in file\n" + _diffRecordsLogFile);        

        if (_numberOfNewRecords > 0)
            logApplicationMessage("Listing of New " + tableName + " records to be added to the RAX db are found in file\n" + _newRecordsLogFile);

        logApplicationMessage(_dm.getDateTimeStamp() + " End of Analysis of " + tableName + " Table.");

        closeOutputFiles();

    } // end of method

    //----------------------------------------------------------------------------------------------
    protected void displayInsertUpdateStatsMessage(String tableName)
    {
        logApplicationMessage(_numberOfRaxRecordsUpdated + " = Number of " + tableName + " records Updated in the RAX db");
        logApplicationMessage(_numberOfRaxRecordsInserted + " = Number of " + tableName + " records Inserted in the RAX db");
    } // end of method

    //---------------------------------------------------------------------
    protected void setupOutputFiles(String tableName)
    {
        String directoryName = null;
        String debugLogFile = null;

        directoryName = _dm.getLogsDirectory();
              
        _newRecordsLogFile = directoryName + "/" + tableName + "RecordsNew.out";
        _diffRecordsLogFile = directoryName + "/" + tableName + "RecordsDifferent.out";
        debugLogFile = directoryName + "/" + tableName + "Debug.out";
 
        _loggerForNew = new RaxSyncFileMgr(_newRecordsLogFile, true, false);
        _loggerForDiffs = new RaxSyncFileMgr(_diffRecordsLogFile, true, false);
        _loggerForDebug = new RaxSyncFileMgr(debugLogFile, true, false);

    } // end of setupOutputFiles method

    //---------------------------------------------------------------------
    private void closeOutputFiles()
    {
        _loggerForNew.close();
        _loggerForDiffs.close();
        _loggerForDebug.close();
        
    } // end of closeOutputFiles method

    //----------------------------------------------------------------------------------------------
    public int processRaxInsertUpdate(int transactionType, String tablename, DbTable raxTable, DbRecord raxRecord, DbRecord modifiedRaxRecord)
    {

        int numberOfFailedUpdateInserts = 0;
        String recordCountErrorMessage = null;
        String exceptionMessage = null;

        try
        {
            int recordCount = 0;
            logInsertUpdateMessage("");

            if (transactionType == UPDATE)
            {
                recordCountErrorMessage = " records were Updated!";
                exceptionMessage = " ERROR - Unable to Update";
                recordCount = update(raxTable, raxRecord, modifiedRaxRecord);
            }
            else // Insert
            {
                recordCountErrorMessage = " records were Inserted!";
                exceptionMessage = " ERROR - Unable to Insert";
                recordCount = insert(raxTable, raxRecord);
            }

            if (recordCount != 1)
            {
                logInsertUpdateMessage("\nERROR - " + recordCount + recordCountErrorMessage);
            }
            else // success
            {
                if (transactionType == UPDATE)
                {
                    logInsertUpdateMessage(formatInsertUpdateMessage(" Replaced old ", tablename, formatColumnsForInsertUpdateDisplay(raxRecord)));
                    logInsertUpdateMessage(formatInsertUpdateMessage(" Updated with ", tablename, formatColumnsForInsertUpdateDisplay(modifiedRaxRecord)));
                    _numberOfRaxRecordsUpdated++;
                }
                else // insert
                {
                    logInsertUpdateMessage(formatInsertUpdateMessage(" Inserted new ", tablename, formatColumnsForInsertUpdateDisplay(raxRecord)));
                    _numberOfRaxRecordsInserted++;
                }
            }
        }
        catch (SQLException e)
        {
            logInsertUpdateMessage(formatInsertUpdateMessage(exceptionMessage, tablename, formatColumnsForInsertUpdateDisplay(raxRecord)));
            logInsertUpdateMessage("Postgres log output " + e.getMessage());
            numberOfFailedUpdateInserts++;
        }

        return numberOfFailedUpdateInserts;
        
    } // end of processRaxInsertUpdate method

    //----------------------------------------------------------------------------------------------
    protected String formatInsertUpdateMessage(String type, String tableName, String formattedColumns)
    {
        String formattedMessage = new String(_dm.getDateTimeStamp() + type + " RAX " + tableName + " record = " + formattedColumns);
        return formattedMessage;
    } // end of method

} // end of BaseDifferenceMgr Base (abstract) class
