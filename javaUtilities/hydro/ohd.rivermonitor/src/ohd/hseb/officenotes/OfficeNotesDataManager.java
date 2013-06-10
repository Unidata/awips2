package ohd.hseb.officenotes;

import java.sql.SQLException;
import java.util.ArrayList;
import java.util.List;

import javax.swing.JOptionPane;

import ohd.hseb.db.Database;
import ohd.hseb.ihfsdb.generated.OfficeNotesRecord;
import ohd.hseb.ihfsdb.generated.OfficeNotesTable;
import ohd.hseb.util.SessionLogger;

public class OfficeNotesDataManager 
{
	private Database _db = null;
	private SessionLogger _logger = null;
	private String _missingRepresentation; 
	
	public OfficeNotesDataManager(Database dbHandler, SessionLogger logger, String missingRepresentation)
	{
		super();
		_db = dbHandler;
		_logger = logger;
		_missingRepresentation = missingRepresentation;
		
		if (_db == null)
		{
			JOptionPane.showMessageDialog(null, "Database should be provided","OfficeNotes Application", JOptionPane.PLAIN_MESSAGE);
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
	
	public int delete(OfficeNotesRecord rec)
	{
	   int ret = -1;
	   OfficeNotesTable officeNotesTable = new OfficeNotesTable(_db);
	   try
	   {
          ret = officeNotesTable.delete(rec);
	   }
	   catch(SQLException e)
	   {
		   logSQLException(e);
	   }
	   return ret;
	}
	
	public int updateOrInsert(OfficeNotesRecord rec)
	{
		int ret = -1;
		OfficeNotesTable officeNotesTable = new OfficeNotesTable(_db);
		try
		{
		    ret = officeNotesTable.insertOrUpdate(rec);
		}
		catch(SQLException e)
		{ 
			logSQLException(e);
		}
		return ret;
	}
	
	public List readDataFromOfficeNotes()
	{
		List officeNotesInfoList = null;
		List dataList = null;
		
		OfficeNotesTable officeNotesTable = new OfficeNotesTable(_db);
		OfficeNotesRecord  officeNotesRecord = null;
		String whereClause = null; 
		String header = "OfficeNotesDataManager.readDataFromOfficeNotes(): ";
		
		try
		{
			whereClause = "order by topic, id, postingtime";
			officeNotesInfoList = officeNotesTable.select(whereClause);
			_logger.log(header+" Where Clause:"+ whereClause);
			if(officeNotesInfoList != null)
			{
				dataList = new ArrayList();
				for(int i=0;i < officeNotesInfoList.size(); i++)
				{
					officeNotesRecord = (OfficeNotesRecord) officeNotesInfoList.get(i);
					OfficeNotesJTableRowData rowData = new OfficeNotesJTableRowData(_missingRepresentation);
					rowData.setOfficeNotesRecord(officeNotesRecord);
					rowData.addAllCellsToMap();
					dataList.add(rowData);
				}
			}
			else
			{
				_logger.log(header+" Officeinfo list is null");
			}
		}
		catch(SQLException e)
		{
			logSQLException(e);
		}
		return dataList;
	}
    
    public int getCountOfOfficeNotes()
    {
        int numOfOfficeNotes = -1;
        List notesList = readDataFromOfficeNotes();
        if(notesList != null)
        {
            numOfOfficeNotes = notesList.size();
        }
        return numOfOfficeNotes;
    }
}
