package ohd.hseb.vtecevent;

import java.sql.SQLException;
import java.util.ArrayList;
import java.util.List;

import javax.swing.JOptionPane;

import ohd.hseb.db.Database;
import ohd.hseb.ihfsdb.generated.VTECeventRecord;
import ohd.hseb.ihfsdb.generated.VTECeventTable;
import ohd.hseb.util.CodeTimer;
import ohd.hseb.util.SessionLogger;
import ohd.hseb.monitor.river.RiverMonitorVtecEventDataManager;

public class VtecEventDataManager 
{
	private Database _db = null;
	private SessionLogger _logger = null;
	private String _missingRepresentation; 
	
	public VtecEventDataManager(Database dbHandler, SessionLogger logger, String missingRepresentation)
	{
		super();
		_db = dbHandler;
		_logger = logger;
		_missingRepresentation = missingRepresentation;
		
		if (_db == null)
		{
			JOptionPane.showMessageDialog(null, "Database should be provided","Vtec Application", JOptionPane.PLAIN_MESSAGE);
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
	
	public List readDataFromVtecEvent()
	{
		List vtecEventInfoList = null;
		List dataList = null;
		
		VTECeventTable vtecEventTable = new VTECeventTable(_db);
		VTECeventRecord  vtecEventRecord = null;
		String whereClause = null; 
		String header = "VtecEventDataManager.readDataFromVtecEvent(): ";
		
		CodeTimer timer = new CodeTimer();
		
		try
		{
			whereClause = "order by geoid";
			timer.start();
			vtecEventInfoList = vtecEventTable.select(whereClause);
			timer.stop("Read from vtec event elapsed time:");
			_logger.log(header+ "Where Clause:"+ whereClause);
			if(vtecEventInfoList != null)
			{
				dataList = new ArrayList();
				for(int i=0;i < vtecEventInfoList.size(); i++)
				{
					vtecEventRecord = (VTECeventRecord) vtecEventInfoList.get(i);
					VtecJTableRowData rowData = new VtecJTableRowData(_missingRepresentation);
					String active = (RiverMonitorVtecEventDataManager.checkIfEventActive(vtecEventRecord)?"Y":"");
					//rowData.setVtecRecord(vtecEventRecord, active);
					
					rowData.setGeoId(vtecEventRecord.getGeoid());
					rowData.setAction(vtecEventRecord.getAction());
					rowData.setBeginTime(vtecEventRecord.getBegintime());
					rowData.setCrests(vtecEventRecord.getCrests());
					rowData.setCrestTime(vtecEventRecord.getCresttime());
					rowData.setEndTime(vtecEventRecord.getEndtime());
					rowData.setUGCExpireTime(vtecEventRecord.getExpiretime());
					rowData.setEtn(vtecEventRecord.getEtn());
					rowData.setFallTime(vtecEventRecord.getFalltime());
					rowData.setFallts(vtecEventRecord.getFallts());
					rowData.setImmedCause(vtecEventRecord.getImmed_cause());
					rowData.setOfficeId(vtecEventRecord.getOffice_id());
					rowData.setPhenom(vtecEventRecord.getPhenom());
					rowData.setProductId(vtecEventRecord.getProduct_id());
					rowData.setProductMode(vtecEventRecord.getProductmode());
					rowData.setProductTime(vtecEventRecord.getProducttime());
					rowData.setRecord(vtecEventRecord.getRecord());
					rowData.setRiseTime(vtecEventRecord.getRisetime());
					rowData.setRisets(vtecEventRecord.getRisets());
					rowData.setSeverity(vtecEventRecord.getSeverity());
					rowData.setSignif(vtecEventRecord.getSignif());
					rowData.setCrestValue(vtecEventRecord.getCrest_value());
		
					rowData.setActive(active);
					
					rowData.addAllCellsToMap();
					dataList.add(rowData);
				}
			}
			else
			{
				_logger.log(header+"VtecEventinfo list is null");
			}
		}
		catch(SQLException e)
		{
			logSQLException(e);
		}
		return dataList;
	}
	
	
}
