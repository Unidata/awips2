package ohd.hseb.fcstservice;   

import java.sql.*;
import ohd.hseb.db.*;
import javax.swing.JOptionPane;
import ohd.hseb.ihfsdb.generated.*;
import java.util.*;

public class LhvmDataManager//This is a singleton class
{
	private static LhvmLogger lhvmLogger = null;
	private static Database db = null;
	
	private static LhvmDataManager lhvmDataManager = null;
	private static String missingRepresentation = null;
	
	public static String databaseMessage = null;
	public static StringBuffer displayedMessageLhvmWindow;
	public static StringBuffer displayedMessageServiceWindow;
	public static StringBuffer displayedMessageDetermWindow;
	public static StringBuffer displayedMessageEnsembleWindow;
	public static StringBuffer displayedMessageWatsupWindow;
	public static StringBuffer displayedMessageReferencesWindow;
	
	private LhvmDataManager()
	{
	}

	public String getMissingRepresentation()
	{
		return missingRepresentation;
	}
	
	public static synchronized LhvmDataManager getSingleInstanceOfDataManager(String baseConnectionString, LhvmLogger lhvmLogger, String missingRepresentation)
	{	
		LhvmDataManager.lhvmLogger = lhvmLogger;
		LhvmDataManager.db = new Database();
		LhvmDataManager.missingRepresentation = missingRepresentation;
		
		if (baseConnectionString == null)
		{
			JOptionPane.showMessageDialog(null, "Database should be provided","LHVM Application", JOptionPane.PLAIN_MESSAGE);
			System.exit(0);
		}
		DbConnectionHelper helper = new DbConnectionHelper(baseConnectionString,"Data.props");
		String connectionString = helper.getConnectionString();
		db.connectWithDriverSearch(connectionString);
		
		if(lhvmDataManager == null)
		{
			LhvmDataManager.lhvmDataManager = new LhvmDataManager();
		}
		
		return LhvmDataManager.lhvmDataManager;
	}
	
	public void disconnect()
	{
		db.disconnect();
	}
	
	public void logSQLException(SQLException e)
	{
		lhvmLogger.log("SQL ERROR = " + e.getErrorCode() +  " " + e.getMessage());
		e.printStackTrace(lhvmLogger.getPrintWriter());
		lhvmLogger.log("End of stack trace");

	}
	
	public int deleteFromServiceTable(FcstPtServiceRecord rec)
	{
	   int ret = -1;
	   FcstPtServiceTable fcstPtServiceTable = new FcstPtServiceTable(db);
	   try
	   {
          ret = fcstPtServiceTable.delete(rec);
	   }
	   catch(SQLException e)
	   {
		   //logSQLException(e);
		   databaseMessage = e.getMessage();
	   }
	   return ret;
	}
	
	public int deleteFromDetermTable(FcstPtDetermRecord rec)
	{
	   int ret = -1;
	   FcstPtDetermTable fcstPtDetermTable = new FcstPtDetermTable(db);
	   try
	   {
          ret = fcstPtDetermTable.delete(rec);
	   }
	   catch(SQLException e)
	   {
		   //logSQLException(e);
		   databaseMessage = e.getMessage();
	   }
	   return ret;
	}
	
	public int deleteFromEnsembleTable(FcstPtESPRecord rec)
	{
	   int ret = -1;
	   FcstPtESPTable fcstPtEspTable = new FcstPtESPTable(db);
	   try
	   {
          ret = fcstPtEspTable.delete(rec);
	   }
	   catch(SQLException e)
	   {
		   //logSQLException(e);
		   databaseMessage = e.getMessage();
	   }
	   return ret;
	}
	
	public int deleteFromWatsupTable(FcstPtWatSupRecord rec)
	{
	   int ret = -1;
	   FcstPtWatSupTable fcstPtWatsupTable = new FcstPtWatSupTable(db);
	   try
	   {
          ret = fcstPtWatsupTable.delete(rec);
	   }
	   catch(SQLException e)
	   {
		   //logSQLException(e);
		   databaseMessage = e.getMessage();
	   }
	   return ret;
	}
	
	public int insertOrUpdateServiceTable(FcstPtServiceRecord rec)
	{
		int ret = -1;
		FcstPtServiceTable fcstPtServiceTable = new FcstPtServiceTable(db);
		try
		{
		    ret = fcstPtServiceTable.insertOrUpdate(rec);
		}
		catch(SQLException e)
		{ 
			//logSQLException(e);
			LhvmDataManager.databaseMessage = e.getMessage();
		}
		return ret;
	}
	public int insertOrUpdateDetermTable(FcstPtDetermRecord rec)
	{
		int ret = -1;
		FcstPtDetermTable fcstPtDetermTable = new FcstPtDetermTable(db);
		try
		{
		    ret = fcstPtDetermTable.insertOrUpdate(rec);
		}
		catch(SQLException e)
		{ 
			//logSQLException(e);
			LhvmDataManager.databaseMessage = e.getMessage();
		}
		return ret;
	}
	
	public int insertOrUpdateEnsembleTable(FcstPtESPRecord rec)
	{
		int ret = -1;
		FcstPtESPTable fcstPtEnsembleTable = new FcstPtESPTable(db);
		try
		{
		    ret = fcstPtEnsembleTable.insertOrUpdate(rec);
		}
		catch(SQLException e)
		{ 
			//logSQLException(e);
			LhvmDataManager.databaseMessage = e.getMessage();
		}
		return ret;
	}
	
	public int insertOrUpdateWatsupTable(FcstPtWatSupRecord rec)
	{
		int ret = -1;
		FcstPtWatSupTable fcstPtWatsupTable = new FcstPtWatSupTable(db);
		try
		{
		    ret = fcstPtWatsupTable.insertOrUpdate(rec);
		}
		catch(SQLException e)
		{ 
			//logSQLException(e);
			LhvmDataManager.databaseMessage = e.getMessage();
		}
		return ret;
	}
		
	public int entriesPerLidInDeterm(String lid)
	{
		int ret = 0;
		String where = new String("where lid = '"+lid+"'") ;
			
		FcstPtDetermTable determTable = new FcstPtDetermTable(db);
		try
		{
			ret = determTable.selectCount(where);
		}
		catch(SQLException e){LhvmDataManager.databaseMessage = e.getMessage();return -1;}
		
		return ret;
	}
	
	public int entriesPerLidInService(String lid)
	{
		int ret = 0;
		String where = new String("where lid = '"+lid+"'") ;
			
		FcstPtServiceTable serviceTable = new FcstPtServiceTable(db);
		try
		{
			ret = serviceTable.selectCount(where);
		}
		catch(SQLException e){LhvmDataManager.databaseMessage = e.getMessage();return -1;}
		
		return ret;
	}
	
	public int entriesPerLidInWatsup(String lid)
	{
		int ret = 0;
		String where = new String("where lid = '"+lid+"'") ;
			
		FcstPtWatSupTable watsupTable = new FcstPtWatSupTable(db);
		try
		{
			ret = watsupTable.selectCount(where);
		}
		catch(SQLException e){LhvmDataManager.databaseMessage = e.getMessage();return -1;}
		
		return ret;
	}
	
	public int entriesPerLidInEnsemble(String lid)
	{
		int ret = 0;
		String where = new String("where lid = '"+lid+"'") ;
			
		FcstPtESPTable ensembleTable = new FcstPtESPTable(db);
		try
		{
			ret = ensembleTable.selectCount(where);
		}
		catch(SQLException e){LhvmDataManager.databaseMessage = e.getMessage();return -1;}
		
		return ret;
	}
	
	public List readDataFromPtServiceView()
	{
		List serviceTableViewInfmList = null;
		List dataList = new ArrayList();
		
		ServiceTableViewView serviceTableView = new ServiceTableViewView(db);
		ServiceTableViewRecord  serviceTableViewRecord = null;
		String whereClause = null;
		
		try
		{
			whereClause = "order by lid";
			serviceTableViewInfmList = serviceTableView.select(whereClause);
			//lhvmLogger.log(header+" Where Clause:"+ whereClause);
			if (serviceTableViewInfmList != null && serviceTableViewInfmList.size() > 0)
			{				
				for(int i=0;i < serviceTableViewInfmList.size(); i++)
				{
					serviceTableViewRecord = (ServiceTableViewRecord) serviceTableViewInfmList.get(i);
					ServiceTableViewViewJTableRowData rowData = new ServiceTableViewViewJTableRowData(LhvmDataManager.missingRepresentation);
					rowData.setLid(serviceTableViewRecord.getLid().toString());
					if(serviceTableViewRecord.getState() != null)
					{
						rowData.setState(serviceTableViewRecord.getState().toString());
					}
					else
					{
						rowData.setState(missingRepresentation);
					}
					if(serviceTableViewRecord.getCounty() != null)
					{
						rowData.setCounty(serviceTableViewRecord.getCounty().toString());
					}
					else
					{
						rowData.setCounty(missingRepresentation);
					}
					if(serviceTableViewRecord.getName() != null)
					{
						rowData.setLocName(serviceTableViewRecord.getName().toString());
					}
					else
					{
						rowData.setLocName(missingRepresentation);
					}
					if(serviceTableViewRecord.getStream() != null)
					{
						rowData.setStream(serviceTableViewRecord.getStream().toString());
					}
					else
					{
						rowData.setStream(missingRepresentation);
					}
					if(serviceTableViewRecord.getHsa() != null)
					{
						rowData.setHsa(serviceTableViewRecord.getHsa().toString());
					}
					else
					{
						rowData.setHsa(missingRepresentation);
					}
					dataList.add(rowData);
				}
				LhvmDataManager.databaseMessage = "Records found in the Service table view";
			}
			else
			{
				//lhvmLogger.log(header+" Officeinfm list is null");
				LhvmDataManager.databaseMessage = "No records in the Service table view";
			}
		}
		catch(SQLException e)
		{
			logSQLException(e);
			LhvmDataManager.databaseMessage = "Encountered an exception reading records from the Service table view";
		}
		return dataList;
	}
	
	public List readDataFromPtService()
	{
		List serviceTableInfmList = null;
		List dataList = new ArrayList();
		
		FcstPtServiceTable serviceTable = new FcstPtServiceTable(db);
		FcstPtServiceRecord  serviceTableRecord = null;
		String whereClause = null;
		
		try
		{
			whereClause = "order by lid";
			serviceTableInfmList = serviceTable.select(whereClause);
			//lhvmLogger.log(header+" Where Clause:"+ whereClause);
			if(serviceTableInfmList != null && serviceTableInfmList.size() >0)
			{
				for(int i=0;i < serviceTableInfmList.size(); i++)
				{
					serviceTableRecord = (FcstPtServiceRecord) serviceTableInfmList.get(i);
					FcstPtServiceJTableRowData rowData = new FcstPtServiceJTableRowData(LhvmDataManager.missingRepresentation);
					rowData.setLid(serviceTableRecord.getLid());
					rowData.setServiceType(serviceTableRecord.getService_type());
					rowData.setVerifRespType(serviceTableRecord.getVerif_resp_type());
					if(serviceTableRecord.getFlood_thres() > Double.MIN_VALUE && serviceTableRecord.getFlood_thres() < Double.MAX_VALUE)
					{
						Double val = new Double(serviceTableRecord.getFlood_thres());
						rowData.setFloodTher(val);
					}
					else
					{
						rowData.setFloodTher(DbTable.getNullDouble());
					}
					if(serviceTableRecord.getDrainage_area() > Double.MIN_VALUE && serviceTableRecord.getDrainage_area() < Double.MAX_VALUE)
					{
						Double val = new Double(serviceTableRecord.getDrainage_area());
						rowData.setDrainageArea(val);
					}
					else
					{
						rowData.setDrainageArea(DbTable.getNullDouble());
					}
					if(serviceTableRecord.getExceed_prob() >= 0 && serviceTableRecord.getExceed_prob() <= 100)
					{
						Integer val1 = new Integer(serviceTableRecord.getExceed_prob());
						rowData.setExceedProb(val1);
					}
					else
					{
						rowData.setExceedProb(DbTable.getNullInt());
					}
					if(DbTable.isNull(serviceTableRecord.getImpl_date()) || serviceTableRecord.getImpl_date() < Long.MIN_VALUE || serviceTableRecord.getImpl_date() > Long.MAX_VALUE)
					{
						rowData.setImplDate(DbTable.getNullLong());
					}
					else
					{
						//rowData.setImplDate(DbTimeHelper.getDateStringFromLongTime(serviceTableRecord.getImpl_date()));
						rowData.setImplDate(serviceTableRecord.getImpl_date());
					}
					if(DbTable.isNull(serviceTableRecord.getWeb_date()) || serviceTableRecord.getWeb_date() < Long.MIN_VALUE || serviceTableRecord.getWeb_date() > Long.MAX_VALUE)
					{
						rowData.setWebDate(DbTable.getNullLong());
					}
					else
					{
						//rowData.setWebDate(DbTimeHelper.getDateStringFromLongTime(serviceTableRecord.getWeb_date()));
						rowData.setWebDate(serviceTableRecord.getWeb_date());
					}
					if(DbTable.isNull(serviceTableRecord.getAnal_start_date()) || serviceTableRecord.getAnal_start_date() < Long.MIN_VALUE || serviceTableRecord.getAnal_start_date() > Long.MAX_VALUE)
					{
						rowData.setStartDate(DbTable.getNullLong());
					}
					else
					{
						//rowData.setStartDate(DbTimeHelper.getDateStringFromLongTime(serviceTableRecord.getAnal_start_date()));
						rowData.setStartDate(serviceTableRecord.getAnal_start_date());
					}
					if(DbTable.isNull(serviceTableRecord.getAnal_end_date()) || serviceTableRecord.getAnal_end_date() < Long.MIN_VALUE || serviceTableRecord.getAnal_end_date() > Long.MAX_VALUE)
					{
						rowData.setEndDate(DbTable.getNullLong());
					}
					else
					{
						//rowData.setEndDate(DbTimeHelper.getDateStringFromLongTime(serviceTableRecord.getAnal_end_date()));
						rowData.setEndDate(serviceTableRecord.getAnal_end_date());
					}
					rowData.addAllCellsToMap();
					dataList.add(rowData);
				}
				LhvmDataManager.databaseMessage = "Records found in service table";
			}
			else
			{
				//lhvmLogger.log(header+" Officeinfm list is null");
				LhvmDataManager.databaseMessage = "No records in Service table";
			}
		}
		catch(SQLException e)
		{
			logSQLException(e);
			LhvmDataManager.databaseMessage = "Encountered an exception reading records from the Service table";
		}
		return dataList;
	}
	
	public List readDataFromPtDeterm()
	{
		List determTableInfmList = null;
		List dataList = new ArrayList();
		
		FcstPtDetermTable determTable = new FcstPtDetermTable(db);
		FcstPtDetermRecord  determTableRecord = null;
		String whereClause = null;
	
		try
		{
			whereClause = "order by lid";
			determTableInfmList = determTable.select(whereClause);
			//lhvmLogger.log(header+" Where Clause:"+ whereClause);
			if(determTableInfmList != null && determTableInfmList.size() >0)
			{
				for(int i=0;i < determTableInfmList.size(); i++)
				{
					determTableRecord = (FcstPtDetermRecord) determTableInfmList.get(i);
					FcstPtDetermJTableRowData rowData = new FcstPtDetermJTableRowData(LhvmDataManager.missingRepresentation);
					rowData.setLid(determTableRecord.getLid());
					rowData.setHydraulMethod(determTableRecord.getHydraul_method());
					rowData.setHydrolMethod(determTableRecord.getHydrol_method());
					rowData.setSnowMethod(determTableRecord.getSnow_method());
					rowData.setIssueCriteria(determTableRecord.getDef_issue_crit());
					rowData.setUpstreamSeg(determTableRecord.getUpstream_seg());
					rowData.setReservoirModel(determTableRecord.getReservoir_model());
					rowData.setGenMethod(determTableRecord.getFcst_gen_method());
					if(determTableRecord.getFcst_horizon() != null)
					{
						rowData.setHorizon(determTableRecord.getFcst_horizon());
					}
					else
					{
						rowData.setHorizon(DbTable.getNullString());
					}
					if(determTableRecord.getConsumptive_use() != null)
					{
						rowData.setConsumptiveUse(determTableRecord.getConsumptive_use().toString());
					}
					else
					{
						rowData.setConsumptiveUse(DbTable.getNullString());
					}
					if(determTableRecord.getChannel_loss() != null)
					{
						rowData.setChannelLoss(determTableRecord.getChannel_loss());
					}
					else
					{
						rowData.setChannelLoss(DbTable.getNullString());
					}
					if(determTableRecord.getNum_elev_zones() < Short.MIN_VALUE || determTableRecord.getNum_elev_zones() > Short.MAX_VALUE)
					{
						rowData.setNumElevZones(DbTable.getNullInt());
					}
					else
					{
						rowData.setNumElevZones(determTableRecord.getNum_elev_zones());
					}
					if(DbTable.isNull(determTableRecord.getImpl_date()) || determTableRecord.getImpl_date() < Long.MIN_VALUE || determTableRecord.getImpl_date() > Long.MAX_VALUE)
					{
						rowData.setImplDate(DbTable.getNullLong());
					}
					else
					{
						rowData.setImplDate(determTableRecord.getImpl_date());
					}
					if(DbTable.isNull(determTableRecord.getWeb_date()) || determTableRecord.getWeb_date() < Long.MIN_VALUE || determTableRecord.getWeb_date() > Long.MAX_VALUE)
					{
						rowData.setWebDate(DbTable.getNullLong());
					}
					else
					{
						rowData.setWebDate(determTableRecord.getWeb_date());
					}
					if(determTableRecord.getFrequpd_normal() != null)
					{
						rowData.setNormal(determTableRecord.getFrequpd_normal());
					}
					else
					{
						rowData.setNormal(DbTable.getNullString());
					}
					if(determTableRecord.getFrequpd_flood() != null)
					{
						rowData.setFlood(determTableRecord.getFrequpd_flood());
					}
					else
					{
						rowData.setFlood(DbTable.getNullString());
					}
					
					if(determTableRecord.getVar_usage() != null)
					{
						rowData.setVarUsage(determTableRecord.getVar_usage());
					}
					else
					{
						rowData.setVarUsage(DbTable.getNullString());
					}
					if(determTableRecord.getFrequpd_drought() != null)
					{
						rowData.setDraught(determTableRecord.getFrequpd_drought());
					}
					else
					{
						rowData.setDraught(DbTable.getNullString());
					}
					if(determTableRecord.getHours_qpf() < Integer.MIN_VALUE || determTableRecord.getHours_qpf() > Integer.MAX_VALUE)
					{
						rowData.setQpf(DbTable.getNullInt());
					}
					else
					{
						rowData.setQpf(determTableRecord.getHours_qpf());
					}
					if(determTableRecord.getHours_qtf() < Integer.MIN_VALUE || determTableRecord.getHours_qtf() > Integer.MAX_VALUE)
					{
						rowData.setQtf(DbTable.getNullInt());
					}
					else
					{
						rowData.setQtf(determTableRecord.getHours_qtf());
					}
					if(determTableRecord.getHours_qzf() < Integer.MIN_VALUE || determTableRecord.getHours_qzf() > Integer.MAX_VALUE)
					{
						rowData.setQzf(DbTable.getNullInt());
					}
					else
					{
						rowData.setQzf(determTableRecord.getHours_qzf());
					}
					rowData.addAllCellsToMap();
					dataList.add(rowData);
				}
				LhvmDataManager.databaseMessage = "Records found in determ table";
			}
			else
			{
				//lhvmLogger.log(header+" Officeinfm list is null");
				LhvmDataManager.databaseMessage = "No records in Determ table";
			}
		}
		catch(SQLException e)
		{
			logSQLException(e);
			LhvmDataManager.databaseMessage = "Encountered an exception reading records from the Determ table";
		}
		return dataList;
	}
	
	public List readReservoirsFromPtDeterm()
	{
		List determTableInfmList = null;
		List dataList = new ArrayList();
		
		FcstPtDetermTable determTable = new FcstPtDetermTable(db);
		FcstPtDetermRecord  determTableRecord = null;
		String whereClause = null;
	
		try
		{
			whereClause = "WHERE reservoir_model NOT IN ('None') order by lid";
			determTableInfmList = determTable.select(whereClause);
			//lhvmLogger.log(header+" Where Clause:"+ whereClause);
			if(determTableInfmList != null && determTableInfmList.size() >0)
			{
				for(int i=0;i < determTableInfmList.size(); i++)
				{
					determTableRecord = (FcstPtDetermRecord) determTableInfmList.get(i);
					FcstPtDetermJTableRowData rowData = new FcstPtDetermJTableRowData(LhvmDataManager.missingRepresentation);
					rowData.setLid(determTableRecord.getLid().toString());
					rowData.setHydraulMethod(determTableRecord.getHydraul_method().toString());
					rowData.setHydrolMethod(determTableRecord.getHydrol_method().toString());
					rowData.setSnowMethod(determTableRecord.getSnow_method().toString());
					rowData.setIssueCriteria(determTableRecord.getDef_issue_crit().toString());
					rowData.setUpstreamSeg(determTableRecord.getUpstream_seg().toString());
					rowData.setReservoirModel(determTableRecord.getReservoir_model().toString());
					rowData.setGenMethod(determTableRecord.getFcst_gen_method().toString());
					if(determTableRecord.getFcst_horizon() != null)
					{
						rowData.setHorizon(determTableRecord.getFcst_horizon());
					}
					else
					{
						rowData.setHorizon(DbTable.getNullString());
					}
					if(determTableRecord.getConsumptive_use() != null)
					{
						rowData.setConsumptiveUse(determTableRecord.getConsumptive_use().toString());
					}
					else
					{
						rowData.setConsumptiveUse(missingRepresentation);
					}
					if(determTableRecord.getChannel_loss() != null)
					{
						rowData.setChannelLoss(determTableRecord.getChannel_loss());
					}
					else
					{
						rowData.setChannelLoss(missingRepresentation);
					}
					if(determTableRecord.getNum_elev_zones() < Short.MIN_VALUE || determTableRecord.getNum_elev_zones() > Short.MAX_VALUE)
					{
						rowData.setNumElevZones(DbTable.getNullInt());
					}
					else
					{
						rowData.setNumElevZones(determTableRecord.getNum_elev_zones());
					}
					if(DbTable.isNull(determTableRecord.getImpl_date()) || determTableRecord.getImpl_date() < Long.MIN_VALUE || determTableRecord.getImpl_date() > Long.MAX_VALUE)
					{
						rowData.setImplDate(DbTable.getNullLong());
					}
					else
					{
						rowData.setImplDate(determTableRecord.getImpl_date());
					}
					if(DbTable.isNull(determTableRecord.getWeb_date()) || determTableRecord.getWeb_date() < Long.MIN_VALUE || determTableRecord.getWeb_date() > Long.MAX_VALUE)
					{
						rowData.setWebDate(DbTable.getNullLong());
					}
					else
					{
						rowData.setWebDate(determTableRecord.getWeb_date());
					}
					if(determTableRecord.getFrequpd_normal() != null)
					{
						rowData.setNormal(determTableRecord.getFrequpd_normal());
					}
					else
					{
						rowData.setNormal(DbTable.getNullString());
					}
					if(determTableRecord.getFrequpd_flood() != null)
					{
						rowData.setFlood(determTableRecord.getFrequpd_flood());
					}
					else
					{
						rowData.setFlood(DbTable.getNullString());
					}
					if(determTableRecord.getVar_usage() != null)
					{
						rowData.setVarUsage(determTableRecord.getVar_usage());
					}
					else
					{
						rowData.setVarUsage(DbTable.getNullString());
					}
					if(determTableRecord.getFrequpd_drought() != null)
					{
						rowData.setDraught(determTableRecord.getFrequpd_drought());
					}
					else
					{
						rowData.setDraught(DbTable.getNullString());
					}
					if(determTableRecord.getHours_qpf() < Integer.MIN_VALUE || determTableRecord.getHours_qpf() > Integer.MAX_VALUE)
					{
						rowData.setQpf(DbTable.getNullInt());
					}
					else
					{
						rowData.setQpf(determTableRecord.getHours_qpf());
					}
					if(determTableRecord.getHours_qtf() < Integer.MIN_VALUE || determTableRecord.getHours_qtf() > Integer.MAX_VALUE)
					{
						rowData.setQtf(DbTable.getNullInt());
					}
					else
					{
						rowData.setQtf(determTableRecord.getHours_qtf());
					}
					if(determTableRecord.getHours_qzf() < Integer.MIN_VALUE || determTableRecord.getHours_qzf() > Integer.MAX_VALUE)
					{
						rowData.setQzf(DbTable.getNullInt());
					}
					else
					{
						rowData.setQzf(determTableRecord.getHours_qzf());
					}
					dataList.add(rowData);
					rowData.addAllCellsToMap();
				}
				LhvmDataManager.databaseMessage = "Records found in determ table";
			}
			else
			{
				//lhvmLogger.log(header+" Officeinfm list is null");
				LhvmDataManager.databaseMessage = "No records in Determ table";
			}
		}
		catch(SQLException e)
		{
			logSQLException(e);
			LhvmDataManager.databaseMessage = "Encountered an exception reading records from the Determ table";
		}
		return dataList;
	}
	
	public List readDataFromPtWatsup()
	{
		List watsupTableInfmList = null;
		List dataList = new ArrayList();
		
		FcstPtWatSupTable watsupTable = new FcstPtWatSupTable(db);
		FcstPtWatSupRecord  watsupTableRecord = null;
		String whereClause = null;
		
		try
		{
			whereClause = "order by lid";
			watsupTableInfmList = watsupTable.select(whereClause);
			//lhvmLogger.log(header+" Where Clause:"+ whereClause);
			if(watsupTableInfmList != null && watsupTableInfmList.size() > 0)
			{
				for(int i=0;i < watsupTableInfmList.size(); i++)
				{
					watsupTableRecord = (FcstPtWatSupRecord) watsupTableInfmList.get(i);
					FcstPtWatSupJTableRowData rowData = new FcstPtWatSupJTableRowData(LhvmDataManager.missingRepresentation);
					rowData.setLid(watsupTableRecord.getLid().toString());
					rowData.setNormal(watsupTableRecord.getFrequpd_normal().toString());
					rowData.setWatsupMethod(watsupTableRecord.getWatsup_method().toString());
					rowData.setWatsupCoordAgency(watsupTableRecord.getWatsup_coord_agency().toString());
					rowData.setPeriodReq(watsupTableRecord.getPeriod_req().toString());
					rowData.setWatsupCrit(watsupTableRecord.getWatsup_crit().toString());
					rowData.setRespAgency(watsupTableRecord.getWatsup_resp_agency().toString());
					if(watsupTableRecord.getCustomer_desc() != null)
					{
						rowData.setCustomerDesc(watsupTableRecord.getCustomer_desc().toString());
					}
					else
					{
						rowData.setCustomerDesc(missingRepresentation);
					}
					if(DbTable.isNull(watsupTableRecord.getImpl_date()) || watsupTableRecord.getImpl_date() < Long.MIN_VALUE || watsupTableRecord.getImpl_date() > Long.MAX_VALUE)
					{
						rowData.setImplDate(DbTable.getNullLong());
					}
					else
					{
						rowData.setImplDate(watsupTableRecord.getImpl_date());
					}
					if(DbTable.isNull(watsupTableRecord.getWeb_date()) || watsupTableRecord.getWeb_date() < Long.MIN_VALUE || watsupTableRecord.getWeb_date() > Long.MAX_VALUE)
					{
						rowData.setWebDate(DbTable.getNullLong());
					}
					else
					{
						rowData.setWebDate(watsupTableRecord.getWeb_date());
					}
					rowData.addAllCellsToMap();
					dataList.add(rowData);
				}
				LhvmDataManager.databaseMessage = "Records found in Watsup table";
			}
			else
			{
				//lhvmLogger.log(header+" Officeinfm list is null");
				LhvmDataManager.databaseMessage = "No records in Watsup table";
			}
		}
		catch(SQLException e)
		{
			logSQLException(e);
			LhvmDataManager.databaseMessage = "Encountered an exception reading records from the Watsup table";
		}
		return dataList;
	}
	
	public List readDataFromPtEnsemble()
	{
		List ensembleTableInfmList = null;
		List dataList = new ArrayList();
		
		FcstPtESPTable ensembleTable = new FcstPtESPTable(db);
		FcstPtESPRecord  ensembleTableRecord = null;
		String whereClause = null;
		
		try
		{
			whereClause = "order by lid";
			ensembleTableInfmList = ensembleTable.select(whereClause);
			//lhvmLogger.log(header+" Where Clause:"+ whereClause);
			if(ensembleTableInfmList != null && ensembleTableInfmList.size() >0)
			{
				for(int i=0;i < ensembleTableInfmList.size(); i++)
				{
					ensembleTableRecord = (FcstPtESPRecord) ensembleTableInfmList.get(i);
					FcstPtEnsembleJTableRowData rowData = new FcstPtEnsembleJTableRowData(LhvmDataManager.missingRepresentation);
					rowData.setLid(ensembleTableRecord.getLid().toString());
					rowData.setHydraulMethod(ensembleTableRecord.getHydraul_method().toString());
					rowData.setHydrolMethod(ensembleTableRecord.getHydrol_method().toString());
					rowData.setSnowMethod(ensembleTableRecord.getSnow_method().toString());
					rowData.setUpstreamSeg(ensembleTableRecord.getUpstream_seg().toString());
					rowData.setReservoirModel(ensembleTableRecord.getReservoir_model().toString());
					if(ensembleTableRecord.getFcst_horizon() != null)
					{
						rowData.setHorizon(ensembleTableRecord.getFcst_horizon());
					}
					else
					{
						rowData.setHorizon(missingRepresentation);
					}
					if(ensembleTableRecord.getConsumptive_use() != null)
					{
						rowData.setConsumptiveUse(ensembleTableRecord.getConsumptive_use().toString());
					}
					else
					{
						rowData.setConsumptiveUse(missingRepresentation);
					}
					if(ensembleTableRecord.getChannel_loss() != null)
					{
						rowData.setChannelLoss(ensembleTableRecord.getChannel_loss());
					}
					else
					{
						rowData.setChannelLoss(missingRepresentation);
					}
					if(ensembleTableRecord.getPost_processor() != null)
					{
						rowData.setPostProcessing(ensembleTableRecord.getPost_processor().toString());
					}
					else
					{
						rowData.setPostProcessing(missingRepresentation);
					}
					rowData.setFlowType(ensembleTableRecord.getFlowtype().toString());
					rowData.setFcstType(ensembleTableRecord.getFcsttype().toString());
					if(DbTable.isNull(ensembleTableRecord.getImpl_date()) || ensembleTableRecord.getImpl_date() < Long.MIN_VALUE || ensembleTableRecord.getImpl_date() > Long.MAX_VALUE)
					{
						rowData.setImplDate(DbTable.getNullLong());
					}
					else
					{
						rowData.setImplDate(ensembleTableRecord.getImpl_date());
					}
					if(DbTable.isNull(ensembleTableRecord.getWeb_date()) || ensembleTableRecord.getWeb_date() < Long.MIN_VALUE || ensembleTableRecord.getWeb_date() > Long.MAX_VALUE)
					{
						rowData.setWebDate(DbTable.getNullLong());
					}
					else
					{
						rowData.setWebDate(ensembleTableRecord.getWeb_date());
					}
					if(DbTable.isNull(ensembleTableRecord.getExternal_date()) || ensembleTableRecord.getExternal_date() < Long.MIN_VALUE || ensembleTableRecord.getExternal_date() > Long.MAX_VALUE)
					{
						rowData.setExternalDate(DbTable.getNullLong());
					}
					else
					{
						rowData.setExternalDate(ensembleTableRecord.getExternal_date());
					}
					if(ensembleTableRecord.getNum_elev_zones() < 0 || ensembleTableRecord.getNum_elev_zones() > Short.MAX_VALUE)
					{
						rowData.setWebDate(DbTable.getNullLong());
					}
					else
					{
						rowData.setNumElevZones(ensembleTableRecord.getNum_elev_zones());
					}
					if(ensembleTableRecord.getFrequpd_normal() != null)
					{
						rowData.setNormal(ensembleTableRecord.getFrequpd_normal().toString());
					}
					else
					{
						rowData.setNormal(DbTable.getNullString());
					}
					if(ensembleTableRecord.getFrequpd_flood() != null)
					{
						rowData.setFlood(ensembleTableRecord.getFrequpd_flood().toString());
					}
					else
					{
						rowData.setFlood(DbTable.getNullString());
					}
					if(ensembleTableRecord.getVar_usage() != null)
					{
						rowData.setVarUsage(ensembleTableRecord.getVar_usage());
					}
					else
					{
						rowData.setVarUsage(DbTable.getNullString());
					}
					if(ensembleTableRecord.getFrequpd_drought() != null)
					{
						rowData.setDraught(ensembleTableRecord.getFrequpd_drought().toString());
					}
					else
					{
						rowData.setDraught(DbTable.getNullString());
					}
					if(ensembleTableRecord.getNummonclim() < Integer.MIN_VALUE || ensembleTableRecord.getNummonclim() > Integer.MAX_VALUE)
					{
						rowData.setNumMonClim(DbTable.getNullInt());
					}
					else
					{
						rowData.setNumMonClim(ensembleTableRecord.getNummonclim());
					}
					if(ensembleTableRecord.getNumdayhyd() < Integer.MIN_VALUE || ensembleTableRecord.getNumdayhyd() > Integer.MAX_VALUE)
					{
						rowData.setNumDayHyd(DbTable.getNullInt());
					}
					else
					{
						rowData.setNumDayHyd(ensembleTableRecord.getNumdayhyd());
					}
					rowData.addAllCellsToMap();
					dataList.add(rowData);
				}
				LhvmDataManager.databaseMessage = "Records found in ensemble table";
			}
			else
			{
				//lhvmLogger.log(header+" Officeinfm list is null");
				LhvmDataManager.databaseMessage = "No records in Ensemble table";
			}
		}
		catch(SQLException e)
		{
			logSQLException(e);
			LhvmDataManager.databaseMessage = "Encountered an exception reading records from the Ensemble table";
		}
		return dataList;
	}
	
	public List readReservoirsFromPtEnsemble()
	{
		List ensembleTableInfmList = null;
		List dataList = new ArrayList();
		
		FcstPtESPTable ensembleTable = new FcstPtESPTable(db);
		FcstPtESPRecord  ensembleTableRecord = null;
		String whereClause = null;
		
		try
		{
			whereClause = "WHERE reservoir_model NOT IN ('None') order by lid";
			ensembleTableInfmList = ensembleTable.select(whereClause);
			//lhvmLogger.log(header+" Where Clause:"+ whereClause);
			if(ensembleTableInfmList != null && ensembleTableInfmList.size() > 0)
			{
				for(int i=0;i < ensembleTableInfmList.size(); i++)
				{
					ensembleTableRecord = (FcstPtESPRecord) ensembleTableInfmList.get(i);
					FcstPtEnsembleJTableRowData rowData = new FcstPtEnsembleJTableRowData(LhvmDataManager.missingRepresentation);
					rowData.setLid(ensembleTableRecord.getLid());
					rowData.setHydraulMethod(ensembleTableRecord.getHydraul_method());
					rowData.setHydrolMethod(ensembleTableRecord.getHydrol_method());
					rowData.setSnowMethod(ensembleTableRecord.getSnow_method());
					rowData.setUpstreamSeg(ensembleTableRecord.getUpstream_seg());
					rowData.setReservoirModel(ensembleTableRecord.getReservoir_model());
					if(ensembleTableRecord.getFcst_horizon() != null)
					{
						rowData.setHorizon(ensembleTableRecord.getFcst_horizon());
					}
					else
					{
						rowData.setHorizon(DbTable.getNullString());
					}
					if(ensembleTableRecord.getConsumptive_use() != null)
					{
						rowData.setConsumptiveUse(ensembleTableRecord.getConsumptive_use());
					}
					else
					{
						rowData.setConsumptiveUse(DbTable.getNullString());
					}
					if(ensembleTableRecord.getChannel_loss() != null)
					{
						rowData.setChannelLoss(ensembleTableRecord.getChannel_loss());
					}
					else
					{
						rowData.setChannelLoss(DbTable.getNullString());
					}
					if(ensembleTableRecord.getPost_processor() != null)
					{
						rowData.setPostProcessing(ensembleTableRecord.getPost_processor());
					}
					else
					{
						rowData.setPostProcessing(DbTable.getNullString());
					}
					rowData.setFlowType(ensembleTableRecord.getFlowtype());
					rowData.setFcstType(ensembleTableRecord.getFcsttype());
					if(DbTable.isNull(ensembleTableRecord.getImpl_date()) || ensembleTableRecord.getImpl_date() < Long.MIN_VALUE || ensembleTableRecord.getImpl_date() > Long.MAX_VALUE)
					{
						rowData.setImplDate(DbTable.getNullLong());
					}
					else
					{
						rowData.setImplDate(ensembleTableRecord.getImpl_date());
					}
					if(DbTable.isNull(ensembleTableRecord.getWeb_date()) || ensembleTableRecord.getWeb_date() < Long.MIN_VALUE || ensembleTableRecord.getWeb_date() > Long.MAX_VALUE)
					{
						rowData.setWebDate(DbTable.getNullLong());
					}
					else
					{
						rowData.setWebDate(ensembleTableRecord.getWeb_date());
					}
					if(DbTable.isNull(ensembleTableRecord.getExternal_date()) || ensembleTableRecord.getExternal_date() < Long.MIN_VALUE || ensembleTableRecord.getExternal_date() > Long.MAX_VALUE)
					{
						rowData.setExternalDate(DbTable.getNullLong());
					}
					else
					{
						rowData.setExternalDate(ensembleTableRecord.getExternal_date());
					}
					if(ensembleTableRecord.getNum_elev_zones() < 0 || ensembleTableRecord.getNum_elev_zones() > Short.MAX_VALUE)
					{
						rowData.setWebDate(DbTable.getNullLong());
					}
					else
					{
						rowData.setNumElevZones(ensembleTableRecord.getNum_elev_zones());
					}
					if(ensembleTableRecord.getFrequpd_normal() != null)
					{
						rowData.setNormal(ensembleTableRecord.getFrequpd_normal());
					}
					else
					{
						rowData.setNormal(DbTable.getNullString());
					}
					if(ensembleTableRecord.getFrequpd_flood() != null)
					{
						rowData.setFlood(ensembleTableRecord.getFrequpd_flood());
					}
					else
					{
						rowData.setFlood(DbTable.getNullString());
					}
					if(ensembleTableRecord.getVar_usage() != null)
					{
						rowData.setVarUsage(ensembleTableRecord.getVar_usage());
					}
					else
					{
						rowData.setVarUsage(DbTable.getNullString());
					}
					if(ensembleTableRecord.getFrequpd_drought() != null)
					{
						rowData.setDraught(ensembleTableRecord.getFrequpd_drought());
					}
					else
					{
						rowData.setDraught(DbTable.getNullString());
					}
					if(ensembleTableRecord.getNummonclim() < Integer.MIN_VALUE || ensembleTableRecord.getNummonclim() > Integer.MAX_VALUE)
					{
						rowData.setNumMonClim(DbTable.getNullInt());
					}
					else
					{
						rowData.setNumMonClim(ensembleTableRecord.getNummonclim());
					}
					if(ensembleTableRecord.getNumdayhyd() < Integer.MIN_VALUE || ensembleTableRecord.getNumdayhyd() > Integer.MAX_VALUE)
					{
						rowData.setNumDayHyd(DbTable.getNullInt());
					}
					else
					{
						rowData.setNumDayHyd(ensembleTableRecord.getNumdayhyd());
					}
					rowData.addAllCellsToMap();
					dataList.add(rowData);
				}
				LhvmDataManager.databaseMessage = "Records found in ensemble table";
			}
			else
			{
				//lhvmLogger.log(header+" Officeinfm list is null");
				LhvmDataManager.databaseMessage = "No records in Ensemble table";
			}
		}
		catch(SQLException e)
		{
			logSQLException(e);
			LhvmDataManager.databaseMessage = "Encountered an exception reading records from the Ensemble table";
		}
		return dataList;
	}
	
	public List readDataFromFcstType()
	{
		List fcstTypeInfmList = null;
		List dataList = new ArrayList();
		
		FcstTypeTable fcstTypeTable = new FcstTypeTable(db);
		FcstTypeRecord  fcstTypeJTableRecord = null;
		String whereClause = null;
		
		try
		{
			whereClause = "";
			fcstTypeInfmList = fcstTypeTable.select(whereClause);
			//lhvmLogger.log(header+" Where Clause:"+ whereClause);
			if(fcstTypeInfmList != null && fcstTypeInfmList.size() > 0)
			{
				for(int i=0;i < fcstTypeInfmList.size(); i++)
				{
					fcstTypeJTableRecord = (FcstTypeRecord) fcstTypeInfmList.get(i);
					FcstTypeJTableRowData rowData = new FcstTypeJTableRowData(LhvmDataManager.missingRepresentation);
					rowData.setFcstType(fcstTypeJTableRecord.getFcsttype().toString());
					rowData.addAllCellsToMap();
					dataList.add(rowData);
				}
				LhvmDataManager.databaseMessage = "Records found in the FcstType table";
			}
			else
			{
				//lhvmLogger.log(header+" Officeinfm list is null");
				LhvmDataManager.databaseMessage = "No records in the FcstType table";
			}
		}
		catch(SQLException e)
		{
			logSQLException(e);
			LhvmDataManager.databaseMessage = "Encountered an exception reading records from the FcstType table";
		}
		return dataList;
	}
	
	public List readDataFromFlowType()
	{
		List flowTypeInfmList = null;
		List dataList = new ArrayList();
		
		FlowTypeTable flowTypeTable = new FlowTypeTable(db);
		FlowTypeRecord  flowTypeJTableRecord = null;
		String whereClause = null;
		
		try
		{
			whereClause = "";
			flowTypeInfmList = flowTypeTable.select(whereClause);
			//lhvmLogger.log(header+" Where Clause:"+ whereClause);
			if(flowTypeInfmList != null && flowTypeInfmList.size() > 0)
			{
				for(int i=0;i < flowTypeInfmList.size(); i++)
				{
					flowTypeJTableRecord = (FlowTypeRecord) flowTypeInfmList.get(i);
					FlowTypeJTableRowData rowData = new FlowTypeJTableRowData(LhvmDataManager.missingRepresentation);
					rowData.setFlowType(flowTypeJTableRecord.getFlowtype().toString());
					rowData.addAllCellsToMap();
					dataList.add(rowData);
				}
				LhvmDataManager.databaseMessage = "Records found in the FlowType table";
			}
			else
			{
				//lhvmLogger.log(header+" Officeinfm list is null");
				LhvmDataManager.databaseMessage = "No records in the FlowType table";
			}
		}
		catch(SQLException e)
		{
			logSQLException(e);
			LhvmDataManager.databaseMessage = "Encountered an exception reading records from the FlowType table";
		}
		return dataList;
	}
	
	public List readDataFromHydrologicMethod()
	{
		List hydrologicMethodInfmList = null;
		List dataList = new ArrayList();
		
		HydrologicMethodTable hydrologicMethodTable = new HydrologicMethodTable(db);
		HydrologicMethodRecord  hydrologicMethodJTableRecord = null;
		String whereClause = null;
		
		try
		{
			whereClause = "";
			hydrologicMethodInfmList = hydrologicMethodTable.select(whereClause);
			//lhvmLogger.log(header+" Where Clause:"+ whereClause);
			if(hydrologicMethodInfmList != null && hydrologicMethodInfmList.size() > 0)
			{
				for(int i=0;i < hydrologicMethodInfmList.size(); i++)
				{
					hydrologicMethodJTableRecord = (HydrologicMethodRecord) hydrologicMethodInfmList.get(i);
					HydrologicMethodJTableRowData rowData = new HydrologicMethodJTableRowData(LhvmDataManager.missingRepresentation);
					rowData.setHydrologicMethod(hydrologicMethodJTableRecord.getHydrol_method().toString());
					rowData.addAllCellsToMap();
					dataList.add(rowData);
				}
				LhvmDataManager.databaseMessage = "Records found in the HydrologicMethod table";
			}
			else
			{
				//lhvmLogger.log(header+" Officeinfm list is null");
				LhvmDataManager.databaseMessage = "No records in the HydrologicMethod table";
			}
		}
		catch(SQLException e)
		{
			logSQLException(e);
			LhvmDataManager.databaseMessage = "Encountered an exception reading records from the HydrologicMethod table";
		}
		return dataList;
	}
	
	public List readDataFromRoutingMethod()
	{
		List routingMethodInfmList = null;
		List dataList = new ArrayList();
		
		RoutingMethodTable routingMethodTable = new RoutingMethodTable(db);
		RoutingMethodRecord  routingMethodJTableRecord = null;
		String whereClause = null;
		
		try
		{
			whereClause = "";
			routingMethodInfmList = routingMethodTable.select(whereClause);
			//lhvmLogger.log(header+" Where Clause:"+ whereClause);
			if(routingMethodInfmList != null && routingMethodInfmList.size() > 0)
			{
				for(int i=0;i < routingMethodInfmList.size(); i++)
				{
					routingMethodJTableRecord = (RoutingMethodRecord) routingMethodInfmList.get(i);
					RoutingMethodJTableRowData rowData = new RoutingMethodJTableRowData(LhvmDataManager.missingRepresentation);
					rowData.setRoutingMethod(routingMethodJTableRecord.getHydraul_method().toString());
					rowData.addAllCellsToMap();
					dataList.add(rowData);
				}
				LhvmDataManager.databaseMessage = "Records found in the RoutingMethod table";
			}
			else
			{
				//lhvmLogger.log(header+" Officeinfm list is null");
				LhvmDataManager.databaseMessage = "No records in the RoutingMethod table";
			}
		}
		catch(SQLException e)
		{
			logSQLException(e);
			LhvmDataManager.databaseMessage = "Encountered an exception reading records from the RoutingMethod table";
		}
		return dataList;
	}
	
	public List readDataFromIssueCriteria()
	{
		List issueCriteriaInfmList = null;
		List dataList = new ArrayList();
		
		DefiningIssueCriteriaTable issueCriteriaTable = new DefiningIssueCriteriaTable(db);
		DefiningIssueCriteriaRecord  issueCriteriaJTableRecord = null;
		String whereClause = null;
		
		try
		{
			whereClause = "";
			issueCriteriaInfmList = issueCriteriaTable.select(whereClause);
			//lhvmLogger.log(header+" Where Clause:"+ whereClause);
			if(issueCriteriaInfmList != null && issueCriteriaInfmList.size() > 0)
			{
				for(int i=0;i < issueCriteriaInfmList.size(); i++)
				{
					issueCriteriaJTableRecord = (DefiningIssueCriteriaRecord) issueCriteriaInfmList.get(i);
					IssueCriteriaJTableRowData rowData = new IssueCriteriaJTableRowData(LhvmDataManager.missingRepresentation);
					rowData.setIssueCriteria(issueCriteriaJTableRecord.getDef_issue_crit().toString());
					rowData.addAllCellsToMap();
					dataList.add(rowData);
				}
				LhvmDataManager.databaseMessage = "Records found in the IssueCriteria table";
			}
			else
			{
				//lhvmLogger.log(header+" Officeinfm list is null");
				LhvmDataManager.databaseMessage = "No records in the IssueCriteria table";
			}
		}
		catch(SQLException e)
		{
			logSQLException(e);
			LhvmDataManager.databaseMessage = "Encountered an exception reading records from the IssueCriteria table";
		}
		return dataList;
	}
	
	public List readDataFromSnowMethod()
	{
		List snowMethodInfmList = null;
		List dataList = new ArrayList();
		
		SnowMethodTable snowMethodTable = new SnowMethodTable(db);
		SnowMethodRecord  snowMethodJTableRecord = null;
		String whereClause = null;
		
		try
		{
			whereClause = "";
			snowMethodInfmList = snowMethodTable.select(whereClause);
			//lhvmLogger.log(header+" Where Clause:"+ whereClause);
			if(snowMethodInfmList != null && snowMethodInfmList.size() > 0)
			{
				for(int i=0;i < snowMethodInfmList.size(); i++)
				{
					snowMethodJTableRecord = (SnowMethodRecord) snowMethodInfmList.get(i);
					SnowMethodJTableRowData rowData = new SnowMethodJTableRowData(LhvmDataManager.missingRepresentation);
					rowData.setSnowMethod(snowMethodJTableRecord.getSnow_method().toString());
					rowData.addAllCellsToMap();
					dataList.add(rowData);
				}
				LhvmDataManager.databaseMessage = "Records found in the SnowMethod table";
			}
			else
			{
				//lhvmLogger.log(header+" Officeinfm list is null");
				LhvmDataManager.databaseMessage = "No records in the SnowMethod table";
			}
		}
		catch(SQLException e)
		{
			logSQLException(e);
			LhvmDataManager.databaseMessage = "Encountered an exception reading records from the SnowMethod table";
		}
		return dataList;
	}
	
	public List readDataFromWatsupCoordAgency()
	{
		List watsupCoordAgencyInfmList = null;
		List dataList = new ArrayList();
		
		WatSupCoordAgencyTable watsupCoordAgencyTable = new WatSupCoordAgencyTable(db);
		WatSupCoordAgencyRecord  watsupCoordAgencyJTableRecord = null;
		String whereClause = null;
		
		try
		{
			whereClause = "";
			watsupCoordAgencyInfmList = watsupCoordAgencyTable.select(whereClause);
			//lhvmLogger.log(header+" Where Clause:"+ whereClause);
			if(watsupCoordAgencyInfmList != null && watsupCoordAgencyInfmList.size() > 0)
			{
				for(int i=0;i < watsupCoordAgencyInfmList.size(); i++)
				{
					watsupCoordAgencyJTableRecord = (WatSupCoordAgencyRecord) watsupCoordAgencyInfmList.get(i);
					WatSupCoordAgencyJTableRowData rowData = new WatSupCoordAgencyJTableRowData(LhvmDataManager.missingRepresentation);
					rowData.setWatsupCoordAgency(watsupCoordAgencyJTableRecord.getWatsup_coord_agency().toString());
					rowData.addAllCellsToMap();
					dataList.add(rowData);
				}
				LhvmDataManager.databaseMessage = "Records found in the WatsupCoordAgency table";
			}
			else
			{
				//lhvmLogger.log(header+" Officeinfm list is null");
				LhvmDataManager.databaseMessage = "No records in the WatsupCoordAgency table";
			}
		}
		catch(SQLException e)
		{
			logSQLException(e);
			LhvmDataManager.databaseMessage = "Encountered an exception reading records from the WatsupCoordAgency table";
		}
		return dataList;
	}
	
	public List readDataFromWatsupCriterion()
	{
		List watsupCriterionInfmList = null;
		List dataList = new ArrayList();
		
		WatSupCriterionTable watsupCriterionTable = new WatSupCriterionTable(db);
		WatSupCriterionRecord  watsupCriterionJTableRecord = null;
		String whereClause = null;
		
		try
		{
			whereClause = "";
			watsupCriterionInfmList = watsupCriterionTable.select(whereClause);
			//lhvmLogger.log(header+" Where Clause:"+ whereClause);
			if(watsupCriterionInfmList != null && watsupCriterionInfmList.size() > 0)
			{
				for(int i=0;i < watsupCriterionInfmList.size(); i++)
				{
					watsupCriterionJTableRecord = (WatSupCriterionRecord) watsupCriterionInfmList.get(i);
					WatSupCriterionJTableRowData rowData = new WatSupCriterionJTableRowData(LhvmDataManager.missingRepresentation);
					rowData.setWatsupCriterion(watsupCriterionJTableRecord.getWatsup_criterion().toString());
					rowData.addAllCellsToMap();
					dataList.add(rowData);
				}
				LhvmDataManager.databaseMessage = "Records found in the WatsupCriterion table";
			}
			else
			{
				//lhvmLogger.log(header+" Officeinfm list is null");
				LhvmDataManager.databaseMessage = "No records in the WatsupCriterion table";
			}
		}
		catch(SQLException e)
		{
			logSQLException(e);
			LhvmDataManager.databaseMessage = "Encountered an exception reading records from the WatsupCriterion table";
		}
		return dataList;
	}
	
	public List readDataFromWatsupMethod()
	{
		List watsupMethodInfmList = null;
		List dataList = new ArrayList();
		
		WatSupMethodTable watsupMethodTable = new WatSupMethodTable(db);
		WatSupMethodRecord  watsupMethodJTableRecord = null;
		String whereClause = null;
		
		try
		{
			whereClause = "";
			watsupMethodInfmList = watsupMethodTable.select(whereClause);
			//lhvmLogger.log(header+" Where Clause:"+ whereClause);
			if(watsupMethodInfmList != null && watsupMethodInfmList.size() > 0)
			{
				for(int i=0;i < watsupMethodInfmList.size(); i++)
				{
					watsupMethodJTableRecord = (WatSupMethodRecord) watsupMethodInfmList.get(i);
					WatSupMethodJTableRowData rowData = new WatSupMethodJTableRowData(LhvmDataManager.missingRepresentation);
					rowData.setWatsupMethod(watsupMethodJTableRecord.getWatsup_method().toString());
					rowData.addAllCellsToMap();
					dataList.add(rowData);
				}
				LhvmDataManager.databaseMessage = "Records found in the WatsupMethod table";
			}
			else
			{
				//lhvmLogger.log(header+" Officeinfm list is null");
				LhvmDataManager.databaseMessage = "No records in the WatsupMethod table";
			}
		}
		catch(SQLException e)
		{
			logSQLException(e);
			LhvmDataManager.databaseMessage = "Encountered an exception reading records from the WatsupMethod table";
		}
		return dataList;
	}
	
	public List readDataFromFrequencyUpdate()
	{
		List frequencyUpdateInfmList = null;
		List dataList = new ArrayList();
		
		FrequencyUpdateTable frequencyUpdateTable = new FrequencyUpdateTable(db);
		FrequencyUpdateRecord  frequencyUpdateRecord = null;
		String whereClause = null;
		
		try
		{
			whereClause = "";
			frequencyUpdateInfmList = frequencyUpdateTable.select(whereClause);
			//lhvmLogger.log(header+" Where Clause:"+ whereClause);
			if(frequencyUpdateInfmList != null && frequencyUpdateInfmList.size() > 0)
			{
				for(int i=0;i < frequencyUpdateInfmList.size(); i++)
				{
					frequencyUpdateRecord = (FrequencyUpdateRecord) frequencyUpdateInfmList.get(i);
					FrequencyUpdateJTableRowData rowData = new FrequencyUpdateJTableRowData(LhvmDataManager.missingRepresentation);
					rowData.setFrequencyUpdate(frequencyUpdateRecord.getFrequency_update().toString());
					rowData.addAllCellsToMap();
					dataList.add(rowData);
				}
				LhvmDataManager.databaseMessage = "Records found in the FrequencyUpdate table";
			}
			else
			{
				//lhvmLogger.log(header+" Officeinfm list is null");
				LhvmDataManager.databaseMessage = "No records in the FrequencyUpdate table";
			}
		}
		catch(SQLException e)
		{
			logSQLException(e);
			LhvmDataManager.databaseMessage = "Encountered an exception reading records from the FrequencyUpdate table";
		}
		return dataList;
	}
	
	public List readDataFromRequiredPeriod()
	{
		List requiredPeriodInfmList = null;
		List dataList = new ArrayList();
		
		RequiredPeriodTable requiredPeriodTable = new RequiredPeriodTable(db);
		RequiredPeriodRecord  requiredPeriodJTableRecord = null;
		String whereClause = null;
		
		try
		{
			whereClause = "";
			requiredPeriodInfmList = requiredPeriodTable.select(whereClause);
			//lhvmLogger.log(header+" Where Clause:"+ whereClause);
			if(requiredPeriodInfmList != null && requiredPeriodInfmList.size() > 0)
			{
				for(int i=0;i < requiredPeriodInfmList.size(); i++)
				{
					requiredPeriodJTableRecord = (RequiredPeriodRecord) requiredPeriodInfmList.get(i);
					RequiredPeriodJTableRowData rowData = new RequiredPeriodJTableRowData(LhvmDataManager.missingRepresentation);
					rowData.setRequiredPeriod(requiredPeriodJTableRecord.getPeriod_req().toString());
					rowData.addAllCellsToMap();
					dataList.add(rowData);
				}
				LhvmDataManager.databaseMessage = "Records found in the RequiredPeriod table";
			}
			else
			{
				//lhvmLogger.log(header+" Officeinfm list is null");
				LhvmDataManager.databaseMessage = "No records in the RequiredPeriod table";
			}
		}
		catch(SQLException e)
		{
			logSQLException(e);
			LhvmDataManager.databaseMessage = "Encountered an exception reading records from the RequiredPeriod table";
		}
		return dataList;
	}
	
	public List readDataFromPostProcessing()
	{
		List postProcessingInfmList = null;
		List dataList = new ArrayList();
		
		PostProcessorTable postProcessingTable = new PostProcessorTable(db);
		PostProcessorRecord  postProcessingRecord = null;
		String whereClause = null;
		
		try
		{
			whereClause = "";
			postProcessingInfmList = postProcessingTable.select(whereClause);
			//lhvmLogger.log(header+" Where Clause:"+ whereClause);
			if(postProcessingInfmList != null && postProcessingInfmList.size() > 0)
			{
				for(int i=0;i < postProcessingInfmList.size(); i++)
				{
					postProcessingRecord = (PostProcessorRecord) postProcessingInfmList.get(i);
					PostProcessingJTableRowData rowData = new PostProcessingJTableRowData(LhvmDataManager.missingRepresentation);
					rowData.setPostProcessing(postProcessingRecord.getPost_processor().toString());
					rowData.addAllCellsToMap();
					dataList.add(rowData);
				}
				LhvmDataManager.databaseMessage = "Records found in the PostProcessor table";
			}
			else
			{
				//lhvmLogger.log(header+" Officeinfm list is null");
				LhvmDataManager.databaseMessage = "No records in the PostProcessor table";
			}
		}
		catch(SQLException e)
		{
			logSQLException(e);
			LhvmDataManager.databaseMessage = "Encountered an exception reading records from the PostProcessor table";
		}
		return dataList;
	}
	
	public List readDataFromHorizon()
	{
		List horizonInfmList = null;
		List dataList = new ArrayList();
		
		FcstHorizonTable horizonTable = new FcstHorizonTable(db);
		FcstHorizonRecord  horizonRecord = null;
		String whereClause = null;
		
		try
		{
			whereClause = "";
			horizonInfmList = horizonTable.select(whereClause);
			//lhvmLogger.log(header+" Where Clause:"+ whereClause);
			if(horizonInfmList != null && horizonInfmList.size() > 0)
			{
				for(int i=0;i < horizonInfmList.size(); i++)
				{
					horizonRecord = (FcstHorizonRecord) horizonInfmList.get(i);
					FcstHorizonJTableRowData rowData = new FcstHorizonJTableRowData(LhvmDataManager.missingRepresentation);
					rowData.setFcstHorizon(horizonRecord.getFcst_horizon().toString());
					rowData.addAllCellsToMap();
					dataList.add(rowData);
				}
				LhvmDataManager.databaseMessage = "Records found in the FcstHorizon table";
			}
			else
			{
				//lhvmLogger.log(header+" Officeinfm list is null");
				LhvmDataManager.databaseMessage = "No records in the FcstHorizon table";
			}
		}
		catch(SQLException e)
		{
			logSQLException(e);
			LhvmDataManager.databaseMessage = "Encountered an exception reading records from the FcstHorizon table";
		}
		return dataList;
	}
	
	public List readDataFromGenMethod()
	{
		List genMethodInfmList = null;
		List dataList = new ArrayList();
		
		FcstGenMethodTable genMethodTable = new FcstGenMethodTable(db);
		FcstGenMethodRecord  genMethodRecord = null;
		String whereClause = null;
		
		try
		{
			whereClause = "";
			genMethodInfmList = genMethodTable.select(whereClause);
			//lhvmLogger.log(header+" Where Clause:"+ whereClause);
			if(genMethodInfmList != null && genMethodInfmList.size() > 0)
			{
				for(int i=0;i < genMethodInfmList.size(); i++)
				{
					genMethodRecord = (FcstGenMethodRecord) genMethodInfmList.get(i);
					FcstGenMethodJTableRowData rowData = new FcstGenMethodJTableRowData(LhvmDataManager.missingRepresentation);
					rowData.setGenMethod(genMethodRecord.getFcst_gen_method().toString());
					rowData.addAllCellsToMap();
					dataList.add(rowData);
				}
				LhvmDataManager.databaseMessage = "Records found in the FcstGenMethod table";
			}
			else
			{
				//lhvmLogger.log(header+" Officeinfm list is null");
				LhvmDataManager.databaseMessage = "No records in the FcstGenMethod table";
			}
		}
		catch(SQLException e)
		{
			logSQLException(e);
			LhvmDataManager.databaseMessage = "Encountered an exception reading records from the FcstGenMethod table";
		}
		return dataList;
	}
	
	public List readDataFromReservoirModel()
	{
		List reservoirModelInfmList = null;
		List dataList = new ArrayList();
		
		ReservoirModelTable reservoirModelTable = new ReservoirModelTable(db);
		ReservoirModelRecord  reservoirModelRecord = null;
		String whereClause = null;
		
		try
		{
			whereClause = "";
			reservoirModelInfmList = reservoirModelTable.select(whereClause);
			//lhvmLogger.log(header+" Where Clause:"+ whereClause);
			if(reservoirModelInfmList != null && reservoirModelInfmList.size() > 0)
			{
				for(int i=0;i < reservoirModelInfmList.size(); i++)
				{
					reservoirModelRecord = (ReservoirModelRecord) reservoirModelInfmList.get(i);
					ReservoirModelJTableRowData rowData = new ReservoirModelJTableRowData(LhvmDataManager.missingRepresentation);
					rowData.setReservoirModel(reservoirModelRecord.getReservoir_model().toString());
					rowData.addAllCellsToMap();
					dataList.add(rowData);
				}
				LhvmDataManager.databaseMessage = "Records found in the ReservoirModel table";
			}
			else
			{
				//lhvmLogger.log(header+" Officeinfm list is null");
				LhvmDataManager.databaseMessage = "No records in the ReservoirModel table";
			}
		}
		catch(SQLException e)
		{
			logSQLException(e);
			LhvmDataManager.databaseMessage = "Encountered an exception reading records from the ReservoirModel table";
		}
		return dataList;
	}
	
	public List readDataFromServiceType()
	{
		List serviceTypeInfmList = null;
		List dataList = new ArrayList();
		
		ServiceTypeTable serviceTypeTable = new ServiceTypeTable(db);
		ServiceTypeRecord  serviceTypeRecord = null;
		String whereClause = null;
		
		try
		{
			whereClause = "";
			serviceTypeInfmList = serviceTypeTable.select(whereClause);
			//lhvmLogger.log(header+" Where Clause:"+ whereClause);
			if(serviceTypeInfmList != null && serviceTypeInfmList.size() > 0)
			{
				for(int i=0;i < serviceTypeInfmList.size(); i++)
				{
					serviceTypeRecord = (ServiceTypeRecord) serviceTypeInfmList.get(i);
					ServiceTypeJTableRowData rowData = new ServiceTypeJTableRowData(LhvmDataManager.missingRepresentation);
					rowData.setServiceType(serviceTypeRecord.getService_type().toString());
					rowData.addAllCellsToMap();
					dataList.add(rowData);
				}
				LhvmDataManager.databaseMessage = "Records found in the ServiceType table";
			}
			else
			{
				//lhvmLogger.log(header+" Officeinfm list is null");
				LhvmDataManager.databaseMessage = "No records in the ServiceType table";
			}
		}
		catch(SQLException e)
		{
			logSQLException(e);
			LhvmDataManager.databaseMessage = "Encountered an exception reading records from the ServiceType table";
		}
		return dataList;
	}
	
	public List readDataFromRespAgency()
	{
		List respAgencyInfmList = null;
		List dataList = new ArrayList();
		
		WatSupRespAgencyTable respAgencyTable = new WatSupRespAgencyTable(db);
		WatSupRespAgencyRecord  respAgencyRecord = null;
		String whereClause = null;
		
		try
		{
			whereClause = "";
			respAgencyInfmList = respAgencyTable.select(whereClause);
			//lhvmLogger.log(header+" Where Clause:"+ whereClause);
			if(respAgencyInfmList != null && respAgencyInfmList.size() > 0)
			{
				for(int i=0;i < respAgencyInfmList.size(); i++)
				{
					respAgencyRecord = (WatSupRespAgencyRecord) respAgencyInfmList.get(i);
					RespAgencyJTableRowData rowData = new RespAgencyJTableRowData(LhvmDataManager.missingRepresentation);
					rowData.setRespAgency(respAgencyRecord.getWatsup_resp_agency().toString());
					rowData.addAllCellsToMap();
					dataList.add(rowData);
				}
				LhvmDataManager.databaseMessage = "Records found in the WatSupRespAgency table";
			}
			else
			{
				//lhvmLogger.log(header+" Officeinfm list is null");
				LhvmDataManager.databaseMessage = "No records in the WatSupRespAgency table";
			}
		}
		catch(SQLException e)
		{
			logSQLException(e);
			LhvmDataManager.databaseMessage = "Encountered an exception reading records from the WatSupRespAgency table";
		}
		return dataList;
	}
	
	public List readDataFromVerificationResponseType()
	{
		List verifRespInfmList = null;
		List dataList = new ArrayList();
		
		VerifRespTypeTable verifRespTypeTable = new VerifRespTypeTable(db);
		VerifRespTypeRecord  verifRespTypeRecord = null;
		String whereClause = null;
		
		try
		{
			whereClause = "";
			verifRespInfmList = verifRespTypeTable.select(whereClause);
			//lhvmLogger.log(header+" Where Clause:"+ whereClause);
			if(verifRespInfmList != null && verifRespInfmList.size() > 0)
			{
				for(int i=0;i < verifRespInfmList.size(); i++)
				{
					verifRespTypeRecord = (VerifRespTypeRecord) verifRespInfmList.get(i);
					VerificationResponseTypeJTableRowData rowData = new VerificationResponseTypeJTableRowData(LhvmDataManager.missingRepresentation);
					rowData.setVerifRespType(verifRespTypeRecord.getVerif_resp_type().toString());
					rowData.addAllCellsToMap();
					dataList.add(rowData);
				}
				LhvmDataManager.databaseMessage = "Records found in the Verification Response Type table";
			}
			else
			{
				//lhvmLogger.log(header+" Officeinfm list is null");
				LhvmDataManager.databaseMessage = "No records in the Verification Response Type table";
			}
		}
		catch(SQLException e)
		{
			logSQLException(e);
			LhvmDataManager.databaseMessage = "Encountered an exception reading records from the Verification Response Type table";
		}
		return dataList;
	}
}
