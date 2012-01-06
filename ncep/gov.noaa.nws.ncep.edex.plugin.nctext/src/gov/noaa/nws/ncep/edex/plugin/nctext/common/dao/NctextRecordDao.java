/**
 * This Data Access Object implements database query methods to get NTEXT 
 * Record
 * 
 * <pre>
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    	Engineer    Description
 * ------------ ---------- 	----------- --------------------------
 * 10/29/2009		TBD		Chin Chen	Initial coding
 *
 * </pre>
 * 
 * @author Chin Chen
 * @version 1.0
 */
package gov.noaa.nws.ncep.edex.plugin.nctext.common.dao;

import java.util.ArrayList;
import java.util.Calendar;
import java.util.List;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import gov.noaa.nws.ncep.edex.common.dao.NcepDefaultPluginDao;
import gov.noaa.nws.ncep.edex.plugin.nctext.common.NctextRecord;

import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.common.dataplugin.persist.IPersistable;
import com.raytheon.uf.common.datastorage.IDataStore;
import com.raytheon.uf.edex.database.DataAccessLayerException;
import com.raytheon.uf.edex.database.plugin.PluginDao;

public class NctextRecordDao extends NcepDefaultPluginDao{
	private Log logger = LogFactory.getLog(getClass());
	/**
	 * @param pluginName
	 * @throws PluginException
	 */
	public NctextRecordDao(String pluginName) throws PluginException {
		super(pluginName);
		/*super(DaoConfig.forDatabase(PluginFactory.getInstance().getDatabase(
                pluginName)));*/
		// TODO Auto-generated constructor stub
	}
	


	/* (non-Javadoc)
	 * @see com.raytheon.edex.db.dao.PluginDao#populateDataStore(com.raytheon.uf.common.datastorage.IDataStore, com.raytheon.uf.common.dataplugin.persist.IPersistable)
	 */
	@Override
	protected IDataStore populateDataStore(IDataStore dataStore,
			IPersistable obj) throws Exception {
		// TODO Auto-generated method stub
		return null;
	}

	@SuppressWarnings("unchecked")
	public List<NctextRecord> getDataByDatauri(String datauri) {
	   	List<String> fields = new ArrayList<String>();
    	List<Object> values = new ArrayList<Object>();
    	List<NctextRecord> lNtextRecord = null;
    	fields.add("dataURI");// the field name defined in NctextRecord
    	values.add(datauri);
 
    	try {
     		lNtextRecord = (List<NctextRecord>) queryByCriteria(fields, values);
		} catch (DataAccessLayerException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		return lNtextRecord;
    }

	@SuppressWarnings("unchecked")
	public List<NctextRecord> getDataById(int id) {
	   	List<String> fields = new ArrayList<String>();
    	List<Object> values = new ArrayList<Object>();
    	List<NctextRecord> lNtextRecord = null;
    	fields.add("id");// the field name defined in NctextRecord
    	values.add(id);
 
    	try {
     		lNtextRecord = (List<NctextRecord>) queryByCriteria(fields, values);
		} catch (DataAccessLayerException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		return lNtextRecord;
    }

	@SuppressWarnings("unchecked")
	public List<NctextRecord> getDataByWmoid(String wmoid) {
	   	List<String> fields = new ArrayList<String>();
    	List<Object> values = new ArrayList<Object>();
    	List<NctextRecord> lNtextRecord = null;
    	fields.add("wmoId");// the field name defined in NctextRecord
    	values.add(wmoid);
 
    	try {
    		lNtextRecord = (List<NctextRecord>) queryByCriteria(fields, values);
		} catch (DataAccessLayerException e) {
			logger.info("getDataByWmoid query exception");
			e.printStackTrace();
		}
		return lNtextRecord;
    	
    }
	@SuppressWarnings("unchecked")
	public List<NctextRecord> getDataBySite(String site) {
	   	List<String> fields = new ArrayList<String>();
    	List<Object> values = new ArrayList<Object>();
    	List<NctextRecord> lNtextRecord = null;
    	fields.add("issueSite");// the field name defined in NctextRecord
    	values.add(site);
 
    	try {
    		lNtextRecord = (List<NctextRecord>) queryByCriteria(fields, values);
		} catch (DataAccessLayerException e) {
			logger.info("getDataBySite query exception");
			e.printStackTrace();
		}
		return lNtextRecord;
    	
    }
	@SuppressWarnings("unchecked")
	public List<NctextRecord> getDataByProductType(String pType) {
	   	List<String> fields = new ArrayList<String>();
    	List<Object> values = new ArrayList<Object>();
    	List<NctextRecord> lNtextRecord = null;
    	fields.add("productType");// the field name defined in NctextRecord
    	values.add(pType);
 
    	try {
    		lNtextRecord = (List<NctextRecord>) queryByCriteria(fields, values);
		} catch (DataAccessLayerException e) {
			logger.info("getDataByProductType query exception");
			e.printStackTrace();
		}
		return lNtextRecord;
    	
    }

	@SuppressWarnings("unchecked")
	public List<NctextRecord> getDataByBBBind(String bbbInd) {
	   	List<String> fields = new ArrayList<String>();
    	List<Object> values = new ArrayList<Object>();
    	List<NctextRecord> lNtextRecord = null;
    	fields.add("bbbInd");// the field name defined in NctextRecord
    	values.add(bbbInd);
 
    	try {
    		lNtextRecord = (List<NctextRecord>) queryByCriteria(fields, values);
		} catch (DataAccessLayerException e) {
			logger.info("getDataByBBBind query exception");
			e.printStackTrace();
		}
		return lNtextRecord;
    	
    }

	@SuppressWarnings("unchecked")
	public List<NctextRecord> getDataByAwipsId(String awipsId) {
	   	List<String> fields = new ArrayList<String>();
    	List<Object> values = new ArrayList<Object>();
    	List<NctextRecord> lNtextRecord = null;
    	fields.add("awipsId");// the field name defined in NctextRecord
    	values.add(awipsId);
 
    	try {
    		lNtextRecord = (List<NctextRecord>) queryByCriteria(fields, values);
		} catch (DataAccessLayerException e) {
			logger.info("getDataByAwipsId query exception");
			e.printStackTrace();
		}
		return lNtextRecord;
    	
    }
	@SuppressWarnings("unchecked")
	public List<NctextRecord> getDataByMultiFields(List<String> fields, List<Object> values) {
    	List<NctextRecord> lNtextRecord = null;
    	try {
    		lNtextRecord = (List<NctextRecord>) queryByCriteria(fields, values);
		} catch (DataAccessLayerException e) {
			logger.info("getDataBySite query exception");
			e.printStackTrace();
		}
		return lNtextRecord;
    	
    }

	@SuppressWarnings("unchecked")
	public List<NctextRecord> getDataByITime(String issueTime) {
	   	List<String> fields = new ArrayList<String>();
    	List<Object> values = new ArrayList<Object>();
    	List<NctextRecord> lNtextRecord = null;
    	//IssueTime format should be yyyy-MM-dd hh:mm:ss
    	int iYear = Integer.parseInt(issueTime.substring(0, 4).trim());
    	int iMon = Integer.parseInt(issueTime.substring(5, 7).trim());
    	int iDay = Integer.parseInt(issueTime.substring(8, 10).trim());
    	int iHour = Integer.parseInt(issueTime.substring(11, 13).trim());
    	int iMin = Integer.parseInt(issueTime.substring(14, 16).trim());
    	int iSec = Integer.parseInt(issueTime.substring(17, 19).trim());
    	Calendar ctime = Calendar.getInstance();
    	ctime.clear();// to clear msec field,otherwise will have problem to retrieve right record
    	ctime.set(iYear,iMon-1,iDay,iHour,iMin,iSec);
    	fields.add("issueTime");// the field name defined in NctextRecord
    	values.add(ctime);
 
    	try {
    		lNtextRecord = (List<NctextRecord>) queryByCriteria(fields, values);
		} catch (DataAccessLayerException e) {
			logger.info("getDataByITime query exception");
			e.printStackTrace();
		}
		return lNtextRecord;
    	
    }

	@SuppressWarnings("unchecked")
	public List<NctextRecord> getDataByITime(String issueTime, List<String> fields, 
			List<Object> values)  {
    	List<NctextRecord> lNtextRecord = null;
    	//IssueTime format should be yyyy-MM-dd hh:mm:ss
    	int iYear = Integer.parseInt(issueTime.substring(0, 4).trim());
    	int iMon = Integer.parseInt(issueTime.substring(5, 7).trim());
    	int iDay = Integer.parseInt(issueTime.substring(8, 10).trim());
    	int iHour = Integer.parseInt(issueTime.substring(11, 13).trim());
    	int iMin = Integer.parseInt(issueTime.substring(14, 16).trim());
    	int iSec = Integer.parseInt(issueTime.substring(17, 19).trim());
    	Calendar ctime = Calendar.getInstance();
    	ctime.clear();// to clear msec field,otherwise will have problem to retrieve right record
    	ctime.set(iYear,iMon-1,iDay,iHour,iMin,iSec);
    	fields.add("issueTime");// the field name defined in NctextRecord
    	values.add(ctime);
 
    	try {
    		lNtextRecord = (List<NctextRecord>) queryByCriteria(fields, values);
		} catch (DataAccessLayerException e) {
			logger.info("getDataByITime (combined fields) query exception");
			e.printStackTrace();
		}
		return lNtextRecord;
    	
    }

	@SuppressWarnings("unchecked")
	public List getDataByIssueTimeRange(String issueTimeStart, String issueTimeEnd, 
			List<String> fields, List<Object> values, List<String> operands){
    	List<NctextRecord> lNtextRecord = null;
    	//IssueTime format should be yyyy-MM-dd hh:mm:ss
    	int iYear = Integer.parseInt(issueTimeStart.substring(0, 4).trim());
    	int iMon = Integer.parseInt(issueTimeStart.substring(5, 7).trim());
    	int iDay = Integer.parseInt(issueTimeStart.substring(8, 10).trim());
    	int iHour = Integer.parseInt(issueTimeStart.substring(11, 13).trim());
    	int iMin = Integer.parseInt(issueTimeStart.substring(14, 16).trim());
    	int iSec = Integer.parseInt(issueTimeStart.substring(17, 19).trim());
    	Calendar cstime = Calendar.getInstance();
    	cstime.clear();// to clear msec field,otherwise will have problem to retrieve right record
    	cstime.set(iYear,iMon-1,iDay,iHour,iMin,iSec);
    	iYear = Integer.parseInt(issueTimeEnd.substring(0, 4).trim());
    	iMon = Integer.parseInt(issueTimeEnd.substring(5, 7).trim());
    	iDay = Integer.parseInt(issueTimeEnd.substring(8, 10).trim());
    	iHour = Integer.parseInt(issueTimeEnd.substring(11, 13).trim());
    	iMin = Integer.parseInt(issueTimeEnd.substring(14, 16).trim());
    	iSec = Integer.parseInt(issueTimeEnd.substring(17, 19).trim());
    	Calendar cetime = Calendar.getInstance();
    	cetime.clear();// to clear msec field,otherwise will have problem to retrieve right record
    	cetime.set(iYear,iMon-1,iDay,iHour,iMin,iSec);
    	fields.add("issueTime");// the field name defined in NctextRecord
    	values.add(cstime);
    	operands.add(">=");
    	fields.add("issueTime");// the field name defined in NctextRecord
    	values.add(cetime);
    	operands.add("<=");
    	try {
    		lNtextRecord = (List<NctextRecord>) queryByCriteria(fields, values, operands);
		} catch (DataAccessLayerException e) {
			logger.info("getDataByITime query exception");
			e.printStackTrace();
		}
		return lNtextRecord;
    	
    }

	@SuppressWarnings("unchecked")
	public List<NctextRecord> getDataByIssueTimeRange(String issueTimeStart, String issueTimeEnd) {
	   	List<String> fields = new ArrayList<String>();
    	List<Object> values = new ArrayList<Object>();
    	List<String> operands  = new ArrayList<String>();
    	List<NctextRecord> lNtextRecord = null;
    	//IssueTime format should be yyyy-MM-dd hh:mm:ss
    	int iYear = Integer.parseInt(issueTimeStart.substring(0, 4).trim());
    	int iMon = Integer.parseInt(issueTimeStart.substring(5, 7).trim());
    	int iDay = Integer.parseInt(issueTimeStart.substring(8, 10).trim());
    	int iHour = Integer.parseInt(issueTimeStart.substring(11, 13).trim());
    	int iMin = Integer.parseInt(issueTimeStart.substring(14, 16).trim());
    	int iSec = Integer.parseInt(issueTimeStart.substring(17, 19).trim());
    	Calendar cstime = Calendar.getInstance();
    	cstime.clear();// to clear msec field,otherwise will have problem to retrieve right record
    	cstime.set(iYear,iMon-1,iDay,iHour,iMin,iSec);
    	iYear = Integer.parseInt(issueTimeEnd.substring(0, 4).trim());
    	iMon = Integer.parseInt(issueTimeEnd.substring(5, 7).trim());
    	iDay = Integer.parseInt(issueTimeEnd.substring(8, 10).trim());
    	iHour = Integer.parseInt(issueTimeEnd.substring(11, 13).trim());
    	iMin = Integer.parseInt(issueTimeEnd.substring(14, 16).trim());
    	iSec = Integer.parseInt(issueTimeEnd.substring(17, 19).trim());
    	Calendar cetime = Calendar.getInstance();
    	cetime.clear();// to clear msec field,otherwise will have problem to retrieve right record
    	cetime.set(iYear,iMon-1,iDay,iHour,iMin,iSec);
    	fields.add("issueTime");// the field name defined in NctextRecord
    	values.add(cstime);
    	operands.add(">=");
    	fields.add("issueTime");// the field name defined in NctextRecord
    	values.add(cetime);
    	operands.add("<=");
    	try {
    		lNtextRecord = (List<NctextRecord>) queryByCriteria(fields, values, operands);
		} catch (DataAccessLayerException e) {
			logger.info("getDataByITime query exception");
			e.printStackTrace();
		}
		return lNtextRecord;
    	
    }

}
