package gov.noaa.nws.ncep.edex.plugin.nctext.query;
/**
 * 
 * NctextQuery
 * 
 * This query is used by EDEX sw to query NCTEXT table. Query str has to follow this format.
<!--  	<BBB>AAA</BBB>														  -->
<!--	<Producttype>Z..Z</Producttype>								      -->
<!--	<Issuesite>CCCC</Issuesite>										  -->
<!--	<Issuetime>YYYY-MM-DD hh:mm:ss</Issuetime>						  -->
<!--    <Timerange>YYYY-MM-DD hh:mm:ss@YYYY-MM-DD hh:mm:ss</Timerange>	  -->
<!--	<Id>N..N</Id>													  -->
<!--       Note: n in Datauri stands for record id; 					  -->
<!--		     Datauri and Issuetime should have exact format           -->
<!--		     Wmoid, Awipsid, Producttype, Issuesite, Id value size is variable -->
<!--			 Timerange with "startime@endtime" format				  -->
<!--																	  -->
<!		  Rule:															  -->
<!--			1.Query with Id or Datauri can not combine with others	  -->
<!--			2.Query with Wmoid, Awipsid, Producttype, Issuesite, BBB  -->
<!--			  can combine with each others							  -->
<!-- 			3.Query with Wmoid, Awipsid, Producttype, Issuesite, BBB  -->
<!--			  can combine with either Issuetime or Timerange		  -->
<!--			4.Query with either Issuetime or Timerange, but can not   -->
<!--			  combine these two. 									  -->
<!--			5.There is no space allowed between <keyword>value</keyword> -->
<!--		    6.There is no space allowed between <keyword></keyword>, if no value used-->
<!--			7.Query precedence order are Id, Datauri, Issuetime, Timerange, and rest. -->													  -->
<!--																	  -->
*<query>
*	<Wmoid></Wmoid>
*	<Awipsid></Awipsid>
*	<Datauri></Datauri>	
*	<BBB></BBB>	
*	<Product></Product>
*	<Issuesite></Issuesite>
*	<Issuetime></Issuetime>
*	<Timerange></Timerange>
*	<Id></Id>
*</query>
 * 
 * <pre>
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    	Engineer    Description
 * -------		------- 	-------- 	-----------
 * 12/01/2009		TBD		Chin Chen	Initial coding
 *
 * </pre>
 * 
 * @author Chin Chen
 * @version 1.0
 */
import gov.noaa.nws.ncep.edex.plugin.nctext.common.NctextRecord;
import gov.noaa.nws.ncep.edex.plugin.nctext.common.dao.*;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.PrintStream;
import java.util.ArrayList;
import java.util.EnumSet;
import java.util.Iterator;
import java.util.List;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
//import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.common.dataplugin.PluginException;

public class NctextQuery{
    private Log logger = LogFactory.getLog(getClass());

    private enum EQueryBit {WMOID_QUERY_BIT, AWIPSID_QUERY_BIT,DATAURI_QUERY_BIT,SITE_QUERY_BIT,BBB_QUERY_BIT, PRODUCT_QUERY_BIT,
    	ISSUETIME_QUERY_BIT,TIMERANGE_QUERY_BIT,ID_QUERY_BIT, NUM_OF_QUERY_BITS}
    
    private EnumSet<EQueryBit> queryTargetBitEnumSet = EnumSet.noneOf(EQueryBit.class);//empty set
	private static final String WMOIDS =  "<Wmoid>";
	private static final String WMOIDE =  "</Wmoid>";
	private static final int WMOIDS_LEN = WMOIDS.length();
	private static final String BBBS =  "<BBB>";
	private static final String BBBE =  "</BBB>";
	private static final int BBBS_LEN = BBBS.length();
	private static final String AWIPSIDS =  "<Awipsid>";
	private static final String AWIPSIDE =  "</Awipsid>";
	private static final int AWIPSIDS_LEN = AWIPSIDS.length();
	private static final String DATAURIS =  "<Datauri>";
	private static final String DATAURIE =  "</Datauri>";
	private static final int DATAURIS_LEN = DATAURIS.length();
	private static final String SITES =  "<Issuesite>";
	private static final String SITEE =  "</Issuesite>";
	private static final int SITES_LEN = SITES.length();
	private static final String PRODUCTS =  "<Product>";
	private static final String PRODUCTE =  "</Product>";
	private static final int PRODUCTS_LEN = PRODUCTS.length();
	private static final String ISSUETIMES =  "<Issuetime>";
	private static final String ISSUETIMEE =  "</Issuetime>";
	private static final int ISSUETIMES_LEN = ISSUETIMES.length();
	private static final String IDS =  "<Id>";
	private static final String IDE =  "</Id>";
	private static final int IDS_LEN = IDS.length();
	private static final String TIMERANGES =  "<Timerange>";
	private static final String TIMERANGEE =  "</Timerange>";
	private static final int TIMERANGES_LEN = TIMERANGES.length();

	private String wmoidStr = null;
	private String bbbStr = null;
	private String awipsidStr = null;
	private String idStr = null;
	private String siteStr = null;
	private String productStr = null;
	private String datauriStr = null;
	private String issueTimeStr = null;
	private String timeRangeBStr = null;
	private String timeRangeEStr = null;
	private NctextRecordDao nctextRecordDao;
	//private BitSet targetSet;
	
	
	private static String OUTPUTFOLDER;
	private static String OUTPUTFOLDERPATH = OUTPUTFOLDER + "/";
	private File outputFolder;
	private boolean printOutput;

	/**
	 * 
	 */
	public NctextQuery(boolean output, String outputPath) throws PluginException{	
		nctextRecordDao = null;
		try {
			nctextRecordDao = new NctextRecordDao("nctext");
			//targetSet = new BitSet(NUM_OF_QUERY_BITS);
		} catch(PluginException e) {
			logger.info("exception when creating NctextRecordDao");
			throw e;
		}
		printOutput = output;
		if(printOutput){
			OUTPUTFOLDER = outputPath;
			outputFolder= new File(outputPath);

			if(outputFolder.exists()==false){

				if( outputFolder.mkdir() == false)
					logger.info(OUTPUTFOLDER + " create failed!");
				else
					logger.info(OUTPUTFOLDER + " created!");
			}
			else
				logger.info(OUTPUTFOLDER + " already existed!!");
		}
	}

	public NctextQuery() throws PluginException{	
		nctextRecordDao = null;
		try {
			nctextRecordDao = new NctextRecordDao("nctext");
			//targetSet = new BitSet(NUM_OF_QUERY_BITS);
		} catch(PluginException e) {
			logger.info("exception when creating NctextRecordDao");
			throw e;
		}
		printOutput = true;
		OUTPUTFOLDER = "../nctext_query";
		outputFolder= new File(OUTPUTFOLDER);
			
		if(outputFolder.exists()==false){
			
			if( outputFolder.mkdir() == false)
				logger.info(OUTPUTFOLDER + " create failed!");
			else
				logger.info(OUTPUTFOLDER + " created!");
		}
		else
			logger.info(OUTPUTFOLDER + " already existed!!");
	}
	/* 
	 * 
	 * Chin - this function can be modified for future use to create query string 
	 * 
	public String createQuery(String productType,NctextStationInfo sta,
			EReportTimeRange rptTimeRange, boolean isState){
		String queryStr = null;
		
		Calendar cal = Calendar.getInstance();
		String timeB, timeE;
		timeB = cal.get(Calendar.YEAR)+"-"+(cal.get(Calendar.MONTH)+1)+"-"+cal.get(Calendar.DAY_OF_MONTH)+" "+
			cal.get(Calendar.HOUR_OF_DAY)+":"+cal.get(Calendar.MINUTE)+":"+cal.get(Calendar.SECOND);
		timeE = cal.get(Calendar.YEAR)+"-"+(cal.get(Calendar.MONTH)+1)+"-"+cal.get(Calendar.DAY_OF_MONTH)+" "+
				(cal.get(Calendar.HOUR_OF_DAY)+rptTimeRange.getTimeRange())+":"+cal.get(Calendar.MINUTE)+":"+cal.get(Calendar.SECOND);
		if((productType!= null)&&(sta!= null)){
			queryStr = "<query> <Wmoid>" + sta.getProductid()+"</Wmoid> <Awipsid></Awipsid> <Datauri></Datauri>	 <BBB></BBB> " +
			"<Product>"+productType+"</Product> <Issuesite>"+ sta.getStnid()+ "</Issuesite> <Issuetime></Issuetime> " +
			"<Timerange>"+timeB+"@"+timeE+"</Timerange> <Id></Id> </query>";
		
		} else if(productType!= null){
			queryStr = "<query> <Wmoid></Wmoid> <Awipsid></Awipsid> <Datauri></Datauri>	 <BBB></BBB> " +
			"<Product>"+productType+"</Product> <Issuesite></Issuesite> <Issuetime></Issuetime> " +
			"<Timerange>"+timeB+"@"+timeE+"</Timerange> <Id></Id> </query>";
			
		} else if(sta!= null){
			queryStr = "<query> <Wmoid>" + sta.getProductid()+"</Wmoid> <Awipsid></Awipsid> <Datauri></Datauri>	 <BBB></BBB> " +
			"<Product></Product> <Issuesite>"+ sta.getStnid()+ "</Issuesite> <Issuetime></Issuetime> " +
			"<Timerange>"+timeB+"@"+timeE+"</Timerange> <Id></Id> </query>";			
		}
		return queryStr;
	} */

	private void parseInput(String  queryStr) {
		int wmoidS, wmoidE;
		int bbbS, bbbE;
		int idS, idE;
		int awipsIdS, awipsIdE;
		int siteS, siteE;
		int issueTimeS, issueTimeE;
		int timeRangeS, timeRangeE, timeRangeDivider;
		int productS, productE;
		int datauriS, datauriE;
		wmoidS = queryStr.indexOf(WMOIDS);
		wmoidE = queryStr.indexOf(WMOIDE);
		bbbS = queryStr.indexOf(BBBS);
		bbbE = queryStr.indexOf(BBBE);
		awipsIdS = queryStr.indexOf(AWIPSIDS);
		awipsIdE = queryStr.indexOf(AWIPSIDE);
		siteS = queryStr.indexOf(SITES);
		siteE = queryStr.indexOf(SITEE);
		issueTimeS = queryStr.indexOf(ISSUETIMES);
		issueTimeE = queryStr.indexOf(ISSUETIMEE);
		productS = queryStr.indexOf(PRODUCTS);
		productE = queryStr.indexOf(PRODUCTE);
		idS = queryStr.indexOf(IDS);
		idE = queryStr.indexOf(IDE);
		datauriS = queryStr.indexOf(DATAURIS);
		datauriE = queryStr.indexOf(DATAURIE);
		timeRangeS = queryStr.indexOf(TIMERANGES);
		timeRangeE = queryStr.indexOf(TIMERANGEE);
		timeRangeDivider = queryStr.indexOf('@');

		//targetSet.clear();
		queryTargetBitEnumSet.clear(); //empty set
		if(wmoidE > (wmoidS+WMOIDS_LEN) ) {
			wmoidStr = queryStr.substring(wmoidS+WMOIDS_LEN, wmoidE);
			//targetSet.set(WMOID_QUERY_BIT);
			queryTargetBitEnumSet.add(EQueryBit.WMOID_QUERY_BIT);
			logger.info("wmoid "+ wmoidStr);
		}
		if(bbbE > (bbbS+BBBS_LEN) ) {
			bbbStr = queryStr.substring(bbbS+BBBS_LEN, bbbE);
			//targetSet.set(BBB_QUERY_BIT);
			queryTargetBitEnumSet.add(EQueryBit.BBB_QUERY_BIT);
			logger.info("bbb "+ bbbStr);
		}
		if(awipsIdE > (awipsIdS+AWIPSIDS_LEN) ) {
			awipsidStr = queryStr.substring(awipsIdS+AWIPSIDS_LEN, awipsIdE);
			//targetSet.set(AWIPSID_QUERY_BIT);
			queryTargetBitEnumSet.add(EQueryBit.AWIPSID_QUERY_BIT);
			logger.info("awipsId "+ awipsidStr);
		}
		if(siteE > (siteS+SITES_LEN) ) {
			siteStr = queryStr.substring(siteS+SITES_LEN, siteE);
			//targetSet.set(SITE_QUERY_BIT);
			queryTargetBitEnumSet.add(EQueryBit.SITE_QUERY_BIT);
			logger.info("site "+ siteStr);
		}
		if(issueTimeE > (issueTimeS+ISSUETIMES_LEN) ) {
			issueTimeStr = queryStr.substring(issueTimeS+ISSUETIMES_LEN, issueTimeE);
			//targetSet.set(ISSUETIME_QUERY_BIT);
			queryTargetBitEnumSet.add(EQueryBit.ISSUETIME_QUERY_BIT);
			logger.info("issueTime "+ issueTimeStr);
		}
		if(productE > (productS+PRODUCTS_LEN) ) {
			productStr = queryStr.substring(productS+PRODUCTS_LEN, productE);
			//targetSet.set(PRODUCT_QUERY_BIT);
			queryTargetBitEnumSet.add(EQueryBit.PRODUCT_QUERY_BIT);
			logger.info("product "+ productStr);
		}
		if(idE > (idS+IDS_LEN) ) {
			idStr = queryStr.substring(idS+IDS_LEN, idE);
			//targetSet.set(ID_QUERY_BIT);
			queryTargetBitEnumSet.add(EQueryBit.ID_QUERY_BIT);
			logger.info("id "+ idStr);
		}
		if(datauriE > (datauriS+DATAURIS_LEN) ) {
			datauriStr = queryStr.substring(datauriS+DATAURIS_LEN, datauriE);
			//targetSet.set(DATAURI_QUERY_BIT);
			queryTargetBitEnumSet.add(EQueryBit.DATAURI_QUERY_BIT);
			logger.info("datauri "+ datauriStr);
		}
		if(timeRangeDivider > timeRangeS+TIMERANGES_LEN){
			timeRangeBStr = queryStr.substring(timeRangeS+TIMERANGES_LEN,timeRangeDivider);
			if(timeRangeE > timeRangeDivider) {
				timeRangeEStr = queryStr.substring(timeRangeDivider+1,timeRangeE);
				//targetSet.set(TIMERANGE_QUERY_BIT);
				queryTargetBitEnumSet.add(EQueryBit.TIMERANGE_QUERY_BIT);
			}
			logger.info("timeRangeB "+ timeRangeBStr + "timeRangeE " + timeRangeEStr) ;
		}
		//logger.info("targetSet is " + targetSet.toString());
		logger.info("targetSet is " + queryTargetBitEnumSet.toString());
		
	}
	@SuppressWarnings("unchecked")
	private List<NctextRecord> queryDb() {
 	   	List<String> fields = new ArrayList<String>();
    	List<Object> values = new ArrayList<Object>();
    	List<String> operands  = new ArrayList<String>();
    	boolean combinedQuery = false;
		//if(targetSet.get(ID_QUERY_BIT)){
		if(queryTargetBitEnumSet.contains(EQueryBit.ID_QUERY_BIT)){
			//ID has highest precedence, and can not be combined with other query
			//logger.info("Query with ID");
			return nctextRecordDao.getDataById(Integer.parseInt(idStr));
		}
		//if(targetSet.get(DATAURI_QUERY_BIT)){
		if(queryTargetBitEnumSet.contains(EQueryBit.DATAURI_QUERY_BIT)){
			//Datauri has 2nd highest precedence, and can not be combined with other query
			//logger.info("Query with datauri");
			return nctextRecordDao.getDataByDatauri(datauriStr);
		}
		//The following 5 fields can be combined together
		//if(targetSet.get(WMOID_QUERY_BIT)){
		if(queryTargetBitEnumSet.contains(EQueryBit.WMOID_QUERY_BIT)){
			//logger.info("Query with wmoid");
	    	fields.add("wmoId");// the field name defined in NctextRecord
	    	values.add(wmoidStr);
	    	operands.add("="); //only used when query by time range
	    	combinedQuery = true;
		}
		if(queryTargetBitEnumSet.contains(EQueryBit.BBB_QUERY_BIT)){
			//logger.info("Query with bbb");
	    	fields.add("bbbInd");// the field name defined in NctextRecord
	    	values.add(bbbStr);
	    	operands.add("="); //only used when query by time range
	    	combinedQuery = true;
		}
		//if(targetSet.get(AWIPSID_QUERY_BIT)){
		if(queryTargetBitEnumSet.contains(EQueryBit.AWIPSID_QUERY_BIT)){
			//logger.info("Query with awipsId");
	    	fields.add("awipsId");// the field name defined in NctextRecord
	    	values.add(awipsidStr);
	    	operands.add("=");//only used when query by time range
	    	combinedQuery = true;
		}
		//if(targetSet.get(SITE_QUERY_BIT)){
		if(queryTargetBitEnumSet.contains(EQueryBit.SITE_QUERY_BIT)){
			//logger.info("Query with issueSite");
	    	fields.add("issueSite");// the field name defined in NctextRecord
	    	values.add(siteStr);
	    	operands.add("=");//only used when query by time range
	    	combinedQuery = true;
		}
		//if(targetSet.get(PRODUCT_QUERY_BIT)){
		if(queryTargetBitEnumSet.contains(EQueryBit.PRODUCT_QUERY_BIT)){
			//logger.info("Query with productType");
	    	fields.add("productType");// the field name defined in NctextRecord
	    	values.add(productStr);
	    	operands.add("=");//only used when query by time range
	    	combinedQuery = true;
		}
		
		// query with issue time and query with time range can not be combined, so do the following
		//if(targetSet.get(ISSUETIME_QUERY_BIT)){		
		if(queryTargetBitEnumSet.contains(EQueryBit.ISSUETIME_QUERY_BIT)){
			if (combinedQuery){
				logger.info("Query with combined fileds and issue time");
				return nctextRecordDao.getDataByITime(issueTimeStr,fields,values);
			} else {
				//logger.info("Query with issue time");
				return nctextRecordDao.getDataByITime(issueTimeStr);
			}
		//} else if  (targetSet.get(TIMERANGE_QUERY_BIT)){
		} else	if(queryTargetBitEnumSet.contains(EQueryBit.TIMERANGE_QUERY_BIT)){
			if (combinedQuery){
				logger.info("Query with combined fileds and issue time range");
				return nctextRecordDao.getDataByIssueTimeRange(timeRangeBStr,timeRangeEStr,fields,values,operands);
			} else {
				//logger.info("Query with issue time range");
				return nctextRecordDao.getDataByIssueTimeRange(timeRangeBStr,timeRangeEStr);
			}
		} else {
			//logger.info("Query with combined fileds");
			return nctextRecordDao.getDataByMultiFields(fields,values);
		}

	}
	public List<NctextRecord> queryInput(String fileName, String queryStr){
		List<NctextRecord> lNtextRecord = null;
		FileOutputStream outStream=null; // declare a file output object
        PrintStream pStream=null; // declare a print stream object

        parseInput(queryStr);
		
		//if(!(targetSet.isEmpty())){
		if(!(queryTargetBitEnumSet.isEmpty())){
			//query DB
			lNtextRecord = (List<NctextRecord>)queryDb();
			if((lNtextRecord != null)/*&&(fileName != null)*/) {
				Iterator<NctextRecord> it = lNtextRecord.iterator();

				if(it.hasNext()){
					List<PluginDataObject> rtnReports = new ArrayList<PluginDataObject>();
					// Create a new file output stream
                    
					if((fileName != null)&&(printOutput==true)){
						try {

							outStream = new FileOutputStream(OUTPUTFOLDERPATH+fileName+".output");

							// Connect print stream to the output stream
							pStream = new PrintStream( outStream );
							int size = lNtextRecord.size();
							if(size > 0){
								if((outStream != null)&& (pStream!=null)){
									pStream.print("\n"+"Number of records found = " + size +"\n");
								}

							}

						} catch (FileNotFoundException e) {
							// TODO Auto-generated catch block
							e.printStackTrace();
						}
					}
					while(it.hasNext()){
						NctextRecord record = (NctextRecord) it.next();
						rtnReports.add(record);
						
						if((outStream != null)&& (pStream!=null)){
							pStream.print("\n"+record.getRawRecord()+"\n");
						}
						
					}
					if((outStream != null)&& (pStream!=null)){
						pStream.close();
					}	
					logger.info("DB query return " + rtnReports.size() + " records");
				} else {
					logger.info("DB query return nil");
					
				}
			}
		} 
		else
			logger.info("No query bit is set");
		return lNtextRecord;
	}
	
	public PluginDataObject[] queryInput(String testFileName, byte[] queryInput){
    	List<NctextRecord> lNtextRecord = null;
		String queryStr =  new String(queryInput);
		PluginDataObject[] returnObjects= null;
		FileOutputStream outStream=null; // declare a file output object
		PrintStream pStream=null; // declare a print stream object

		if(testFileName!=null){

			parseInput(queryStr);

			//if(!(targetSet.isEmpty())){
			if(!(queryTargetBitEnumSet.isEmpty())){
				//query DB
				lNtextRecord = (List<NctextRecord>)queryDb();
				if(lNtextRecord != null) {
					Iterator<NctextRecord> it = lNtextRecord.iterator();

					if(it.hasNext()){
						List<PluginDataObject> rtnReports = new ArrayList<PluginDataObject>();
						// Create a new file output stream
						// connected to "testFileName.output"
						try {
							outStream = new FileOutputStream(OUTPUTFOLDERPATH+testFileName+".output");

							// Connect print stream to the output stream
							pStream = new PrintStream( outStream );
							int size = lNtextRecord.size();
							if(size > 0){
								if((outStream != null)&& (pStream!=null)){
									pStream.print("\n"+"Number of records found = " + size +"\n");
								}

							}

						} catch (FileNotFoundException e) {
							// TODO Auto-generated catch block
							e.printStackTrace();
						}

						while(it.hasNext()){
							NctextRecord record = (NctextRecord) it.next();
							rtnReports.add(record);
							//record.printData(); //for debug 
							if((outStream != null)&& (pStream!=null)){
								pStream.print("\n"+record.getRawRecord()+"\n");
							}
						}
						if((outStream != null)&& (pStream!=null))
							pStream.close();
						returnObjects = new PluginDataObject[rtnReports.size()];
						if(rtnReports.size() > 0){
							rtnReports.toArray(returnObjects);
							logger.info("DB query return " + rtnReports.size() + " records");
						}	
					} else {
						logger.info("DB query return nil");

					}
				}
			} 
			else
				logger.info("No query bit is set");
		}
		if(returnObjects == null ) {
			returnObjects = new PluginDataObject[0];
		}
		return returnObjects;
	}
}
