/**
 * This code has unlimited rights, and is provided "as is" by the National Centers 
 * for Environmental Prediction, without warranty of any kind, either expressed or implied, 
 * including but not limited to the implied warranties of merchantability and/or fitness 
 * for a particular purpose.
 * 
 * 
 * This code has been developed by the NCEP-SIB for use in the AWIPS2 system.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    	Engineer    Description
 * -------		------- 	-------- 	-----------
 * 05/30/2013				Chin J. Chen	Initial coding
 *
 * </pre>
 * 
 * @author Chin J. Chen
 * @version 1.0
 */
package gov.noaa.nws.ncep.edex.plugin.gpd.decoder;

import gov.noaa.nws.ncep.common.dataplugin.gpd.GenericPointDataConstants;
import gov.noaa.nws.ncep.common.dataplugin.gpd.GenericPointDataRecord;
import gov.noaa.nws.ncep.common.dataplugin.gpd.dao.GenericPointDataDao;
import gov.noaa.nws.ncep.common.dataplugin.gpd.product.GenericPointDataLevel;
import gov.noaa.nws.ncep.common.dataplugin.gpd.product.GenericPointDataParameter;
import gov.noaa.nws.ncep.common.dataplugin.gpd.product.GenericPointDataProductContainer;
import gov.noaa.nws.ncep.common.dataplugin.gpd.product.GenericPointDataProductInfo;
import gov.noaa.nws.ncep.common.dataplugin.gpd.product.GenericPointDataStationProduct;
import gov.noaa.nws.ncep.common.dataplugin.gpd.query.GenericPointDataReqMsg.GenericPointDataReqType;

import java.io.BufferedReader;
import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.StringWriter;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.List;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;
import javax.xml.bind.SchemaOutputResolver;
import javax.xml.bind.Unmarshaller;
import javax.xml.transform.Result;
import javax.xml.transform.stream.StreamResult;

import org.apache.commons.io.IOUtils;

import com.raytheon.edex.exception.DecoderException;
import com.raytheon.edex.plugin.AbstractDecoder;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.common.parameter.Parameter;
import com.raytheon.uf.common.pointdata.Dimension;
import com.raytheon.uf.common.pointdata.ParameterDescription;
import com.raytheon.uf.common.pointdata.PointDataContainer;
import com.raytheon.uf.common.pointdata.PointDataDescription;
import com.raytheon.uf.common.pointdata.PointDataDescription.Type;
import com.raytheon.uf.common.pointdata.PointDataView;
import com.raytheon.uf.common.pointdata.spatial.ObStation;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.edex.core.EDEXUtil;

public class GenericPointDataDecoder extends AbstractDecoder {
    private String pluginName;
    private GenericPointDataDao gpdDao ;
    private static GenericPointDataDecoder instance= null;
    private long totalHDF5Time;
    private static String OK= "ok";
    private static int BLOCKSIZE = 50;
    private PointDataDescription createPointDataDescription(GenericPointDataProductInfo prodInfo){
    	int parmsize= prodInfo.getParameterLst().size();
    	if(parmsize<=0)
    		return null;
    	PointDataDescription newPdd = new PointDataDescription();
    	int i=0;
    	//String maxLevel = Integer.toString(rptType.getMaxNumberOfLevel());
    	newPdd.parameters  = new ParameterDescription[parmsize+ GenericPointDataConstants.MANDATORY_DATASET_NUM];
    	ParameterDescription[] parameterDescriptions = newPdd.parameters;
    	for (Parameter parm: prodInfo.getParameterLst()){
    		parameterDescriptions[i]=new ParameterDescription(parm.getAbbreviation(),Type.FLOAT,GenericPointDataConstants.GPD_INVALID_FLOAT_VALUE,
    	            parm.getUnitString());
    		if(prodInfo.getMaxNumberOfLevel() > 1){
    			// 2 dimensional parameters, when level is more than one, ie. sounding product
    			parameterDescriptions[i].setNumDims(2);
    			parameterDescriptions[i].setDimension(GenericPointDataConstants.MAX_LEVELS);
    		}
    		i++;
    	}
    	//numOfLevel is one dmension
    	parameterDescriptions[i]=new ParameterDescription(GenericPointDataConstants.HDF5_NUM_LEVEL,Type.INT);
    	i++;
    	// stationId is one dimension
    	parameterDescriptions[i]=new ParameterDescription(GenericPointDataConstants.HDF5_STN_ID,Type.STRING);
    	parameterDescriptions[i].setMaxLength(GenericPointDataConstants.MAX_STNID_STRING_SIZE);
    	i++;
    	//level values is 1 or 2 dimensional,  2 dimensional when level is more than one, ie. sounding product
    	parameterDescriptions[i]=new ParameterDescription(GenericPointDataConstants.HDF5_LEVEL_VALUE,Type.FLOAT);
    	if(prodInfo.getMaxNumberOfLevel() > 1){
    		parameterDescriptions[i].setNumDims(2);
    		parameterDescriptions[i].setDimension(GenericPointDataConstants.MAX_LEVELS);
    	}
		
		newPdd.dimensions = new Dimension[1];
		newPdd.dimensions[0] = new Dimension();
		newPdd.dimensions[0].setDimensionLength(prodInfo.getMaxNumberOfLevel());
		newPdd.dimensions[0].setDimensionName(GenericPointDataConstants.MAX_LEVELS);// not important, but we need to set a name here.
    	//for(int k=0; k < newPdd.parameters.length; k++)
		//{
			//System.out.println("PointDataDescription is creating Parm="+newPdd.parameters[k].getParameterName());
		//}
    	newPdd.resolveDimensions();
    	return newPdd;
    }
    
    private PointDataView createPointDataView(PointDataDescription pdd, GenericPointDataStationProduct stnPd){
    	PointDataView pdv = null;
    	try{
    		PointDataContainer container = PointDataContainer.build(pdd);

    		pdv = container.append();
    		//set 1-dimensional mandatory datasets STN_ID and NUM_LEVEL
    		pdv.setString(GenericPointDataConstants.HDF5_STN_ID,stnPd.getLocation().getStationId());
    		pdv.setInt(GenericPointDataConstants.HDF5_NUM_LEVEL,stnPd.getNumLevel());
    		for(int index=0; index < stnPd.getNumLevel(); index++){
    			GenericPointDataLevel gpdLevel= stnPd.getLevelLst().get(index);
    			//set 2-dimensional mandatory dataset LEVEL_VALUE
    			pdv.setFloat(GenericPointDataConstants.HDF5_LEVEL_VALUE, (float)gpdLevel.getLevelValue(), index);

    			//set all optional 2-dimensional datasets
    			for (GenericPointDataParameter gpdParm: gpdLevel.getGpdParameters()){
    				pdv.setFloat(gpdParm.getName(), gpdParm.getValue(),index);
    			}
    		}
    	} catch (OutOfMemoryError e) {
    		System.out.println("out of memory when createPointDataView for stnId = "+stnPd.getLocation().getStationId());
    		e.printStackTrace();	
    	}
    	//System.out.println("createPointDataView for stnId = "+stnPd.getLocation().getStationId()+" is created");
    	return pdv;
    }
	public GenericPointDataDecoder() throws DecoderException{
		instance = this;
		try {
			gpdDao = new GenericPointDataDao("gpd");
			
		} catch (PluginException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}
	public static GenericPointDataDecoder getInstance(){
		if (instance!=null)
			return instance;
		else{
			try {
				instance = new GenericPointDataDecoder();
			} catch (DecoderException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
			return instance;
		}
	}
	/**
	 * @param inputFile : XML file in String
	 * @return ok or erro message
	 * @throws DecoderException
	 * @throws PluginException 
	 *
	public String decodeProdInfo(String inputFile)throws DecoderException, PluginException {
		InputStream is = null; 
		GenericPointDataProductInfo prodInfo=null;
		JAXBContext ctx;	
		is = new ByteArrayInputStream(inputFile.getBytes());
		try {
			ctx = JAXBContext.newInstance(GenericPointDataProductContainer.class);
			if (ctx != null && is !=null) {
				
				Unmarshaller um = ctx.createUnmarshaller();
				if(um !=null){
					// test example: to unmarshal from file system 
					//String strmPath = "/res/pointdata/gpdProduct.xml";
					//GenericPointDataProductContainer gpdc = (GenericPointDataProductContainer)um.unmarshal(GenericPointDataProductContainer.class.getResourceAsStream(strmPath));
					Object result = um.unmarshal(is);
					if(result instanceof GenericPointDataProductInfo)
						prodInfo = (GenericPointDataProductInfo)result;
					else
						return "Data persistence failed. XML file is not formatted correctly!";
				}
			}

		} catch (JAXBException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
			return "Data persistence failed. JAXBException happened!";
		}
		if(prodInfo!=null){
			GenericPointDataProductInfo updatedProdInfo = gpdDao.updateProductInfo(prodInfo);
			if(updatedProdInfo == null)
				return "Update product information failed";
			else 
				return "Update product information done!";
				
		}
		return "Data persistence failed! XML file is not valid.";
	} */
	/*
	 * Parsing Gempak generated point data profile (3D) table.
	 * Input example- sounding data
	 * *******************************************************************
SNPARM = PRES;TMPC;DWPT;HGHT;DRCT;SKNT;TMPK;RELH;MIXR;UKNT;VKNT;THTA;THTE      
 STNPRM = LIFT;SHOW;KINX                                                        

 STID = FFC           STNM =    72215   TIME = 130731/1200         
 SLAT =  33.36     SLON =   -84.57   SELV =   245.0
 STIM =  1200

 LIFT =    -1.56    SHOW =     0.38    KINX =    36.10

      PRES     TMPC     DWPT     HGHT     DRCT     SKNT     TMPK     RELH
               MIXR     UKNT     VKNT     THTA     THTE
    992.00    21.80    21.00   245.00     0.00     0.00   294.95    95.21
              16.06     0.00     0.00   295.63   341.92
   1000.00 -9999.00 -9999.00   170.00 -9999.00 -9999.00 -9999.00 -9999.00
           -9999.00 -9999.00 -9999.00 -9999.00 -9999.00
    925.00    22.00    18.40   851.00   210.00    10.99   295.15    80.03
              14.62     5.50     9.52   301.80   345.11
    850.00    17.00    13.90  1581.00   230.00    16.01   290.15    81.96
              11.88    12.26    10.29   303.94   339.55
    700.00     5.80     4.30  3209.00   225.00    27.00   278.95    90.08
               7.49    19.09    19.09   308.88   332.09
    500.00    -6.70    -7.60  5900.00    10.00     6.99   266.45    93.30
               4.34    -1.21    -6.89   324.81   339.47
    400.00   -16.10   -18.70  7630.00   245.00     8.00   257.05    80.35
               2.20     7.25     3.38   333.98   341.92
    300.00   -31.70   -36.70  9730.00   275.00    21.00   241.45    61.19
               0.55    20.92    -1.83   340.58   342.79
    250.00   -42.90   -51.90 10980.00   315.00    27.99   230.25    36.51
               0.13    19.79   -19.79   342.15   342.70
    200.00   -54.10   -62.10 12450.00   310.00    42.00   219.05    36.72
               0.04    32.17   -27.00   346.94   347.14
    150.00   -65.70   -74.70 14250.00   310.00    35.99   207.45    27.52
               0.01    27.57   -23.14   356.71   356.76
    100.00   -70.70   -78.70 16650.00   305.00    17.00   202.45    29.86
               0.01    13.92    -9.75   390.87   390.92

 STID = BMX           STNM =    72230   TIME = 130731/1200         
 SLAT =  33.16     SLON =   -86.76   SELV =   178.0
 STIM =  1200

 LIFT =    -7.20    SHOW =    -4.07    KINX =    34.50

      PRES     TMPC     DWPT     HGHT     DRCT     SKNT     TMPK     RELH
               MIXR     UKNT     VKNT     THTA     THTE
    998.00    24.40    22.80   178.00     0.00     0.00   297.55    90.80
              17.87     0.00     0.00   297.72   349.73
   1000.00 -9999.00 -9999.00   158.00 -9999.00 -9999.00 -9999.00 -9999.00
           -9999.00 -9999.00 -9999.00 -9999.00 -9999.00
   	 ********************************************************************************************
	 *   
	 */
	private String parseSoundingTable(String prodTblString,String prodName,int versionNum, int levelNumMax, GenericPointDataReqType reqType){
		if( levelNumMax < 2) {//should set to a minimum of 2, see comments in parseSurfaceTable()
			return "Bad input, max number of level should be greater than 1";
		}
		
		GenericPointDataProductInfo prodInfo = new GenericPointDataProductInfo(prodName, levelNumMax );
		prodInfo.setMaxNumberOfLevel(levelNumMax);
		String returnStatus = "";
		//input start with "SNPARM =" and followed by a list of sounding parameters
		int firstEqualSignIndex= prodTblString.indexOf('=');
		// optional station parameter line
		int stnprmIndex = prodTblString.indexOf(GenericPointDataConstants.SND_STN_PARM);
		//station id line
		int stnIdIndex = prodTblString.indexOf(GenericPointDataConstants.SND_STN_ID);
		if(stnIdIndex <0){
			if(stnIdIndex <0){
				return "Bad input, no STID key word, could not determine sounding parameters!";
			}
		}
		String sndParmStr="", stnParmStr="";
		String[] sndParmArray=null, stnParmArray=null;
		if(stnprmIndex <0){
			//optional station parameter line is not provided
			if(stnIdIndex > firstEqualSignIndex){
				sndParmStr = prodTblString.substring(firstEqualSignIndex+1,stnIdIndex);
			}
			else{
				return "Bad input, no STNPRM and STID, could not determine sounding parameters!";
			}
		}
		else{
			//optional station parameter line is provided
			//get station parameters
			stnParmStr = prodTblString.substring(stnprmIndex);
			sndParmStr = prodTblString.substring(firstEqualSignIndex+1,stnprmIndex);
			int stnParmEqualIndex = stnParmStr.indexOf('=');
			int stnIdIndex2 = stnParmStr.indexOf(GenericPointDataConstants.SND_STN_ID);
			if(stnIdIndex2 > firstEqualSignIndex){
				stnParmStr = stnParmStr.substring(stnParmEqualIndex+1, stnIdIndex2);
				stnParmStr = stnParmStr.trim();
				stnParmArray = stnParmStr.split("[; \n]+");
			}
		}
		if(sndParmStr.length() <=0){
			return "Bad input, no SNPARM (sounding parameters) defined!";
		}
		sndParmStr = sndParmStr.trim();
		sndParmArray = sndParmStr.split("[; \n]+");
		int numSndParm = sndParmArray.length;
		int numSndParmAtLastLine = numSndParm % GenericPointDataConstants.SND_PARM_PER_LINE;
		int numLinePerLevel ;
		if(numSndParmAtLastLine ==0){
			numLinePerLevel = numSndParm/ GenericPointDataConstants.SND_PARM_PER_LINE;	
			numSndParmAtLastLine =GenericPointDataConstants.SND_PARM_PER_LINE;
		}
		else {
			numLinePerLevel = numSndParm/ GenericPointDataConstants.SND_PARM_PER_LINE+1;
		}
		System.out.println("sndParmStr= "+sndParmStr+"\nNum of snd parm ="+numSndParm);
		//System.out.println("numLinePerLevel= "+numLinePerLevel);
		for(String pm: sndParmArray){
			Parameter parm= new Parameter(pm);
			prodInfo.getParameterLst().add(parm);
			//System.out.println(pm + " length="+ pm.length());	
		}
		boolean found = true;
		//remainDataString: point to data starting from first STID
		String remainDataString = prodTblString.substring(stnIdIndex);
		//split remaining data and parse data for each station. 
		//Chin NOTE: current time is reference time for OBS SND (e.g. Uair) and is forecast time for Model SND (for example pfc-nam)
		Date currentTime=null;
		//Chin Note: reference time is indeed reference time for Model SND. Currently, we use the first decoded TIME 
		// as reference time.
		Date refTime=null;
		boolean firstRoundOfPersistence = true;
		PointDataDescription pdd=null;
		//int blockCount=0;
		List<GenericPointDataStationProduct> stnProdList = new ArrayList<GenericPointDataStationProduct>();
		/*
		 * Chin Note: To speed up HDF5 persistence speed and also avoid out of heap memory problem,
		 * we have to split input data to smaller blocks. Each block has N (BLOCKSIZE) number of station/time line data.
		 * We did not pre-allocate memory here. We do that for parseSurfaceTable. If we ran into mempry issue, then we will
		 * have to do that here too.
		 */
		while(found) {		
			int nextStnIdIndex = remainDataString.indexOf(GenericPointDataConstants.SND_STN_ID, GenericPointDataConstants.SND_STN_ID.length());
			//System.out.println("nextId="+nextStnIdIndex);
			//stnDataString: pointer to each station data section
			String stnDataString;
			if(nextStnIdIndex >0){
				stnDataString = remainDataString.substring(0, nextStnIdIndex);
				remainDataString = remainDataString.substring(nextStnIdIndex);
			}
			else {
				stnDataString = remainDataString;
				found = false;
				//System.out.println("reach last station, contents= \n"+stnDataString);
			}
			boolean goodStation = true;
			//System.out.println(stnDataString);
			int stnSndParmIndex = stnDataString.indexOf(sndParmArray[0]);
			//System.out.println("stnSndParmIndex=" + stnSndParmIndex);
			if(stnSndParmIndex < 0)// this stn does not have real sounding data
				continue;
			int stnParmIndex;
			//stnMetaDataStr: first part of station data section, containing station specific
			// meta data information e.g STID = ALB           STNM =    72518   
			// AND reference time, e.g. TIME = 130724/1200  
			String stnMetaDataStr;
			//stnParamDataStr: Optional 2nd part of station data section, containing 
			// optional station parameters, e.g STNPRM = KINX;SWET;LIFT 
			//String stnParamDataStr;
			
			String[] stnMetaArray;
			
			//String[] stnParmDataArray;
			if(stnParmArray!=null){
				stnParmIndex = stnDataString.indexOf(stnParmArray[0]);		
				stnMetaDataStr = stnDataString.substring(0,stnParmIndex);			
				//stnParamDataStr = stnDataString.substring(stnParmIndex,stnSndParmIndex);
				//System.out.println("stnParmIndex="+stnParmIndex+"\n stnMetaDataStr="+stnMetaDataStr+"\n stnParamDataStr="+stnParamDataStr);
				//stnParmDataArray = stnParamDataStr.split("[ =\n]+");
				//for(String pm: stnParmDataArray){
				//	System.out.println("STNPRM="+pm);
					//TBD...if we decide to save STNPRM, then we have to decode stnParmDataArray and save them into GenericPointDataStationProduct
				//}
			}
			else {
				stnMetaDataStr = stnDataString.substring(0,stnSndParmIndex);		
			}
			stnMetaArray= stnMetaDataStr.split("[ =\n]+");
			int i=0;
			GenericPointDataStationProduct stnProd = new GenericPointDataStationProduct(versionNum,prodName);
			boolean reftimeNotAvail = true, stnIdNotAvail=true;
			String stid="", timeStr="";
			for(String pm: stnMetaArray){
				//System.out.println("Metainfo="+pm);	
				if(pm.equals(GenericPointDataConstants.SND_STN_ID)){
					if(i+1 < stnMetaArray.length && stnMetaArray[i+1].equals(GenericPointDataConstants.SND_STN_NUM)==false){
						stnProd.getLocation().setStationId(stnMetaArray[i+1]);
						stnIdNotAvail=false;
						stid = stnMetaArray[i+1];
					}
				}
				else if(pm.equals(GenericPointDataConstants.SND_STN_NUM) && stnIdNotAvail == true){
					//if not stid then try stnm
					if(i+1 < stnMetaArray.length && stnMetaArray[i+1].equals(GenericPointDataConstants.SND_REFTIME)==false){
						stnProd.getLocation().setStationId(stnMetaArray[i+1]);
						stnIdNotAvail=false;
						stid = stnMetaArray[i+1];
					}
					else{
						returnStatus = "*****************************************\nDropped one station product. STID / STNM not found \n"+ stnMetaDataStr + "\n";
						break;
					}
				}
				else if(pm.equals(GenericPointDataConstants.SND_STN_LAT)){
					if(i+1 < stnMetaArray.length){
						try{
							float lat = Float.parseFloat(stnMetaArray[i+1]);
							stnProd.setSlat(lat);
						}
						catch ( NumberFormatException e){
							stnProd.setSlat(-9999.00f);
						}
					}
				}
				else if(pm.equals(GenericPointDataConstants.SND_STN_LON)){
					if(i+1 < stnMetaArray.length){
						try{
							float lon = Float.parseFloat(stnMetaArray[i+1]);
							stnProd.setSlon(lon);
						}
						catch ( NumberFormatException e){
							stnProd.setSlon(-9999.00f);
						}
					}
				}
				else if(pm.equals(GenericPointDataConstants.SND_STN_ELEVATION)){
					if(i+1 < stnMetaArray.length){
						try{
							float elv = Float.parseFloat(stnMetaArray[i+1]);
							stnProd.getLocation().setElevation((int)(Math.round(elv)));
						}
						catch ( NumberFormatException e){
							stnProd.getLocation().setElevation(0);
						}
					}
				}
				else if(pm.equals(GenericPointDataConstants.SND_REFTIME)){
					if(i+1 < stnMetaArray.length&& stnMetaArray[i+1].equals(GenericPointDataConstants.SND_STN_LAT)==false){
						SimpleDateFormat df = new SimpleDateFormat("yyMMdd/HHmm");
						
						try {
							//For PFC sounding, reference time and forecast time may be different
							//We use the first station's "TIME" decoded as reference time for all time lines / stations in
							//one GEMPAK file..
							// The rest decoded TIME is decoded as forecast time for each time line. 
							//However, for UAIR, all decoded "TIME" are treated as reference time for that station/time line
							if(refTime == null) 
								refTime = df.parse(stnMetaArray[i+1]);
							currentTime = df.parse(stnMetaArray[i+1]);
							timeStr = stnMetaArray[i+1];
							//System.out.println("date="+date.toString());
							reftimeNotAvail=false;
						} catch (ParseException e) {
							// TODO Auto-generated catch block
							e.printStackTrace();
						}
						if(currentTime!=null){
							if(GenericPointDataReqType.STORE_GPD_MDL_SND_PRODUCT_FROM_GEMPAK_TBL == reqType){
								stnProd.setRefTime(refTime);
								int forecastTime= (int)((currentTime.getTime() - refTime.getTime())/1000);
								stnProd.setForecastTime(forecastTime);
							}
							else {// should be (GenericPointDataReqType.STORE_GPD_OBS_SND_PRODUCT_FROM_GEMPAK_TBL == reqType){
								stnProd.setRefTime(currentTime);
								stnProd.setForecastTime(-1);
							}
						}
						else {
							//no mandatory reference time found
							returnStatus = "*****************************************\nDropped one station product. TIME not found. \n"+ stnMetaDataStr + "\n";
							break;
						}
					}
				}
				/*else if(pm.equals(GenericPointDataConstants.SND_STN_IM)){
				     
				}*/
				i++;
			}
			if(reftimeNotAvail == false && stnIdNotAvail == false){
				//Chin Note: for each station...
				//stnSndDataStr: 3rd part of station data section, containing sounding parameters,
				// e.g.      PRES     TMPC     DWPT     HGHT     DRCT     SKNT     TMPK     RELH
	            //   MIXR     UKNT     VKNT     THTA     THTE
	            //   998.00    24.40    22.80   178.00     0.00     0.00   297.55    90.80
	            //             17.87     0.00     0.00   297.72   349.73			
				//
				
				String stnSndDataStr = stnDataString.substring(stnSndParmIndex);
				stnSndDataStr = stnSndDataStr.trim();
				String[] stnSndDataLineArray = stnSndDataStr.split("[\n]+");
				//adjust max level number in prodInfo, note that, the value should be the largest for all station.
				//Chin NOTE:: : since we use fixed levelNumMax, not necessary to do the following for now
				//if(levelNumMax < stnSndDataLineArray.length/numLinePerLevel){
				//	levelNumMax = stnSndDataLineArray.length/numLinePerLevel;
				//	prodInfo.setMaxNumberOfLevel(levelNumMax);
					//System.out.println("max num of level = "+ levelNumMax);
				//}
				//We do not get Master Level info from CLI, therefore, level value is not really used for now.
				// just incrementing 1 per level advanced
				int levelValue =1;
				//first N (=numLinePerLevel) lines are parameter names, like, "PRES     THTA     MIXR     DRCT     SPED     TVRK     HGHT   etc", just skip them. 
				//Therefore, for loop starts from N (=numLinePerLevel).
				//System.out.println("Number of lines: "+stnSndDataLineArray.length);
				int stnLevelNum=1;
				for(int j=numLinePerLevel; j< stnSndDataLineArray.length; j= j+numLinePerLevel){
					if(stnLevelNum > levelNumMax){
						// can not save more data exceeding levelNumMax
						break;
					}
					stnLevelNum++;
					GenericPointDataLevel gpdLevel= new GenericPointDataLevel();
					gpdLevel.setLevelValue(levelValue++);
					boolean goodLevel = true;
					for(int lineCountWithinlevel=0; lineCountWithinlevel< numLinePerLevel;lineCountWithinlevel++){
						if(j+lineCountWithinlevel >= stnSndDataLineArray.length){
							goodLevel = false;
							break;
						}
						String lineStr = stnSndDataLineArray[j+lineCountWithinlevel];
						lineStr= lineStr.trim();
						//System.out.println("SNDPRM Line " + (j+lineCountAtlevel)+" = "+lineStr);
						String[] stnSndDataParmAtCurLineArray = lineStr.split("[ ]+");
						int numSndParmAtLine;
						if(lineCountWithinlevel == numLinePerLevel-1){
							numSndParmAtLine = numSndParmAtLastLine;
						}
						else {
							numSndParmAtLine = GenericPointDataConstants.SND_PARM_PER_LINE;
						}
						if(stnSndDataParmAtCurLineArray.length == numSndParmAtLine){						
							for(int k=0; k< stnSndDataParmAtCurLineArray.length; k++){
								String pmStr = stnSndDataParmAtCurLineArray[k];
								float pmValue = -9999;
								try{
									pmValue = Float.parseFloat(pmStr);
									//System.out.println("SNDPRM " + k+" = "+pmStr);
									//sndParmArray contains all parms in one dimension, therefore, when refer to it using index k, need add line count adjustment to it.  
									GenericPointDataParameter gpdParm = new GenericPointDataParameter(sndParmArray[k+lineCountWithinlevel*GenericPointDataConstants.SND_PARM_PER_LINE],pmValue);
									gpdLevel.getGpdParameters().add(gpdParm);
								}
								catch ( NumberFormatException e){
									System.out.println("Bad parameter value:"+ pmStr);
								}
								GenericPointDataParameter gpdParm = new GenericPointDataParameter(sndParmArray[k+lineCountWithinlevel*GenericPointDataConstants.SND_PARM_PER_LINE],pmValue);
								gpdLevel.getGpdParameters().add(gpdParm);
							}									
						}
						else{
							//Chin NOTE: with this simple error handling design,
							//When a missing parameter in line, we will drop all data for this level.
							//System.out.println("sdnParmlength="+stnSndDataParmAtCurLineArray.length+" should be"+  numSndParmAtLine);
							returnStatus = returnStatus +"Dropped incomplete sounding data level, missing data at line @ "+ stid+ " Time="+timeStr+"\n"+ lineStr + "\n stnStr="+stnDataString+"\n found="+found;
							goodLevel = false;
							break;
						}
					
					}
					if(goodLevel)
						stnProd.getLevelLst().add(gpdLevel);
					else{
						//if there is a bad level, we drop this station
						goodStation = false;
						break;
					}
				}
				if(goodStation)
					stnProdList.add(stnProd);
			}
			if(firstRoundOfPersistence == true ){
				int latestProdVersion = gpdDao.getGpdProductLatestVersion(currentTime, prodName);
				if(versionNum <= latestProdVersion)
					//input product should not have version smaller or equal to same product's latest product version in DB
					//If a new product, its product version should be 0 and latestProdVersion should be  -1.
					return "Data persistence failed. Input version number "+ versionNum+ " DB latest version number "+latestProdVersion;
				prodInfo = gpdDao.lookupUpdateGpdProdInfo(prodInfo, true, versionNum);
				if(prodInfo == null)
					return "Data persistence failed. Bad Product info\n";
				//NOte: we assume at first round, we will already have max level set for generating 
				// pdd
				pdd = createPointDataDescription(prodInfo);
			}
			if(pdd != null && stnProdList.size() >=BLOCKSIZE ){
				try {
					String sts = performPersist( prodInfo,stnProdList
							,versionNum, pdd) ;
					
					if(sts.equals(OK)== false){
						//either product info or version is not right, stop parsing.
						return returnStatus+ sts;
					}
					firstRoundOfPersistence = false; //only check once
					stnProdList.clear();
				} catch (PluginException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				}
				//blockCount++;
			}
		}
		//System.out.println("Prod " + prodName+": persisted "+ (blockCount*BLOCKSIZE + stnProdList.size())+ " records.");
		
		// finish up the last round of stations data
		if(pdd!=null && stnProdList.size() > 0 ){
			try {
				String sts = performPersist( prodInfo,stnProdList
						,versionNum,  pdd) ;
				if(sts.equals(OK)== false){
					//either product info or version is not right, stop parsing.
					return returnStatus+ sts;
				}
				stnProdList.clear();
			} catch (PluginException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		} 
		return returnStatus;
	}
	/*
	 * Parsing Gempak generated point data surface (2D) table.
	 * Input example- surface data - single station, multiple time lines
	 * *******************************************************************
	PARM = PMSL;ALTI;TMPC;DWPC;SKNT;DRCT;GUST;WNUM;CHC1;CHC2;CHC3;VSBY;P03D;P03I;   
        MSUN;SNOW;WEQS;P24I;TDXC;TDNC;P03C;CTYL;CTYM;CTYH;P06I;T6XC;T6NC;CEIL;   
        P01I;SNEW                                                                

    STN    YYMMDD/HHMM      PMSL     ALTI     TMPC     DWPC     SKNT     DRCT
                            GUST     WNUM     CHC1     CHC2     CHC3     VSBY
                            P03D     P03I     MSUN     SNOW     WEQS     P24I
                            TDXC     TDNC     P03C     CTYL     CTYM     CTYH
                            P06I     T6XC     T6NC     CEIL     P01I     SNEW
    DCA    130724/0000   1005.00    29.68    29.40    17.80     8.00   330.00
                        -9999.00 -9999.00   556.00  2502.00 -9999.00    10.00
                         3002.00 -9999.00 -9999.00 -9999.00 -9999.00 -9999.00
                        -9999.00 -9999.00     0.20 -9999.00 -9999.00 -9999.00
                        -9999.00    31.70    29.40 -9999.00 -9999.00 -9999.00
    DCA    130724/0100   1005.40    29.69    28.90    17.80     6.00   330.00
                        -9999.00 -9999.00  1506.00  2502.00 -9999.00    10.00
                        -9999.00 -9999.00 -9999.00 -9999.00 -9999.00 -9999.00
                        -9999.00 -9999.00 -9999.00 -9999.00 -9999.00 -9999.00
                        -9999.00 -9999.00 -9999.00 -9999.00 -9999.00 -9999.00
	 ********************************************************************************************
	 * Input example- surface data - multiple stations,
	 * *******************************************************************
	 PARM = SLAT;SLON;PMSL;PRES;P03D;TMPC;DWPC;TMWC;SPED;DRCT;VSBK;WWMO;PWWM;CFRT;   
        CFRL;CTYL;CLHL;SSTC;WPER;WHGT;PWHR;PWMN;PWDR;PWSP;TOST;CBAS;POWW;HOWW;   
        DOSW;POSW;HOSW;DOS2;POS2;HOS2;IDTH;ROIA;GUST;ALTI;SHPD;SHPK;PKWD;PKWS;   
        PKWT;PMN1                                                                

    STN    YYMMDD/HHMM      SLAT     SLON     PMSL     PRES     P03D     TMPC
                            DWPC     TMWC     SPED     DRCT     VSBK     WWMO
                            PWWM     CFRT     CFRL     CTYL     CLHL     SSTC
                            WPER     WHGT     PWHR     PWMN     PWDR     PWSP
                            TOST     CBAS     POWW     HOWW     DOSW     POSW
                            HOSW     DOS2     POS2     HOS2     IDTH     ROIA
                            GUST     ALTI     SHPD     SHPK     PKWD     PKWS
                            PKWT     PMN1
                            
 WDF7019   130724/1200     44.50   -83.00  1015.90 -9999.00  3010.00    27.70
                        -9999.00 -9999.00     5.10   350.00    10.00 -9999.00
                        -9999.00     2.00 -9999.00 -9999.00 -9999.00 -9999.00
                        -9999.00 -9999.00 -9999.00 -9999.00 -9999.00 -9999.00
                            1.00    -1.00     2.00     0.50 -9999.00 -9999.00
                        -9999.00 -9999.00 -9999.00 -9999.00 -9999.00 -9999.00
                        -9999.00 -9999.00     0.00     3.00 -9999.00 -9999.00
                        -9999.00 -9999.00
   KCDK    130724/1200     34.60   -75.40  1007.00 -9999.00  2002.00    26.10
                           23.00    23.90     6.70   180.00    20.00 -9999.00
                            0.00     2.00     1.00     1.00    74.00 -9999.00
                        -9999.00     1.10 -9999.00 -9999.00 -9999.00 -9999.00
                            1.00     8.00     2.00     0.50   190.00     6.00
                            1.00 -9999.00 -9999.00 -9999.00 -9999.00 -9999.00
                        -9999.00 -9999.00    45.00    13.00 -9999.00 -9999.00
                        -9999.00 -9999.00
  WDD2876   130724/1200     43.40   -86.90  1017.20 -9999.00 -9999.00    16.60
                        -9999.00 -9999.00     5.10    60.00    50.00     3.00
                            0.00     2.00 -9999.00 -9999.00 -9999.00 -9999.00
                        -9999.00 -9999.00 -9999.00 -9999.00 -9999.00 -9999.00
                            1.00    -1.00 -9999.00     1.00 -9999.00 -9999.00
                        -9999.00 -9999.00 -9999.00 -9999.00 -9999.00 -9999.00
                        -9999.00 -9999.00   180.00    13.00 -9999.00 -9999.00
                        -9999.00 -9999.00
      ********************************************************************************************		
  NOTE:::: Based on Gempak Fortran source code. The max number of parameters per line is
  		   defined as "6". A fixed number. Our decoding is based on this number. If
  		   Gempak source code changed, then we will have to change code as well.

	 */
	private String parseSurfaceTable(String prodTblString,String prodName, int versionNum){
		int levelNumMax = 1;
		GenericPointDataProductInfo prodInfo = new GenericPointDataProductInfo(prodName, levelNumMax );
		String returnStatus = "";
		//input start with "PARM =" and followed by a list of sounding parameters, terminated at STN
		int parmindex = prodTblString.indexOf(GenericPointDataConstants.SFC_PARM);
		if(parmindex <0){
			return "Bad input, no PARM key word, could not determine sounding parameters!";
		}
		else{
			//make sure no more PARM= line.
			if(prodTblString.substring(parmindex+1).indexOf(GenericPointDataConstants.SFC_PARM)>=0){
				return "Bad input, more than one  PARM key word, could not determine sounding parameters!";
			}
		}
		int firstEqualSignIndex= prodTblString.indexOf('=');
		//station id line
		int stnIdIndex = prodTblString.indexOf(GenericPointDataConstants.SFC_STN);
		if(stnIdIndex <0){
			return "Bad input, no STN key word, could not determine sounding parameters!";
		}
		else {
			if(prodTblString.indexOf(GenericPointDataConstants.SFC_STN, stnIdIndex+1)>=0){
				return "Bad input, double STN key word, could not determine sounding parameters!";
			}
		}
		String parmStr="";
		String[] parmArray=null;
		if(stnIdIndex > firstEqualSignIndex){
			parmStr = prodTblString.substring(firstEqualSignIndex+1,stnIdIndex);
		}
		else{
			return "Bad input, wrong STN key word position, could not determine sounding parameters!";
		}
		
		if(parmStr.length() <=0){
			return "Bad input, no PARM (weather parameters) defined!";
		}
		parmStr = parmStr.trim();
		parmArray = parmStr.split("[; \n]+");
		System.out.println("ParmStr= "+parmStr+"\nNum of parm ="+parmArray.length);
		int parmNumInLastLine = parmArray.length % GenericPointDataConstants.SFC_PARM_PER_LINE;
		//create product info parameter list 
		for(String pm: parmArray){
			//slat/slon saved in postgres only, this parm list is for saving weather parameters in HDF5
			if(pm.equals(GenericPointDataConstants.SFC_LAT)){
				continue;
			}
			if(pm.equals(GenericPointDataConstants.SFC_LON)){
				continue;
			}
			Parameter parm= new Parameter(pm);
			prodInfo.getParameterLst().add(parm);
			//System.out.println(pm + " length="+ pm.length());	
		}
		
		//remainDataString: point to data starting from STN key word
		String remainDataString = prodTblString.substring(stnIdIndex);
		remainDataString = remainDataString.trim();
		String[] stnDataLineArray = remainDataString.split("[\n]+");
		
		int numDataLinePerStn= parmArray.length  / GenericPointDataConstants.SFC_PARM_PER_LINE;
		if((parmArray.length  % GenericPointDataConstants.SFC_PARM_PER_LINE) >0)
			numDataLinePerStn++;
		int numStation = stnDataLineArray.length / numDataLinePerStn - 1;
		System.out.println("Total line number = "+ stnDataLineArray.length+", around "+ numStation + " stations in input table, each station has "+ numDataLinePerStn + " line");
		List<List<String>> headerTbl = new ArrayList<List<String>>();
		int parmNumInFirstLine =2;
		// create header table for reference by each station data and find slat/slon index for ship data
		for(int lineIndex= 0; lineIndex < numDataLinePerStn; lineIndex++){
			String lineStr = stnDataLineArray[lineIndex];
			//System.out.println("header line: "+lineStr + " length="+ lineStr.length());	
			lineStr = lineStr.trim();
			String[] parmArry = lineStr.split("[ ]+");
			if(lineIndex==0){
				parmNumInFirstLine = parmArry.length;
			}
			headerTbl.add(new ArrayList(Arrays.asList(parmArry)));
		}
		// keep records of slat, slon index 
		int lineNum=0,slatLn=-1, slatRow=-1,slonLn=-1,slonRow=-1;
		for(List<String> hdrLine: headerTbl ){
			for(int k=0; k < hdrLine.size(); k++){
				String pmStr = hdrLine.get(k);
				if(pmStr.equals(GenericPointDataConstants.SFC_LAT)){
					slatLn=lineNum;
					slatRow=k;
				}
				if(pmStr.equals(GenericPointDataConstants.SFC_LON)){
					slonLn=lineNum;
					slonRow=k;
				}
			}
			lineNum++;
		}
		// drop stn, time, slat, slon from table to speed up parsing.
		// stn and time are always in first line
		List<String> hdrLst = headerTbl.get(0);
		hdrLst.remove(GenericPointDataConstants.SFC_STN);
		hdrLst.remove(GenericPointDataConstants.SFC_REFTIME);
		if(slatLn>=0){
			hdrLst = headerTbl.get(slatLn);
			hdrLst.remove(GenericPointDataConstants.SFC_LAT);
		}
		if(slonLn>=0){
			hdrLst = headerTbl.get(slonLn);
			hdrLst.remove(GenericPointDataConstants.SFC_LON);
		}
		boolean firstRoundOfPersistence = true;
		PointDataDescription pdd=null;
		SimpleDateFormat df = new SimpleDateFormat("yyMMdd/HHmm");
		
		// Chin Note::To speed up HDF5 persistence speed and also avoid out of heap memory problem,
		// we have to split input data to smaller blocks. Each block has N (BLOCKSIZE) number of station/time line data. 
		// We also allocate N (BLOCKSIZE) number of  GenericPointDataStationProduct memory in advance here.
		// This is for reuse memory when there are big number of stations/time lines are input.
		// If not doing this, we may run into out of heap memory problem. As Java garbage collection 
		// is too slow for our need.
		int stnBlockSize;
		if(numStation > BLOCKSIZE){
			stnBlockSize = BLOCKSIZE;
		}
		else {
			stnBlockSize = numStation;
		}
		List<GenericPointDataStationProduct> stnProdList = new ArrayList<GenericPointDataStationProduct>();
		for (int i=0; i< stnBlockSize ; i++){
			GenericPointDataStationProduct stnProd = new GenericPointDataStationProduct(versionNum,prodName);
			GenericPointDataLevel gpdLevel= new GenericPointDataLevel(0);
			for(List<String> hdrLine: headerTbl ){
				for(int k=0; k < hdrLine.size(); k++){
					String pmStr = hdrLine.get(k);
					//System.out.println("PARM " + k+" = "+pmStr);
					GenericPointDataParameter gpdParm = new GenericPointDataParameter(pmStr,-9999.00f);
					gpdLevel.getGpdParameters().add(gpdParm);
				}
			}
			stnProd.getLevelLst().add(gpdLevel);
			stnProdList.add(stnProd);
		}
		List<List<String>> stnTbl = new ArrayList<List<String>>();
		//allocate memory for decoding end
		
		int stnIndexInBlock = 0;
		int blockCount=0;
		for(int currentIndex=numDataLinePerStn; currentIndex<stnDataLineArray.length; ){
			String stnlineStr = stnDataLineArray[currentIndex];
			stnlineStr = stnlineStr.trim();
			String[] stnparmArry = stnlineStr.split("[ ]+");
			// check if this line is the start of next station chunk
			// check the first line (stn id) line length
			if(parmNumInFirstLine != stnparmArry.length){
				System.out.println( "Not a start of a line::"+stnlineStr);
				returnStatus =returnStatus +  "*****************************************\nDropped one line. \n"+ stnlineStr + "\n";
				currentIndex++;
				continue;
			}
			
			//to here means that this is a none numeric string, then it should be a station id line, ie. the first line of a record
			//add first line to stnTbl
			//System.out.println(" line: "+stnlineStr + " length="+ stnlineStr.length());

			stnTbl.add((new ArrayList(Arrays.asList(stnparmArry))));
			currentIndex++;
			boolean stationLineNotRight= false;
			//lineidx start from 1 as first line (the station id line) already parsed
			for(int lineidx= 1; lineidx < numDataLinePerStn; lineidx++){
				String lineStr = stnDataLineArray[currentIndex];
				//System.out.println(" line: "+lineStr + " length="+ lineStr.length());	
				lineStr = lineStr.trim();
				String[] parmArry = lineStr.split("[ ]+");
				if(GenericPointDataConstants.SFC_PARM_PER_LINE == parmArry.length || 
						(lineidx == numDataLinePerStn-1 && parmNumInLastLine == parmArry.length))
				{
					stnTbl.add((new ArrayList(Arrays.asList(parmArry))));
					currentIndex++;
				}
				else{
					// line length is not right 
					stationLineNotRight = true;
					break;
				}
			}
			if(stationLineNotRight == true){
				System.out.println("Dropped one station "+stnlineStr);
				returnStatus =returnStatus +  "Dropped one station \n"+ stnlineStr + "\n";
				stnTbl.clear();	
				continue;
			}

			GenericPointDataStationProduct newstnProd = stnProdList.get(stnIndexInBlock);
			newstnProd.cleanUp();
			GenericPointDataLevel gpdLevel = newstnProd.getLevelLst().get(0);
			
			//get slat value
			String slatStr=null;
			if(slatLn!=-1 && slatRow!=-1){
				slatStr = stnTbl.get(slatLn).get(slatRow);
				newstnProd.setSlat(Float.parseFloat(slatStr));
			}
			//get slon value
			String slonStr=null;
			if(slonLn!=-1 && slonRow!=-1){
				slonStr = stnTbl.get(slonLn).get(slonRow);
				newstnProd.setSlon(Float.parseFloat(slonStr));
			}
			// get stnId value and drop it from table
			String stnIdStr = stnTbl.get(0).get(0);
			//System.out.println("stnId="+stnIdStr);
			if(stnIdStr.length()<=0){
				returnStatus =returnStatus +  "Dropped one station product. STN not found \n"+ stnTbl.get(0) + "\n";
				stnTbl.clear();
				continue;
			}
			newstnProd.getLocation().setStationId(stnIdStr);
			// get reference time, it should be the 2nd element of the first line
			String refTime =  stnTbl.get(0).get(1);
			Date date = null;
			if(refTime.length()<=0){
				returnStatus =returnStatus +  "Dropped one station product. Time not found \n"+ stnTbl.get(0) + "\n";
				stnTbl.clear();
				continue; 
			}
			try {
				date = df.parse(refTime);
				//System.out.println("date="+date.toString());
			} catch (ParseException e) {
				// TODO Auto-generated catch block
				//e.printStackTrace();
				System.out.println("bad reftime"+refTime);
			}
			if(date!=null){
				newstnProd.setRefTime(date);
				newstnProd.setForecastTime(-1);
			}
			else{
				returnStatus = returnStatus + "Dropped one station product. Time in bad format \n"+ stnTbl.get(0) + "\n";
				stnTbl.clear();
				continue;  
			}
			//drop stnId. time, slat, slon from table to in sync with headerTbl
			List<String> lineLst = stnTbl.get(0);
			lineLst.remove(stnIdStr);
			lineLst.remove(refTime);
			if(slatLn>=0 && slatStr!=null){
				lineLst = stnTbl.get(slatLn);
				lineLst.remove(slatStr);
			}
			if(slonLn>=0 && slonStr!=null){
				lineLst = stnTbl.get(slonLn);
				lineLst.remove(slonStr);
			}
			lineNum=0;
			int totalParmIndex=0;
			for(List<String> hdrLine: headerTbl ){
				if(stnTbl.get(lineNum).size() < hdrLine.size()){
					// this stn data line has less parameter data than expected. It should have same number (length) of data as header line.
					// drop this line.
					returnStatus = returnStatus + "Dropped one data line.\n"+ hdrLine+"\n"+ stnTbl.get(lineNum) + "\n";
					continue;
				}
				
				for(int k=0; k < hdrLine.size(); k++){
					try{
						float value = Float.parseFloat(stnTbl.get(lineNum).get(k));
						gpdLevel.getGpdParameters().get(k+totalParmIndex).setValue(value);
						//System.out.println("PARM " + (k+totalParmIndex)+" = "+gpdLevel.getGpdParameters().get(k+totalParmIndex).getName()+ "value="+value);
					}
					catch ( NumberFormatException e){
						gpdLevel.getGpdParameters().get(k+totalParmIndex).setValue(-9999);
						System.out.println("Bad parameter value:"+ stnTbl.get(lineNum).get(k));
					}
				}
				totalParmIndex= totalParmIndex + hdrLine.size();
				lineNum++;
			}		
			stnIndexInBlock++;
			stnTbl.clear();
			if(firstRoundOfPersistence == true ){
				int latestProdVersion = gpdDao.getGpdProductLatestVersion(stnProdList.get(0).getRefTime(), prodName);
				if(versionNum <= latestProdVersion)
					//input product should not have version smaller or equal to same product's latest product version in DB
					//If a new product, its product version should be 0 and latestProdVersion should be  -1.
					return "Data persistence failed. Input version number "+ versionNum+ " DB latest version number "+latestProdVersion;
				prodInfo = gpdDao.lookupUpdateGpdProdInfo(prodInfo, true, versionNum);
				if(prodInfo == null)
					return "Data persistence failed. Bad Product info\n";
				pdd = createPointDataDescription(prodInfo);
				
			}
			if(pdd != null && stnIndexInBlock >=BLOCKSIZE ){
				try {
					String sts = performPersist( prodInfo,stnProdList
							,versionNum,  pdd) ;
					if(sts.equals(OK)== false){
						//either product info or version is not right, stop parsing.
						return returnStatus+ sts;
					}
					
					firstRoundOfPersistence = false; //only check once
					stnIndexInBlock=0;
					blockCount++;
				} catch (PluginException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				}
			}
		}
		//System.out.println("Prod " + prodName+": persisted "+ (blockCount*BLOCKSIZE + stnProdList.size())+ " records.");
		// finish up the last round of stations data (size of stnIndexInBlock) that was not persisted yet.
		if(pdd!=null && stnIndexInBlock > 0 ){
			try {
				String sts = performPersist( prodInfo,stnProdList
						,versionNum,  pdd) ;
				if(sts.equals(OK)== false){
					//either product info or version is not right, stop parsing.
					return returnStatus+ sts;
				}
				stnProdList.clear();
			} catch (PluginException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		} 
		return returnStatus;
	}
	
	/*
	 * Return status: OK or error status
	 */
	private String performPersist(GenericPointDataProductInfo prodInfo,List<GenericPointDataStationProduct> stnProdLst 
			, int prodVersion, PointDataDescription pdd) throws PluginException {
		String prodName = prodInfo.getName();		
		List<PluginDataObject> rtObLst =null;
		
		
		DataTime dataTime;
		//System.out.println("GenericPointDataProductInfo is obtained! report name="+prodInfo.getName()+
		//		" Reftime="+ dataTime.getRefTimeAsCalendar().getTime()+" in ms="+ dataTime.getRefTimeAsCalendar().getTimeInMillis()+ " stnLstSize="+lstSize + " numMaxLvls="+prodInfo.getMaxNumberOfLevel());
		if(pdd!=null){
			rtObLst = new ArrayList<PluginDataObject>();
			int stnProdSize = stnProdLst.size();
			GenericPointDataRecord gpdRec;
			for(int i = 0; i < stnProdSize; i++){
				GenericPointDataStationProduct stnPd = stnProdLst.get(i);
				int numLevel = stnPd.getLevelLst().size();
				ObStation location= stnPd.getLocation();	
				if(numLevel >0 && location.getStationId() !=null){
					if(location.getCatalogType()==null)
						location.setCatalogType(ObStation.CAT_TYPE_MESONET);
					//construct gid as it is not an xml element in OBStation definition
					location.setGid(location.getCatalogType()+"-"+location.getStationId());
					//System.out.println("gpd location gid = "+location.getGid());
					gpdDao.lookupGpdLocation(location, true);				
					stnPd.setNumLevel(numLevel);
					PointDataView view = createPointDataView(pdd, stnPd);
					if(view !=null){
						Date refTime = stnPd.getRefTime();
						int forecastTimeSec = stnPd.getForecastTime();
						if(forecastTimeSec == -1)
							dataTime = new DataTime(stnPd.getRefTime());
						else
							dataTime = new DataTime(stnPd.getRefTime(), (int)forecastTimeSec);
						gpdRec= new GenericPointDataRecord(prodInfo,location,stnPd.getSlat(), stnPd.getSlon(),view, dataTime, prodVersion);
						rtObLst.add(gpdRec);
					}
				}
			}
			PluginDataObject [] recordObjects = rtObLst.toArray(new PluginDataObject[rtObLst.size()]);
			EDEXUtil.checkPersistenceTimes(recordObjects);
			long t0 = System.currentTimeMillis();
			gpdDao.persistToHDF5(recordObjects);
			long t1 = System.currentTimeMillis();
			gpdDao.persistToDatabase(recordObjects);
			long t2 = System.currentTimeMillis();
			//System.out.println("\nTime spent to persist "+recordObjects.length+ " records to HDF5: "
			//		+ (t1 - t0)+" persist to Postgres: "
			//		+ (t2 - t1));
			totalHDF5Time = totalHDF5Time + (t1 - t0);
		}
		return OK;
	}
	/**
	 * @param prodTblString :  Gempak product table file in String
	 * @param ProdName :  Gempak product name
	 * @return ok or error message
	 * @throws DecoderException
	 * @throws PluginException 
	 */
	public String decodeGempakTblProdFmCli(String prodTblString, String ProdName,int versionNum, int maxNumOfLevel, GenericPointDataReqType reqType)throws DecoderException, PluginException {
		long tin = System.currentTimeMillis();
		String returnStatus="";	
		totalHDF5Time=0;
		if ( prodTblString !=null) {
			//check to see what type of product is 
			if(reqType == GenericPointDataReqType.STORE_GPD_OBS_SFC_PRODUCT_FROM_GEMPAK_TBL)
				returnStatus = parseSurfaceTable(prodTblString, ProdName,versionNum);
			else if(reqType == GenericPointDataReqType.STORE_GPD_OBS_SND_PRODUCT_FROM_GEMPAK_TBL||reqType == GenericPointDataReqType.STORE_GPD_MDL_SND_PRODUCT_FROM_GEMPAK_TBL)
				returnStatus= parseSoundingTable(prodTblString, ProdName, versionNum,maxNumOfLevel,reqType);
			/*
			if(prodTblString.contains(GenericPointDataConstants.SND_PARM)){
				//"SNPARM" is an unique string in sounding table file
				totalHDF5Time=0;
				returnStatus= parseSoundingTable(prodTblString, ProdName, versionNum,maxNumOfLevel);
			}
			else if(prodTblString.contains(GenericPointDataConstants.SFC_REFTIME)){
				totalHDF5Time=0;
				//"YYMMDD/HHMM" is an unique string in surface table file
				returnStatus = parseSurfaceTable(prodTblString, ProdName,versionNum);
			}*/
			else{
				return("Decode failed! Bad product type, can not tell what product type is input!");
			}
		}
		else{
			return("Failed! Empty product input!");
		}
		long tout = System.currentTimeMillis();
		System.out.println("Total Time spent in decoding product: "+ (tout-tin) + " msecs, to HDF5= "+ totalHDF5Time +" msecs");
		return "Data persistence done!!\n"+ returnStatus;
	}
	/**
	 * @param prodXMLString : product XML file in String
	 * @return ok or error message
	 * @throws DecoderException
	 * @throws PluginException 
	 * Note: Since operation product is not going through this route, this function does not split product to smaller chucks.
	 * If ran into out of heap memory issue, then we will have to do it. See parseSurfaceTable() or parseSoundingTable().
	 */
	public String decodeXmlProdFmCli(String prodXMLString)throws DecoderException, PluginException {
		System.out.println("IN GPD XML String decode!");
		long tin = System.currentTimeMillis();
		PluginDataObject[] recordObjects = new PluginDataObject[0];
				
		InputStream is = null; 
		GenericPointDataProductContainer gpdc=null;
		GenericPointDataRecord gpdRec=null;
		JAXBContext ctx;	
		is = new ByteArrayInputStream(prodXMLString.getBytes());
		try {
			ctx = JAXBContext.newInstance(GenericPointDataProductContainer.class);
			if (ctx != null && is !=null) {
				
				Unmarshaller um = ctx.createUnmarshaller();
				if(um !=null){
					// test example: to unmarshal from file system 
					//String strmPath = "/res/pointdata/gpdProduct.xml";
					//GenericPointDataProductContainer gpdc = (GenericPointDataProductContainer)um.unmarshal(GenericPointDataProductContainer.class.getResourceAsStream(strmPath));
					Object result = um.unmarshal(is);
					if(result instanceof GenericPointDataProductContainer)
						gpdc = (GenericPointDataProductContainer)result;
					else
						return "Data persistence failed. XML file is not formatted correctly!";
				}
			}

		} catch (JAXBException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
			return "Data persistence failed. JAXBException happened!";
		}
		List<PluginDataObject> rtObLst =null;
		if(gpdc!=null){
			int prodVersion = gpdc.getProductCorrectionVersion();
			GenericPointDataProductInfo prodInfo=gpdc.getProductInfo();
			int latestProdVersion = gpdDao.getGpdProductLatestVersion(gpdc.getRefTime(), prodInfo.getName());
			if(prodVersion <= latestProdVersion)
				//input product should not have version smaller or equal to same product's latest product version in DB
				//If a new product, its product version should be 0 and latestProdVersion should be  -1.
				return "Data persistence failed. Wrong version number!";
			List<GenericPointDataStationProduct> stnProdLst = gpdc.getStnProdLst();
			int lstSize = stnProdLst.size();
			if(lstSize  <= 0)
				return  "Data persistence failed. No stn product";
			prodInfo = gpdDao.lookupUpdateGpdProdInfo(prodInfo, true, prodVersion);
			if(prodInfo == null)
				return "Data persistence failed. Bad Product info";
			DataTime dataTime = new DataTime(gpdc.getRefTime());
			System.out.println("GenericPointDataProductInfo is obtained! report name="+prodInfo.getName()+ " number of param="+prodInfo.getParameterLst().size() +
					" Reftime="+ dataTime.getRefTimeAsCalendar().getTime()+" in ms="+ dataTime.getRefTimeAsCalendar().getTimeInMillis()+ " stnLstSize="+lstSize + " numMaxLvls="+prodInfo.getMaxNumberOfLevel());
			
			PointDataDescription pdd = createPointDataDescription(prodInfo);
			if(pdd!=null){
				rtObLst = new ArrayList<PluginDataObject>();
				for(GenericPointDataStationProduct stnPd: stnProdLst){
					int numLevel = stnPd.getLevelLst().size();
					if(numLevel <=0)
						continue;
					ObStation location= stnPd.getLocation();	
					if(location.getCatalogType()==null)
						location.setCatalogType(ObStation.CAT_TYPE_MESONET);
					//construct gid as it is not an xml element in OBStation definition
					location.setGid(location.getCatalogType()+"-"+location.getStationId());
					gpdDao.lookupGpdLocation(location, true);				
					stnPd.setNumLevel(numLevel);
					PointDataView view = createPointDataView(pdd, stnPd);
					gpdRec= new GenericPointDataRecord(prodInfo,location,stnPd.getSlat(), stnPd.getSlon(),view, dataTime, gpdc.getProductCorrectionVersion());
					//gpdRec.constructTransientListAndMap(stnPd);
					//gpdRec.constructDataURI();
					//if(gpdDao.lookupGpdDataUri(gpdRec.getDataURI())== true ){
					//	System.out.println("daturi existing!!");
					//	continue;
					//}
					rtObLst.add(gpdRec);
				}
			}
		}
		if(rtObLst == null || rtObLst.size() <=0)
			return "Data persistence failed. XML file contains all duplicated data or does not contain any good data!";
		// batch persist
		int objCount=rtObLst.size();
		int loopCount = objCount/50+1;
		int restCount = objCount%50;
		int fromObjIx=0, toObjIx=0;
		for(int loop= 0; loop < loopCount; loop++){
			if(loop == loopCount-1){
				toObjIx = fromObjIx+ restCount;				
			}
			else {
				toObjIx = fromObjIx+ 50;
			}
			recordObjects = rtObLst.subList(fromObjIx, toObjIx).toArray(new PluginDataObject[toObjIx-fromObjIx]);
			EDEXUtil.checkPersistenceTimes(recordObjects);
			gpdDao.persistToHDF5(recordObjects);
			gpdDao.persistToDatabase(recordObjects);
			fromObjIx = fromObjIx + 50;	
			
		}
		long tout = System.currentTimeMillis();
		System.out.println("Total Time spent in decoding product: "+ (tout-tin) );
		
		// end batch persist
		//recordObjects = rtObLst.toArray(new PluginDataObject[rtObLst.size()]);
		//EDEXUtil.checkPersistenceTimes(recordObjects);
		//gpdDao.persistToHDF5(recordObjects);
		//gpdDao.persistToDatabase(recordObjects);
		return "Data persistence done!!";
	}
		
		/*
		//test code to persistent to DB and HDF5, and to generate xml and schema
		try {
			pdd = gpdDao.getPointDataDescription();
		} catch (JAXBException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		returnObjects = new PluginDataObject[1];
		gpdRec= new GenericPointDataRecord();
		gpdRec.setPluginName("gpd");
		gpdRec.setSlat(30.0);
		gpdRec.setNumLevel(1);
		gpdRec.getLevelValueLst().add(10.0f);
		gpdRec.getMap_ParmToValue().put("temp", 270.6);
		gpdRec.getMap_LevelToParmValueMap().put(10.0f, gpdRec.getMap_ParmToValue());
		GenericPointDataReporttype reportType = new GenericPointDataReporttype();
		reportType.setName("sib1");
		MasterLevel masterLevel = new MasterLevel();
		masterLevel.setName("MB");
		masterLevel.setDescription("isobaric surface");
		masterLevel.setUnitString("hPa");
		masterLevel.setType("DEC");
		reportType.setMasterLevel(masterLevel);
		for(int i=0;i<10;i++){
			Parameter p= new Parameter("TEMP"+i, "temperature "+i,"K");
			reportType.getParameterLst().add( p);
		}

		gpdRec.setReportType(reportType);
		gpdRec.setProductVersion(0);
		//Old code GenericPointDataLocation location = new GenericPointDataLocation();
		//location.setLatitude(35.0);
		//location.setLongitude(130.0);
		//location.setStnId("BWI");
		//location.setCountry("USA");
		//location.setElevation(100);
		//location.setState("MD");
		//location.setStationName("Baltimore");
		
		ObStation location = new ObStation();
		Coordinate coord = new Coordinate(-100, 100);
		GeometryFactory geometryFactory = new GeometryFactory();
		Point pt = geometryFactory.createPoint(coord);
		location.setStationGeom(pt);
		location.setStationId("MYIAD");
		location.setCountry("USA");
		location.setElevation(200);
		location.setState("VA");
		location.setName("Dulles");
		location.setCatalogType(1000);
		location.setGid(location.getCatalogType()+"-"+location.getStationId());
		gpdRec.setLocation(location);

		gpdDao.lookupGpdLocation(location, true);
		gpdDao.lookupGpdReportType(reportType, true);
		
		Calendar cal = Calendar.getInstance(TimeZone.getTimeZone("GMT"));
		DataTime dataTime = new DataTime(cal);
		gpdRec.setDataTime(dataTime);
		gpdRec.constructDataURI();

		returnObjects[0] = gpdRec;
		System.out.println("retrun ojct set");
		PointDataContainer container = PointDataContainer.build(pdd);
		PointDataView view = container.append();
		view.setString("REPORTTYPE","sib1");
		view.setInt("UTC", 5);
		gpdRec.setPointDataView(view);
		String path = gpdDao.getFullFilePath(gpdRec).getAbsolutePath();
		String filename = gpdDao.getPointDataFileName(gpdRec);
		System.out.println("hdf5 path="+path + " name="+filename);

		//*** generate XML schema and XML file for development use
		GenericPointDataProductContainer pdCon= new GenericPointDataProductContainer();
		pdCon.setReportType(reportType);
		for(int j=1; j<4; j++){
			GenericPointDataStationProduct stnP= new GenericPointDataStationProduct();
			//GenericPointDataLocation stn = new GenericPointDataLocation();
			ObStation stn = new ObStation();
			stn.setStationGeom(pt);
			stn.setCatalogType(1000);
			stn.setStationId("BWI"+j);
			stn.setCountry("USA");
			stn.setElevation(100);
			stn.setState("MD");
			stn.setName("Baltimore");
			stn.setGid(location.getCatalogType()+"-"+location.getStationId());
			stnP.setLocation(stn);
			for(int k=0; k<5;k++){
				GenericPointDataLevel lv = new GenericPointDataLevel(50+k);
				for(int i=0;i<10;i++){
					GenericPointDataParameter gpram = new GenericPointDataParameter("TEMP"+i,273+i+k*5+j*2);
					lv.getGpdParameters().add(gpram);
				}
				stnP.getLevelLst().add(lv);
			}
			pdCon.getStnProdLst().add(stnP);
		}
		pdCon.setRefTime(dataTime.getRefTime());

		try {
			ctx = JAXBContext.newInstance(GenericPointDataProductContainer.class);
			if (ctx != null ) {
				//
				// This block is to generate point data product container XML schema
				// 
				try {
					ctx.generateSchema(new gpdSchemaOutputResolver("/awips2/edex/data/hdf5/gpd/gpdProduct.xsd"));
				} catch (IOException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				} 
				// This block to to generate XML
				Marshaller mar = ctx.createMarshaller();
				if (mar != null) {
					mar.setProperty(Marshaller.JAXB_FORMATTED_OUTPUT, Boolean.TRUE);
					mar.marshal(pdCon, System.out);
				}
			}
		} catch (JAXBException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		***/
	
	private enum INPUT_TYPE{
		GPD_PROD,
		GPD_PRODINFO,
		GPD_UNKNOWN
	}
	private INPUT_TYPE getInputType(File inputFile){
		String currentLine;
		int lineCounter=0;
		int numLinesToRead= 5;
        FileReader fileReader;
		try {
			fileReader = new FileReader(inputFile.getAbsoluteFile());
			BufferedReader bufferedReader = new BufferedReader(fileReader);    				
			StringBuffer stringBuffer = new StringBuffer();
			while((currentLine = bufferedReader.readLine()) != null && lineCounter < numLinesToRead){
					stringBuffer.append(currentLine);
					lineCounter++;
			}
			bufferedReader.close();
			String headerLinesStr = stringBuffer.toString();
			int index =headerLinesStr.indexOf("GenericPointDataProduct");
			if(index >0)
				return INPUT_TYPE.GPD_PROD;
			/* Chin: do not support update product info only
			index =headerLinesStr.indexOf("GPD-ProdInfo-Def");
			if(index >0)
				return INPUT_TYPE.GPD_PRODINFO;	*/
			
		} catch (FileNotFoundException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		return INPUT_TYPE.GPD_UNKNOWN;
	}
	public void decodeGempakProdFmSbn(File inputFile)throws DecoderException, PluginException {
		System.out.println("GPD decodeGempakProdFmSbn() entered!");
		if(inputFile.getName().equals("test")){
			test(inputFile);
			return;// returnObjects;
		}
		
		InputStream is = null; 
		try {
			is = new FileInputStream(inputFile);
		} catch (FileNotFoundException e1) {
			// TODO Auto-generated catch block
			e1.printStackTrace();
			return;// returnObjects;
		} 
		StringWriter writer = new StringWriter();
		try {
			IOUtils.copy(is, writer, "UTF-8");
			String gempakFileStr = writer.toString();
			String status = decodeGempakTblProdFmCli(gempakFileStr, "nampfc", 0, 100, GenericPointDataReqType.STORE_GPD_MDL_SND_PRODUCT_FROM_GEMPAK_TBL);
			System.out.println(status);
			is.close();
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}
	/**
	 * Decode GPD XML file input from SBN/gpd
	 * @param inputProdStr : XML file 
	 * @return PluginDataObject[]
	 * @throws DecoderException
	 * @throws PluginException 
	 * Note: Since operation product is not going through this route, this function does not split product to smaller chucks.
	 * If ran into out of heap memory issue, then we will have to do it. See parseSurfaceTable() or parseSoundingTable().
	 * 
	 */

	public void decodeXmlProdFmSbn(File inputFile)throws DecoderException, PluginException {
		System.out.println("GPD decode() entered!");
		PluginDataObject[] recordObjects = new PluginDataObject[0];
		if(inputFile.getName().equals("test")){
			test(inputFile);
			return;// returnObjects;
		}
		
		InputStream is = null; 
		GenericPointDataProductContainer gpdc=null;
		GenericPointDataProductInfo prodInfo=null;
		GenericPointDataRecord gpdRec=null;
		JAXBContext ctx;
		try {
			is = new FileInputStream(inputFile);
		} catch (FileNotFoundException e1) {
			// TODO Auto-generated catch block
			e1.printStackTrace();
			return;// returnObjects;
		} 
		//INPUT_TYPE inType = getInputType(inputFile);
		try {
			//if(inType == INPUT_TYPE.GPD_PROD)
				ctx = JAXBContext.newInstance(GenericPointDataProductContainer.class);
			//else if(inType == INPUT_TYPE.GPD_PRODINFO)
			//	ctx = JAXBContext.newInstance(GenericPointDataProductInfo.class);
			//else
			//	ctx=null;
			if (ctx != null && is !=null) {

				Unmarshaller um = ctx.createUnmarshaller();
				if(um !=null){
					// test example: to unmarshal from file system 
					//String strmPath = "/res/pointdata/gpdProduct.xml";
					//GenericPointDataProductContainer gpdc = (GenericPointDataProductContainer)um.unmarshal(GenericPointDataProductContainer.class.getResourceAsStream(strmPath));
					Object result = um.unmarshal(is);
					if(result instanceof GenericPointDataProductContainer)
						gpdc = (GenericPointDataProductContainer)result;
					//else if(result instanceof GenericPointDataProductInfo)
						//prodInfo = (GenericPointDataProductInfo)result;
				}
			}

		} catch (JAXBException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
			return;// returnObjects;
		}
		List<PluginDataObject> rtObLst =null;
		if(gpdc!=null){
			prodInfo=gpdc.getProductInfo();
			int prodVersion = gpdc.getProductCorrectionVersion();
			int latestProdVersion = gpdDao.getGpdProductLatestVersion(gpdc.getRefTime(), prodInfo.getName());
			if(prodVersion <= latestProdVersion)
				//input product should not have version smaller or equal to same product's latest product version in DB
				//If a new product, its product version should be 0 and latestProdVersion should be  -1.
				return;
			List<GenericPointDataStationProduct> stnProdLst = gpdc.getStnProdLst();
			DataTime dataTime = new DataTime(gpdc.getRefTime());
			int lstSize = stnProdLst.size();
			if(lstSize  <= 0)
				return;// returnObjects;
			prodInfo = gpdDao.lookupUpdateGpdProdInfo(prodInfo, true,prodVersion);
			if(prodInfo == null)
				return;// returnObjects;
			System.out.println("GenericPointDataProductContainer is obtained! prod name="+prodInfo.getName()+
					" Reftime="+ dataTime.getRefTimeAsCalendar().getTime()+" in ms="+ dataTime.getRefTimeAsCalendar().getTimeInMillis()+ " stnLstSize="+lstSize + " numMaxLvls="+prodInfo.getMaxNumberOfLevel());

			PointDataDescription pdd = createPointDataDescription(prodInfo);
			if(pdd!=null){
				rtObLst = new ArrayList<PluginDataObject>();
				for(GenericPointDataStationProduct stnPd: stnProdLst){
					int numLevel = stnPd.getLevelLst().size();
					if(numLevel <=0)
						continue;
					ObStation location= stnPd.getLocation();
					if(location.getCatalogType()==null)
						location.setCatalogType(ObStation.CAT_TYPE_MESONET);
					//construct gid as it is not an xml element in OBStation definition
					location.setGid(location.getCatalogType()+"-"+location.getStationId());
					gpdDao.lookupGpdLocation(location, true);				
					stnPd.setNumLevel(numLevel);
					PointDataView view = createPointDataView(pdd, stnPd);
					gpdRec= new GenericPointDataRecord(prodInfo,location,stnPd.getSlat(), stnPd.getSlon(),view, dataTime, gpdc.getProductCorrectionVersion());
					//gpdRec.constructTransientListAndMap(stnPd);
					//gpdRec.constructDataURI();
					//if(gpdDao.lookupGpdDataUri(gpdRec.getDataURI())== true ){
					//	continue;
					//}
					System.out.println("adding record "+gpdRec.getLocation().getStationId());
					rtObLst.add(gpdRec);
				}
			}

		}
		//else if(prodInfo!=null){
			//just update/replace product information meta data in Postgres DB
		//	gpdDao.updateProductInfo(prodInfo);
		//}
		if(rtObLst == null || rtObLst.size() <=0)
			return;// returnObjects;
		recordObjects = rtObLst.toArray(new PluginDataObject[rtObLst.size()]);
		System.out.println("returnObjects size "+recordObjects.length);
		for(int k=0; k <recordObjects.length; k++ ){
			GenericPointDataRecord rec =(GenericPointDataRecord)(recordObjects[k]);
			System.out.println("returnObjects  "+k+" "+ rec.getLocation().getStationId());
		}
		//Chin:note:: for an unknow reason, persist to Prosgres does not work properly when rely on Spring's route to persist automatically
		//Therefore, do the same way as in decodeProd()
		EDEXUtil.checkPersistenceTimes(recordObjects);
		gpdDao.persistToHDF5(recordObjects);
		gpdDao.persistToDatabase(recordObjects);
		//returnObjects = new PluginDataObject[0];
		return ;//returnObjects;
	}
	   /**
     * 
     * @return the pluginName
     */
    public String getPluginName() {
        return pluginName;
    }

    /**
     * 
     * @param pluginName
     *            the pluginName to set
     */
    
    public void setPluginName(String pluginName) {
        this.pluginName = pluginName;
    }

    
    /**
	 * This API is to generate  XML schema
	 */
    public void generateSchema(Class<?> target, String filePath) { 

    	JAXBContext ctx=null;
		try {
			ctx = JAXBContext.newInstance(target);
		} catch (JAXBException e1) {
			// TODO Auto-generated catch block
			e1.printStackTrace();
		}// e.g. (GenericPointDataProductContainer.class);
    	if (ctx != null) {

    		try {
    			ctx.generateSchema(new gpdSchemaOutputResolver(filePath));// e.g ("/awips2/edex/data/hdf5/gpd/gpdProduct.xsd"));
    		} catch (IOException e) {
    			// TODO Auto-generated catch block
    			e.printStackTrace();
    		} 
    	}
    }
    class gpdSchemaOutputResolver extends SchemaOutputResolver {

    	private String filePath;

    	public gpdSchemaOutputResolver(String filePath) {
    		super();
    		this.filePath = filePath;
    	}

    	@Override
    	public Result createOutput(String namespaceUri, String suggestedFileName)
    	throws IOException {


    		// create new file
    		File file = new File(filePath);
    		//
    		// create stream result
    		StreamResult result = new StreamResult(file);

    		// set system id
    		result.setSystemId(file.toURI().toURL().toString());

    		// return result
    		return result;
    	}
    }
    public void test(File inputFile){
    	GenericPointDataProductInfo report = gpdDao.getGpdProdInfo("sib1");//GenericPointDataReportQuery.getReport();
    	System.out.println(" report name= "+report.getName()+ " master="+report.getMasterLevel().getName()+ " maxLevel="+report.getMaxNumberOfLevel());
    	for(Parameter pam: report.getParameterLst() ){
    		System.out.println("parm ="+pam.getAbbreviation());
    	}
    }
}
