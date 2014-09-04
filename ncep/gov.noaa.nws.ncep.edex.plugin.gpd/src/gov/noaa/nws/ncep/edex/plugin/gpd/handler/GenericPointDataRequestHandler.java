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
package gov.noaa.nws.ncep.edex.plugin.gpd.handler;

import gov.noaa.nws.ncep.common.dataplugin.gpd.GenericPointDataConstants;
import gov.noaa.nws.ncep.common.dataplugin.gpd.dao.GenericPointDataDao;
import gov.noaa.nws.ncep.common.dataplugin.gpd.product.GenericPointDataLevel;
import gov.noaa.nws.ncep.common.dataplugin.gpd.product.GenericPointDataParameter;
import gov.noaa.nws.ncep.common.dataplugin.gpd.product.GenericPointDataProductContainer;
import gov.noaa.nws.ncep.common.dataplugin.gpd.product.GenericPointDataProductInfo;
import gov.noaa.nws.ncep.common.dataplugin.gpd.product.GenericPointDataStationProduct;
import gov.noaa.nws.ncep.common.dataplugin.gpd.query.GenericPointDataReqMsg;
import gov.noaa.nws.ncep.common.dataplugin.gpd.query.GenericPointDataReqMsg.GenericPointDataQueryKey;
import gov.noaa.nws.ncep.common.dataplugin.gpd.query.GenericPointDataReqMsg.GenericPointDataReqType;
import gov.noaa.nws.ncep.edex.plugin.gpd.decoder.GenericPointDataDecoder;

import java.io.ByteArrayOutputStream;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.HashMap;
import java.util.List;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Marshaller;

import com.raytheon.edex.exception.DecoderException;
import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.common.parameter.Parameter;
import com.raytheon.uf.common.serialization.comm.IRequestHandler;

public class GenericPointDataRequestHandler implements IRequestHandler<GenericPointDataReqMsg> {

	private  GenericPointDataDao gpdDao;
	private GenericPointDataDecoder decoder;
	
	public GenericPointDataRequestHandler() throws DecoderException {
		super();
		try {
			gpdDao = new GenericPointDataDao("gpd");
			decoder = new GenericPointDataDecoder();
			
		} catch (PluginException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}
	private String createGempakFileString(GenericPointDataProductInfo prodInfo) {
		StringBuilder fileStr = new StringBuilder();
		List<Parameter> pmLst =prodInfo.getParameterLst();
		int pmSz= pmLst.size();
		if(prodInfo.getMaxNumberOfLevel() <= 2){
			// we defined surface data with MaxNumberOfLevel = 2, see parseSurfaceTable() comments
			//surface data - single level
			
			// Add PARM hdr
			fileStr.append(" "+GenericPointDataConstants.SFC_PARM +"= "+GenericPointDataConstants.SFC_LAT+
					";"+GenericPointDataConstants.SFC_LON+";");
			int pmCount=3; //start from 3 as we added slat ans slon already.
			for(int i=0; i<pmSz;i++ ){
				Parameter pm = pmLst.get(i);
				fileStr.append(pm.getAbbreviation());
				pmCount++;
				if(i== pmSz-1){
					fileStr.append("\n\n");
					break;
				}		
				fileStr.append(";");
				if(pmCount>GenericPointDataConstants.SFC_HDR_PARM_PER_LINE){
					fileStr.append("\n       ");
					pmCount=1;
				}
			}
		}
		else {
			//sounding data - multiple levels
			// Add SNDPARM hdr
			fileStr.append(" "+GenericPointDataConstants.SND_PARM +" = ");
			int pmCount=1; 
			for(int i=0; i<pmSz;i++ ){
				Parameter pm = pmLst.get(i);
				fileStr.append(pm.getAbbreviation());
				pmCount++;
				if(i== pmSz-1){
					fileStr.append("\n\n");
					break;
				}		
				fileStr.append(";");
				if(pmCount>GenericPointDataConstants.SND_HDR_PARM_PER_LINE){
					fileStr.append("\n       ");
					pmCount=1;
				}
			}
			//TBD:: Add STNPARM hdr
		}
		return fileStr.toString();
	}
	/*
	 * createGempakFileString()
	 * 	Surface data table format:::
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
	******************************************************************************************
		Sounding data table format:::
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

	 */
	private String createGempakFileString(GenericPointDataProductContainer pdCon) {
		GenericPointDataProductInfo prodInfo = pdCon.getProductInfo();
		
		List<GenericPointDataStationProduct> stnLst = pdCon.getStnProdLst();
		if(stnLst.size()<=0)
			return null;
		SimpleDateFormat df = new SimpleDateFormat("yyMMdd/HHmm");
		StringBuilder fileStr = new StringBuilder();
		List<Parameter> pmLst =prodInfo.getParameterLst();
		int pmSz= pmLst.size();
		if(prodInfo.getMaxNumberOfLevel() <= 2 && stnLst.get(0).getNumLevel() <=1 ){
			// we defined surface data with MaxNumberOfLevel = 2, see parseSurfaceTable() comments
			//surface data - single level
			
			// Add PARM hdr
			fileStr.append(" "+GenericPointDataConstants.SFC_PARM +"= "+GenericPointDataConstants.SFC_LAT+
					";"+GenericPointDataConstants.SFC_LON+";");
			int pmCount=3; //start from 3 as we added slat ans slon already.
			for(int i=0; i<pmSz;i++ ){
				Parameter pm = pmLst.get(i);
				fileStr.append(pm.getAbbreviation());
				pmCount++;
				if(i== pmSz-1){
					fileStr.append("\n\n");
					break;
				}		
				fileStr.append(";");
				if(pmCount>GenericPointDataConstants.SFC_HDR_PARM_PER_LINE){
					fileStr.append("\n       ");
					pmCount=1;
				}
			}
			// Note: List<GenericPointDataParameter> in GenericPointDataLevel is not in sync with pmLst			
			// use this map to save stn parameters to  be in sync with product parameter list,
			// so, we can list all parameters value in sync with parameter name listed.
			HashMap<String, Float>pmMap = new HashMap<String, Float>(pmSz);
			//Add stn/time/parm header
			pmCount=3;//start from 3 as we added slat ans slon already.
			String titleStr = "    STN    YYMMDD/HHMM      "+GenericPointDataConstants.SFC_LAT+"     "+GenericPointDataConstants.SFC_LON+"     ";
			String blankStr = "                            ";
			fileStr.append(titleStr);
			for(int i=0; i<pmSz;i++ ){
				Parameter pm = pmLst.get(i);
				fileStr.append(pm.getAbbreviation());
				pmMap.put(pm.getAbbreviation(), -9999.0f);
				pmCount++;
				if(i== pmSz-1){
					fileStr.append("\n");
					break;
				}		
				if(pmCount>GenericPointDataConstants.SFC_PARM_PER_LINE){
					fileStr.append("\n"+blankStr);
					pmCount=1;
				}
				else
					fileStr.append("     ");
			}
			// add stn/time/parm data
			Date time = pdCon.getRefTime();
			String timeStr = df.format(time);
			for(GenericPointDataStationProduct stnPd: stnLst ){
				for(GenericPointDataParameter pam: stnPd.getLevelLst().get(0).getGpdParameters()){
					//System.out.println("lP ="+pam.getName());
					pmMap.put(pam.getName(), pam.getValue());
				}
				fileStr.append(String.format("%9s  %s    %9.2f%9.2f",stnPd.getLocation().getStationId(),timeStr,stnPd.getSlat(),stnPd.getSlon()));
				pmCount=3;//start from 3 as we added slat ans slon already.
				for(int i=0; i<pmSz;i++ ){
					Parameter pm = pmLst.get(i);
					fileStr.append(String.format("%9.2f",pmMap.get(pm.getAbbreviation())));
					if(i== pmSz-1){
						fileStr.append("\n");
						break;
					}		
					pmCount++;
					if(pmCount>GenericPointDataConstants.SFC_PARM_PER_LINE){
						fileStr.append(String.format("\n%26s"," "));
						pmCount=1;
					}
					
				}
			}
		}
		else {
			//sounding data - multiple levels
			// Add SNDPARM hdr
			fileStr.append(" "+GenericPointDataConstants.SND_PARM +" = ");
			int pmCount=1; 
			for(int i=0; i<pmSz;i++ ){
				Parameter pm = pmLst.get(i);
				fileStr.append(pm.getAbbreviation());
				pmCount++;
				if(i== pmSz-1){
					fileStr.append("\n\n");
					break;
				}		
				fileStr.append(";");
				if(pmCount>GenericPointDataConstants.SND_HDR_PARM_PER_LINE){
					fileStr.append("\n       ");
					pmCount=1;
				}
			}
			//TBD:: Add STNPARM hdr
			
			// Note: List<GenericPointDataParameter> in GenericPointDataLevel is not in sync with pmLst			
			// use this map to save stn parameters to  be in sync with product parameter list,
			// so, we can list all parameters value in sync with parameter name listed.
			// All stations have same parameters name to be listed first, therefore constructed it here once for used by
			// all stations.
			HashMap<String, Float>pmMap = new HashMap<String, Float>(pmSz);
			//create parm header
			pmCount=1;
			String blankStr = " ";
			StringBuilder parmHdrStr = new StringBuilder(blankStr);
			for(int i=0; i<pmSz;i++ ){
				Parameter pm = pmLst.get(i);
				parmHdrStr.append(String.format("%9s",pm.getAbbreviation()));
				pmMap.put(pm.getAbbreviation(), -9999.0f);
				pmCount++;
				if(i== pmSz-1){
					parmHdrStr.append("\n\n ");
					break;
				}		
				if(pmCount>GenericPointDataConstants.SND_PARM_PER_LINE){
					parmHdrStr.append("\n ");
					pmCount=1;
				}
			}
			
			// add stn/time/parm data
			Date time = pdCon.getRefTime();
			String timeStr = df.format(time);
			for(GenericPointDataStationProduct stnPd: stnLst ){
				//add station meta info, e.g. STID = FFC           STNM =    72215   TIME = 130731/1200
				//Note: STNM is not saved now
				fileStr.append(String.format(" %s = %4s      %s = %11s%s =%s\n",GenericPointDataConstants.SND_STN_ID,stnPd.getLocation().getStationId(),
						GenericPointDataConstants.SND_STN_NUM,"",GenericPointDataConstants.SND_REFTIME,timeStr));
				
				// add station meta info, e.g.SLAT =  33.36     SLON =   -84.57   SELV =   245.0
				fileStr.append(String.format(" %s = %6.2f    %s = %6.2f     %s = %6.2f\n", GenericPointDataConstants.SND_STN_LAT,stnPd.getSlat(),GenericPointDataConstants.SND_STN_LON,
						stnPd.getSlon(),GenericPointDataConstants.SND_STN_ELEVATION,stnPd.getLocation().getElevation()));
				//add STIM line, Note we do not save its value now..
				fileStr.append(" "+GenericPointDataConstants.SND_STN_IM+ " = \n\n");
								
				//TBD::: add STNPARM value 
				
				// add SNDPARM hdr
				fileStr.append(parmHdrStr);
				
				// put SNDPARM to pmMap, so we can easily to retrieve and list it later in same order as pmLst
				List<GenericPointDataLevel> levelLst = stnPd.getLevelLst();
				for(GenericPointDataLevel level: levelLst){
					for(GenericPointDataParameter pam: level.getGpdParameters()){
						//System.out.println("lP ="+pam.getName());
						pmMap.put(pam.getName(), pam.getValue());
					}
					// add SNDPARM values
					pmCount=1;
					for(int i=0; i<pmSz;i++ ){
						Parameter pm = pmLst.get(i);
						fileStr.append(String.format("%9.2f",pmMap.get(pm.getAbbreviation())));
						if(i== pmSz-1){
							fileStr.append("\n\n");
							break;
						}		
						pmCount++;
						if(pmCount>GenericPointDataConstants.SND_PARM_PER_LINE){
							fileStr.append("\n ");
							pmCount=1;
						}

					}

				}
				fileStr.append("\n");
			}
		}
		return fileStr.toString();
	}
	private Object createXmlFileString(Class<?> targetClass, Object targetInstance) {
		try {
			JAXBContext ctx;
			ctx = JAXBContext.newInstance(targetClass);
			if (ctx != null ) {
				//System.out.println("XML marshalling.................");
				// This block to to generate XML
				Marshaller mar = ctx.createMarshaller();
				if (mar != null) {
					mar.setProperty(Marshaller.JAXB_FORMATTED_OUTPUT, Boolean.TRUE);
					ByteArrayOutputStream os = new ByteArrayOutputStream();
					mar.marshal(targetInstance, os);//System.out);
					return (Object)(os.toString());
				}
			}
		} catch (JAXBException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();

		}
		return null;
	}

	@Override
	public Object handleRequest(GenericPointDataReqMsg request)
			throws Exception {
		if(gpdDao== null)
			return null;
		GenericPointDataReqType msgType = request.getReqType();
		String prodName= request.getProductName();;
		Date refTime;
		float slon ;
		float slat;
		GenericPointDataProductContainer stnProd;
		switch(msgType) {
		case GET_GPD_PRODUCT_INFO_GEMPAK_TBL:
		case GET_GPD_PRODUCT_INFO_OBJECT:
		case GET_GPD_PRODUCT_INFO_XML:
			if(prodName !=null){
				GenericPointDataProductInfo prodInfo = gpdDao.getGpdProdInfo(prodName);
				//*System.out.println(" report name= "+prodInfo.getName()+ " master="+prodInfo.getMasterLevel().getName()+ " maxLevel="+prodInfo.getMaxNumberOfLevel());
				//for(Parameter pam: prodInfo.getParameterLst() ){
				//	System.out.println("parm ="+pam.getAbbreviation());
				//}
				if(prodInfo!=null)
					if(msgType.equals(GenericPointDataReqType.GET_GPD_PRODUCT_INFO_OBJECT))
						return  (Object)prodInfo;
					else if(msgType.equals(GenericPointDataReqType.GET_GPD_PRODUCT_INFO_GEMPAK_TBL))
						return createGempakFileString(prodInfo);
					else
						return createXmlFileString(GenericPointDataProductInfo.class,(Object)prodInfo);
			}
			break;
		case GET_GPD_STATION_PRODUCT_GEMPAK_TBL:
		case GET_GPD_STATION_PRODUCT_OBJECT:
		case GET_GPD_STATION_PRODUCT_XML:
			String stnId = request.getStnId();
			refTime = request.getRefTime();
			stnProd=gpdDao.getGpdProduct(refTime,GenericPointDataQueryKey.BY_STN_ID, stnId,-9999,-9999,prodName,request.isQuerySpecifiedProductVersion(), request.getProductVersion());
			//System.out.println("stnProduct id="+ stnProd.getLocation().getStationId());
			if(stnProd!=null)
				if(msgType.equals(GenericPointDataReqType.GET_GPD_STATION_PRODUCT_OBJECT)){
					//System.out.println("GET_GPD_STATION_PRODUCT.................");
					return (Object)stnProd;
				}
				else if(msgType.equals(GenericPointDataReqType.GET_GPD_STATION_PRODUCT_GEMPAK_TBL)){
					return createGempakFileString(stnProd);
				}
				else{
					return createXmlFileString(GenericPointDataProductContainer.class,(Object)stnProd);
				}
			break;
		case GET_GPD_MOVING_PRODUCT_GEMPAK_TBL:
		case GET_GPD_MOVING_PRODUCT_OBJECT:
		case GET_GPD_MOVING_PRODUCT_XML:
			slon = request.getSlon();
			slat = request.getSlat();
			refTime = request.getRefTime();
			stnProd=gpdDao.getGpdProduct(refTime,GenericPointDataQueryKey.BY_SLAT_SLON,null, slat,slon,prodName,request.isQuerySpecifiedProductVersion(), request.getProductVersion());
			//System.out.println("stnProduct id="+ stnProd.getLocation().getStationId());
			if(stnProd!=null)
				if(msgType.equals(GenericPointDataReqType.GET_GPD_MOVING_PRODUCT_OBJECT)){
					//System.out.println("GET_GPD_MOVING_PRODUCT.................");
					return (Object)stnProd;
				}
				else if(msgType.equals(GenericPointDataReqType.GET_GPD_MOVING_PRODUCT_GEMPAK_TBL)){
					return createGempakFileString(stnProd);
				}
				else{
					return createXmlFileString(GenericPointDataProductContainer.class,(Object)stnProd);				
				}
			break;
		case GET_GPD_PRODUCT_GEMPAK_TBL:
		case GET_GPD_PRODUCT_OBJECT:
		case GET_GPD_PRODUCT_XML:
			refTime = request.getRefTime();
			GenericPointDataProductContainer pdCon=gpdDao.getGpdProduct(refTime,GenericPointDataQueryKey.BY_PRODUCT_NAME,null, -9999,-9999,prodName,request.isQuerySpecifiedProductVersion(), request.getProductVersion());//(refTime, reportName,request.isQuerySpecifiedProductVersion(), request.getProductVersion());
			//System.out.println("stnProduct id="+ stnProd.getLocation().getStationId());
			if(pdCon!=null)
				if(msgType.equals(GenericPointDataReqType.GET_GPD_PRODUCT_OBJECT)){
					//System.out.println("GET_GPD_PRODUCT.................");
					return (Object)pdCon;
				}
				else if(msgType.equals(GenericPointDataReqType.GET_GPD_PRODUCT_GEMPAK_TBL)){
					return createGempakFileString(pdCon);
				}
				else{
					return createXmlFileString(GenericPointDataProductContainer.class,(Object)pdCon);
				}
			break;
		case GET_GPD_STATION_PRODUCT_OBJECT_LIST:
			stnId = request.getStnId();
			return (Object)gpdDao.getGpdStationProduct(request.getQueryTimeList(),GenericPointDataQueryKey.BY_STN_ID, stnId,-9999,-9999,prodName);
		case GET_GPD_STATION_MDL_SND_PRODUCT_OBJECT_LIST:
			stnId = request.getStnId();
			//System.out.println("GET_GPD_STATION_MDL_SND_PRODUCT_OBJECT_LIST stnProduct id="+ stnId);
			return (Object)gpdDao.getGpdStationModelSndProduct(request.getQueryTimeList(),request.getRefTime(),GenericPointDataQueryKey.BY_STN_ID, stnId,-9999,-9999,prodName);
			//System.out.println("stnProduct id="+ stnProd.getLocation().getStationId());
		case GET_GPD_MOVING_PRODUCT_OBJECT_LIST:
			slon = request.getSlon();
			slat = request.getSlat();
			return (Object)gpdDao.getGpdStationProduct(request.getQueryTimeList(),GenericPointDataQueryKey.BY_SLAT_SLON, null,slat,slon,prodName);
		case GET_GPD_MOVING_MDL_SND_PRODUCT_OBJECT_LIST:
			slon = request.getSlon();
			slat = request.getSlat();
			return (Object)gpdDao.getGpdStationModelSndProduct(request.getQueryTimeList(),request.getRefTime(),GenericPointDataQueryKey.BY_SLAT_SLON, null,slat,slon,prodName);
		case STORE_GPD_PRODUCT_FROM_XML:
			return (decoder.decodeXmlProdFmCli(request.getGpdDataString()));
		case STORE_GPD_PRODUCT_FROM_GEMPAK_TBL:
		case STORE_GPD_OBS_SFC_PRODUCT_FROM_GEMPAK_TBL:
		case STORE_GPD_OBS_SND_PRODUCT_FROM_GEMPAK_TBL:
		case STORE_GPD_MDL_SND_PRODUCT_FROM_GEMPAK_TBL:
			return (decoder.decodeGempakTblProdFmCli(request.getGpdDataString(), prodName, request.getProductVersion(), request.getMaxNumLevel(), msgType));
		case PURGE_GPD_EXPIRED_PRODUCT:
			gpdDao.purgeExpiredData();	
			break;
		case PURGE_GPD_ALL_PRODUCTS:
			gpdDao.purgeAllData();
			break;
		case GET_GPD_PRODUCT_TIMELINE_OBJECT:
			return (Object)(gpdDao.getGpdProductTimeline(prodName));
		case GET_GPD_PRODUCT_RANGESTART_TIME_OBJECT:
			return (Object)(gpdDao.getGpdProductRangestartTimes(prodName,request.getRefTimeStr()));
		case GET_GPD_STATION_INFO_COLLECTION_OBJECT:
			return (Object)(gpdDao.getGpdStationInfoCollection(request.getRefTimeStr(), request.getRangeStartTimeStr(),prodName));
		case GET_GPD_ALL_AVAILABLE_PRODUCTS:
		case GET_GPD_AVAILABLE_OBSERVED_SOUNDING_PRODUCTS:
		case GET_GPD_AVAILABLE_MODEL_SOUNDING_PRODUCTS:
		case GET_GPD_AVAILABLE_SURFACE_PRODUCTS:
			return (Object)  (gpdDao.getGpdAvailProducts(msgType));
		default:
			break;
		}
		return null;
	}

}
