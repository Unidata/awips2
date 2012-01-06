/**
 * H5UairToRecord
 * 
 * This java class provides a transform from point data to 
 * H5UairRecord.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    	Description
 * ------------ ---------- ----------- 	-----------------------
 * 4/2011				   T. Lee		Created
 * 6/2/2011                Chin Chen    Fixed bugs and reorganize code  
 * 
 * </pre>
 * 
 * This code has been developed by the SIB for use in the AWIPS2 system. 
 * @author T. Lee
 * @version 1.0
 */

package gov.noaa.nws.ncep.common.dataplugin.h5uair.dao;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.raytheon.uf.common.pointdata.PointDataContainer;
import com.raytheon.uf.common.pointdata.PointDataView;
import com.raytheon.uf.edex.decodertools.time.TimeTools;
import gov.noaa.nws.ncep.common.dataplugin.h5uair.H5LiftedIndex;
import gov.noaa.nws.ncep.common.dataplugin.h5uair.H5MaxWind;
import gov.noaa.nws.ncep.common.dataplugin.h5uair.H5ObsLevels;
import gov.noaa.nws.ncep.common.dataplugin.h5uair.H5Tropopause;
import gov.noaa.nws.ncep.common.dataplugin.h5uair.H5UairRecord;

public class H5UairToRecord {

    /** The logger */
    private static Log logger = LogFactory.getLog(H5UairToRecord.class);
    public static final String HDR_PARAMS_LIST;
    static {
        StringBuffer sb = new StringBuffer();
        sb.append("DATAURI,");
        sb.append("WMOHEADER,");
        sb.append("STNUM,");
        sb.append("STID,");
        sb.append("SLAT,");
        sb.append("SLON,");
        sb.append("SELV,");
        sb.append("ISSUETIME,");
        sb.append("OBSTIME,");
        sb.append("SYNOPTIME,");
        sb.append("REPORTTYPE,");       
        sb.append("DATATYPE,");
        sb.append("CORR,");  
        sb.append("NIL,");
        sb.append("UTC");
        HDR_PARAMS_LIST = sb.toString();
    }
    
    public static final String MAN_PARAMS_LIST;
    static {
        StringBuffer sb = new StringBuffer();
        sb.append(HDR_PARAMS_LIST);
        sb.append(",numTTAA,");
        sb.append("TTAA_PRES,");
        sb.append("TTAA_TEMP,");
        sb.append("TTAA_DWPT,");
        sb.append("TTAA_DRCT,");
        sb.append("TTAA_SPED,");
        sb.append("TTAA_HGHT,");
        //-------------------------
        sb.append("numTTBB,");
        sb.append("TTBB_PRES,");
        sb.append("TTBB_TEMP,");
        sb.append("TTBB_DWPT,");
        //--------------------------
        sb.append("numTTCC,");
        sb.append("TTCC_PRES,");
        sb.append("TTCC_TEMP,");
        sb.append("TTCC_DWPT,");
        sb.append("TTCC_DRCT,");
        sb.append("TTCC_SPED,");
        sb.append("TTCC_HGHT,");
        //-------------------------
        sb.append("numTTDD,");
        sb.append("TTDD_PRES,");
        sb.append("TTDD_TEMP,");
        sb.append("TTDD_DWPT,");
        //-------------------------
        sb.append("numPPAA,");
        sb.append("PPAA_PRES,");
        sb.append("PPAA_TEMP,");
        sb.append("PPAA_DWPT,");
        sb.append("PPAA_DRCT,");
        sb.append("PPAA_SPED,");
        sb.append("PPAA_HGHT,");
        //-------------------------
        sb.append("numPPBB,");
        sb.append("PPBB_DRCT,");
        sb.append("PPBB_SPED,");
        sb.append("PPBB_HGHT,");
        //-------------------------
        sb.append("numPPCC,");
        sb.append("PPCC_PRES,");
        sb.append("PPCC_TEMP,");
        sb.append("PPCC_DWPT,");
        sb.append("PPCC_DRCT,");
        sb.append("PPCC_SPED,");
        sb.append("PPCC_HGHT,");
        //-------------------------
        sb.append("numPPDD,");
        sb.append("PPDD_DRCT,");
        sb.append("PPDD_SPED,");
        sb.append("PPDD_HGHT,");
        //-------------------------
        sb.append("numTrop,");
        sb.append("TROP_PRES,");
        sb.append("TROP_TEMP,");
        sb.append("TROP_DWPT,");
        sb.append("TROP_DRCT,");
        sb.append("TROP_SPED,");
        //-------------------------
        sb.append("numWmax,");
        sb.append("WMAX_PRES,");
        sb.append("WMAX_DRCT,");
        sb.append("WMAX_SPED,");
        sb.append("WMAX_LO_SHEAR,");
        sb.append("WMAX_HI_SHEAR,");
        //-------------------------
        sb.append("numMiscTTAA,");
        sb.append("TTAA_LIFT,");
        sb.append("TTAA_LO_MEAN_DRCT,");
        sb.append("TTAA_LO_MEAN_SPED,");
        sb.append("TTAA_HI_MEAN_DRCT,");
        sb.append("TTAA_HI_MEAN_SPED,");
        //-------------------------         
        sb.append("numMiscTTBB,");
        sb.append("TTBB_LIFT,");
        sb.append("TTBB_LO_MEAN_DRCT,");
        sb.append("TTBB_LO_MEAN_SPED,");
        sb.append("TTBB_HI_MEAN_DRCT,");
        sb.append("TTBB_HI_MEAN_SPED");
        //-------------------------
        MAN_PARAMS_LIST = sb.toString();
    }
    //static int count=0;
    private static H5UairRecord getH5UairRecord(PointDataView pdv) {
        H5UairRecord record = null;
        //count++;
        if (pdv != null) {
        	Set<String> parameters = pdv.getContainer().getParameters();
        	//for(String str: parameters){
        	//	System.out.println("record "+ count+" H5 uair parameter:" + str);
        	//}
            if (parameters.contains("DATAURI")) {
            	String uri = pdv.getString("DATAURI");
            	logger.debug("URI = " + uri);
            	record = new H5UairRecord(uri);
            }
            else{
            	//System.out.println("no data uri");
            	record = new H5UairRecord();
            }
            	
            if (parameters.contains("UTC")) {
            	record.setUTC(pdv.getInt("UTC"));
            }
            if (parameters.contains("OBSTIME")) {
            	long vt = pdv.getNumber("OBSTIME").longValue();
            	record.setObsTime(TimeTools.newCalendar(vt));
            }
            if (parameters.contains("ISSUETIME")) {
            	long vt = pdv.getNumber("ISSUETIME").longValue();
            	record.setIssueTime(TimeTools.newCalendar(vt));      
            }
            if (parameters.contains("SYNOPTIME")) {
            	long vt = pdv.getNumber("SYNOPTIME").longValue();
            	record.setSynopticTime(TimeTools.newCalendar(vt));
            }
            if (parameters.contains("WMOHEADER")) 
            	record.setWmoHeader(pdv.getString("WMOHEADER"));
            if (parameters.contains("REPORTTYPE")) 
            	record.setReportType(pdv.getString("REPORTTYPE"));
            if (parameters.contains("DATATYPE")) 
            	record.setDataType(pdv.getString("DATATYPE"));
            if (parameters.contains("CORR")) 
            	record.setCorr(pdv.getString("CORR"));
            if (parameters.contains("STID")) 
            	record.setStid(pdv.getString("STID"));
            if (parameters.contains("STNUM")) 
            	record.setStnum(pdv.getString("STNUM"));
            if (parameters.contains("SELV")) {
            	float elev = pdv.getNumber("SELV").floatValue();
            	record.setSelv(elev);
            }
            if (parameters.contains("SLAT")) {
            	float slat = pdv.getNumber("SLAT").floatValue();
            	record.setSlat(slat);
            }
            if (parameters.contains("SLON")) {
            	float slon = pdv.getNumber("SLON").floatValue();
            	record.setSlon(slon);   
            }
            
        }
        return record;
    }
    private static H5UairRecord getTTAA(PointDataView pdv, H5UairRecord record) {
    	H5ObsLevels obslevels;

    	/*
    	 * Add TTAA data
    	 */
    	if(record != null) {
    		Set<String> parameters = pdv.getContainer().getParameters();
    		Number[] pres, hght, temp, dwpt, drct,sped;
    		if (parameters.contains("numTTAA")) {
    			Number numTTAA = pdv.getInt("numTTAA");
    			if (numTTAA != null) {
    				//System.out.println("numTTAA="+numTTAA);
    				pres = pdv.getNumberAllLevels("TTAA_PRES");
    				hght = pdv.getNumberAllLevels("TTAA_HGHT");
    				temp = pdv.getNumberAllLevels("TTAA_TEMP");
    				dwpt = pdv.getNumberAllLevels("TTAA_DWPT");
    				drct = pdv.getNumberAllLevels("TTAA_DRCT");
    				sped = pdv.getNumberAllLevels("TTAA_SPED");

    				for (int i = 0; i < numTTAA.intValue(); i++) {
    					obslevels = new H5ObsLevels();
    					obslevels.setPres(pres[i].floatValue());
    					obslevels.setHght(hght[i].floatValue());
    					obslevels.setTemp(temp[i].floatValue());
    					obslevels.setDwpt(dwpt[i].floatValue());
    					obslevels.setDrct(drct[i].floatValue());
    					obslevels.setSped(sped[i].floatValue());
    					record.addObsLevels(obslevels);
    				}
    			}
    		}
    			
    			/*
    			 * Add lifted index
    			 */ 
    		if (parameters.contains("numMiscTTAA")) {
    			Number numMiscTTAA = pdv.getNumber("numMiscTTAA");
    			if (numMiscTTAA != null) {
    				Number[] ttaa_lift, ttaa_lo_drct, ttaa_lo_sped, ttaa_hi_drct, ttaa_hi_sped;
    				ttaa_lift = pdv.getNumberAllLevels("TTAA_LIFT");
    				ttaa_lo_drct = pdv.getNumberAllLevels("TTAA_LO_MEAN_DRCT");
    				ttaa_lo_sped = pdv.getNumberAllLevels("TTAA_LO_MEAN_SPED");
    				ttaa_hi_drct = pdv.getNumberAllLevels("TTAA_HI_MEAN_DRCT");
    				ttaa_hi_sped = pdv.getNumberAllLevels("TTAA_HI_MEAN_SPED");
    				for (int i = 0; i < numMiscTTAA.intValue(); i++) {
    					H5LiftedIndex liftedindex = new H5LiftedIndex();
    					liftedindex.setLiTemp(ttaa_lift[i].floatValue());
    					liftedindex.setLoDrct(ttaa_lo_drct[i].floatValue());
    					liftedindex.setLoSped(ttaa_lo_sped[i].floatValue());
    					liftedindex.setHiDrct(ttaa_hi_drct[i].floatValue());
    					liftedindex.setHiSped(ttaa_hi_sped[i].floatValue());
    					record.addLiftedIndex(liftedindex);
    				}
    			}
    		} 
 
            /*
             * Add tropopause data
             */ 
    		if (parameters.contains("numTrop")) {
    			Number numTrop = pdv.getInt("numTrop");
    			if (numTrop != null) {
    				//System.out.println("numTropA="+numTrop);
    				pres = pdv.getNumberAllLevels("TROP_PRES");
    				temp = pdv.getNumberAllLevels("TROP_TEMP");
    				dwpt = pdv.getNumberAllLevels("TROP_DWPT");
    				drct = pdv.getNumberAllLevels("TROP_DRCT");
    				sped = pdv.getNumberAllLevels("TROP_SPED");
    				for (int i = 0; i < numTrop.intValue(); i++) {
    					H5Tropopause trop = new H5Tropopause();
    					trop.setPres(pres[i].floatValue());
    					trop.setTemp(temp[i].floatValue());
    					trop.setDwpt(dwpt[i].floatValue());
    					trop.setDrct(drct[i].floatValue());
    					trop.setSped(sped[i].floatValue());
    					record.addTropopause(trop);
    				}
    			}
    		}

            /*
             * Add max wind data
             */ 
    		if (parameters.contains("numWmax")) {
    			Number numWmax = pdv.getInt("numWmax");
    			if (numWmax != null) {
    				//System.out.println("numWmaxA="+numWmax);
    				Number[] lo_shear = pdv.getNumberAllLevels("WMAX_LO_SHEAR");
    				Number[] hi_shear = pdv.getNumberAllLevels("WMAX_HI_SHEAR");
    				pres = pdv.getNumberAllLevels("WMAX_PRES");
    				drct = pdv.getNumberAllLevels("WMAX_DRCT");
    				sped = pdv.getNumberAllLevels("WMAX_SPED");

    				for (int i = 0; i < numWmax.intValue(); i++) {
    					H5MaxWind wmax = new H5MaxWind();
    					wmax.setPres(pres[i].floatValue());
    					wmax.setDrct(drct[i].floatValue());
    					wmax.setSped(sped[i].floatValue());
    					wmax.setHiShear(hi_shear[i].floatValue());
    					wmax.setLoShear(lo_shear[i].floatValue());
    					record.addMaxWind(wmax);
    				}
    			}
    		}
        }
        return record;
    }

    private static H5UairRecord getTTCC(PointDataView pdv, H5UairRecord record) {
    	if(record != null) {
    		Set<String> parameters = pdv.getContainer().getParameters();
    		Number[] pres, hght, temp, dwpt, drct,sped;
    		if (parameters.contains("numTTCC")) {
    			Number numTTCC = pdv.getInt("numTTCC");
    			if (numTTCC != null) {
    				//System.out.println("numTTCC="+numTTCC);
    				H5ObsLevels obslevels;
    		    	pres = pdv.getNumberAllLevels("TTCC_PRES");
    				hght = pdv.getNumberAllLevels("TTCC_HGHT");
    				temp = pdv.getNumberAllLevels("TTCC_TEMP");
    				dwpt = pdv.getNumberAllLevels("TTCC_DWPT");
    				drct = pdv.getNumberAllLevels("TTCC_DRCT");
    				sped = pdv.getNumberAllLevels("TTCC_SPED");
    				for (int i = 0; i < numTTCC.intValue(); i++) {
    					obslevels = new H5ObsLevels();
    					obslevels.setPres(pres[i].floatValue());
    					obslevels.setHght(hght[i].floatValue());
    					obslevels.setTemp(temp[i].floatValue());
    					obslevels.setDwpt(dwpt[i].floatValue());
    					obslevels.setDrct(drct[i].floatValue());
    					obslevels.setSped(sped[i].floatValue());
    					record.addObsLevels(obslevels);
    				}
    			}
    		}
    		 
            /*
             * Add tropopause data
             */ 
    		if (parameters.contains("numTrop")) {
    			Number numTrop = pdv.getInt("numTrop");
    			if (numTrop != null) {
    				//System.out.println("numTropC="+numTrop);
    				pres = pdv.getNumberAllLevels("TROP_PRES");
    				temp = pdv.getNumberAllLevels("TROP_TEMP");
    				dwpt = pdv.getNumberAllLevels("TROP_DWPT");
    				drct = pdv.getNumberAllLevels("TROP_DRCT");
    				sped = pdv.getNumberAllLevels("TROP_SPED");
    				for (int i = 0; i < numTrop.intValue(); i++) {
    					H5Tropopause trop = new H5Tropopause();
    					trop.setPres(pres[i].floatValue());
    					trop.setTemp(temp[i].floatValue());
    					trop.setDwpt(dwpt[i].floatValue());
    					trop.setDrct(drct[i].floatValue());
    					trop.setSped(sped[i].floatValue());
    					record.addTropopause(trop);
    				}
    			}
    		}

            /*
             * Add max wind data
             */ 
    		if (parameters.contains("numWmax")) {
    			Number numWmax = pdv.getInt("numWmax");
    			if (numWmax != null) {
    				//System.out.println("numWmaxC="+numWmax);
    				Number[] lo_shear = pdv.getNumberAllLevels("WMAX_LO_SHEAR");
    				Number[] hi_shear = pdv.getNumberAllLevels("WMAX_HI_SHEAR");
    				pres = pdv.getNumberAllLevels("WMAX_PRES");
    				drct = pdv.getNumberAllLevels("WMAX_DRCT");
    				sped = pdv.getNumberAllLevels("WMAX_SPED");

    				for (int i = 0; i < numWmax.intValue(); i++) {
    					H5MaxWind wmax = new H5MaxWind();
    					wmax.setPres(pres[i].floatValue());
    					wmax.setDrct(drct[i].floatValue());
    					wmax.setSped(sped[i].floatValue());
    					wmax.setHiShear(hi_shear[i].floatValue());
    					wmax.setLoShear(lo_shear[i].floatValue());
    					record.addMaxWind(wmax);
    				}
    			}
    		}
    	}
        return record;
    }
    
    /**
     * @param pdv
     * @param record
     * @return
     */
    private static H5UairRecord getPPBB(PointDataView pdv, H5UairRecord record) {
    	if (record != null) {
    		Set<String> parameters = pdv.getContainer().getParameters();
    		if (parameters.contains("numPPBB")) {
    			Number numPPBB = pdv.getInt("numPPBB");
    			if (numPPBB != null){
    				//System.out.println("numPPBB="+numPPBB);
    				Number[] drct, hght, sped;
    				if (record.getDataType().equals("PPBB")) {
    					drct = pdv.getNumberAllLevels("PPBB_DRCT");
    					hght = pdv.getNumberAllLevels("PPBB_HGHT");
    					sped = pdv.getNumberAllLevels("PPBB_SPED");
   					for (int i = 0; i < numPPBB.intValue(); i++) {
    						H5ObsLevels obslevels = new H5ObsLevels();
    						obslevels.setDrct(drct[i].floatValue());
    						obslevels.setHght(hght[i].floatValue());
    						obslevels.setSped(sped[i].floatValue());
    						record.addObsLevels(obslevels);
    					}
    				}
    			} 
    		}
    	}
        return record;
    }
    /**
     * @param pdv
     * @param record
     * @return
     */
    private static H5UairRecord getPPDD(PointDataView pdv, H5UairRecord record) {
    	if (record != null) {
    		Set<String> parameters = pdv.getContainer().getParameters();
    		if (parameters.contains("numPPDD")) {
    			Number numPPDD = pdv.getInt("numPPDD");
    			if (numPPDD != null){
    				Number[] drct, hght, sped;
    				if (record.getDataType().equals("PPDD")) {
    					drct = pdv.getNumberAllLevels("PPDD_DRCT");
    					hght = pdv.getNumberAllLevels("PPDD_HGHT");
    					sped = pdv.getNumberAllLevels("PPDD_SPED");
    					for (int i = 0; i < numPPDD.intValue(); i++) {
    						H5ObsLevels obslevels = new H5ObsLevels();
    						obslevels.setDrct(drct[i].floatValue());
    						obslevels.setHght(hght[i].floatValue());
    						obslevels.setSped(sped[i].floatValue());
    						record.addObsLevels(obslevels);
    					}
    				}
    			}
    		}
    	}
        return record;
    }

    /**
     * @param pdv
     * @param record
     * @return H5UairRecord
     */
    private static H5UairRecord getPPAA(PointDataView pdv, H5UairRecord record) {
    	if (record != null) {
    		Set<String> parameters = pdv.getContainer().getParameters();
    		if (parameters.contains("numPPAA")) {
    			Number numPPAA = pdv.getInt("numPPAA");
    			if (numPPAA != null){
    				//System.out.println("numPPAA="+numPPAA);
    				Number[] pres, temp, dwpt, drct, sped, hght;
    				pres = pdv.getNumberAllLevels("PPAA_PRES");
    				temp = pdv.getNumberAllLevels("PPAA_TEMP");
    				dwpt = pdv.getNumberAllLevels("PPAA_DWPT");
    				drct = pdv.getNumberAllLevels("PPAA_DRCT");
    				sped = pdv.getNumberAllLevels("PPAA_SPED");
    				hght = pdv.getNumberAllLevels("PPAA_HGHT");
    				{
    					for (int i = 0; i < numPPAA.intValue(); i++) {
    						H5ObsLevels obslevels = new H5ObsLevels();
    						obslevels.setPres(pres[i].floatValue());
    						obslevels.setTemp(temp[i].floatValue());
    						obslevels.setDwpt(dwpt[i].floatValue());
    						obslevels.setDrct(drct[i].floatValue());
    						obslevels.setSped(sped[i].floatValue());
    						obslevels.setHght(hght[i].floatValue());
    						record.addObsLevels(obslevels);
    					}
    				}
    			} 
    		}
    	}
    	return record;
    }
    /**
     * @param pdv
     * @param record
     * @return H5UairRecord
     */
    private static H5UairRecord getPPCC(PointDataView pdv, H5UairRecord record) {
    	if (record != null) {
    		Set<String> parameters = pdv.getContainer().getParameters();
    		if (parameters.contains("numPPCC")) {
    			Number numPPCC = pdv.getInt("numPPCC");
    			if (numPPCC != null){
    				//System.out.println("numPPCC="+numPPCC);
    				Number[] pres, temp, dwpt, drct, sped, hght;
    				pres = pdv.getNumberAllLevels("PPCC_PRES");
    				temp = pdv.getNumberAllLevels("PPCC_TEMP");
    				dwpt = pdv.getNumberAllLevels("PPCC_DWPT");
    				drct = pdv.getNumberAllLevels("PPCC_DRCT");
    				sped = pdv.getNumberAllLevels("PPCC_SPED");
    				hght = pdv.getNumberAllLevels("PPCC_HGHT");
    				for (int i = 0; i < numPPCC.intValue(); i++) {
    					H5ObsLevels obslevels = new H5ObsLevels();
    					obslevels.setPres(pres[i].floatValue());
    					obslevels.setTemp(temp[i].floatValue());
    					obslevels.setDwpt(dwpt[i].floatValue());
    					obslevels.setDrct(drct[i].floatValue());
    					obslevels.setSped(sped[i].floatValue());
    					obslevels.setHght(hght[i].floatValue());
    					record.addObsLevels(obslevels);
    				}
    			}
    		}
    	}
    	return record;
    }
    private static H5UairRecord getTTDD(PointDataView pdv, H5UairRecord record) {
    	if (record != null) {
    		Set<String> parameters = pdv.getContainer().getParameters();
    		if (parameters.contains("numTTDD")) {
    			Number numTTDD = pdv.getInt("numTTDD");
    			if (numTTDD != null) {
    				Number[] pres, temp, dwpt;
    				//System.out.println("numTTDD="+numTTDD);
    				pres = pdv.getNumberAllLevels("TTDD_PRES");
    				temp = pdv.getNumberAllLevels("TTDD_TEMP");
    				dwpt = pdv.getNumberAllLevels("TTDD_DWPT");
    				for (int i = 0; i < numTTDD.intValue(); i++) {
    					H5ObsLevels obslevels = new H5ObsLevels();
    					obslevels.setPres(pres[i].floatValue());
    					obslevels.setTemp(temp[i].floatValue());
    					obslevels.setDwpt(dwpt[i].floatValue());
    					record.addObsLevels(obslevels);
    				}
    			}
    		}
    	}
        return record;
    }

    private static H5UairRecord getTTBB(PointDataView pdv, H5UairRecord record) {
        if (record != null) {
        	Set<String> parameters = pdv.getContainer().getParameters();
        	if (parameters.contains("numTTBB")) {

        		Number numTTBB = pdv.getInt("numTTBB");
        		if (numTTBB != null) {
        			//System.out.println("numTTBB="+numTTBB);
        			Number[] pres, temp, dwpt;
        			pres = pdv.getNumberAllLevels("TTBB_PRES");
        			temp = pdv.getNumberAllLevels("TTBB_TEMP");
        			dwpt = pdv.getNumberAllLevels("TTBB_DWPT");


        			for (int i = 0; i < numTTBB.intValue(); i++) {
        				H5ObsLevels obslevels = new H5ObsLevels();
        				obslevels.setPres(pres[i].floatValue());
        				obslevels.setTemp(temp[i].floatValue());
        				obslevels.setDwpt(dwpt[i].floatValue());
        				record.addObsLevels(obslevels);
        			}
        		}
        	}
        	/*
        	 * Add lifted index
        	 */ 
        	if (parameters.contains("numMiscTTBB")) {
        		Number numMiscTTBB = pdv.getInt("numMiscTTBB");
        		if (numMiscTTBB != null) {
        			Number[] ttbb_lift, ttbb_lo_drct, ttbb_lo_sped, ttbb_hi_drct, ttbb_hi_sped;

        			ttbb_lift = pdv.getNumberAllLevels("TTBB_LIFT");
        			ttbb_lo_drct = pdv.getNumberAllLevels("TTBB_LO_MEAN_DRCT");
        			ttbb_lo_sped = pdv.getNumberAllLevels("TTBB_LO_MEAN_SPED");
        			ttbb_hi_drct = pdv.getNumberAllLevels("TTBB_HI_MEAN_DRCT");
        			ttbb_hi_sped = pdv.getNumberAllLevels("TTBB_HI_MEAN_SPED");

        			for (int i = 0; i < numMiscTTBB.intValue(); i++) {
        				H5LiftedIndex liftedindex = new H5LiftedIndex();
        				liftedindex.setLiTemp(ttbb_lift[i].floatValue());
        				liftedindex.setLoDrct(ttbb_lo_drct[i].floatValue());
        				liftedindex.setLoSped(ttbb_lo_sped[i].floatValue());
        				liftedindex.setHiDrct(ttbb_hi_drct[i].floatValue());
        				liftedindex.setHiSped(ttbb_hi_sped[i].floatValue());
        				record.addLiftedIndex(liftedindex);
        			}

        		}
        	}
        }
        return record;
    }
    
    /** 
     * @param container
     * @return
     */

    private enum DATATYPE {TTAA, TTBB, TTCC, TTDD, PPAA, PPBB, PPCC, PPDD};
    public static H5UairRecord [] toH5UairRecords(PointDataContainer container) {
        List<H5UairRecord> records = new ArrayList<H5UairRecord>();
        container.setCurrentSz(container.getAllocatedSz());
        for (int i = 0; i < container.getCurrentSz(); i++) {
        	PointDataView pdv = container.readRandom(i);
        	H5UairRecord record = getH5UairRecord(pdv);
        	if(record != null) {
        		switch(DATATYPE.valueOf(record.getDataType())) {
        		case TTAA:
        			record = getTTAA(pdv,record);
        			break;
        		case TTCC: 
        			record = getTTCC(pdv,record);
        			break;
         		case TTBB:
        			record = getTTBB(pdv,record);
        			break;
        		case TTDD: 
        			record = getTTDD(pdv,record);
        			break;
        		case PPAA:
        			record = getPPAA(pdv,record);
        			break;
        		case PPCC: 
        			record = getPPCC(pdv,record);
        			break;
        		case PPBB:
        			record = getPPBB(pdv,record);
        			break;
        		case PPDD: 
        			record = getPPDD(pdv,record);
        			break;
        		} 
        		records.add(record);
        	}
        }
        return records.toArray(new H5UairRecord[records.size()]);
    }
}