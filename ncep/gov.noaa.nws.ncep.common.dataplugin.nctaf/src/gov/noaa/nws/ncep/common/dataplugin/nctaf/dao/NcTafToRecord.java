/**
 * NcTafToRecord
 * 
 * This java class provides a transform from point data to 
 * NcTafRecord.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    	Description
 * ------------ ---------- ----------- 	-----------------------
 * 09/23/2011   458        S. Gurung    Initial Creation
 * 09/29/2011              S. Gurung    Added REPORTTYPE
 * 10/26/2011              S. Gurung    Added probable parameters (for TEMPO/PROB)
 * 11/03/2011              S. Gurung    Added additional parameters and renamed some parameters
 * 11/07/2011              S. Gurung    Added LOW_LEVEL_WIND_SHEAR
 * Jul 23, 2014 3410       bclement    location changed to floats
 * 
 * </pre>
 * 
 * This code has been developed by the SIB for use in the AWIPS2 system. 
 * @author S. Gurung
 * @version 1.0
 */

package gov.noaa.nws.ncep.common.dataplugin.nctaf.dao;

import gov.noaa.nws.ncep.common.dataplugin.nctaf.NcTafIcingLayer;
import gov.noaa.nws.ncep.common.dataplugin.nctaf.NcTafRecord;
import gov.noaa.nws.ncep.common.dataplugin.nctaf.NcTafSkyCover;
import gov.noaa.nws.ncep.common.dataplugin.nctaf.NcTafTemperatureForecast;
import gov.noaa.nws.ncep.common.dataplugin.nctaf.NcTafTurbulenceLayer;
import gov.noaa.nws.ncep.common.dataplugin.nctaf.NcTafWeatherCondition;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Set;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.raytheon.uf.common.pointdata.PointDataContainer;
import com.raytheon.uf.common.pointdata.PointDataView;
import com.raytheon.uf.common.pointdata.spatial.SurfaceObsLocation;

public class NcTafToRecord {

    /** The logger */
    private static Log logger = LogFactory.getLog(NcTafToRecord.class);
    
    private static final String LONGITUDE = "longitude";

    private static final String LATITUDE = "latitude";

    private static final String ELEVATION = "elevation";
    
    public static final String HDR_PARAMS_LIST;
    
    static {
        StringBuffer sb = new StringBuffer();
        sb.append("DATAURI,");
        sb.append("WMOHEADER,");
        sb.append("TAFTEXT,");
        sb.append("REPORTTYPE,");       
        sb.append("STID,");
        sb.append("CORR,");
        sb.append("AMD,");
        sb.append("ISSUETIME,");
        sb.append("ISSUETIMESTR,");
        sb.append("BULLETINTIME,");
        sb.append("CGRP,");
        sb.append("IND,");
        sb.append("PROB,"); 
        sb.append("MAX_TEMP,"); 
        sb.append("MIN_TEMP,"); 
        sb.append("ALTIM,");
        sb.append("REMARKS,");
        sb.append("SEQID,"); 
        sb.append("END_DATE,");
        sb.append("START_DATE,");
        sb.append("TRANS_END_DATE,");
        sb.append("VISIBILITY,"); 
        sb.append("VERT_VISIBILITY,");
        sb.append("WIND_DIR,");
        sb.append("WIND_GUST,");
        sb.append("WIND_SPEED,"); 
        sb.append("SHEAR_WIND_DIR,"); 
        sb.append("SHEAR_WIND_SPEED,"); 
        sb.append("SHEAR_WIND_HGT,"); 
        sb.append("LOW_LEVEL_WIND_SHEAR,");
        sb.append("CEILING,"); 
        sb.append("PROBABLE_CEILING,"); 
        sb.append("PROBABLE_VISIBILITY,"); 
        sb.append("PROBABLE_VERT_VISIBILITY,");
        sb.append("PROBABLE_WIND_DIR,");
        sb.append("PROBABLE_WIND_GUST,");
        sb.append("PROBABLE_WIND_SPEED,"); 
        sb.append("PROBABLE_SHEAR_WIND_DIR,"); 
        sb.append("PROBABLE_SHEAR_WIND_SPEED,"); 
        sb.append("PROBABLE_SHEAR_WIND_HGT,"); 
        
        HDR_PARAMS_LIST = sb.toString();
    }
    
    public static final String MAN_PARAMS_LIST;
    static {
        StringBuffer sb = new StringBuffer();
        sb.append(HDR_PARAMS_LIST);
        //------------------------- 
        sb.append("numICNG,");
        sb.append("ICNG_INTENSITY,");
        sb.append("ICNG_MAX_ALT,");
        sb.append("ICNG_MIN_ALT,");
        //-------------------------
        sb.append("numSCVR,");
        sb.append("SCVR_HGT,");
        sb.append("SCVR_TYPE,");
        sb.append("SCVR_GENUS,"); 
        //-------------------------
        sb.append("numPROBABLE_SCVR,");
        sb.append("PROBABLE_SCVR_HGT,");
        sb.append("PROBABLE_SCVR_TYPE,");
        sb.append("PROBABLE_SCVR_GENUS,"); 
        //------------------------- 
        sb.append("numTMPFCST,");
        sb.append("TMPFCST_VALID_TIME,");
        sb.append("TMPFCST_TEMP,");        
        //-------------------------
        sb.append("numTURB,");
        sb.append("TURB_INTENSITY,");
        sb.append("TURB_MAX_ALT,");
        sb.append("TURB_MIN_ALT,"); 
        //-------------------------         
        sb.append("numWTHRCOND,");
        sb.append("WTHRCOND_DESCRIPTOR,"); 
        sb.append("WTHRCOND_INTSTY_PRXMTY,");
        sb.append("WTHRCOND_OBSCURATION,");
        sb.append("WTHRCOND_OTHER,");
        sb.append("WTHRCOND_PRECIPITATION");
        sb.append("PRES_WEATHER");
        //-------------------------       
        sb.append("numPROBABLE_WTHRCOND,");
        sb.append("PROBABLE_WTHRCOND_DESCRIPTOR,"); 
        sb.append("PROBABLE_WTHRCOND_INTSTY_PRXMTY,");
        sb.append("PROBABLE_WTHRCOND_OBSCURATION,");
        sb.append("PROBABLE_WTHRCOND_OTHER,");
        sb.append("PROBABLE_WTHRCOND_PRECIPITATION");
        sb.append("PROBABLE_PRES_WEATHER");        
        //-------------------------          
        MAN_PARAMS_LIST = sb.toString();
    }
   
    private static NcTafRecord getNcTafRecord(PointDataView pdv) {
        NcTafRecord record = null;
       
        if (pdv != null) {
        	Set<String> parameters = pdv.getContainer().getParameters();
        	        	
            if (parameters.contains("DATAURI")) {
            	String uri = pdv.getString("DATAURI");
            	logger.debug("URI = " + uri);
            	record = new NcTafRecord(uri);
            }
            else{
            	logger.debug("No DATAURI");
            	record = new NcTafRecord();
            }
            
            if (parameters.contains("WMOHEADER")) 
            	record.setWmoHeader(pdv.getString("WMOHEADER"));
           
            if (parameters.contains("TAFTEXT")) {
            	record.setTafText(pdv.getString("TAFTEXT"));
            }
             
            if (parameters.contains("REPORTTYPE")) 
            	record.setReportType(pdv.getString("REPORTTYPE"));
            
            if (parameters.contains("STID")) 
            	record.setStationId(pdv.getString("STID"));
            
            SurfaceObsLocation loc = new SurfaceObsLocation(pdv.getString("STID"));
            float lat = pdv.getNumber(LATITUDE).floatValue();
            float lon = pdv.getNumber(LONGITUDE).floatValue();
            loc.assignLocation(lat, lon);
            loc.setElevation(pdv.getNumber(ELEVATION).intValue());              
            record.setLocation(loc);
         
            if (parameters.contains("CORR")) 
            	record.setCorIndicator(pdv.getString("CORR"));
            
            if (parameters.contains("AMD")) 
            	record.setAmdIndicator(pdv.getString("AMD"));
            
            if (parameters.contains("ISSUETIME")) {
            	long vt = pdv.getNumber("ISSUETIME").longValue();
            	record.setIssue_time(new Date(vt));      
            }
            
            if (parameters.contains("ISSUETIMESTR")) 
            	record.setIssue_timeString(pdv.getString("ISSUETIMESTR"));
           
            if (parameters.contains("BULLETINTIME")) {
            	long vt = pdv.getNumber("BULLETINTIME").longValue();
            	record.setBulletin_time(new Date(vt));
            }
            
            if (parameters.contains("REMARKS")) 
            	record.setRemarks(pdv.getString("REMARKS"));
            
            if (parameters.contains("IND"))
				record.setChange_indicator(pdv.getString("IND"));
          
            if (parameters.contains("CGRP"))
				record.setChangeGroup(pdv.getString("CGRP"));
            
            if (parameters.contains("PROB"))
				record.setProbability(pdv.getInt("PROB"));
            
            if (parameters.contains("MAX_TEMP"))
				record.setMax_temp_c(pdv.getInt("MAX_TEMP"));
            
            if (parameters.contains("MIN_TEMP"))
				record.setMin_temp_c(pdv.getInt("MIN_TEMP"));
            
            if (parameters.contains("ALTIM"))
				record.setAltim_in_hg(pdv.getFloat("ALTIM"));
            
            if (parameters.contains("REMARKS"))
				record.setRemarks(pdv.getString("REMARKS"));
            
            if (parameters.contains("SEQID"))
				record.setSequenceId(pdv.getInt("SEQID"));
			
            if (parameters.contains("END_DATE")) {
                record.setEndDate(pdv.getCalendar("END_DATE"));
			}
			
			if (parameters.contains("START_DATE")) {
                record.setStartDate(pdv.getCalendar("START_DATE"));
			}
			
			if (parameters.contains("TRANS_END_DATE")) {
                record.setTransitionEndDate(pdv.getCalendar("TRANS_END_DATE"));
			}
		  
            if (parameters.contains("VISIBILITY"))
				record.setVisibility_mi(pdv.getFloat("VISIBILITY"));
            
            if (parameters.contains("VERT_VISIBILITY"))
				record.setVert_vis_ft(pdv.getFloat("VERT_VISIBILITY"));
            
            if (parameters.contains("WIND_DIR"))
				record.setWind_dir_degrees(pdv.getFloat("WIND_DIR"));
            
            if (parameters.contains("WIND_GUST"))
				record.setWind_gust_kt(pdv.getFloat("WIND_GUST"));
            
            if (parameters.contains("WIND_SPEED"))
				record.setWind_speed_kt(pdv.getFloat("WIND_SPEED"));
            
            if (parameters.contains("SHEAR_WIND_DIR"))
				record.setWind_shear_dir_degrees(pdv.getFloat("SHEAR_WIND_DIR"));
            
            if (parameters.contains("SHEAR_WIND_SPEED"))
				record.setWind_shear_speed_kt(pdv.getFloat("SHEAR_WIND_SPEED"));
            
            if (parameters.contains("SHEAR_WIND_HGT"))
            	record.setWind_shear_hgt_ft_agl(pdv.getFloat("SHEAR_WIND_HGT"));
			                      
            if (parameters.contains("PROBABLE_VISIBILITY"))
				record.setProbable_visibility_mi(pdv.getFloat("PROBABLE_VISIBILITY"));
            
            if (parameters.contains("PROBABLE_VERT_VISIBILITY"))
				record.setProbable_vert_vis_ft(pdv.getFloat("PROBABLE_VERT_VISIBILITY"));
            
            if (parameters.contains("PROBABLE_WIND_DIR"))
				record.setProbable_wind_dir_degrees(pdv.getFloat("PROBABLE_WIND_DIR"));
            
            if (parameters.contains("PROBABLE_WIND_GUST"))
				record.setProbable_wind_gust_kt(pdv.getFloat("PROBABLE_WIND_GUST"));
            
            if (parameters.contains("PROBABLE_WIND_SPEED"))
				record.setProbable_wind_speed_kt(pdv.getFloat("PROBABLE_WIND_SPEED"));
            
            if (parameters.contains("PROBABLE_SHEAR_WIND_DIR"))
				record.setProbable_wind_shear_dir_degrees(pdv.getFloat("PROBABLE_SHEAR_WIND_DIR"));
            
            if (parameters.contains("PROBABLE_SHEAR_WIND_SPEED"))
				record.setProbable_wind_shear_speed_kt(pdv.getFloat("PROBABLE_SHEAR_WIND_SPEED"));
            
            if (parameters.contains("PROBABLE_SHEAR_WIND_HGT"))
            	record.setProbable_wind_shear_hgt_ft_agl(pdv.getFloat("PROBABLE_SHEAR_WIND_HGT"));
			    
        }
        return record;
    }
    
    private static NcTafRecord getIcingLayerData(PointDataView pdv, NcTafRecord record) {
    	if (record != null) {
    		Set<String> parameters = pdv.getContainer().getParameters();
    		if (parameters.contains("numICNG")) {
    			Number numICNG = pdv.getInt("numICNG");
    			if (numICNG != null) {
    				Number[] intensity, maxAlt, minAlt;
    				intensity = pdv.getNumberAllLevels("ICNG_INTENSITY");
    				maxAlt = pdv.getNumberAllLevels("ICNG_MAX_ALT");
    				minAlt = pdv.getNumberAllLevels("ICNG_MIN_ALT");
    				for (int i = 0; i < numICNG.intValue(); i++) {
    					NcTafIcingLayer icngLyr = new NcTafIcingLayer();
    					icngLyr.setIcing_intensity(intensity[i].intValue());
    					icngLyr.setIcing_max_alt_ft_agl(maxAlt[i].intValue());
    					icngLyr.setIcing_min_alt_ft_agl(minAlt[i].intValue());
    					record.addIcing_layer(icngLyr);
    				}
    			}
    		}
    	}
        return record;
    }
   
    private static NcTafRecord getWeatherConditionData(PointDataView pdv, NcTafRecord record) {
    	if (record != null) {
    		Set<String> parameters = pdv.getContainer().getParameters();
    		if (parameters.contains("numWTHRCOND")) {
    			Number numWTHRCOND = pdv.getInt("numWTHRCOND");
    			if (numWTHRCOND != null) {
    				Number[] desc, intPrxty, obscur, other, precip;
    				desc = pdv.getNumberAllLevels("WTHRCOND_DESCRIPTOR");
    				intPrxty = pdv.getNumberAllLevels("WTHRCOND_INTSTY_PRXMTY");
    				obscur = pdv.getNumberAllLevels("WTHRCOND_OBSCURATION");
    				other = pdv.getNumberAllLevels("WTHRCOND_OTHER");
    				precip = pdv.getNumberAllLevels("WTHRCOND_PRECIPITATION");
    				for (int i = 0; i < numWTHRCOND.intValue(); i++) {
    					NcTafWeatherCondition wthrCond = new NcTafWeatherCondition();
    					wthrCond.setDescriptor(desc[i].toString());
    					wthrCond.setIntensityProximity(intPrxty[i].toString());
    					wthrCond.setObscuration(obscur[i].toString());
    					wthrCond.setOther(other[i].toString());
    					wthrCond.setPrecipitation(precip[i].toString());
    					record.addWeather(wthrCond);
    				}
    			}
    		}
    	}
        return record;
    }
    
    private static NcTafRecord getProbableWeatherConditionData(PointDataView pdv, NcTafRecord record) {
    	if (record != null) {
    		Set<String> parameters = pdv.getContainer().getParameters();
    		if (parameters.contains("numPROBABLE_WTHRCOND")) {
    			Number numWTHRCOND = pdv.getInt("numPROBABLE_WTHRCOND");
    			if (numWTHRCOND != null) {
    				Number[] desc, intPrxty, obscur, other, precip;
    				desc = pdv.getNumberAllLevels("PROBABLE_WTHRCOND_DESCRIPTOR");
    				intPrxty = pdv.getNumberAllLevels("PROBABLE_WTHRCOND_INTSTY_PRXMTY");
    				obscur = pdv.getNumberAllLevels("PROBABLE_WTHRCOND_OBSCURATION");
    				other = pdv.getNumberAllLevels("PROBABLE_WTHRCOND_OTHER");
    				precip = pdv.getNumberAllLevels("PROBABLE_WTHRCOND_PRECIPITATION");
    				for (int i = 0; i < numWTHRCOND.intValue(); i++) {
    					NcTafWeatherCondition wthrCond = new NcTafWeatherCondition();
    					wthrCond.setDescriptor(desc[i].toString());
    					wthrCond.setIntensityProximity(intPrxty[i].toString());
    					wthrCond.setObscuration(obscur[i].toString());
    					wthrCond.setOther(other[i].toString());
    					wthrCond.setPrecipitation(precip[i].toString());
    					record.addProbable_weather(wthrCond);
    				}
    			}
    		}
    	}
        return record;
    }
    
    private static NcTafRecord getTurbulenceLayerData(PointDataView pdv, NcTafRecord record) {
    	if (record != null) {
    		Set<String> parameters = pdv.getContainer().getParameters();
    		if (parameters.contains("numTURB")) {
    			Number numTURB = pdv.getInt("numTURB");
    			if (numTURB != null) {
    				Number[] intensity, maxAlt, minAlt;
    				intensity = pdv.getNumberAllLevels("TURB_INTENSITY");
    				maxAlt = pdv.getNumberAllLevels("TURB_MAX_ALT");
    				minAlt = pdv.getNumberAllLevels("TURB_MIN_ALT");
    				for (int i = 0; i < numTURB.intValue(); i++) {
    					NcTafTurbulenceLayer turbLyr = new NcTafTurbulenceLayer();
    					turbLyr.setTurbulence_intensity(intensity[i].intValue());
    					turbLyr.setTurbulence_max_alt_ft_agl(maxAlt[i].intValue());
    					turbLyr.setTurbulence_min_alt_ft_agl(minAlt[i].intValue());
    					record.addTurbulence_layer(turbLyr);
    				}
    			}
    		}
    	}
        return record;
    }
    
    private static NcTafRecord getTemperatureForecastData(PointDataView pdv, NcTafRecord record) {
    	if (record != null) {
    		Set<String> parameters = pdv.getContainer().getParameters();
    		if (parameters.contains("numTMPFCST")) {
    			Number numTMPFCST = pdv.getInt("numTMPFCST");
    			if (numTMPFCST != null) {
    				Number[] time, temp;
    				time = pdv.getNumberAllLevels("TMPFCST_VALID_TIME");
    				temp = pdv.getNumberAllLevels("TMPFCST_TEMP");
    				for (int i = 0; i < numTMPFCST.intValue(); i++) {
    					NcTafTemperatureForecast tempFcst = new NcTafTemperatureForecast();
    					tempFcst.setValid_time(time[i].intValue());
    					tempFcst.setSfc_temp_c(temp[i].intValue());
    					record.addTemp_forecast(tempFcst);
    				}
    			}
    		}
    	}
        return record;
    }
    
    private static NcTafRecord getSkyCoverData(PointDataView pdv, NcTafRecord record) {
    	if (record != null) {
    		Set<String> parameters = pdv.getContainer().getParameters();
    		if (parameters.contains("numSCVR")) {
    			Number numSCVR = pdv.getInt("numSCVR");
    			if (numSCVR != null) {
    				Number[] height, type, genus;
    				height = pdv.getNumberAllLevels("SCVR_HGT");
    				type = pdv.getNumberAllLevels("SCVR_TYPE");
    				genus = pdv.getNumberAllLevels("SCVR_GENUS");
    				for (int i = 0; i < numSCVR.intValue(); i++) {
    					NcTafSkyCover skyCvr = new NcTafSkyCover();
    					skyCvr.setHeight(height[i].intValue());
    					skyCvr.setType(type[i].toString());
    					skyCvr.setGenus(genus[i].toString());
    					record.addSky_cover(skyCvr);
    				}
    			}
    		}
    	}
        return record;
    }
    
    private static NcTafRecord getProbableSkyCoverData(PointDataView pdv, NcTafRecord record) {
    	if (record != null) {
    		Set<String> parameters = pdv.getContainer().getParameters();
    		if (parameters.contains("numPROBABLE_SCVR")) {
    			Number numSCVR = pdv.getInt("numPROBABLE_SCVR");
    			if (numSCVR != null) {
    				Number[] height, type, genus;
    				height = pdv.getNumberAllLevels("PROBABLE_SCVR_HGT");
    				type = pdv.getNumberAllLevels("PROBABLE_SCVR_TYPE");
    				genus = pdv.getNumberAllLevels("PROBABLE_SCVR_GENUS");
    				for (int i = 0; i < numSCVR.intValue(); i++) {
    					NcTafSkyCover skyCvr = new NcTafSkyCover();
    					skyCvr.setHeight(height[i].intValue());
    					skyCvr.setType(type[i].toString());
    					skyCvr.setGenus(genus[i].toString());
    					record.addProbable_sky_cover(skyCvr);
    				}
    			}
    		}
    	}
        return record;
    }
    
    public static NcTafRecord [] toNcTafRecords(PointDataContainer container) {
        List<NcTafRecord> records = new ArrayList<NcTafRecord>();
        container.setCurrentSz(container.getAllocatedSz());
        for (int i = 0; i < container.getCurrentSz(); i++) {
        	PointDataView pdv = container.readRandom(i);
        	NcTafRecord record = getNcTafRecord(pdv);
        	record = getIcingLayerData(pdv, record);
        	record = getSkyCoverData(pdv, record);
        	record = getProbableSkyCoverData(pdv, record);
        	record = getTemperatureForecastData(pdv, record);
        	record = getTurbulenceLayerData(pdv, record);
        	record = getWeatherConditionData(pdv, record);
        	record = getProbableWeatherConditionData(pdv, record);
        	records.add(record);
        }
        return records.toArray(new NcTafRecord[records.size()]);
    }
}