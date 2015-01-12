/**
 * NcTafToPointData
 * 
 * This java class provides the data transform from NcTafRecord to 
 * point data.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    	Engineer    	Description
 * ------------ ---------- 	----------- 	--------------------------
 * 09/23/2011   458         S. Gurung       Initial Creation
 * 09/29/2011               sgurung         Added REPORT_TYPE
 * 10/26/2011               sgurung         Added probable parameters (for TEMPO/PROB)
 * 11/03/2011               sgurung         Added additional parameters
 * 11/07/2011               sgurung         Added LOW_LEVEL_WIND_SHEAR
 * Jul 30, 2014 3410        bclement        dataURI no longer stored in hdf5
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
import gov.noaa.nws.ncep.common.tools.IDecoderConstantsN;

import java.io.File;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.pointdata.Dimension;
import com.raytheon.uf.common.pointdata.PointDataContainer;
import com.raytheon.uf.common.pointdata.PointDataDescription;
import com.raytheon.uf.common.pointdata.PointDataView;

public class NcTafToPointData {
	private static final String DATAURI = "DATAURI";
	private static final String WMO_HEADER = "WMOHEADER";
	private static final String TAF_TEXT = "TAFTEXT";
    private static final String STATION_ID = "STID";
    private static final String CORRECTION_INDICATOR = "CORR";
    private static final String AMD_INDICATOR = "AMD";
    private static final String ISSUE_TIME = "ISSUETIME";
    private static final String ISSUE_TIME_STRING = "ISSUETIMESTR";
    private static final String BULLETIN_TIME = "BULLETINTIME";    
    //private static final String RAW_DATA = "RAWDATA";
    private static final String REPORT_TYPE = "REPORTTYPE";
      
    private static final String CGRP = "CGRP";
    private static final String IND = "IND";
    private static final String PROB = "PROB"; 
    private static final String MAX_TEMP = "MAX_TEMP"; 
    private static final String MIN_TEMP = "MIN_TEMP"; 
    private static final String ALTIM = "ALTIM";
    private static final String REMARKS = "REMARKS";
    private static final String SEQID = "SEQID"; 
    private static final String END_DATE = "END_DATE";
    private static final String START_DATE = "START_DATE";
    private static final String TRANS_END_DATE = "TRANS_END_DATE";
    private static final String VISIBILITY = "VISIBILITY"; 
    private static final String VERT_VISIBILITY ="VERT_VISIBILITY";
    private static final String WIND_DIR = "WIND_DIR";
    private static final String WIND_GUST = "WIND_GUST";
    private static final String WIND_SPEED = "WIND_SPEED"; 
    private static final String SHEAR_WIND_DIR = "SHEAR_WIND_DIR"; 
    private static final String SHEAR_WIND_SPEED = "SHEAR_WIND_SPEED"; 
    private static final String SHEAR_WIND_HGT = "SHEAR_WIND_HGT"; 
    private static final String PROBABLE_VISIBILITY = "PROBABLE_VISIBILITY"; 
    private static final String PROBABLE_VERT_VISIBILITY ="PROBABLE_VERT_VISIBILITY";
    private static final String PROBABLE_WIND_DIR = "PROBABLE_WIND_DIR";
    private static final String PROBABLE_WIND_GUST = "PROBABLE_WIND_GUST";
    private static final String PROBABLE_WIND_SPEED = "PROBABLE_WIND_SPEED"; 
    private static final String PROBABLE_SHEAR_WIND_DIR = "PROBABLE_SHEAR_WIND_DIR"; 
    private static final String PROBABLE_SHEAR_WIND_SPEED = "PROBABLE_SHEAR_WIND_SPEED"; 
    private static final String PROBABLE_SHEAR_WIND_HGT = "PROBABLE_SHEAR_WIND_HGT"; 
    private static final String CEILING ="CEILING";
    private static final String PROBABLE_CEILING ="PROBABLE_CEILING";
    private static final String STIM = "STIM";
    private static final String LOW_LEVEL_WIND_SHEAR_FLAG = "LOW_LEVEL_WIND_SHEAR";
  
    private static final String ICNG_INTENSITY = "ICNG_INTENSITY";
    private static final String ICNG_MAX_ALT = "ICNG_MAX_ALT";
    private static final String ICNG_MIN_ALT = "ICNG_MIN_ALT";
   
    private static final String SCVR_HGT = "SCVR_HGT";
    private static final String SCVR_TYPE = "SCVR_TYPE";
    private static final String SCVR_GENUS = "SCVR_GENUS"; 
  
    private static final String PROBABLE_SCVR_HGT = "PROBABLE_SCVR_HGT";
    private static final String PROBABLE_SCVR_TYPE = "PROBABLE_SCVR_TYPE";
    private static final String PROBABLE_SCVR_GENUS = "PROBABLE_SCVR_GENUS"; 
  
    private static final String TMPFCST_VALID_TIME = "TMPFCST_VALID_TIME";
    private static final String TMPFCST_TEMP = "TMPFCST_TEMP";    

    private static final String TURB_INTENSITY = "TURB_INTENSITY";
    private static final String TURB_MAX_ALT = "TURB_MAX_ALT";
    private static final String TURB_MIN_ALT = "TURB_MIN_ALT"; 
     
    private static final String WTHRCOND_DESCRIPTOR = "WTHRCOND_DESCRIPTOR"; 
    private static final String WTHRCOND_INTSTY_PRXMTY = "WTHRCOND_INTSTY_PRXMTY";
    private static final String WTHRCOND_OBSCURATION = "WTHRCOND_OBSCURATION";
    private static final String WTHRCOND_OTHER = "WTHRCOND_OTHER";
    private static final String WTHRCOND_PRECIPITATION = "WTHRCOND_PRECIPITATION"; 
    
    private static final String PRES_WEATHER = "PRES_WEATHER";

    private static final String PROBABLE_WTHRCOND_DESCRIPTOR = "PROBABLE_WTHRCOND_DESCRIPTOR"; 
    private static final String PROBABLE_WTHRCOND_INTSTY_PRXMTY = "PROBABLE_WTHRCOND_INTSTY_PRXMTY";
    private static final String PROBABLE_WTHRCOND_OBSCURATION = "PROBABLE_WTHRCOND_OBSCURATION";
    private static final String PROBABLE_WTHRCOND_OTHER = "PROBABLE_WTHRCOND_OTHER";
    private static final String PROBABLE_WTHRCOND_PRECIPITATION = "PROBABLE_WTHRCOND_PRECIPITATION"; 
    
    private static final String PROBABLE_PRES_WEATHER = "PROBABLE_PRES_WEATHER";
    
    /**
     * !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! It is important to
     * keep this up to date or risk breaking backwards compatibility
     */
    private static final String[] ALL_PARAMS = { DATAURI, 
    	WMO_HEADER, TAF_TEXT, REPORT_TYPE, STATION_ID, CORRECTION_INDICATOR, AMD_INDICATOR, 
    	ISSUE_TIME, ISSUE_TIME_STRING, BULLETIN_TIME, 
    	CGRP, IND, PROB, MAX_TEMP, MIN_TEMP, ALTIM, 
    	REMARKS, SEQID, END_DATE, START_DATE, TRANS_END_DATE, 
    	VISIBILITY, VERT_VISIBILITY,WIND_DIR, WIND_GUST, 
    	WIND_SPEED,SHEAR_WIND_DIR,SHEAR_WIND_SPEED, SHEAR_WIND_HGT, LOW_LEVEL_WIND_SHEAR_FLAG,
    	CEILING, PROBABLE_CEILING,
    	PROBABLE_VISIBILITY, PROBABLE_VERT_VISIBILITY,PROBABLE_WIND_DIR, PROBABLE_WIND_GUST, 
    	PROBABLE_WIND_SPEED,PROBABLE_SHEAR_WIND_DIR,PROBABLE_SHEAR_WIND_SPEED, PROBABLE_SHEAR_WIND_HGT,  
    	ICNG_INTENSITY, ICNG_MAX_ALT, ICNG_MIN_ALT,
    	SCVR_HGT, SCVR_TYPE, SCVR_GENUS,
    	PROBABLE_SCVR_HGT, PROBABLE_SCVR_TYPE, PROBABLE_SCVR_GENUS, 
    	TMPFCST_VALID_TIME, TMPFCST_TEMP, 
    	TURB_INTENSITY, TURB_MAX_ALT, TURB_MIN_ALT,
    	WTHRCOND_DESCRIPTOR, WTHRCOND_INTSTY_PRXMTY, WTHRCOND_OBSCURATION, WTHRCOND_OTHER, WTHRCOND_PRECIPITATION, PRES_WEATHER,
    	PROBABLE_WTHRCOND_DESCRIPTOR, PROBABLE_WTHRCOND_INTSTY_PRXMTY, PROBABLE_WTHRCOND_OBSCURATION, PROBABLE_WTHRCOND_OTHER, PROBABLE_WTHRCOND_PRECIPITATION, PROBABLE_PRES_WEATHER }; 

    public static final String ALL_PARAMS_LIST;
    static {
        StringBuffer sb = new StringBuffer();
        boolean first = true;
        for (String s : ALL_PARAMS) {
            if (!first) {
                sb.append(", ");
            } else {
                first = false;
            }
            sb.append(s);
        }
        ALL_PARAMS_LIST = sb.toString();
    }

    private NcTafDao dao;

    private PointDataDescription pdd;

    public NcTafToPointData() {
        try {
            this.dao = new NcTafDao("nctaf");
            this.pdd = dao.getPointDataDescription();  
            
        } catch (Exception e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }
    }

    public PluginDataObject[] toPointData(PluginDataObject[] pdo) {

        if (pdo.length > 0) {
            Map<File, PointDataContainer> pointMap = new HashMap<File, PointDataContainer>();

            for (PluginDataObject p : pdo) {
                if (!(p instanceof NcTafRecord))
                    continue;
                File f = this.dao.getFullFilePath(p);
                PointDataContainer pdc = pointMap.get(f);
 
                if (pdc == null) {                    
                    pdc = PointDataContainer.build(this.pdd);
                    pointMap.put(f, pdc);
                }
                NcTafRecord mr = (NcTafRecord) p;
                PointDataView pdv = buildView(pdc, mr);
                mr.setPointDataView(pdv);
            }
        }
        return pdo;
    }
       
    private PointDataView buildView(PointDataContainer container,
    		NcTafRecord record) {

        PointDataView pdv = container.append();
        
        int maxIcngLyr = -1;
        int maxSkyCover = -1;
        int maxTempFcst = -1;
        int maxTurbFcst = -1;
        int maxWthrCond = -1;
        
        Dimension [] dims = pdd.dimensions;
        for(Dimension d : dims) {
            
            if("maxIcngLyr".equals(d.getDimensionName())) {
            	maxIcngLyr = d.getDimensionLength();
            }
            
            if("maxSkyCover".equals(d.getDimensionName())) {
            	maxSkyCover = d.getDimensionLength();
            }
            
            if("maxTempFcst".equals(d.getDimensionName())) {
            	maxTempFcst = d.getDimensionLength();
            }
            
            if("maxTurbFcst".equals(d.getDimensionName())) {
            	maxTurbFcst = d.getDimensionLength();
            }
            
            if("maxWthrCond".equals(d.getDimensionName())) {
            	maxWthrCond = d.getDimensionLength();
            }
        }
        
        if (record.getWmoHeader()!=null)
        	pdv.setString(WMO_HEADER, record.getWmoHeader());
        
        if (record.getTafText()!=null)
        	pdv.setString(TAF_TEXT, record.getTafText());
        
        if (record.getReportType()!=null)
            pdv.setString(REPORT_TYPE, record.getReportType());
        
        if (record.getStationId()!=null)
        	pdv.setString(STATION_ID, record.getStationId());
        
        if (record.getCorIndicator() != null) {
            pdv.setString(CORRECTION_INDICATOR,record.getCorIndicator());
        } else {
            pdv.setString(CORRECTION_INDICATOR, "");
        }

        if (record.getCorIndicator() != null) {
            pdv.setString(AMD_INDICATOR,record.getAmdIndicator());
        } else {
            pdv.setString(AMD_INDICATOR, "");
        }

        if (record.getIssue_time()!=null)
        	pdv.setLong(ISSUE_TIME, record.getIssue_time().getTime());
        
        if (record.getIssue_timeString()!=null)
        	pdv.setString(ISSUE_TIME_STRING, record.getIssue_timeString());
        
        if (record.getBulletin_time()!=null)
        	pdv.setLong(BULLETIN_TIME, record.getBulletin_time().getTime());
        
        if (record.getChange_indicator() != null) 
			pdv.setString(IND, record.getChange_indicator());
		
        if (record.getChangeGroup() != null) 
        	pdv.setString(CGRP, record.getChangeGroup().trim());
		
		if (record.getProbability() != null) 
			pdv.setInt(PROB, record.getProbability());
		
		if (record.getMax_temp_c() != null) 
			pdv.setFloat(MAX_TEMP, record.getMax_temp_c());
		
		if (record.getMin_temp_c() != null) 
			pdv.setFloat(MIN_TEMP, record.getMin_temp_c());
		
		pdv.setFloat(ALTIM, record.getAltim_in_hg());
		
		if (record.getRemarks() != null)
			pdv.setString(REMARKS, record.getRemarks());
		
		if (record.getSequenceId() != null)
			pdv.setInt(SEQID, record.getSequenceId());
		
		if (record.getEndDate() != null)
			pdv.setLong(END_DATE, record.getEndDate().getTime().getTime());
		
		if (record.getStartDate() != null)
			pdv.setLong(START_DATE, record.getStartDate().getTime().getTime());
		
		if (record.getTransitionEndDate() != null)
			pdv.setLong(TRANS_END_DATE, record.getTransitionEndDate().getTime().getTime());
		
		if (record.getVisibility_mi() != null)
			pdv.setFloat(VISIBILITY, record.getVisibility_mi());
		
		pdv.setFloat(VERT_VISIBILITY, record.getVert_vis_ft());
		
		if (record.getWind_dir_degrees() != null)
			pdv.setFloat(WIND_DIR, record.getWind_dir_degrees());
		
		if (record.getWind_gust_kt() != null)
			pdv.setFloat(WIND_GUST, record.getWind_gust_kt());
		
		if (record.getWind_speed_kt() != null)
			pdv.setFloat(WIND_SPEED, record.getWind_speed_kt());
		
		if (record.getWind_shear_dir_degrees() != null)
			pdv.setFloat(SHEAR_WIND_DIR, record.getWind_shear_dir_degrees());
		
		if (record.getWind_shear_speed_kt() != null)
			pdv.setFloat(SHEAR_WIND_SPEED, record.getWind_shear_speed_kt());
		
		if (record.getWind_shear_hgt_ft_agl() != null)
			pdv.setFloat(SHEAR_WIND_HGT, record.getWind_shear_hgt_ft_agl());
		
		// Set low level wind shear forecast flag		
		if ((record.getWind_shear_hgt_ft_agl() != IDecoderConstantsN.NEGATIVE_FLOAT_MISSING) 
				&& (record.getWind_shear_dir_degrees() != IDecoderConstantsN.NEGATIVE_FLOAT_MISSING) 
				&& (record.getWind_shear_speed_kt() != IDecoderConstantsN.NEGATIVE_FLOAT_MISSING))
			pdv.setInt(LOW_LEVEL_WIND_SHEAR_FLAG, 1);
		
		pdv.setFloat(PROBABLE_VISIBILITY, record.getProbable_visibility_mi());
		
		pdv.setFloat(PROBABLE_VERT_VISIBILITY, record.getProbable_vert_vis_ft());
		
		pdv.setFloat(PROBABLE_WIND_DIR, record.getProbable_wind_dir_degrees());
		
		pdv.setFloat(PROBABLE_WIND_GUST, record.getProbable_wind_gust_kt());
		
		pdv.setFloat(PROBABLE_WIND_SPEED, record.getProbable_wind_speed_kt());
		
		pdv.setFloat(PROBABLE_SHEAR_WIND_DIR, record.getProbable_wind_shear_dir_degrees());
		
		pdv.setFloat(PROBABLE_SHEAR_WIND_SPEED, record.getProbable_wind_shear_speed_kt());
		
		pdv.setFloat(PROBABLE_SHEAR_WIND_HGT, record.getProbable_wind_shear_hgt_ft_agl());
		
		String reportIssueTime = record.getIssue_timeString();
		
		if (reportIssueTime != null && reportIssueTime.length() == 7) {
			reportIssueTime = reportIssueTime.substring(2, reportIssueTime.length()-1);
			pdv.setInt(STIM, Integer.parseInt(reportIssueTime));			
		}
		
		int index = 0;
		if (record.getIcing_layers() != null) {
			Iterator<NcTafIcingLayer> icngLyrs = record.getIcing_layers().iterator();
			
			if (icngLyrs != null ) {
			  while (icngLyrs.hasNext()) {
	    			NcTafIcingLayer icngLyr = icngLyrs.next();
	    			if (index < maxIcngLyr ) {
	    				if (icngLyr.getIcing_intensity() != null) 
	    					pdv.setInt(ICNG_INTENSITY, icngLyr.getIcing_intensity(), index);
	    				if (icngLyr.getIcing_max_alt_ft_agl() != null) 
	    					pdv.setInt(ICNG_MAX_ALT, icngLyr.getIcing_max_alt_ft_agl(), index);
	    				if (icngLyr.getIcing_min_alt_ft_agl() != null) 
	    					pdv.setInt(ICNG_MIN_ALT, icngLyr.getIcing_min_alt_ft_agl(), index);
	    				index++;
	    			}
	    		}
	        	pdv.setInt ("numICNG", index);
	    	}
		}
		
		index = 0;
		if (record.getSky_cover() != null) {
			Iterator<NcTafSkyCover> skyCvrs = record.getSky_cover().iterator();
			
			if (skyCvrs != null ) {
			  while (skyCvrs.hasNext()) {
				  NcTafSkyCover skyCvr = skyCvrs.next();
				  if (index < maxSkyCover ) {
	    				if (skyCvr.getHeight() != null) 
	    					pdv.setFloat(SCVR_HGT, skyCvr.getHeight(), index);
	    				if (skyCvr.getType() != null) 
	    					pdv.setString(SCVR_TYPE, skyCvr.getType(), index);
	    				if (skyCvr.getGenus() != null) 
	    					pdv.setString(SCVR_GENUS, skyCvr.getGenus(), index);
	    				index++;
	    			}
	    		}
	        	pdv.setInt ("numSCVR", index);
	    	}
			
			// Set ceiling
    		float ceiling = NcTafRecord.getCeiling(record.getSky_cover(), record.getVert_vis_ft());
    		if (ceiling != IDecoderConstantsN.NEGATIVE_FLOAT_MISSING)
    			pdv.setFloat(CEILING, ceiling);
		}
		
		index = 0;
		if (record.getProbable_sky_cover() != null) {
			Iterator<NcTafSkyCover> skyCvrs = record.getProbable_sky_cover().iterator();
			
			if (skyCvrs != null ) {
			  while (skyCvrs.hasNext()) {
				  NcTafSkyCover skyCvr = skyCvrs.next();
				  if (index < maxSkyCover ) {
	    				if (skyCvr.getHeight() != null) 
	    					pdv.setFloat(PROBABLE_SCVR_HGT, skyCvr.getHeight(), index);
	    				if (skyCvr.getType() != null) 
	    					pdv.setString(PROBABLE_SCVR_TYPE, skyCvr.getType(), index);
	    				if (skyCvr.getGenus() != null) 
	    					pdv.setString(PROBABLE_SCVR_GENUS, skyCvr.getGenus(), index);
	    				index++;
	    			}
	    		}
	        	pdv.setInt ("numPROBABLE_SCVR", index); 	    		
	    	}
			
			// Set probable ceiling (TEMPO/PROB)
    		float probableCeiling = NcTafRecord.getCeiling(record.getProbable_sky_cover(), record.getProbable_vert_vis_ft());
    		if (probableCeiling != IDecoderConstantsN.NEGATIVE_FLOAT_MISSING)
    			pdv.setFloat(PROBABLE_CEILING, probableCeiling);
		}
		
		index = 0;
		if (record.getTemp_forecasts() != null) {
			Iterator<NcTafTemperatureForecast> tmpFcsts = record.getTemp_forecasts().iterator();	
			
			if (tmpFcsts != null ) {
				  while (tmpFcsts.hasNext()) {
					  NcTafTemperatureForecast tmpFcst = tmpFcsts.next();
					  if (index < maxTempFcst ) {
		    				if (tmpFcst.getSfc_temp_c() != null) 
		    					pdv.setFloat(TMPFCST_VALID_TIME, tmpFcst.getSfc_temp_c(), index);
		    				if (tmpFcst.getValid_time() != null) 
		    					pdv.setFloat(TMPFCST_TEMP, tmpFcst.getValid_time(), index);
		    				index++;
		    			}
		    		}
		        	pdv.setInt ("numTMPFCST", index);
		     }
		}
				
		index = 0;
		if (record.getTurbulence_layers() != null) {
			Iterator<NcTafTurbulenceLayer> turbLyrs = record.getTurbulence_layers().iterator();
			
			if (turbLyrs != null ) {
			  while (turbLyrs.hasNext()) {
				NcTafTurbulenceLayer turbLyr = turbLyrs.next();
				  if (index < maxTurbFcst ) {
	    				if (turbLyr.getTurbulence_intensity() != null) 
	    					pdv.setFloat(TURB_INTENSITY, turbLyr.getTurbulence_intensity(), index);
	    				if (turbLyr.getTurbulence_max_alt_ft_agl() != null) 
	    					pdv.setFloat(TURB_MAX_ALT, turbLyr.getTurbulence_max_alt_ft_agl(), index);
	    				if (turbLyr.getTurbulence_min_alt_ft_agl() != null) 
	    					pdv.setFloat(TURB_MIN_ALT, turbLyr.getTurbulence_min_alt_ft_agl(), index);
	    				index++;
	    			}
	    		}
	        	pdv.setInt ("numTURB", index);
	        }			
		}
		
		index = 0;
		if (record.getWeather() != null) {
			Iterator<NcTafWeatherCondition> wthrConds = record.getWeather().iterator();
			
			if (wthrConds != null ) {
			  while (wthrConds.hasNext()) {
					  NcTafWeatherCondition wthrCond = wthrConds.next();
					  if (index < maxWthrCond ) {
		    				if (wthrCond.getDescriptor() != null) 
		    					pdv.setString(WTHRCOND_DESCRIPTOR, wthrCond.getDescriptor(), index);
		    				if (wthrCond.getIntensityProximity() != null) 
		    					pdv.setString(WTHRCOND_INTSTY_PRXMTY, wthrCond.getIntensityProximity(), index);
		    				if (wthrCond.getObscuration() != null) 
		    					pdv.setString(WTHRCOND_OBSCURATION, wthrCond.getObscuration(), index);
		    				if (wthrCond.getOther() != null) 
		    					pdv.setString(WTHRCOND_OTHER, wthrCond.getOther(), index);
		    				if (wthrCond.getPrecipitation() != null) 
		    					pdv.setString(WTHRCOND_PRECIPITATION, wthrCond.getPrecipitation(), index);
		    				index++;
		    			}
		    	}
		       	pdv.setInt ("numWTHRCOND", index);
		     }	        
		}
		
		// Write this data in "backwards" so that the plot model stuff
        // displays correctly.
        if (record.getWeather() != null) {
            int i = record.getWeather().size() - 1;
            for (NcTafWeatherCondition wc : record.getWeather()) {
                pdv.setString(PRES_WEATHER, wc.toCanonicalForm(), i--);
            }
        }
		
		index = 0;
		if (record.getProbable_weather() != null) {
			Iterator<NcTafWeatherCondition> wthrConds = record.getProbable_weather().iterator();
			
			if (wthrConds != null ) {
			  while (wthrConds.hasNext()) {
					  NcTafWeatherCondition wthrCond = wthrConds.next();
					  if (index < maxWthrCond ) {
		    				if (wthrCond.getDescriptor() != null) 
		    					pdv.setString(PROBABLE_WTHRCOND_DESCRIPTOR, wthrCond.getDescriptor(), index);
		    				if (wthrCond.getIntensityProximity() != null) 
		    					pdv.setString(PROBABLE_WTHRCOND_INTSTY_PRXMTY, wthrCond.getIntensityProximity(), index);
		    				if (wthrCond.getObscuration() != null) 
		    					pdv.setString(PROBABLE_WTHRCOND_OBSCURATION, wthrCond.getObscuration(), index);
		    				if (wthrCond.getOther() != null) 
		    					pdv.setString(PROBABLE_WTHRCOND_OTHER, wthrCond.getOther(), index);
		    				if (wthrCond.getPrecipitation() != null) 
		    					pdv.setString(PROBABLE_WTHRCOND_PRECIPITATION, wthrCond.getPrecipitation(), index);
		    				index++;
		    			}
		    	}
		       	pdv.setInt ("numPROBABLE_WTHRCOND", index);
		     }	        
		}
		
		// Write this data in "backwards" so that the plot model stuff
        // displays correctly.
        if (record.getProbable_weather() != null) {
            int i = record.getProbable_weather().size() - 1;
            for (NcTafWeatherCondition wc : record.getWeather()) {
                pdv.setString(PROBABLE_PRES_WEATHER, wc.toCanonicalForm(), i--);
            }
        }
        
    	return pdv;
    } 
}