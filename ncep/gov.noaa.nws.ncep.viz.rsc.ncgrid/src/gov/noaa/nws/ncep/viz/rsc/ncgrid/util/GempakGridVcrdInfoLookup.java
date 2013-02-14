package gov.noaa.nws.ncep.viz.rsc.ncgrid.util;

import gov.noaa.nws.ncep.common.log.logger.NcepLogger;
import gov.noaa.nws.ncep.common.log.logger.NcepLoggerManager;
import gov.noaa.nws.ncep.viz.localization.NcPathManager;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;

import com.raytheon.uf.common.serialization.SerializationUtil;

public class GempakGridVcrdInfoLookup {
	public static final String NCGRID_GEMPAK_VCORD_FILE = "ncep"+File.separator +
														 "GempakGridUnits" + File.separator + 
														 "gempakGridVcrdUnits.xml";
	/** The logger */
	private static NcepLogger logger = NcepLoggerManager.getNcepLogger(GempakGridVcrdInfoLookup.class);
	/** The singleton instance of GridLookupFileName **/
    private static GempakGridVcrdInfoLookup instance;
    
    private final Map<String, GempakGridVcrdInfo> vcrdInfo;
    
    public static GempakGridVcrdInfoLookup getInstance () {
    	if (instance == null) {
    		instance = new GempakGridVcrdInfoLookup ();
    	}
    	return instance;
    }
    
    private GempakGridVcrdInfoLookup () {
    	vcrdInfo = new HashMap <String,GempakGridVcrdInfo>();
    	try {
    		initVcordInfo ();
    	}catch (IOException e) {
            logger.error("Unable to initialize gempak vcord information list!", e);
        }
    }
    
    private void initVcordInfo () throws IOException {
    	
    	logger.info("Initial Gempak VCORD information===");
    	File gempakVcrdInfo = 
    		NcPathManager.getInstance().getStaticFile(NCGRID_GEMPAK_VCORD_FILE);
    	
    	GempakGridVcrdInfoSet vcrdInfoList = null;
    	try {
    		if ( gempakVcrdInfo.exists()) {
    			vcrdInfoList = (GempakGridVcrdInfoSet) SerializationUtil
                .jaxbUnmarshalFromXmlFile(gempakVcrdInfo.getPath());
    		} else {
                ArrayList<GempakGridVcrdInfo> emptyList = new ArrayList<GempakGridVcrdInfo>();
                vcrdInfoList = new GempakGridVcrdInfoSet();
                vcrdInfoList.setVcordinfo(emptyList);
            }
    		for (GempakGridVcrdInfo vcrd : vcrdInfoList.getVcordinfo()) {
    			vcrdInfo.put(vcrd.getGnam(), vcrd);
    		}
    	} catch (Exception e) {
            throw new IOException ("Unable to unmarshal ncep gempak vcrd info file");
        }    	
    }
    
    public String getVcrdUnit ( String parm ) {
    	
    	String units = null;
    	GempakGridVcrdInfo pInfo = vcrdInfo.get(parm);
    	if ( pInfo != null ) {
    		units = pInfo.getUnits();
    	}
    	return units;
    }
    
    public int getParmScale ( String parm ) {
    	
    	int scale = 0;
    	GempakGridVcrdInfo pInfo = vcrdInfo.get(parm);
    	if ( pInfo != null ) {
    		scale = pInfo.getScale();
    	}
    	return scale;
    }
    
    public void display () {
    	int cnt = 1;
    	logger.info ("Size of vcrd table:" + vcrdInfo.size());
    	for (GempakGridVcrdInfo parm: vcrdInfo.values() ) {
    		logger.info("No." + cnt + " name:" + parm.getName() + " gname:" + parm.getGnam() + " units:" + parm.getUnits() + " scale:" + parm.getScale());
    		cnt ++;
    	}
    }
}
