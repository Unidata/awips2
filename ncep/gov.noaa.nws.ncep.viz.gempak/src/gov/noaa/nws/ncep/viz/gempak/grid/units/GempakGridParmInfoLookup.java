package gov.noaa.nws.ncep.viz.gempak.grid.units;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;

import com.raytheon.uf.common.localization.FileUpdatedMessage;
import com.raytheon.uf.common.localization.ILocalizationFileObserver;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.localization.FileUpdatedMessage.FileChangeType;
import com.raytheon.uf.common.serialization.SerializationUtil;

import gov.noaa.nws.ncep.common.log.logger.NcepLogger;
import gov.noaa.nws.ncep.common.log.logger.NcepLoggerManager;
import gov.noaa.nws.ncep.viz.localization.NcPathManager;

public class GempakGridParmInfoLookup implements ILocalizationFileObserver {
	public static final String GRID_GEMPAK_PARM_FILE = "ncep"+File.separator +
															"GempakGridUnits" + File.separator + 
															"gempakGridParmeterUnits.xml";
	/** The logger */
	private static NcepLogger logger = NcepLoggerManager.getNcepLogger(GempakGridParmInfoLookup.class);

	/** The singleton instance of GridLookupFileName **/
    private static GempakGridParmInfoLookup instance;
    
    private final Map<String, GempakGridParmInfo> parmInfo;
    
    public static GempakGridParmInfoLookup getInstance () {
    	if (instance == null) {
    		instance = new GempakGridParmInfoLookup ();
    	}
    	return instance;
    }
    
    private GempakGridParmInfoLookup () {
    	parmInfo = new HashMap <String,GempakGridParmInfo>();
    	try {
    		initParmInfo ();
    	}catch (IOException e) {
            logger.error("Unable to initialize gempak parameter information list!", e);
        }
    }
    
    private void initParmInfo () throws IOException {
    	
//    	logger.info("Initial Gempak Parameter information===");
    	File gempakParmInfo = 
    		NcPathManager.getInstance().getStaticFile(GRID_GEMPAK_PARM_FILE);
    	
    	GempakGridParmInfoSet parmInfoList = null;
    	try {
    		if ( gempakParmInfo.exists()) {
    			parmInfoList = (GempakGridParmInfoSet) SerializationUtil
                .jaxbUnmarshalFromXmlFile(gempakParmInfo.getPath());
    		} else {
                ArrayList<GempakGridParmInfo> emptyList = new ArrayList<GempakGridParmInfo>();
                parmInfoList = new GempakGridParmInfoSet();
                parmInfoList.setParmeterinfo(emptyList);
            }
    		for (GempakGridParmInfo parm : parmInfoList.getParmeterinfo()) {
    			parmInfo.put(parm.getGnam(), parm);
    		}
    	} catch (Exception e) {
            throw new IOException ("Unable to unmarshal ncep gempak parm info file");
        }
    	
    }
  
    public String getParmUnit ( String parm ) {
    	
    	String units = null;
    	GempakGridParmInfo pInfo = parmInfo.get(parm);
    	if ( pInfo != null ) {
    		units = pInfo.getUnits();
    	}
    	return units;
    }
    
    public int getParmScale ( String parm ) {
    	
    	int scale = 0;
    	GempakGridParmInfo pInfo = parmInfo.get(parm);
    	if ( pInfo != null ) {
    		scale = pInfo.getScale();
    	}
    	return scale;
    }
    
    public void display () {
    	int cnt = 1;
    	logger.info ("Size of parm table:" + parmInfo.size());
    	for (GempakGridParmInfo parm: parmInfo.values() ) {
    		logger.info("No." + cnt + " name:" + parm.getName() + " gname:" + parm.getGnam() + " units:" + parm.getUnits() + " scale:" + parm.getScale());
    		cnt ++;
    	}
    }
	@Override
	public void fileUpdated( FileUpdatedMessage fumsg) {
		String fName = fumsg.getFileName();
		LocalizationFile lFile;
		logger.info("======fileUpdated:" + fumsg.getChangeType());
		// if the file had been deleted 
		if( fumsg.getChangeType() == FileChangeType.DELETED ) {

		}
		else {
		
		}
	}
}

