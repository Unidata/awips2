/**
 * This software was developed and / or modified by Raytheon Company,
 * pursuant to Contract DG133W-05-CQ-1067 with the US Government.
 * 
 * U.S. EXPORT CONTROLLED TECHNICAL DATA
 * This software product contains export-restricted data whose
 * export/transfer/disclosure is restricted by U.S. law. Dissemination
 * to non-U.S. persons whether in the United States or abroad requires
 * an export license or other authorization.
 * 
 * Contractor Name:        Raytheon Company
 * Contractor Address:     6825 Pine Street, Suite 340
 *                         Mail Stop B8
 *                         Omaha, NE 68106
 *                         402.291.0100
 * 
 * See the AWIPS II Master Rights File ("Master Rights File.pdf") for
 * further licensing information.
 **/
package com.raytheon.edex.plugin.ldadprofiler.common;

import java.io.File;
import java.io.InputStream;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import javax.xml.bind.JAXBException;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.raytheon.edex.plugin.ldadprofiler.dao.LdadProfilerDao;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.pointdata.ParameterDescription;
import com.raytheon.uf.common.pointdata.PointDataContainer;
import com.raytheon.uf.common.pointdata.PointDataDescription;
import com.raytheon.uf.common.pointdata.PointDataView;

/**
 * Provides a transform from Ldadprofiler Records to PointDataContainer and vice versa.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 29, 2009            vkorolev    Initial creation
 * 
 * </pre>
 * 
 * @author vkorolev
 * @version 1.0
 */

public class LdadprofilerPointDataTransform {
    
//	private static final String BASE_TIME = "base_time";
//	
//	private static final String START_TIME_OFFSET = "start_time_offset";
//    
//    private static final String END_TIME_OFFSET = "end_time_offset";
    
    private static final String HEIGHTS_NUM = "nhts"; 
	
    private static final String LEVEL_HEIGHT = "levelHeight";
    
    private static final String WIND_SPEED = "windSpeed";
    
    private static final String WIND_DIR = "windDir";
    
    private static final String UC_WIND = "ucWind";
    
    private static final String VC_WIND = "vcWind";
    
    private static final String WC_WIND = "wcWind";
    
    private static final String U_CONF = "uconf";
    
    private static final String V_CONF = "vconf";
    
    private static final String W_CONF ="wconf";

//    private static final String REPORT_TYPE = "reportType";
//
//    private static final String TIME_OBS = "timeObs";
//
//    private static final String LONGITUDE = "longitude";
//
//    private static final String LATITUDE = "latitude";
//
//    private static final String STATION_NAME = "stationName";
//
//    private static final String DATAURI = "dataURI";
//
//    private static final String ELEVATION = "elevation";

	
    private LdadProfilerDao dao;

    private PointDataDescription pdd;
    
    private Log logger = LogFactory.getLog(getClass());
	
    public LdadprofilerPointDataTransform() {
    	
        try {
            this.pdd = getDescription("ldadprofiler");
            this.dao = new LdadProfilerDao("ldadprofiler");
            logger.info("=============PointDataDescription loaded==============");
        } catch (Exception e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }
    }
    
	public PluginDataObject[] toPointData(PluginDataObject[] pdo) {
        if (pdo.length > 0) {
            Map<File, PointDataContainer> pointMap = new HashMap<File, PointDataContainer>();

            for (PluginDataObject p : pdo) {
                if (!(p instanceof ProfilerLdadObs))
                    continue;
                File f = this.dao.getFullFilePath(p);
                PointDataContainer pdc = pointMap.get(f);
                if (pdc == null) {
                    pdc = PointDataContainer.build(this.pdd);
                    pointMap.put(f, pdc);
                }
                
                ProfilerLdadObs ldadr = (ProfilerLdadObs) p;
                PointDataView pdv = buildView(pdc, ldadr);
                ldadr.setPointDataView(pdv);
            }
        }
        return pdo;
    }
	
    @SuppressWarnings("unchecked")
	private PointDataView buildView(PointDataContainer container,
			ProfilerLdadObs record) {
        PointDataView pdv = container.append();
        
        pdv.setInt(HEIGHTS_NUM, record.getNhts());
        if (record.getLevels() != null) {
            Iterator lvlIterator = record.getLevels().iterator();
            int i = 0;
            while (lvlIterator.hasNext()) {
            	ProfilerLdadLevel lvl = (ProfilerLdadLevel) lvlIterator.next();
				if (lvl.getLevelHeight() != null) {
					pdv.setInt(LEVEL_HEIGHT, lvl.getLevelHeight(), i);
				}
                if (lvl.getWindSpeed()!= null) {
					pdv.setFloat(WIND_SPEED, lvl.getWindSpeed().floatValue(), i);
				}
				if (lvl.getWindDir() != null) {
					pdv.setFloat(WIND_DIR, lvl.getWindDir().floatValue(), i);
				}
				if (lvl.getUcWind() != null) {
					pdv.setFloat(UC_WIND, lvl.getUcWind().floatValue(), i);
				}
				if (lvl.getVcWind() != null) {
					pdv.setFloat(VC_WIND, lvl.getVcWind().floatValue(), i);
				}
				if (lvl.getWcWind() != null) {
					pdv.setFloat(WC_WIND, lvl.getWcWind().floatValue(), i);
				}
				if (lvl.getUconf() != null) {
					pdv.setFloat(U_CONF, lvl.getUconf().floatValue(), i);
				}
				if (lvl.getVconf() != null) {
					pdv.setFloat(V_CONF, lvl.getVconf().floatValue(), i);
				}
				if (lvl.getWconf() != null) {
					pdv.setFloat(W_CONF, lvl.getWconf().floatValue(), i);
				}
				i++;
            }
        }
        
        return pdv;
	}


	private PointDataDescription getDescription(String type) throws JAXBException{
        InputStream is = this.getClass().getResourceAsStream(
                "/res/pointdata/" + type + ".xml");
        if (is == null) {
            throw new RuntimeException("Cannot find descriptor for: " + type);
        }
        return PointDataDescription.fromStream(is);
	}
	
}
