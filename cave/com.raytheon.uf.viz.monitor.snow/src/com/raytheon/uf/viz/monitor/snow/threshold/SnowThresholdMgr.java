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
package com.raytheon.uf.viz.monitor.snow.threshold;

import java.util.ArrayList;

import com.raytheon.uf.common.monitor.config.MonitorConfigurationManager;
import com.raytheon.uf.common.monitor.config.SnowMonitorConfigurationManager;
import com.raytheon.uf.common.monitor.data.ObConst.DataUsageKey;
import com.raytheon.uf.viz.core.localization.LocalizationManager;
import com.raytheon.uf.viz.monitor.thresholds.AbstractThresholdMgr;
import com.raytheon.uf.viz.monitor.util.MonitorConfigConstants.SnowDisplay;
import com.raytheon.uf.viz.monitor.util.MonitorConfigConstants.SnowMonitor;


/**
 * This class manages the Snow thresholds for display and monitor.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 27, 2009 #3963      lvenable     Initial creation
 *
 * </pre>
 *
 * @author lvenable
 * @version 1.0
 */
public class SnowThresholdMgr extends AbstractThresholdMgr
{
private static SnowThresholdMgr classInstance;
    
    private SnowThresholdMgr()
    {
        super("DefaultSnowDisplayThresholds.xml",
        "DefaultSnowMonitorThresholds.xml",
        "snow");

        areaConfigMgr = getAreaConfigMgr();
        init(); // call init() after areaConfigMgr is set
    }
    
    public static SnowThresholdMgr getInstance()
    {
        if (classInstance == null)
        {
            classInstance = new SnowThresholdMgr();
        }
        
        return classInstance;
    }

    /**
     * DR#11279:
     * When monitor area configuration is changed, 
     * threshold manager needs to be re-initialized 
     * using the new monitor area configuration
     */
    public static void reInitialize() {
    	if ( classInstance != null ) {
    		classInstance = null;
    		classInstance = new SnowThresholdMgr();
    	}
    }

    @Override
    protected ArrayList<String> getThresholdKeys(DataUsageKey dataUsage)
    {
        ArrayList<String> threshKeys = new ArrayList<String>();
        
        if (dataUsage == DataUsageKey.DISPLAY)
        {
            for (SnowDisplay snowDisp : SnowDisplay.values())
            {
                threshKeys.add(snowDisp.getXmlKey());
            }
        }
        else if (dataUsage == DataUsageKey.MONITOR)
        {
            for (SnowMonitor snowMon : SnowMonitor.values())
            {
                threshKeys.add(snowMon.getXmlKey());
            }
        }
        
        return threshKeys;
    }

	@Override
	public MonitorConfigurationManager getAreaConfigMgr() {
        if (areaConfigMgr == null) {
            LocalizationManager mgr = LocalizationManager.getInstance();
            String siteScope = mgr.getCurrentSite();
                  
            areaConfigMgr = SnowMonitorConfigurationManager.getInstance();
            areaConfigMgr.readConfigXml(siteScope);
        }
        return areaConfigMgr;
	}

//    /**
//     * Get the path where the display thresholds XML files are contained.
//     * @return File path.
//     */
//    @Override
//    public String getDisplayThresholdPath()
//    {
//        String fs = String.valueOf(File.separatorChar);
//        StringBuilder sb = new StringBuilder();
//        
//        sb.append("snow").append(fs);
//        sb.append("threshold").append(fs);
//        sb.append("display").append(fs);        
//        
//        return sb.toString();
//    }
//    
//    /**
//     * Get the path where the monitor thresholds XML files are contained.
//     * @return File path.
//     */
//    @Override
//    public String getMonitorThresholdPath()
//    {
//        String fs = String.valueOf(File.separatorChar);
//        StringBuilder sb = new StringBuilder();
//        
//        sb.append("snow").append(fs);
//        sb.append("threshold").append(fs);
//        sb.append("monitor").append(fs);        
//        
//        return sb.toString();
//    }
//    
//    /**
//     * Get the path where the XML file containing the user selected
//     * default file is located.
//     * @return The path of the user selected default file XML.
//     */
//    @Override
//    public String getDefaultThresholdFilePath()
//    {
//        String fs = String.valueOf(File.separatorChar);
//        StringBuilder sb = new StringBuilder();
//        
//        sb.append("snow").append(fs);
//        sb.append("threshold").append(fs);
//        sb.append("display").append(fs);
//        sb.append("defaultThresh").append(fs);
//        
//        return sb.toString();
//    }
}
