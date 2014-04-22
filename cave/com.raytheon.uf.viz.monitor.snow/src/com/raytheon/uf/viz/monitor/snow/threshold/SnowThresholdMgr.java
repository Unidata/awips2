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
 * Feb 03, 2014 #2757      skorolev     Fixed reInitialize()
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 */
public class SnowThresholdMgr extends AbstractThresholdMgr {
    private static SnowThresholdMgr classInstance;

    private SnowThresholdMgr() {
        super("DefaultSnowDisplayThresholds.xml",
                "DefaultSnowMonitorThresholds.xml", "snow");

        areaConfigMgr = getAreaConfigMgr();
        // call init() after areaConfigMgr is set
        init();
    }

    /**
     * Get instance.
     * 
     * @return
     */
    public static synchronized SnowThresholdMgr getInstance() {
        if (classInstance == null) {
            classInstance = new SnowThresholdMgr();
        }

        return classInstance;
    }

    /**
     * DR#11279: When monitor area configuration is changed, threshold manager
     * needs to be re-initialized using the new monitor area configuration
     */
    public static synchronized void reInitialize() {
        if (classInstance != null) {
            classInstance = null;
        }
        // Update threshold file.
        classInstance = new SnowThresholdMgr();
        classInstance.loadDefaultMonitorThreshold();
        classInstance.saveMonitorThresholds();
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.monitor.thresholds.AbstractThresholdMgr#getThresholdKeys
     * (com.raytheon.uf.common.monitor.data.ObConst.DataUsageKey)
     */
    @Override
    protected ArrayList<String> getThresholdKeys(DataUsageKey dataUsage) {
        ArrayList<String> threshKeys = new ArrayList<String>();

        if (dataUsage == DataUsageKey.DISPLAY) {
            for (SnowDisplay snowDisp : SnowDisplay.values()) {
                threshKeys.add(snowDisp.getXmlKey());
            }
        } else if (dataUsage == DataUsageKey.MONITOR) {
            for (SnowMonitor snowMon : SnowMonitor.values()) {
                threshKeys.add(snowMon.getXmlKey());
            }
        }

        return threshKeys;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.monitor.thresholds.AbstractThresholdMgr#getAreaConfigMgr
     * ()
     */
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
}
