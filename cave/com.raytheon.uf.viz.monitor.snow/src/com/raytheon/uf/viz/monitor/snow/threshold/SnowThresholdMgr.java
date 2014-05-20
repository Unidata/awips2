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

import com.raytheon.uf.common.monitor.config.FSSObsMonitorConfigurationManager;
import com.raytheon.uf.common.monitor.config.FSSObsMonitorConfigurationManager.MonName;
import com.raytheon.uf.common.monitor.data.CommonConfig.AppName;
import com.raytheon.uf.common.monitor.data.ObConst.DataUsageKey;
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
 * May 21, 2014  3086      skorolev     Cleaned code
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 */
public class SnowThresholdMgr extends AbstractThresholdMgr {
    private static SnowThresholdMgr classInstance;

    /**
     * Constructor
     */
    private SnowThresholdMgr() {
        super("DefaultSnowDisplayThresholds.xml",
                "DefaultSnowMonitorThresholds.xml", AppName.SNOW.name()
                        .toLowerCase());

        areaConfigMgr = new FSSObsMonitorConfigurationManager(site,
                MonName.snow.name());
        init();
    }

    /**
     * Gets instance.
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
     * Re-initialization of threshold manager.
     * 
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
     * @see com.raytheon.uf.viz.monitor.thresholds.AbstractThresholdMgr#
     * getMonitorAreaConfigInstance()
     */
    @Override
    protected FSSObsMonitorConfigurationManager getMonitorAreaConfigInstance() {
        if (areaConfigMgr == null) {
            areaConfigMgr = new FSSObsMonitorConfigurationManager(site,
                    MonName.snow.name());
        }
        return areaConfigMgr;
    }
}
