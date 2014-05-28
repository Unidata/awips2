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
package com.raytheon.uf.viz.monitor.safeseas.threshold;

import java.util.ArrayList;

import com.raytheon.uf.common.monitor.config.FSSObsMonitorConfigurationManager;
import com.raytheon.uf.common.monitor.config.FSSObsMonitorConfigurationManager.MonName;
import com.raytheon.uf.common.monitor.data.CommonConfig.AppName;
import com.raytheon.uf.common.monitor.data.ObConst.DataUsageKey;
import com.raytheon.uf.viz.monitor.thresholds.AbstractThresholdMgr;
import com.raytheon.uf.viz.monitor.util.MonitorConfigConstants.SafeSeasDisplay;
import com.raytheon.uf.viz.monitor.util.MonitorConfigConstants.SafeSeasMonitor;

/**
 * This class manages the SafeSeas thresholds for display and monitor.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 27, 2009 #3963      lvenable     Initial creation
 * Feb 03, 2014 #2757      skorolev     Fixed reInitialize().
 * Apr 28, 2014 3086       skorolev     Removed local getMonitorAreaConfig method.
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 */
public class SSThresholdMgr extends AbstractThresholdMgr {

    private static SSThresholdMgr classInstance;

    /**
     * Private constructor.
     */
    public SSThresholdMgr() {
        super("DefaultSSDisplayThresholds.xml",
                "DefaultSSMonitorThresholds.xml", AppName.SAFESEAS.name()
                        .toLowerCase());
        areaConfigMgr = new FSSObsMonitorConfigurationManager(site,
                MonName.ss.name());
        init();
    }

    /**
     * Get instance.
     * 
     * @return
     */
    public static synchronized SSThresholdMgr getInstance() {
        if (classInstance == null) {
            classInstance = new SSThresholdMgr();
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
        classInstance = new SSThresholdMgr();
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
            for (SafeSeasDisplay ssDisp : SafeSeasDisplay.values()) {
                threshKeys.add(ssDisp.getXmlKey());
            }
        } else if (dataUsage == DataUsageKey.MONITOR) {
            for (SafeSeasMonitor ssMon : SafeSeasMonitor.values()) {
                threshKeys.add(ssMon.getXmlKey());
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
                    MonName.ss.name());
        }
        return areaConfigMgr;
    }
}
