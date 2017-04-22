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
package com.raytheon.uf.viz.monitor.fog.threshold;

import java.util.ArrayList;

import com.raytheon.uf.common.monitor.data.CommonConfig.AppName;
import com.raytheon.uf.common.monitor.data.MonitorConfigConstants.FogDisplay;
import com.raytheon.uf.common.monitor.data.MonitorConfigConstants.FogMonitor;
import com.raytheon.uf.common.monitor.data.ObConst.DataUsageKey;
import com.raytheon.uf.viz.monitor.thresholds.AbstractThresholdMgr;

/**
 * This class manages the FOG thresholds for display and monitor.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 15, 2009 #3963      lvenable     Initial creation
 * Feb 03, 2014 #2757      skorolev     Fixed reInitialize()
 * May 20, 2014  3086      skorolev     Cleaned code.
 * Sep 04, 2014  3220      skorolev     Removed "site".
 * Sep 18, 2015  3873      skorolev     Adjusted to AppName and removed areaConfigMgr.
 * Dec 26, 2015  5114      skorolev     Corrected imports.
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 */
public class FogThresholdMgr extends AbstractThresholdMgr {
    /**
     * Class instance.
     */
    private static FogThresholdMgr classInstance;

    /**
     * Private constructor.
     */
    private FogThresholdMgr() {
        super("DefaultFogDisplayThresholds.xml",
                "DefaultFogMonitorThresholds.xml", AppName.FOG);
        init();
    }

    /**
     * Gets an instance of the manager class.
     * 
     * @return Class instance.
     */
    public static FogThresholdMgr getInstance() {
        if (classInstance == null) {
            classInstance = new FogThresholdMgr();
        }
        return classInstance;
    }

    /**
     * Re-initialization of threshold manager.
     * 
     * DR#11279: When monitor area configuration is changed, threshold manager
     * needs to be re-initialized using the new monitor area configuration
     * 
     */
    public static void reInitialize() {
        if (classInstance != null) {
            classInstance = null;
        }
        classInstance = new FogThresholdMgr();
        // Update threshold file.
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
            for (FogDisplay fogDisp : FogDisplay.values()) {
                threshKeys.add(fogDisp.getXmlKey());
            }
        } else if (dataUsage == DataUsageKey.MONITOR) {
            for (FogMonitor fogMon : FogMonitor.values()) {
                threshKeys.add(fogMon.getXmlKey());
            }
        }
        return threshKeys;
    }

}
