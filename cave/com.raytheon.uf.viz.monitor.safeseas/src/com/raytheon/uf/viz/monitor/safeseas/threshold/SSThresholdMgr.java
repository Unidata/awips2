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

import com.raytheon.uf.common.monitor.config.MonitorConfigurationManager;
import com.raytheon.uf.common.monitor.config.SSMonitorConfigurationManager;
import com.raytheon.uf.common.monitor.data.ObConst.DataUsageKey;
import com.raytheon.uf.viz.core.localization.LocalizationManager;
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
 * Feb 03, 2014 #2757      skorolev     Fixed reInitialize()
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 */
public class SSThresholdMgr extends AbstractThresholdMgr {
    private static SSThresholdMgr classInstance;

    private SSThresholdMgr() {
        super("DefaultSSDisplayThresholds.xml",
                "DefaultSSMonitorThresholds.xml", "safeseas");

        areaConfigMgr = getAreaConfigMgr();
        init(); // call init() after areaConfigMgr is set
    }

    public static SSThresholdMgr getInstance() {
        if (classInstance == null) {
            classInstance = new SSThresholdMgr();
        }

        return classInstance;
    }

    /**
     * DR#11279: When monitor area configuration is changed, threshold manager
     * needs to be re-initialized using the new monitor area configuration
     */
    public static void reInitialize() {
        if (classInstance != null) {
            classInstance = null;
        }
        // Update threshold file.
        classInstance = new SSThresholdMgr();
        classInstance.loadDefaultMonitorThreshold();
        classInstance.saveMonitorThresholds();
    }

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

    @Override
    public MonitorConfigurationManager getAreaConfigMgr() {
        if (areaConfigMgr == null) {
            LocalizationManager mgr = LocalizationManager.getInstance();
            String siteScope = mgr.getCurrentSite();

            areaConfigMgr = SSMonitorConfigurationManager.getInstance();
            areaConfigMgr.readConfigXml(siteScope);
        }
        return areaConfigMgr;
    }

    // /**
    // * Get the path where the display thresholds XML files are contained.
    // * @return File path.
    // */
    // @Override
    // public String getDisplayThresholdPath()
    // {
    // String fs = String.valueOf(File.separatorChar);
    // StringBuilder sb = new StringBuilder();
    //
    // sb.append("safeseas").append(fs);
    // sb.append("threshold").append(fs);
    // sb.append("display").append(fs);
    //
    // return sb.toString();
    // }
    //
    // /**
    // * Get the path where the monitor thresholds XML files are contained.
    // * @return File path.
    // */
    // @Override
    // public String getMonitorThresholdPath()
    // {
    // String fs = String.valueOf(File.separatorChar);
    // StringBuilder sb = new StringBuilder();
    //
    // sb.append("safeseas").append(fs);
    // sb.append("threshold").append(fs);
    // sb.append("monitor").append(fs);
    //
    // return sb.toString();
    // }
    //
    // /**
    // * Get the path where the XML file containing the user selected
    // * default file is located.
    // * @return The path of the user selected default file XML.
    // */
    // @Override
    // public String getDefaultThresholdFilePath()
    // {
    // String fs = String.valueOf(File.separatorChar);
    // StringBuilder sb = new StringBuilder();
    //
    // sb.append("safeseas").append(fs);
    // sb.append("threshold").append(fs);
    // sb.append("display").append(fs);
    // sb.append("defaultThresh").append(fs);
    //
    // return sb.toString();
    // }
}
