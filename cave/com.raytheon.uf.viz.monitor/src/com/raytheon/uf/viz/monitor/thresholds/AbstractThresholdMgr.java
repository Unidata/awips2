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
package com.raytheon.uf.viz.monitor.thresholds;

import org.apache.commons.lang.StringUtils;

import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.LocalizationUtil;
import com.raytheon.uf.common.localization.exception.LocalizationException;
import com.raytheon.uf.common.monitor.config.FSSObsMonitorConfigurationManager;
import com.raytheon.uf.common.monitor.config.ThresholdMgr;
import com.raytheon.uf.common.monitor.data.CommonConfig.AppName;
import com.raytheon.uf.common.monitor.data.MonitorConfigConstants;
import com.raytheon.uf.common.monitor.data.ObConst;
import com.raytheon.uf.common.monitor.data.ObConst.DataUsageKey;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.viz.core.localization.LocalizationManager;
import com.raytheon.uf.viz.monitor.config.CommonTableConfig.CellType;
import com.raytheon.uf.viz.monitor.filename.DefaultFilenameMgr;

/**
 * Abstract threshold manager class used for FOG, SNOW, and SAFESEAS.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Jan 05, 2010  3963     lvenable  Initial creation
 * Mar 22, 2010  4282     zhao      obtain zone IDs from
 *                                  monitoring-area-config-manager
 * Feb 16, 2011  7346     zhao      added
 *                                  getDirectionalThresholdValueCellType(...)
 * Apr 28, 2014  3086     skorolev  Updated getAreaConfigMgr method.
 * Sep 18, 2015  3873     skorolev  Added getCfgMgr().
 * Dec 26, 2015  5114     skorolev  Corrected imports and file path for default
 *                                  thresholds.
 * May 07, 2019  7689     randerso  Code cleanup
 * May 21, 2019  7689     randerso  Refactor handling of FSSObs thresholds
 *
 * </pre>
 *
 * @author lvenable
 */
public abstract class AbstractThresholdMgr {

    private final IUFStatusHandler statusHandler = UFStatus
            .getHandler(AbstractThresholdMgr.class);

    /**
     * Monitor Area Configuration Manager.
     */
    public FSSObsMonitorConfigurationManager areaConfigMgr;

    /**
     * Default file name for the display thresholds.
     */
    private String defDisplayThreshName = "Unknown";

    /**
     * Default file name for the monitor thresholds.
     */
    private String defMonitorThreshName = "Unknown";

    /**
     * Application name that will be the starting path for the XML data.
     */
    private final AppName appName;

    /**
     * Full path and file name for the current FOG display file.
     */
    private String currDisplayXmlFileName;

    /**
     * Full path and file name for the current FOG monitor file.
     */
    private String currMonitorXmlFileName;

    /**
     * Display threshold manager.
     */
    private ThresholdMgr displayThreshMgr;

    /**
     * Monitor threshold manager.
     */
    private ThresholdMgr monitorThreshMgr;

    /**
     * Manager of the XML containing the name of the user specified default
     * display threshold.
     */
    private DefaultFilenameMgr defaultFileNameMgr;

    /**
     * Threshold key to determine if the value retrieved is red or yellow.
     */
    public static enum ThresholdKey {
        RED,
        YELLOW
    };

    /** current site **/
    protected String site;

    /** threshold usage **/
    protected DataUsageKey dataUsageKey;

    /**
     * Constructor.
     *
     * @param defDisplayThreshName
     * @param defMonitorThreshName
     * @param appName
     */
    public AbstractThresholdMgr(String defDisplayThreshName,
            String defMonitorThreshName, AppName appName) {
        this.defDisplayThreshName = defDisplayThreshName;
        this.defMonitorThreshName = defMonitorThreshName;
        this.appName = appName;
        this.site = LocalizationManager.getInstance().getCurrentSite();
        this.areaConfigMgr = getCfgMgr();
    }

    /**
     * Setup the default threshold file manager that stores the file name to use
     * for the display thresholds. If the file name is blank or null then the
     * file name to use is the default file name.
     */
    protected void init() {
        defaultFileNameMgr = new DefaultFilenameMgr(
                getDefaultThresholdFilePath());
        defaultFileNameMgr.readXmlConfig();

        String fileName = defaultFileNameMgr.getDefaultThresholdFilename()
                .trim();

        loadDisplayThreshold(fileName);

        /*
         * Setup the monitor threshold manager
         */
        this.currMonitorXmlFileName = defMonitorThreshName;

        try {
            monitorThreshMgr = new ThresholdMgr(appName, DataUsageKey.MONITOR,
                    this.currMonitorXmlFileName);
        } catch (Exception e) {
            statusHandler.error(String.format(
                    "Error loading monitor thresholds for %s", appName), e);
        }
    }

    /**
     * Get the threshold value.
     *
     * @param dataUsage
     *            Display/Monitor key.
     * @param thresholdKey
     *            Red/Yellow key.
     * @param areaID
     *            Specified area ID.
     * @param key
     *            Key to determine which red/yellow value to get.
     * @return The threshold value.
     */
    public double getThresholdValue(DataUsageKey dataUsage,
            ThresholdKey thresholdKey, String areaID, String key) {
        if (dataUsage == DataUsageKey.DISPLAY) {
            if (thresholdKey == ThresholdKey.RED) {
                return displayThreshMgr.getRedValue(areaID, key);
            } else if (thresholdKey == ThresholdKey.YELLOW) {
                return displayThreshMgr.getYellowValue(areaID, key);
            }
        } else if (dataUsage == DataUsageKey.MONITOR) {
            if (thresholdKey == ThresholdKey.RED) {
                return monitorThreshMgr.getRedValue(areaID, key);
            } else if (thresholdKey == ThresholdKey.YELLOW) {
                return monitorThreshMgr.getYellowValue(areaID, key);
            }
        }
        return 0;
    }

    /**
     * Set the threshold value.
     *
     * @param dataUsage
     *            Display/Monitor key.
     * @param thresholdKey
     *            Red/Yellow key.
     * @param areaID
     *            Specified area ID.
     * @param key
     *            Key to determine which red/yellow value to get.
     * @param value
     *            Value to set the red/yellow threshold to.
     */
    public void setThresholdValue(DataUsageKey dataUsage,
            ThresholdKey thresholdKey, String areaID, String key,
            double value) {
        if (dataUsage == DataUsageKey.DISPLAY) {
            if (thresholdKey == ThresholdKey.RED) {
                displayThreshMgr.setRedValue(areaID, key, value);
            } else if (thresholdKey == ThresholdKey.YELLOW) {
                displayThreshMgr.setYellowValue(areaID, key, value);
            }
        } else if (dataUsage == DataUsageKey.MONITOR) {
            if (thresholdKey == ThresholdKey.RED) {
                monitorThreshMgr.setRedValue(areaID, key, value);
            } else if (thresholdKey == ThresholdKey.YELLOW) {
                monitorThreshMgr.setYellowValue(areaID, key, value);
            }
        }
    }

    /**
     * Gets Threshold Value Cell Type
     *
     * @param dataUsage
     * @param areaID
     * @param key
     * @param value
     * @return the cell type
     */
    public CellType getThresholdValueCellType(DataUsageKey dataUsage,
            String areaID, String key, double value) {
        if (Double.isNaN(value)) {
            return CellType.NotMonitored;
        }
        if (value == ObConst.MISSING) {
            return CellType.NotAvailable;
        }
        double red = Double.NaN;
        double yellow = Double.NaN;

        if (dataUsage == DataUsageKey.DISPLAY) {
            red = displayThreshMgr.getRedValue(areaID, key);
            yellow = displayThreshMgr.getYellowValue(areaID, key);
            return calcCellType(key, red, yellow, value);
        } else if (dataUsage == DataUsageKey.MONITOR) {
            red = monitorThreshMgr.getRedValue(areaID, key);
            yellow = monitorThreshMgr.getYellowValue(areaID, key);
            return calcCellType(key, red, yellow, value);
        }
        return CellType.NotMonitored;
    }

    /**
     * Determines Table Cell Type for a directional variable such as Wind
     * Direction or Swell Direction
     *
     * @param dataUsage
     * @param areaID
     * @param keyFrom
     * @param keyTo
     * @param value
     * @return Cell Type
     */
    public CellType getDirectionalThresholdValueCellType(DataUsageKey dataUsage,
            String areaID, String keyFrom, String keyTo, double value) {
        if (Double.isNaN(value)) {
            return CellType.NotMonitored;
        }
        if (value == ObConst.MISSING) {
            return CellType.NotAvailable;
        }

        double redFrom = Double.NaN;
        double redTo = Double.NaN;
        double yellowFrom = Double.NaN;
        double yellowTo = Double.NaN;

        if (dataUsage == DataUsageKey.DISPLAY) {
            redFrom = displayThreshMgr.getRedValue(areaID, keyFrom);
            redTo = displayThreshMgr.getRedValue(areaID, keyTo);
            yellowFrom = displayThreshMgr.getYellowValue(areaID, keyFrom);
            yellowTo = displayThreshMgr.getYellowValue(areaID, keyTo);
            return calcDirectionalCellType(redFrom, redTo, yellowFrom, yellowTo,
                    value);
        } else if (dataUsage == DataUsageKey.MONITOR) {
            redFrom = monitorThreshMgr.getRedValue(areaID, keyFrom);
            redTo = monitorThreshMgr.getRedValue(areaID, keyTo);
            yellowFrom = monitorThreshMgr.getYellowValue(areaID, keyFrom);
            yellowTo = monitorThreshMgr.getYellowValue(areaID, keyTo);
            return calcDirectionalCellType(redFrom, redTo, yellowFrom, yellowTo,
                    value);
        }
        return CellType.NotMonitored;
    }

    /**
     * Calculates Table Cell Type for a directional variable.
     *
     * @param redFrom
     * @param redTo
     * @param yellowFrom
     * @param yellowTo
     * @param value
     * @return
     */
    private CellType calcDirectionalCellType(double redFrom, double redTo,
            double yellowFrom, double yellowTo, double value) {

        if (redFrom < redTo) {
            if (value > redFrom && value < redTo) {
                return CellType.R;
            }
        }
        if (redFrom > redTo) {
            if (value > redFrom || value < redTo) {
                return CellType.R;
            }
        }
        if (yellowFrom < yellowTo) {
            if (value > yellowFrom && value < yellowTo) {
                return CellType.Y;
            }
        }
        if (yellowFrom > yellowTo) {
            if (value > yellowFrom || value < yellowTo) {
                return CellType.Y;
            }
        }
        return CellType.G;
    }

    /**
     * Calculates CellType.
     *
     * @param key
     * @param red
     * @param yellow
     * @param value
     * @return
     */
    private CellType calcCellType(String key, double red, double yellow,
            double value) {
        Boolean redIsHigher = MonitorConfigConstants.rValueIsHigher(key,
                appName);
        // SK. Take integer part of data for threshold comparison to avoid color
        // confusion.
        // To be consistent with table display for which Math.round() is used,
        // [zhao 10/06/2011]
        value = Math.round(value);

        if (redIsHigher == null) {
            if (red <= yellow) {
                if (value <= red) {
                    return CellType.R;
                } else if (value <= yellow) {
                    return CellType.Y;
                }
                return CellType.G;
            } else {
                if (value < yellow) {
                    return CellType.G;
                } else if (value <= red) {
                    return CellType.Y;
                }
                return CellType.R;
            }
        } else if (redIsHigher) {
            if (value < yellow) {
                return CellType.G;
            } else if (value < red) {
                return CellType.Y;
            }
            return CellType.R;
        } else if (!redIsHigher) {
            if (value <= red) {
                return CellType.R;
            } else if (value <= yellow) {
                return CellType.Y;
            }
            return CellType.G;
        }
        return CellType.NotMonitored;
    }

    /**
     * Set the default display threshold file name.
     *
     * @param fileName
     *            File name.
     */
    public void setDefaultDisplayFileName(String fileName) {
        if (fileName == null) {
            defaultFileNameMgr.setDefaultThresholdFilename("");
            currDisplayXmlFileName = defDisplayThreshName;
            return;
        }
        if (!fileName.endsWith(".xml")) {
            fileName += ".xml";
        }
        if (fileName.trim().equals(defDisplayThreshName)) {
            defaultFileNameMgr.setDefaultThresholdFilename("");
        } else {
            defaultFileNameMgr.setDefaultThresholdFilename(fileName);
        }
    }

    /**
     * Load the display threshold.
     *
     * @param fileName
     *            The file name.
     */
    public void loadDisplayThreshold(String fileName) {

        if (StringUtils.isNotEmpty(fileName)) {
            try {
                this.currDisplayXmlFileName = fileName.trim();
                displayThreshMgr = new ThresholdMgr(appName,
                        DataUsageKey.DISPLAY, this.currDisplayXmlFileName);
                return;
            } catch (Exception e) {
                statusHandler.error(String.format(
                        "Error loading display thresholds for %s. Reverting to default thresholds",
                        appName), e);
            }
        }

        loadDefaultDisplayThreshold();

    }

    /**
     * Load the default display thresholds.
     */
    public void loadDefaultDisplayThreshold() {
        this.currDisplayXmlFileName = this.defDisplayThreshName;
        try {
            displayThreshMgr = new ThresholdMgr(appName, DataUsageKey.DISPLAY,
                    this.currDisplayXmlFileName);
        } catch (Exception e) {
            statusHandler.fatal(String.format(
                    "Error loading default display thresholds for %s.",
                    appName), e);
        }
    }

    /**
     * Save the display thresholds to the specified XML file name.
     *
     * @param newFileName
     *            NEw XML file name.
     */
    public void saveAsDisplayThresholds(String newFileName) {
        if (newFileName.trim().equals(defDisplayThreshName)) {
            return;
        }
        displayThreshMgr.saveThresholds(newFileName);
    }

    /**
     * Save the monitor thresholds.
     */
    public void saveMonitorThresholds() {
        monitorThreshMgr.saveThresholds(currMonitorXmlFileName);
    }

    /**
     * Get the default threshold file name for the display/monitor.
     *
     * @param usageKey
     *            Display or Monitor key.
     * @return The default XML file name.
     */
    public String getDefaultFileName(DataUsageKey usageKey) {
        /*
         * If data key is Display then return the default display threshold file
         * name.
         */
        if (usageKey == DataUsageKey.DISPLAY) {
            return defDisplayThreshName;
        }
        // Return the Monitor threshold file name as the default.
        return defMonitorThreshName;
    }

    /**
     * Get the default display file name. This file is specified by the user as
     * to which display threshold file will be used by default.
     *
     * @return The default display threshold file name.
     */
    public String getDefDisplayThreshFileName() {
        return defaultFileNameMgr.getDefaultThresholdFilename();
    }

    /**
     * Gets the path where the display thresholds XML files are contained.
     *
     * @param usage
     *
     * @return File path.
     */
    public String getThresholdPath(DataUsageKey usage) {
        ThresholdMgr threshMgr = null;
        if (usage.equals(DataUsageKey.DISPLAY)) {
            threshMgr = displayThreshMgr;
        } else {
            threshMgr = monitorThreshMgr;
        }

        return threshMgr.getThresholdDir();
    }

    /**
     * Gets the path where the XML file containing the user selected default
     * file is located.
     *
     * @return The path of the user selected default file XML.
     */
    public String getDefaultThresholdFilePath() {
        return LocalizationUtil.join(appName.name().toLowerCase(), "threshold",
                "display", "defaultThresh");
    }

    /**
     * Deletes File.
     *
     * @param fileName
     * @return true if deleted
     */
    public boolean deleteFile(LocalizationFile fileName) {
        boolean deletedUserSelectedDefault = false;
        String fileNameStr = fileName.getFile().getName();

        /*
         * Check if the file to be deleted is the user selected default
         * threshold file name. If so then set the user default threshold file
         * name to "" and load in the base default values.
         */
        if (fileNameStr
                .equals(defaultFileNameMgr.getDefaultThresholdFilename())) {
            defaultFileNameMgr.setDefaultThresholdFilename("");
            loadDefaultDisplayThreshold();
            deletedUserSelectedDefault = true;
        }
        /*
         * Delete the file.
         */
        try {
            fileName.delete();
        } catch (LocalizationException e) {
            statusHandler.error("Error deleteing " + fileName, e);
        }
        return deletedUserSelectedDefault;
    }

    /**
     * Gets Monitor Area Configuration manager.
     *
     * @return manager
     */
    public FSSObsMonitorConfigurationManager getCfgMgr() {
        FSSObsMonitorConfigurationManager mgr = null;
        switch (this.appName) {
        case FOG:
            mgr = FSSObsMonitorConfigurationManager.getInstance(AppName.FOG);
            break;
        case SAFESEAS:
            mgr = FSSObsMonitorConfigurationManager
                    .getInstance(AppName.SAFESEAS);
            break;
        case SNOW:
            mgr = FSSObsMonitorConfigurationManager.getInstance(AppName.SNOW);
            break;
        default:
            statusHandler
                    .error("Unable to handle unknown appName: " + this.appName);
            break;
        }
        return mgr;
    }

    /**
     * @return the dataUsageKey
     */
    public DataUsageKey getDataUsageKey() {
        return dataUsageKey;
    }

    /**
     * @param dataUsageKey
     */
    public void setDataUsageKey(DataUsageKey dataUsageKey) {
        this.dataUsageKey = dataUsageKey;
    }
}
