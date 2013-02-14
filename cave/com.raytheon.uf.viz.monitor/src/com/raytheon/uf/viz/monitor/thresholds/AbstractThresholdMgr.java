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

import java.io.File;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.monitor.config.MonitorConfigurationManager;
import com.raytheon.uf.common.monitor.data.CommonTableConfig.CellType;
import com.raytheon.uf.common.monitor.data.ObConst;
import com.raytheon.uf.common.monitor.data.ObConst.DataUsageKey;
import com.raytheon.uf.viz.monitor.filename.DefaultFilenameMgr;
import com.raytheon.uf.viz.monitor.util.MonitorConfigConstants;
import com.raytheon.uf.viz.monitor.xml.AreaThresholdXML;
import com.raytheon.uf.viz.monitor.xml.AreaXML;
import com.raytheon.uf.viz.monitor.xml.ThresholdsXML;

/**
 * Abstract threshold manager class used for FOG, SNOW, and SAFESEAS.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 5, 2010  #3963      lvenable     Initial creation
 * Mar 22, 2010 #4282      zhao         obtain zone IDs from monitoring-area-config-manager
 * Feb 16, 2011 #7346      zhao         added getDirectionalThresholdValueCellType(...)
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 */
public abstract class AbstractThresholdMgr {
    protected MonitorConfigurationManager areaConfigMgr = null;

    /**
     * Default file name for the FOG display thresholds.
     */
    private String defDisplayThreshName = "Unknown";

    /**
     * Default file name for the FOG monitor thresholds.
     */
    private String defMonitorThreshName = "Unknown";

    /**
     * Application name that will be the starting path for the XML data.
     */
    private final String appName;

    /**
     * Full path and file name for the current FOG display file.
     */
    private String currFullDisplayXmlFileName;

    /**
     * Full path and file name for the current FOG monitor file.
     */
    private String currFullMonitorXmlFileName;

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
        RED, YELLOW
    };

    /*
     * TODO : remove this when debugging is complete
     */
    public ThresholdsXML threshXmlCopy;

    /**
     * 
     * @param defDisplayThreshName
     * @param defMonitorThreshName
     */
    public AbstractThresholdMgr(String defDisplayThreshName,
            String defMonitorThreshName, String appName) {
        this.defDisplayThreshName = defDisplayThreshName;
        this.defMonitorThreshName = defMonitorThreshName;
        this.appName = appName;
    }

    protected void init() {
        /*
         * Setup the default threshold file manager that stores the file name to
         * use for the display thresholds. If the file name is blank or null
         * then the file name to use is the default file name.
         */
        defaultFileNameMgr = new DefaultFilenameMgr(
                getDefaultThresholdFilePath());
        defaultFileNameMgr.readXmlConfig();

        /*
         * Setup the Fog display threshold manager
         */
        if (defaultFileNameMgr.getDefaultThresholdFilename() != null
                && defaultFileNameMgr.getDefaultThresholdFilename().length() > 0) {
            boolean fileNameValid = validateFileName(getDisplayThresholdPath()
                    + defaultFileNameMgr.getDefaultThresholdFilename());

            if (defaultFileNameMgr.getDefaultThresholdFilename().compareTo(
                    defDisplayThreshName) == 0
                    || fileNameValid == false) {
                setDefaultDisplayFileName(null);
                displayThreshMgr = new ThresholdMgr(currFullDisplayXmlFileName);
                loadDefaultDisplayThreshold();
            } else {
                currFullDisplayXmlFileName = getDisplayThresholdPath()
                        + defaultFileNameMgr.getDefaultThresholdFilename();
                displayThreshMgr = new ThresholdMgr(currFullDisplayXmlFileName);
                displayThreshMgr.readThresholdXml();
            }
        } else {
            currFullDisplayXmlFileName = getDisplayThresholdPath()
                    + defDisplayThreshName;
            displayThreshMgr = new ThresholdMgr(currFullDisplayXmlFileName);
            loadDefaultDisplayThreshold();
        }

        /*
         * Setup the Fog monitor threshold manager
         */
        currFullMonitorXmlFileName = getMonitorThresholdPath()
                + defMonitorThreshName;

        monitorThreshMgr = new ThresholdMgr(currFullMonitorXmlFileName);
        if (validateFileName(currFullMonitorXmlFileName) == true) {
            monitorThreshMgr.readThresholdXml();
        } else {
            loadDefaultMonitorThreshold();
        }
    }

    private boolean validateFileName(String pathAndFileName) {
        IPathManager pm = PathManagerFactory.getPathManager();
        LocalizationContext context = pm.getContext(
                LocalizationType.CAVE_STATIC, LocalizationLevel.SITE);
        LocalizationFile locFile = pm.getLocalizationFile(context,
                pathAndFileName);

        System.out.println("--- validate path = "
                + locFile.getFile().getAbsolutePath());

        return locFile.getFile().exists();
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
            ThresholdKey thresholdKey, String areaID, String key, double value) {
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

    public CellType getThresholdValueCellType(DataUsageKey dataUsage,
            String areaID, String key, double value) {
        if (hasArea(areaID, dataUsage) == false) {
            return CellType.NotMonitored;
        }

        if (Double.isNaN(value) == true) {
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
    public CellType getDirectionalThresholdValueCellType(
            DataUsageKey dataUsage, String areaID, String keyFrom,
            String keyTo, double value) {
        if (hasArea(areaID, dataUsage) == false) {
            return CellType.NotMonitored;
        }

        if (Double.isNaN(value) == true) {
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
            return calcDirectionalCellType(redFrom, redTo, yellowFrom,
                    yellowTo, value);
        } else if (dataUsage == DataUsageKey.MONITOR) {
            redFrom = monitorThreshMgr.getRedValue(areaID, keyFrom);
            redTo = monitorThreshMgr.getRedValue(areaID, keyTo);
            yellowFrom = monitorThreshMgr.getYellowValue(areaID, keyFrom);
            yellowTo = monitorThreshMgr.getYellowValue(areaID, keyTo);
            return calcDirectionalCellType(redFrom, redTo, yellowFrom,
                    yellowTo, value);
        }

        return CellType.NotMonitored;
    }

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

    private CellType calcCellType(String key, double red, double yellow,
            double value) {
        Boolean redIsHigher = MonitorConfigConstants.rValueIsHigher(key,
                appName);
        // SK. Take integer part of data for threshold comparison to avoid color
        // confusion.
        // To be consistent with table display for which Math.round() is used,
        // [zhao 10/06/2011]
        value = Math.round(new Float(value));

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
        } else if (redIsHigher == true) {
            if (value < yellow) {
                return CellType.G;
            } else if (value < red) {
                return CellType.Y;
            }

            return CellType.R;
        } else if (redIsHigher == false) {
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
            currFullDisplayXmlFileName = getDisplayThresholdPath()
                    + defDisplayThreshName;
            return;
        }

        if (fileName.endsWith(".xml") == false) {
            fileName.concat(".xml");
        }

        if (fileName.compareTo(defDisplayThreshName) == 0) {
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
    public void loadDisplayThreashold(String fileName) {
        if (fileName.trim().compareTo(defDisplayThreshName) == 0) {
            loadDefaultDisplayThreshold();
            return;
        }

        currFullDisplayXmlFileName = getDisplayThresholdPath() + fileName;

        displayThreshMgr.setFullPathFileName(currFullDisplayXmlFileName);
        displayThreshMgr.readThresholdXml();
    }

    /**
     * Load the default display thresholds.
     */
    public void loadDefaultDisplayThreshold() {
        String filename = defaultFileNameMgr.getDefaultThresholdFilename();

        if (filename == null || filename.trim().length() == 0) {
            currFullDisplayXmlFileName = getDisplayThresholdPath()
                    + defDisplayThreshName;

            List<String> areaIDs = null;

            try {
                areaIDs = areaConfigMgr.getAreaList();
            } catch (Exception e) {
                e.printStackTrace();
                return;
            }

            // Sort the area IDs
            Collections.sort(areaIDs);

            ArrayList<String> threshKeys = getThresholdKeys(DataUsageKey.DISPLAY);

            System.out.println("---- " + currFullDisplayXmlFileName);

            displayThreshMgr.createConfigFromDefaults(
                    currFullDisplayXmlFileName, areaIDs, threshKeys);
        } else {
            currFullDisplayXmlFileName = getDisplayThresholdPath() + filename;
            displayThreshMgr.setFullPathFileName(currFullDisplayXmlFileName);
            displayThreshMgr.readThresholdXml();
        }
    }

    public void loadDefaultMonitorThreshold() {
        currFullMonitorXmlFileName = getMonitorThresholdPath()
                + defMonitorThreshName;

        List<String> areaIDs = null;

        try {
            areaIDs = areaConfigMgr.getAreaList();
        } catch (Exception e) {
            e.printStackTrace();
            return;
        }

        // Sort the area IDs
        Collections.sort(areaIDs);

        ArrayList<String> threshKeys = getThresholdKeys(DataUsageKey.MONITOR);

        System.out.println("********** currFullMonitorXmlFileName = "
                + currFullMonitorXmlFileName);

        monitorThreshMgr.createConfigFromDefaults(currFullMonitorXmlFileName,
                areaIDs, threshKeys);
    }

    /**
     * Save the FOG display thresholds to the specified XML file name.
     * 
     * @param newFileName
     *            NEw XML file name.
     */
    public void saveAsDisplayThresholds(String newFileName) {
        if (newFileName.trim().compareTo(defDisplayThreshName) == 0) {
            return;
        }

        currFullDisplayXmlFileName = getDisplayThresholdPath() + newFileName;

        displayThreshMgr.setFullPathFileName(currFullDisplayXmlFileName);
        displayThreshMgr.saveThresholdXml();
    }

    /**
     * Save the monitor thresholds.
     */
    public void saveMonitorThresholds() {
        monitorThreshMgr.saveThresholdXml();
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

    public ThresholdsXML getThresholdsXmlData(DataUsageKey usageKey) {
        if (usageKey == DataUsageKey.DISPLAY) {
            return displayThreshMgr.getThresholdXML();
        }

        return monitorThreshMgr.getThresholdXML();
    }

    /**
     * Get the path where the display thresholds XML files are contained.
     * 
     * @return File path.
     */
    public String getDisplayThresholdPath() {
        String fs = String.valueOf(File.separatorChar);
        StringBuilder sb = new StringBuilder();

        sb.append(appName).append(fs);
        sb.append("threshold").append(fs);
        sb.append("display").append(fs);

        return sb.toString();
    }

    /**
     * Get the path where the monitor thresholds XML files are contained.
     * 
     * @return File path.
     */
    public String getMonitorThresholdPath() {
        String fs = String.valueOf(File.separatorChar);
        StringBuilder sb = new StringBuilder();

        sb.append(appName).append(fs);
        sb.append("threshold").append(fs);
        sb.append("monitor").append(fs);

        return sb.toString();
    }

    /**
     * Get the path where the XML file containing the user selected default file
     * is located.
     * 
     * @return The path of the user selected default file XML.
     */
    public String getDefaultThresholdFilePath() {
        String fs = String.valueOf(File.separatorChar);
        StringBuilder sb = new StringBuilder();

        sb.append(appName).append(fs);
        sb.append("threshold").append(fs);
        sb.append("display").append(fs);
        sb.append("defaultThresh").append(fs);

        return sb.toString();
    }

    public ThresholdsXML getThresholdsXmlCopy(DataUsageKey dataUsageKey) {
        if (dataUsageKey == DataUsageKey.DISPLAY) {
            return displayThreshMgr.getThresholdsXmlCopy();
        } else if (dataUsageKey == DataUsageKey.MONITOR) {
            return monitorThreshMgr.getThresholdsXmlCopy();
        }

        return null;
    }

    public boolean deleteFile(LocalizationFile fileName) {
        boolean deletedUserSelectedDefault = false;

        String fileNameStr = fileName.getFile().getName();

        /*
         * Check if the file to be deleted is the user selected default
         * threshold file name. If so then set the user default threshold file
         * name to "" and load in the base default values.
         */
        if (fileNameStr.compareTo(defaultFileNameMgr
                .getDefaultThresholdFilename()) == 0) {
            defaultFileNameMgr.setDefaultThresholdFilename("");
            loadDefaultDisplayThreshold();
            deletedUserSelectedDefault = true;
        }

        /*
         * Delete the file.
         */
        try {
            fileName.delete();
        } catch (Exception e) {
            e.printStackTrace();
        }

        return deletedUserSelectedDefault;
    }

    public boolean hasArea(String areaID, DataUsageKey dataUsageKey) {
        if (dataUsageKey == DataUsageKey.DISPLAY) {
            return displayThreshMgr.getThresholdXML().hasAreaId(areaID);
        }

        return monitorThreshMgr.getThresholdXML().hasAreaId(areaID);
    }

    protected abstract ArrayList<String> getThresholdKeys(
            DataUsageKey dataUsageKey);

    /*
     * 
     * 
     * 
     * 
     * 
     * 
     * TODO : USED for debugging purposes...
     */
    public void printDisplayThresholds() {
        ThresholdsXML displayXML = displayThreshMgr.getThresholdXML();

        ArrayList<AreaXML> areasArray = displayXML.getAreas();

        for (AreaXML area : areasArray) {
            System.out.println("--- " + area.getAreaId());

            ArrayList<AreaThresholdXML> atXmlArray = area.getAreaThresholds();

            for (AreaThresholdXML atXml : atXmlArray) {
                System.out.println("******    " + atXml.getKey());
                System.out.println("     R    " + atXml.getRed());
                System.out.println("     Y    " + atXml.getYellow());
            }
        }
    }

    public void printDisplayThresholdsXMLCopy() {
        if (threshXmlCopy == null) {
            threshXmlCopy = displayThreshMgr.getThresholdsXmlCopy();
        }

        ArrayList<AreaXML> areasArray = threshXmlCopy.getAreas();

        for (AreaXML area : areasArray) {
            System.out.println("--- " + area.getAreaId());

            ArrayList<AreaThresholdXML> atXmlArray = area.getAreaThresholds();

            for (AreaThresholdXML atXml : atXmlArray) {
                System.out.println("******    " + atXml.getKey());
                System.out.println("     R    " + atXml.getRed());
                System.out.println("     Y    " + atXml.getYellow());
            }
        }
    }

    public abstract MonitorConfigurationManager getAreaConfigMgr();

}
