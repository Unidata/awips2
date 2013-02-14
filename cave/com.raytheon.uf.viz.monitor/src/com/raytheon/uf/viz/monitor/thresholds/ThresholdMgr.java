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
import java.util.List;

import javax.xml.bind.JAXB;

import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.monitor.xml.AreaThresholdXML;
import com.raytheon.uf.viz.monitor.xml.AreaXML;
import com.raytheon.uf.viz.monitor.xml.ThresholdsXML;

/**
 * This is a "generic" threshold manager class that handles display and monitor
 * thresholds for FOG, SNOW, and SAFESEAS.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 15, 2009 #3963      lvenable     Initial creation
 * Dec 4,  2012 #1351      skorolev     Cleaned code
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 */
public class ThresholdMgr {

    private final IUFStatusHandler statusHandler = UFStatus
            .getHandler(ThresholdMgr.class);

    /**
     * Threshold XML data.
     */
    private ThresholdsXML cfgXML;

    /**
     * The full path and file name for the current threshold XML file.
     */
    private String currFullPathAndFileName;

    /**
     * Constructor.
     * 
     * @param fullPathAndFileName
     *            Full path and file name for the current threshold XML file.
     */
    public ThresholdMgr(String fullPathAndFileName) {
        this.currFullPathAndFileName = fullPathAndFileName;
    }

    /**
     * Read the XML threshold data for the current XML file name.
     */
    public void readThresholdXml() {
        try {
            cfgXML = null;
            IPathManager pm = PathManagerFactory.getPathManager();
            File path = pm.getStaticFile(currFullPathAndFileName);
            cfgXML = JAXB.unmarshal(path, ThresholdsXML.class);
        } catch (Exception e) {
            statusHandler.handle(Priority.ERROR, e.getMessage());
        }
    }

    /**
     * Save the XML threshold data to the current XML file name.
     */
    public void saveThresholdXml() {
        IPathManager pm = PathManagerFactory.getPathManager();
        LocalizationContext context = pm.getContext(
                LocalizationType.CAVE_STATIC, LocalizationLevel.SITE);
        LocalizationFile locFile = pm.getLocalizationFile(context,
                currFullPathAndFileName);
        if (locFile.getFile().getParentFile().exists() == false) {
            locFile.getFile().getParentFile().mkdirs();
        }
        try {
            JAXB.marshal(cfgXML, locFile.getFile());
            locFile.save();
        } catch (Exception e) {
            statusHandler.handle(Priority.ERROR, e.getMessage());
        }
    }

    /**
     * Set the full path and file name of the current threshold XML file.
     * 
     * @param fullPathAndFileName
     */
    public void setFullPathFileName(String fullPathAndFileName) {
        this.currFullPathAndFileName = fullPathAndFileName;
    }

    /**
     * Create the configuration XML using the default values.
     * 
     * @param fullDefaultPathName
     *            Full default path and filename of the default XML file.
     * @param areaIDs
     *            Array of area IDs.
     * @param keys
     *            Array of "keys" that are used to identify the Red and Yellow
     *            values.
     * @return True if the configuration XML was successfully created.
     */
    public boolean createConfigFromDefaults(String fullDefaultPathName,
            List<String> areaIDs, ArrayList<String> keys) {
        try {
            IPathManager pm = PathManagerFactory.getPathManager();
            File path = pm.getStaticFile(fullDefaultPathName);
            ThresholdsXML cfgXmlDefaults = JAXB.unmarshal(path,
                    ThresholdsXML.class);
            createXmlFromDefaults(cfgXmlDefaults, areaIDs, keys);
        } catch (Exception e) {
            statusHandler.handle(Priority.ERROR, e.getMessage());
            return false;
        }
        return true;
    }

    /**
     * Create the configuration XML from the default values read in.
     * 
     * @param cfgXmlDefaults
     *            Default configuration values
     * @param areaIDs
     *            Array of area IDs.
     * @param keys
     *            Array of "keys" that are used to identify the Red and Yellow
     *            values.
     */
    private void createXmlFromDefaults(ThresholdsXML cfgXmlDefaults,
            List<String> areaIDs, ArrayList<String> keys) {
        cfgXML = new ThresholdsXML();
        createAreas(cfgXmlDefaults, areaIDs, keys);
    }

    /**
     * Creates Areas.
     * 
     * @param defaultThreshXML
     * @param areaIDs
     * @param keys
     */
    private void createAreas(ThresholdsXML defaultThreshXML,
            List<String> areaIDs, ArrayList<String> keys) {
        ArrayList<AreaXML> areas = new ArrayList<AreaXML>();
        for (String areaID : areaIDs) {
            AreaXML area = new AreaXML();
            area.setAreaId(areaID);
            createAreaThreshold(area, defaultThreshXML, keys);
            areas.add(area);
        }
        cfgXML.setAreas(areas);
    }

    /**
     * Create the area threshold XML data.
     * 
     * @param area
     *            Area XML data.
     * @param defaultThreshXML
     *            The default threshold data.
     * @param keys
     *            List of threshold keys associated with each red and yellow
     *            value.
     */
    private void createAreaThreshold(AreaXML area,
            ThresholdsXML defaultThreshXML, ArrayList<String> keys) {
        ArrayList<AreaThresholdXML> areaThreshArray = new ArrayList<AreaThresholdXML>();
        for (String key : keys) {
            AreaThresholdXML areaThresh = new AreaThresholdXML();
            areaThresh.setKey(key);
            areaThresh.setRed(defaultThreshXML.getAreas().get(0)
                    .getRedValue(key));
            areaThresh.setYellow(defaultThreshXML.getAreas().get(0)
                    .getYellowValue(key));
            areaThreshArray.add(areaThresh);
        }
        area.setAreaThresholds(areaThreshArray);
    }

    /**
     * Get the red value.
     * 
     * @param areaID
     *            Area ID.
     * @param key
     *            Identifying key.
     * @return The red value.
     */
    public double getRedValue(String areaID, String key) {
        // if areaId exist, return the threshold; otherwise returns a default
        // value
        if (cfgXML.hasAreaId(areaID)) {
            return cfgXML.getRedValue(areaID, key);
        }
        return cfgXML.getAreas().get(0).getRedValue(key);
    }

    /**
     * Set the red value.
     * 
     * @param areaID
     *            Area ID.
     * @param key
     *            Identifying key.
     * @param value
     *            The red value.
     */
    public void setRedValue(String areaID, String key, double value) {
        cfgXML.setRedValue(areaID, key, value);
    }

    /**
     * Get the yellow value.
     * 
     * @param areaID
     *            Area ID.
     * @param key
     *            Identifying key.
     * @return The yellow value.
     */
    public double getYellowValue(String areaID, String key) {
        // if areaId exist, return the threshold; otherwise returns a default
        // value
        if (cfgXML.hasAreaId(areaID)) {
            return cfgXML.getYellowValue(areaID, key);
        }
        return cfgXML.getAreas().get(0).getYellowValue(key);
    }

    /**
     * Set the yellow value.
     * 
     * @param areaID
     *            Area ID.
     * @param key
     *            Identifying key.
     * @param value
     *            The yellow value.
     */
    public void setYellowValue(String areaID, String key, double value) {
        cfgXML.setYellowValue(areaID, key, value);
    }

    /**
     * Get the threshold XML data.
     * 
     * @return
     */
    public ThresholdsXML getThresholdXML() {
        return this.cfgXML;
    }

    /**
     * Sets the threshold XML.
     * 
     * @param newCfgXML
     */
    public void setThresholdXML(ThresholdsXML newCfgXML) {
        this.cfgXML = newCfgXML;
    }

    /**
     * Gets Thresholds Xml Copy.
     * 
     * @return newThreshXML
     */
    public ThresholdsXML getThresholdsXmlCopy() {
        ThresholdsXML newThreshXML = new ThresholdsXML();
        ArrayList<AreaXML> currAreas = cfgXML.getAreas();
        ArrayList<AreaXML> newAreas = new ArrayList<AreaXML>();
        for (AreaXML currAreaXML : currAreas) {
            AreaXML newAreaXML = new AreaXML();
            newAreaXML.setAreaId(currAreaXML.getAreaId());
            ArrayList<AreaThresholdXML> currAreaThreshholds = currAreaXML
                    .getAreaThresholds();
            ArrayList<AreaThresholdXML> newAreaThreshholds = new ArrayList<AreaThresholdXML>();
            for (AreaThresholdXML currAreaThreshXML : currAreaThreshholds) {
                AreaThresholdXML newAreaThreshXML = new AreaThresholdXML();
                newAreaThreshXML.setKey(currAreaThreshXML.getKey());
                newAreaThreshXML.setRed(currAreaThreshXML.getRed());
                newAreaThreshXML.setYellow(currAreaThreshXML.getYellow());
                newAreaThreshholds.add(newAreaThreshXML);
            }
            newAreaXML.setAreaThresholds(newAreaThreshholds);
            newAreas.add(newAreaXML);
        }
        newThreshXML.setAreas(newAreas);
        return newThreshXML;
    }
}
