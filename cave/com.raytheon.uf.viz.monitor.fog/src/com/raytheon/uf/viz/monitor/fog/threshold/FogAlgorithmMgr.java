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

import java.io.File;

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
import com.raytheon.uf.viz.monitor.filename.DefaultFilenameMgr;
import com.raytheon.uf.viz.monitor.fog.xml.FogMonitorAlgorithmXML;

/**
 * Fog Algorithm Threshold Manager
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 3, 2012  1351       skorolev    Cleaned code
 * 
 * </pre>
 * 
 * @author
 * @version 1.0
 */
public class FogAlgorithmMgr {

    private final IUFStatusHandler statusHandler = UFStatus
            .getHandler(FogAlgorithmMgr.class);

    /** Fog Algorithm Threshold Manager instance **/
    private final static FogAlgorithmMgr classInstance = new FogAlgorithmMgr();

    /** Default Algorithm Threshold file name **/
    private final String defaultAlgFilename = "FogMonitorAlgThresh.xml";

    /** Current Algorithm Threshold full file path **/
    private String currFullPathAndFileName;

    /** Algorithm Threshold XML **/
    private FogMonitorAlgorithmXML algXML;

    /** Default File Manager **/
    private DefaultFilenameMgr defaultFileNameMgr;

    /**
     * Gets instance of Fog Algorithm Threshold Manager.
     * 
     * @return
     */
    public static FogAlgorithmMgr getInstance() {
        return classInstance;
    }

    /**
     * Constructor
     */
    private FogAlgorithmMgr() {
        init();
    }

    /**
     * Fog Algorithm Threshold Manager initialization.
     */
    private void init() {
        defaultFileNameMgr = new DefaultFilenameMgr(getDefaultFileNamePath());
        defaultFileNameMgr.readXmlConfig();
        readInDefaultXML();
    }

    /**
     * Reads default Threshold XML file.
     */
    private void readInDefaultXML() {
        if (defaultFileNameMgr.getDefaultThresholdFilename() != null
                && defaultFileNameMgr.getDefaultThresholdFilename().length() > 0) {
            if (defaultFileNameMgr.getDefaultThresholdFilename().compareTo(
                    defaultAlgFilename) == 0) {
                setDefaultAlgorithmFileName(null);
                readAlgorithmXml();
            } else {
                currFullPathAndFileName = getAlgorithmThresholdPath()
                        + defaultFileNameMgr.getDefaultThresholdFilename();
                readAlgorithmXml();
            }
        } else {
            setDefaultAlgorithmFileName(null);
            readAlgorithmXml();
        }
    }

    /**
     * Gets path of current Algorithm Threshold file
     * 
     * @return file path
     */
    public String getAlgorithmThresholdPath() {
        String fs = IPathManager.SEPARATOR;
        StringBuilder sb = new StringBuilder();
        sb.append("fog").append(fs);
        sb.append("algorithm").append(fs);
        return sb.toString();
    }

    /**
     * Gets path of default Algorithm Threshold file
     * 
     * @return file path
     */
    public String getDefaultFileNamePath() {
        String fs = IPathManager.SEPARATOR;
        StringBuilder sb = new StringBuilder();
        sb.append("fog").append(fs);
        sb.append("threshold").append(fs);
        sb.append("display").append(fs);
        sb.append("defaultThresh").append(fs);
        return sb.toString();
    }

    /**
     * Sets default Algorithm Threshold file path.
     * 
     * @param fileName
     */
    public void setDefaultAlgorithmFileName(String fileName) {
        if (fileName == null) {
            defaultFileNameMgr.setDefaultThresholdFilename("");
            currFullPathAndFileName = getAlgorithmThresholdPath()
                    + defaultAlgFilename;
            return;
        }
        if (fileName.endsWith(".xml") == false) {
            fileName.concat(".xml");
        }
        if (fileName.compareTo(defaultAlgFilename) == 0) {
            defaultFileNameMgr.setDefaultThresholdFilename("");
        } else {
            defaultFileNameMgr.setDefaultThresholdFilename(fileName);
        }
    }

    /**
     * Reads Algorithm Threshold XML.
     */
    public void readAlgorithmXml() {
        try {
            algXML = null;
            IPathManager pm = PathManagerFactory.getPathManager();
            File path = pm.getStaticFile(currFullPathAndFileName);
            algXML = JAXB.unmarshal(path, FogMonitorAlgorithmXML.class);
        } catch (Exception e) {
            statusHandler.handle(Priority.ERROR, e.getMessage());
        }
    }

    /**
     * Menu option Save as... Algorithm Threshold XML file.
     * 
     * @param newFileName
     */
    public void saveAlgorithmXmlAs(String newFileName) {
        if (newFileName.trim().compareTo(defaultAlgFilename) == 0) {
            return;
        }
        currFullPathAndFileName = getAlgorithmThresholdPath() + newFileName;
        saveAlgorithmXml();
    }

    /**
     * Menu option Save... Algorithm Threshold XML file.
     */
    public void saveAlgorithmXml() {
        IPathManager pm = PathManagerFactory.getPathManager();
        LocalizationContext context = pm.getContext(
                LocalizationType.CAVE_STATIC, LocalizationLevel.SITE);
        LocalizationFile locFile = pm.getLocalizationFile(context,
                currFullPathAndFileName);
        if (locFile.getFile().getParentFile().exists() == false) {
            locFile.getFile().getParentFile().mkdirs();
        }
        try {
            JAXB.marshal(algXML, locFile.getFile());
            locFile.save();
        } catch (Exception e) {
            statusHandler.handle(Priority.ERROR, e.getMessage());
        }
    }

    /**
     * Menu option Delete... Algorithm Threshold XML file.
     */
    public void deleteCurrentAlgorithmFile() {
        IPathManager pm = PathManagerFactory.getPathManager();
        LocalizationContext context = pm.getContext(
                LocalizationType.CAVE_STATIC, LocalizationLevel.SITE);
        LocalizationFile locFile = pm.getLocalizationFile(context,
                currFullPathAndFileName);
        try {
            if (locFile != null) {
                locFile.delete();
            }
        } catch (Exception e) {
            statusHandler.handle(Priority.ERROR, e.getMessage());
        }
    }

    /**
     * Menu option Load... Algorithm Threshold XML file.
     * 
     * @param fileName
     */
    public void loadAlgorithmThreashold(String fileName) {
        currFullPathAndFileName = getAlgorithmThresholdPath() + fileName;
        readAlgorithmXml();
    }

    /**
     * Menu option Load default... Algorithm Threshold XML file.
     */
    public void loadDefaultAlgorithmData() {
        readInDefaultXML();
    }

    /**
     * Gets default Algorithm Threshold XML file name.
     * 
     * @return defaultAlgFilename
     */
    public String getDefaultAlgorithmFileName() {
        return defaultAlgFilename;
    }

    /**
     * Gets Algorithm Threshold XML.
     * 
     * @return algXML
     */
    public FogMonitorAlgorithmXML getAlgorithmXML() {
        return algXML;
    }
}
