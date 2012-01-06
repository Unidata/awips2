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
import com.raytheon.uf.viz.monitor.filename.DefaultFilenameMgr;
import com.raytheon.uf.viz.monitor.fog.xml.FogMonitorAlgorithmXML;

public class FogAlgorithmMgr {
    private static FogAlgorithmMgr classInstance;

    private final String defaultAlgFilename = "FogMonitorAlgThresh.xml";

    private String currFullPathAndFileName;

    private FogMonitorAlgorithmXML algXML;

    private DefaultFilenameMgr defaultFileNameMgr;

    private FogAlgorithmMgr() {
        init();
    }

    public static FogAlgorithmMgr getInstance() {
        if (classInstance == null) {
            classInstance = new FogAlgorithmMgr();
        }

        return classInstance;
    }

    private void init() {
        defaultFileNameMgr = new DefaultFilenameMgr(getDefaultFileNamePath());
        defaultFileNameMgr.readXmlConfig();

        readInDefaultXML();
    }

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

    public String getAlgorithmThresholdPath() {
        String fs = String.valueOf(File.separatorChar);
        StringBuilder sb = new StringBuilder();

        sb.append("fog").append(fs);
        sb.append("algorithm").append(fs);

        return sb.toString();
    }

    public String getDefaultFileNamePath() {
        String fs = String.valueOf(File.separatorChar);
        StringBuilder sb = new StringBuilder();

        sb.append("fog").append(fs);
        sb.append("threshold").append(fs);
        sb.append("display").append(fs);
        sb.append("defaultThresh").append(fs);

        return sb.toString();
    }

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

    public void readAlgorithmXml() {
        try {
            algXML = null;
            IPathManager pm = PathManagerFactory.getPathManager();
            File path = pm.getStaticFile(currFullPathAndFileName);

            System.out.println("**** readAlgorithmXml() path = " + path);

            algXML = JAXB.unmarshal(path, FogMonitorAlgorithmXML.class);
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    public void saveAlgorithmXmlAs(String newFileName) {
        if (newFileName.trim().compareTo(defaultAlgFilename) == 0) {
            return;
        }

        currFullPathAndFileName = getAlgorithmThresholdPath() + newFileName;
        saveAlgorithmXml();
    }

    public void saveAlgorithmXml() {
        IPathManager pm = PathManagerFactory.getPathManager();
        LocalizationContext context = pm.getContext(
                LocalizationType.CAVE_STATIC, LocalizationLevel.SITE);
        LocalizationFile locFile = pm.getLocalizationFile(context,
                currFullPathAndFileName);

        if (locFile.getFile().getParentFile().exists() == false) {
            System.out.println("Creating new directory");

            if (locFile.getFile().getParentFile().mkdirs() == false) {
                System.out.println("Could not create new directory...");
            }
        }

        try {
            System.out.println("saveAlgorithmXml() -- "
                    + locFile.getFile().getAbsolutePath());
            JAXB.marshal(algXML, locFile.getFile());
            locFile.save();
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

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
            e.printStackTrace();
        }
    }

    public void loadAlgorithmThreashold(String fileName) {
        currFullPathAndFileName = getAlgorithmThresholdPath() + fileName;
        readAlgorithmXml();
    }

    public void loadDefaultAlgorithmData() {
        readInDefaultXML();
    }

    public String getDefaultAlgorithmFileName() {
        return defaultAlgFilename;
    }

    public FogMonitorAlgorithmXML getAlgorithmXML() {
        return algXML;
    }
}
