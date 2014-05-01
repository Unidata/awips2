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
package com.raytheon.uf.viz.monitor.filename;

import java.io.File;

import javax.xml.bind.JAXB;

import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.viz.monitor.xml.DefaultFileNameXML;

public class DefaultFilenameMgr {
    private DefaultFileNameXML defThreshFileXML;

    private String defaultPathName = null;

    public DefaultFilenameMgr(String defaultPathName) {
        this.defaultPathName = defaultPathName;

        if (this.defaultPathName.endsWith("/") == false) {
            this.defaultPathName = this.defaultPathName.trim().concat("/");
        }

        this.defaultPathName = this.defaultPathName.trim().concat(
                "DefaultFileName.xml");
    }

    public void readXmlConfig() {
        try {
            IPathManager pm = PathManagerFactory.getPathManager();
            File path = pm.getStaticFile(defaultPathName);

            defThreshFileXML = JAXB.unmarshal(path, DefaultFileNameXML.class);
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    public void saveConfig() {
        IPathManager pm = PathManagerFactory.getPathManager();
        LocalizationContext context = pm.getContext(
                LocalizationType.CAVE_STATIC, LocalizationLevel.USER);
        LocalizationFile locFile = pm.getLocalizationFile(context,
                defaultPathName);

        if (locFile.getFile().getParentFile().exists() == false) {
            System.out.println("Creating new directory");

            if (locFile.getFile().getParentFile().mkdirs() == false) {
                System.out.println("Could not create new directory...");
            }
        }

        try {
            System.out.println("saveConfig() "
                    + locFile.getFile().getAbsolutePath());
            JAXB.marshal(defThreshFileXML, locFile.getFile());
            locFile.save();
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    public void setDefaultThresholdFilename(String fileName) {
        defThreshFileXML.setFileName(fileName);
        saveConfig();
    }

    public String getDefaultThresholdFilename() {
        return defThreshFileXML.getFileName();
    }
}
