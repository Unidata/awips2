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
package com.raytheon.viz.hydro.timeseries;

import java.io.File;
import java.util.Map;

import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.serialization.SingleTypeJAXBManager;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;

/**
 * Shef Issue Configuration Manager.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 28, 2011            mpduff      Initial creation
 * Nov 04, 2013 2361       njensen     Use JAXBManager for XML
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class ShefIssueMgr {

    private static final SingleTypeJAXBManager<ShefIssueXML> jaxb = SingleTypeJAXBManager
            .createWithoutException(ShefIssueXML.class);

    private final IUFStatusHandler statusHandler = UFStatus
            .getHandler(ShefIssueMgr.class);

    private static ShefIssueMgr instance = null;

    private ShefIssueXML xml = null;

    private ShefIssueMgr() {
        readXML();
    }

    public static synchronized ShefIssueMgr getInstance() {
        if (instance == null) {
            instance = new ShefIssueMgr();
        }

        return instance;
    }

    public static synchronized void recycle() {
        instance = null;
    }

    private void readXML() {
        LocalizationFile file = null;
        try {
            IPathManager pm = PathManagerFactory.getPathManager();
            Map<LocalizationLevel, LocalizationFile> shefIssueMap = pm.getTieredLocalizationFile(LocalizationType.COMMON_STATIC, "hydro" + File.separatorChar + "shefIssue.xml");


            if (shefIssueMap.containsKey(LocalizationLevel.SITE)) {
                file = shefIssueMap.get(LocalizationLevel.SITE);
            } else if (shefIssueMap.containsKey(LocalizationLevel.BASE)) {
                file = shefIssueMap.get(LocalizationLevel.BASE);
            }
            
            if (file != null) {
                xml = jaxb
                        .unmarshalFromXmlFile(file.getFile());
            } else {
                xml = new ShefIssueXML();
            }
        } catch (Exception e) {
            statusHandler.error("Error reading " + file.getName(), e);
        }
    }

    public ShefIssueXML getShefIssueXml() {
        return this.xml;
    }
    
    public void setShefIssueXml(ShefIssueXML xml) {
        this.xml = xml;
    }
    
    public void saveXml() {
        // Save the xml object to disk
        IPathManager pm = PathManagerFactory.getPathManager();
        LocalizationContext lc = pm.getContext(LocalizationType.COMMON_STATIC,
                LocalizationLevel.SITE);
        LocalizationFile newXmlFile = pm.getLocalizationFile(lc,
                "hydro/shefIssue.xml");

        if (newXmlFile.getFile().getParentFile().exists() == false) {
            // System.out.println("Creating new directory");

            if (newXmlFile.getFile().getParentFile().mkdirs() == false) {
                // System.out.println("Could not create new directory...");
            }
        }

        try {
            if (xml == null) {
                xml = new ShefIssueXML();
            }
            jaxb.marshalToXmlFile(xml, newXmlFile
                    .getFile().getAbsolutePath());
            newXmlFile.save();
        } catch (Exception e) {
            statusHandler.error("Error writing " + newXmlFile.getName(), e);
        }        
    }
}
