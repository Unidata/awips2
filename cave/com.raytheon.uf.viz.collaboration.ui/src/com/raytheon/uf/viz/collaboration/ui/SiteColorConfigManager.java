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
package com.raytheon.uf.viz.collaboration.ui;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.util.List;
import java.util.Map;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.Marshaller;
import javax.xml.bind.Unmarshaller;

import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManager;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.viz.collaboration.ui.SiteColorInformation.SiteColor;

/**
 * Site coloring configuration manager
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 10, 2014 3708       bclement    Moved color methods from SiteConfigurationManager
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */
public class SiteColorConfigManager {

    private static SiteColorInformation colorInfo;

    /**
     * 
     */
    private SiteColorConfigManager() {
    }

    /**
     * Write the colorInfo.xml file out to user localization so that the user
     * can retrieve it on CAVE restart
     * 
     * @param information
     */
    public static void writeSiteColorInformation(
            SiteColorInformation information) {
        colorInfo = information;
        IPathManager pathMgr = PathManagerFactory.getPathManager();
        LocalizationContext lContext = pathMgr.getContext(
                LocalizationType.CAVE_STATIC, LocalizationLevel.USER);
        LocalizationFile file = pathMgr.getLocalizationFile(lContext,
                "collaboration" + File.separator + "colorInfo.xml");
        try {
            JAXBContext context = JAXBContext
                    .newInstance(SiteColorInformation.class);
            Marshaller marshaller = context.createMarshaller();
            marshaller.setProperty(Marshaller.JAXB_FORMATTED_OUTPUT,
                    new Boolean(true));
            marshaller.marshal(information, file.getFile());
            file.save();
        } catch (Exception e) {
            Activator.statusHandler.error(
                    "Unable to write color information to file: "
                            + file.getName() + " in context " + lContext, e);
        }
    }

    /**
     * Instantiate the colorInfo object so that the colors can be read in from
     * the colorInfo.xml file and retrieved from localization
     * 
     * @return
     */
    public static SiteColorInformation getSiteColorInformation() {
        if (colorInfo == null) {
            PathManager pm = (PathManager) PathManagerFactory.getPathManager();
            Map<LocalizationLevel, LocalizationFile> files = pm
                    .getTieredLocalizationFile(LocalizationType.CAVE_STATIC,
                            "collaboration" + File.separator + "colorInfo.xml");
            LocalizationLevel[] levels = LocalizationLevel.values();

            for (int i = levels.length - 1; i >= 0 && colorInfo == null; --i) {
                LocalizationLevel level = levels[i];
                if (level == LocalizationLevel.SITE
                        || level == LocalizationLevel.USER) {
                    LocalizationFile file = files.get(level);
                    if (file != null) {
                        InputStream in = null;
                        try {
                            in = file.openInputStream();
                            JAXBContext context = JAXBContext
                                    .newInstance(SiteColorInformation.class);
                            Unmarshaller unmarshaller = context
                                    .createUnmarshaller();
                            colorInfo = (SiteColorInformation) unmarshaller
                                    .unmarshal(in);
                        } catch (Exception e) {
                            Activator.statusHandler.error(
                                    "Unable to read color information from file: "
                                            + file.getName() + " in level "
                                            + level, e);
                        }
                        if (in != null) {
                            try {
                                in.close();
                            } catch (IOException e) {
                                Activator.statusHandler.error(
                                        "Problem closing color information file: "
                                                + file.getName(), e);
                            }
                        }
                    }
                }
            }
        }
        return colorInfo;
    }

    /**
     * @return list of colors from site information config
     */
    public static List<SiteColor> getSiteColors() {
        SiteColorInformation colorInfo = getSiteColorInformation();
        if (colorInfo != null) {
            return getSiteColorInformation().getColors();
        } else {
            return null;
        }
    }

}
