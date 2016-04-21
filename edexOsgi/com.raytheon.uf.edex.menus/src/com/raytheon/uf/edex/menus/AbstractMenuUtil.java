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
package com.raytheon.uf.edex.menus;

import java.io.File;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Marshaller;
import javax.xml.bind.Unmarshaller;

import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManager;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.localization.exception.LocalizationException;
import com.raytheon.uf.common.localization.exception.LocalizationOpFailedException;
import com.raytheon.uf.common.menus.MenuSerialization;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;

/**
 * 
 * Abstract class for generating menu files
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 08, 2012            mschenke    Initial javadoc creation
 * Mar 11, 2014    2858    mpduff      javadoc updates
 * 
 * </pre>
 * 
 * @author unknown
 * @version 1.0
 */
public abstract class AbstractMenuUtil {
    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(AbstractMenuUtil.class);

    private final PathManager pm = (PathManager) PathManagerFactory
            .getPathManager();

    protected LocalizationContext caveConfigured = pm.getContext(
            LocalizationType.CAVE_STATIC, LocalizationLevel.CONFIGURED);

    private final LocalizationContext edexBase = pm.getContext(
            LocalizationType.EDEX_STATIC, LocalizationLevel.BASE);

    private String site = null;

    /**
     * Create the menu contribution xml.
     */
    public abstract void createMenus();

    /**
     * Create the menus for a different site
     * 
     * @param site
     */
    public void createMenus(String site) {
        if (site != null) {
            caveConfigured = pm.getContextForSite(LocalizationType.CAVE_STATIC,
                    site);
            caveConfigured.setLocalizationLevel(LocalizationLevel.CONFIGURED);
        }
        setSite(site);
        if (!checkCreated()) {
            createMenus();
        }
    }

    /**
     * Convert menu objects to xml and writes them to disk
     * 
     * @param object
     *            The object to marshal
     * 
     * @param path
     *            The path
     */
    public void toXml(Object object, String path) {
        try {
            JAXBContext context = MenuSerialization.getJaxbContext();
            Marshaller marshaller = context.createMarshaller();
            marshaller.setProperty(Marshaller.JAXB_FORMATTED_OUTPUT,
                    new Boolean(true));
            LocalizationFile file = pm
                    .getLocalizationFile(caveConfigured, path);
            // make sure the directory exists!
            if (!file.getFile().exists()) {
                file.getFile().getParentFile().mkdirs();
            }

            marshaller.marshal(object, pm.getFile(caveConfigured, path));
            file.save();
        } catch (JAXBException e) {
            statusHandler.error("Unable to process the menu: " + path, e);
        } catch (LocalizationOpFailedException e) {
            statusHandler.error("Unable to process the menu: " + path, e);
        }
    }

    /**
     * Convert xml to an object using jaxb (for menu templates)
     * 
     * @param path
     * @return
     */
    public Object fromXml(String path) {
        Object object = fromXml(path, edexBase);
        return object;
    }

    /**
     * Unmarshal an xml file.
     * 
     * @param path
     *            The path of the file
     * @param lContext
     *            The localization context
     * 
     * @return The xml object
     */
    public Object fromXml(String path, LocalizationContext lContext) {
        Object object = null;
        try {
            LocalizationFile file = pm.getLocalizationFile(lContext, path);
            JAXBContext context = MenuSerialization.getJaxbContext();
            Unmarshaller unmarshaller = context.createUnmarshaller();
            if (file != null && file.exists()) {
                object = unmarshaller.unmarshal(file.getFile());
            }
        } catch (JAXBException e) {
            statusHandler.error("Error unmarshalling " + path, e);
        }

        return object;
    }

    /**
     * Check to see whether the menu creator needs to be run
     * 
     * @return true if it needs to be run
     */
    protected abstract boolean checkCreated();

    /**
     * Check to see whether the menu creator needs to be run
     * 
     * @param fileName
     * @param type
     * 
     * @return true if it needs to be run
     */
    public boolean checkCreated(String fileName, String type) {
        LocalizationContext context = pm.getContextForSite(
                LocalizationType.COMMON_STATIC, getSite());
        LocalizationFile lFile = pm.getLocalizationFile(context, type
                + File.separator + fileName);
        if (lFile == null || !lFile.exists()) {
            return false;
        }

        // get the file from localization that has the
        LocalizationFile olFile = pm.getLocalizationFile(caveConfigured,
                "menus" + File.separator + type + File.separator + "." + type
                        + "MenuTime");

        long useTime = lFile.getFile().lastModified();
        long writeTime = olFile.getFile().lastModified();

        if (writeTime < useTime) {
            try {
                // Update menu creation time file
                olFile.write(new byte[0]);
                olFile.save();
            } catch (LocalizationException e) {
                statusHandler.error("Error saving menu creation time file", e);
            }
            return false;
        }

        statusHandler.info("Timestamp in " + fileName
                + " was before timestamp in ." + type + "MenuTime");
        statusHandler.info("Menus already created for site " + getSite()
                + " for " + type);
        return true;

    }

    /**
     * @return the site
     */
    public String getSite() {
        return site;
    }

    /**
     * @param site
     *            the site to set
     */
    public void setSite(String site) {
        this.site = site;
    }
}
