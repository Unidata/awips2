package com.raytheon.uf.viz.productbrowser.xml;

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

import java.io.File;
import java.io.IOException;

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
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.productbrowser.bookmarks.ProductBrowserBookmark;
import com.raytheon.uf.viz.productbrowser.bookmarks.ProductBrowserFolder;

/**
 * XML class for the bookmarks, uses JAXB
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * June 4, 2010            mnash     Initial creation
 * 
 * </pre>
 * 
 * @author mnash
 * @version 1.0
 */

public class ProductBrowserBookmarksXML {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(ProductBrowserBookmarksXML.class);

    /**
     * Gets the file whether it be in base or user
     * 
     * @return
     * @throws IOException
     */
    public static File getFile() throws IOException {
        PathManager pathMgr = (PathManager) PathManagerFactory.getPathManager();
        LocalizationFile file = pathMgr.getStaticLocalizationFile("bookmarks"
                + File.separator + "bookmarks.xml");
        File newFile = null;
        if (!file.exists()) {
            file.getFile().getParentFile().mkdirs();
            newFile = new File(file.getFile().getAbsolutePath());
            newFile.createNewFile();
        } else {
            newFile = file.getFile();
        }
        return newFile;
    }

    /**
     * Does the same thing as above except looks in ONLY user, this way it won't
     * be trying to save in base in case the user happens to delete the user
     * file
     * 
     * @return
     * @throws IOException
     */
    private static File getUserFile() throws IOException {
        PathManager pathMgr = (PathManager) PathManagerFactory.getPathManager();
        LocalizationContext context = pathMgr.getContext(
                LocalizationType.CAVE_STATIC, LocalizationLevel.USER);
        LocalizationFile file = pathMgr.getLocalizationFile(context,
                "bookmarks" + File.separator + "bookmarks.xml");
        File newFile = null;
        if (!file.exists()) {
            file.getFile().getParentFile().mkdirs();
            newFile = new File(file.getFile().getAbsolutePath());
            newFile.createNewFile();
        } else {
            newFile = file.getFile();
        }
        return newFile;
    }

    /**
     * Reads in the xml file that is got from the getFile() method, and returns
     * the folder that describes all the folders and bookmarks
     * 
     * @return
     */
    public static ProductBrowserFolder readXML() {
        ProductBrowserFolder folder = null;
        try {
            File file = getFile();
            JAXBContext context = JAXBContext.newInstance(
                    ProductBrowserFolder.class, ProductBrowserBookmark.class);
            Unmarshaller unmarshaller = context.createUnmarshaller();
            folder = (ProductBrowserFolder) unmarshaller.unmarshal(file);
        } catch (JAXBException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Could not parse bookmarks.xml file", e);
        } catch (IOException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Could not parse bookmarks.xml file", e);
        }
        return folder;
    }

    /**
     * Takes the folder object and writes it back to xml
     * 
     * @param xml
     */
    public static void writeXML(ProductBrowserFolder xml) {
        try {
            JAXBContext context = JAXBContext.newInstance(
                    ProductBrowserFolder.class, ProductBrowserBookmark.class);
            Marshaller marshaller = context.createMarshaller();
            marshaller.setProperty(Marshaller.JAXB_FORMATTED_OUTPUT,
                    new Boolean(true));
            marshaller.marshal(xml, getUserFile());
        } catch (JAXBException e) {
            statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(), e);

        } catch (IOException e) {
            statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(), e);

        }
    }

    public static ProductBrowserFolder populate() {
        return readXML();
    }
}
