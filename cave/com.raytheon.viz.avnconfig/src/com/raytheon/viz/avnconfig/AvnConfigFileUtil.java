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
package com.raytheon.viz.avnconfig;

import java.io.File;
import java.io.FileNotFoundException;

import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.localization.LocalizationManager;

/**
 * This class contains static methods for handling the localization setup up for
 * AvnFPS.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 07/08/2010   5078        rferrel     Initial creation.
 * 
 * </pre>
 * 
 * @author rferrel
 * @version 1.0
 */

public class AvnConfigFileUtil {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(AvnConfigFileUtil.class);

    /**
     * Obtain the LocalizationFile for configuration file.
     * 
     * @param configFile
     *            - name of the configuration file
     * @return lFile
     */
    public static LocalizationFile getStaticLocalizationFile(String configFile) {
        IPathManager pm = PathManagerFactory.getPathManager();
        LocalizationFile lFile = pm.getStaticLocalizationFile(configFile);

        if (lFile == null) {
            String site = LocalizationManager.getInstance().getCurrentSite();
            statusHandler.handle(Priority.CRITICAL, "Unable to find \""
                    + configFile + "\" under the directory for site " + site
					+ ".");
        }

        return lFile;
    }

    /**
     * Determine location of fileName for siteId. Throws the exception when
     * Localization File information does not exist or the desired file does not
     * exist.
     * 
     * @param fileName
     *            relative path file name
     * @param siteId
     *            the site file is for
     * @return file
     * @throws FileNotFoundException
     *             unable to find file
     */
    public static File getStaticSiteFile(String fileName, String siteId)
            throws FileNotFoundException {
        IPathManager pm = PathManagerFactory.getPathManager();
        LocalizationFile lFile = pm.getStaticLocalizationFile(fileName);
        File file = null;

        if (lFile != null) {
            file = lFile.getFile();
            if (file.exists() == false) {
                file = null;
            }
        }

        if (file == null) {
            throw new FileNotFoundException("Error: file " + fileName
                    + " for site " + siteId + " does not exist.");
        }

        return file;
    }

    /**
     * Obtain the File for a configuration file. Log problem status message when
     * unable to get the file.
     * 
     * @param configFile
     *            - file name for the configuration file
     * @return file
     */
    public static File getStaticFile(String configFile) {
        IPathManager pm = PathManagerFactory.getPathManager();
        File file = pm.getStaticFile(configFile);

        if (file == null) {
            statusHandler.handle(Priority.PROBLEM, "Unable to find \""
                    + configFile + "\" under the directory for site "
					+ LocalizationManager.getInstance().getCurrentSite() + ".");
        }

        return file;
    }
}
