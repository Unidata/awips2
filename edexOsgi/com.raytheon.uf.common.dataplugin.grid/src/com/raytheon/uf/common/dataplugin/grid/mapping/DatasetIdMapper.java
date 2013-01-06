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
package com.raytheon.uf.common.dataplugin.grid.mapping;

import javax.xml.bind.JAXBException;

import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManager;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.util.mapping.Mapper;

/**
 * Provide mappings of grid datasetId. The "base" namespace is not formally
 * defined and can be considered as whatever the decoders are storing in the
 * database.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 30, 2012            bsteffen     Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */

public class DatasetIdMapper extends Mapper {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(DatasetIdMapper.class);

    private DatasetIdMapper() {
        IPathManager pathMgr = PathManagerFactory.getPathManager();
        LocalizationContext commonStaticBase = pathMgr.getContext(
                LocalizationContext.LocalizationType.COMMON_STATIC,
                LocalizationContext.LocalizationLevel.BASE);

        LocalizationContext commonStaticSite = pathMgr.getContext(
                LocalizationContext.LocalizationType.COMMON_STATIC,
                LocalizationContext.LocalizationLevel.SITE);

        // read in the namespace map
        LocalizationFile[] files = pathMgr.listFiles(new LocalizationContext[] {
                commonStaticSite, commonStaticBase }, "grid"
                + PathManager.SEPARATOR + "dataset" + IPathManager.SEPARATOR
                + "alias", new String[] { ".xml" }, true, true);
        for (LocalizationFile file : files) {
            try {
                addAliasList(file.getFile());
            } catch (JAXBException e) {
                statusHandler.error(
                        "Error reading datasetid aliases: " + file.getName()
                                + " has been ignored.", e);
            }
        }
    }

    private static DatasetIdMapper instance;

    public static synchronized DatasetIdMapper getInstance() {
        if (instance == null) {
            instance = new DatasetIdMapper();
        }
        return instance;
    }
}
