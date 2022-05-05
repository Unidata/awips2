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
package com.raytheon.uf.common.dataplugin.grid.dataset;

import java.io.IOException;
import java.io.InputStream;
import java.util.HashMap;
import java.util.Map;

import javax.xml.bind.JAXBException;

import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.localization.exception.LocalizationException;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.serialization.SingleTypeJAXBManager;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;

/**
 *
 * Provides logic to read datasetInfo files from localization and provide lookup
 * by datasetId.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Feb 27, 2012           bsteffen     Initial creation
 * Dec 16, 2013  2574     bsteffen     Update deprecated method call.
 * Jul 21, 2014  3373     bclement     changed to use single type JAXB manager
 * Feb 15, 2016  5244     nabowle      Replace deprecated LocalizationFile methods.
 *
 * </pre>
 *
 * @author bsteffen
 * @version 1.0
 */
public class DatasetInfoLookup {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(DatasetInfoLookup.class);

    private static DatasetInfoLookup instance;

    public static DatasetInfoLookup getInstance() {
        if (instance == null) {
            instance = new DatasetInfoLookup();
        }
        return instance;
    }

    private Map<String, DatasetInfo> infoMap = new HashMap<String, DatasetInfo>();

    private DatasetInfoLookup() {
        init();
    }

    private void init() {
        SingleTypeJAXBManager<DatasetInfoSet> manager = null;
        try {
            manager = new SingleTypeJAXBManager<DatasetInfoSet>(true,
                    DatasetInfoSet.class);
        } catch (JAXBException e) {
            statusHandler
                    .error("Error loading context for DatasetInfo, no datasetInfo will be loaded.",
                            e);
        }
        IPathManager pathMgr = PathManagerFactory.getPathManager();
        LocalizationContext commonStaticBase = pathMgr.getContext(
                LocalizationContext.LocalizationType.COMMON_STATIC,
                LocalizationContext.LocalizationLevel.BASE);

        LocalizationContext commonStaticSite = pathMgr.getContext(
                LocalizationContext.LocalizationType.COMMON_STATIC,
                LocalizationContext.LocalizationLevel.SITE);

        LocalizationFile[] files = pathMgr.listFiles(new LocalizationContext[] {
                commonStaticSite, commonStaticBase }, "grid"
                + IPathManager.SEPARATOR + "datasetInfo",
                new String[] { ".xml" }, true, true);
        for (LocalizationFile file : files) {
            if (file == null || !file.exists()) {
                return;
            }
            try (InputStream is = file.openInputStream()) {
                DatasetInfoSet set = manager.unmarshalFromInputStream(is);
                for (DatasetInfo info : set.getInfos()) {
                    infoMap.put(info.getDatasetId(), info);
                }
            } catch (SerializationException | IOException
                    | LocalizationException e) {
                statusHandler.error(
                        "Error reading dataset info: " + file.getPath()
                                + " has been ignored.", e);
            }
        }

    }

    public DatasetInfo getInfo(String datasetId) {
        return infoMap.get(datasetId);
    }
}
