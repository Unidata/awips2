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
package com.raytheon.uf.viz.monitor.scan;

import java.io.File;
import java.io.InputStream;
import java.util.Map;

import com.raytheon.uf.common.localization.FileUpdatedMessage;
import com.raytheon.uf.common.localization.ILocalizationFile;
import com.raytheon.uf.common.localization.ILocalizationFileObserver;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.serialization.JAXBManager;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.monitor.scan.xml.CellXML;
import com.raytheon.uf.viz.monitor.scan.xml.DmdXML;
import com.raytheon.uf.viz.monitor.scan.xml.ScanSampleConfigXML;

/**
 * SCAN Sampling configuration file manager.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 5, 2011             mpduff      Initial creation
 * Aug 09, 2017 6373       tgurney     Move config to common_static
 *
 * </pre>
 *
 * @author mpduff
 */

public class ScanSampleConfigManager implements ILocalizationFileObserver {
    private final IUFStatusHandler statusHandler = UFStatus
            .getHandler(ScanSampleConfigManager.class);

    private static ScanSampleConfigManager instance = null;

    private static final String CONFIG_FILE_NAME = "scan" + File.separatorChar
            + "config" + File.separatorChar + "ScanSampleConfig.xml";

    private ScanSampleConfigXML configXml = null;

    private boolean updated = true;

    private JAXBManager jaxbManager;

    private ScanSampleConfigManager() {
        readXml();
    }

    public static synchronized ScanSampleConfigManager getInstance() {
        if (instance == null) {
            instance = new ScanSampleConfigManager();
        }

        return instance;
    }

    private void readXml() {
        IPathManager pm = PathManagerFactory.getPathManager();
        Map<LocalizationLevel, LocalizationFile> configMap = pm
                .getTieredLocalizationFile(LocalizationType.COMMON_STATIC,
                        CONFIG_FILE_NAME);

        ILocalizationFile lFile = null;
        try {
            if (configMap.get(LocalizationLevel.USER) != null) {
                lFile = configMap.get(LocalizationLevel.USER);
            } else if (configMap.get(LocalizationLevel.SITE) != null) {
                lFile = configMap.get(LocalizationLevel.SITE);
            } else {
                lFile = configMap.get(LocalizationLevel.BASE);
                if (lFile == null) {
                    throw new SerializationException(
                            "File Not Found:  " + CONFIG_FILE_NAME);
                }
            }

            synchronized (this) {
                if (jaxbManager == null) {
                    jaxbManager = new JAXBManager(ScanSampleConfigXML.class);
                }
            }
            try (InputStream is = lFile.openInputStream()) {
                configXml = (ScanSampleConfigXML) jaxbManager
                        .unmarshalFromInputStream(is);
            }
        } catch (Exception e) {
            statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(), e);
            updated = false;

        }

        updated = true;
    }

    public DmdXML getDmdSampleConfig() {
        updated = false;
        return configXml.getDmdSampleConfig();
    }

    public CellXML getCellSampleConfig() {
        updated = false;
        return configXml.getCellSampleConfig();
    }

    @Override
    public synchronized void fileUpdated(FileUpdatedMessage message) {
        readXml();
    }

    public boolean isUpdated() {
        return updated;
    }
}
