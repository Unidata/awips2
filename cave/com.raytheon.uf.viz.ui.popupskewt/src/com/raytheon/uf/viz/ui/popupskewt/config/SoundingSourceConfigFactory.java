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
package com.raytheon.uf.viz.ui.popupskewt.config;

import javax.xml.bind.JAXBException;

import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.localization.exception.LocalizationException;
import com.raytheon.uf.common.serialization.JAXBManager;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.viz.ui.popupskewt.Activator;

/**
 * Configuration factory for getting the available {@link SoundingSource}s
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 19, 2013       2190 mschenke    Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class SoundingSourceConfigFactory {

    private static final String SKEWT_SOURCES_DIR = "popupSkewT";

    private static final String SKEWT_SOURCES_FILE = "sources.xml";

    private static SoundingSource[] sources;

    public static synchronized SoundingSource[] getSoundingSources() {
        if (sources == null) {
            String filePath = SKEWT_SOURCES_DIR + IPathManager.SEPARATOR
                    + SKEWT_SOURCES_FILE;
            try {
                JAXBManager mgr = new JAXBManager(SoundingSourceConfig.class);
                LocalizationFile file = PathManagerFactory.getPathManager()
                        .getStaticLocalizationFile(filePath);
                if (file == null) {
                    sources = new SoundingSource[0];
                    throw new LocalizationException(
                            "Could not find skewt sources file: " + filePath);
                }
                SoundingSourceConfig config = (SoundingSourceConfig) mgr
                        .unmarshalFromInputStream(file.openInputStream());
                if (config == null) {
                    throw new SerializationException(
                            "SkewT Config file could not be deserialized");
                }
                sources = config.getSources();
            } catch (JAXBException e) {
                Activator.statusHandler.handle(UFStatus.Priority.PROBLEM,
                        e.getLocalizedMessage(), e);
            } catch (SerializationException e) {
                Activator.statusHandler.handle(UFStatus.Priority.PROBLEM,
                        e.getLocalizedMessage() + filePath, e);
            } catch (LocalizationException e) {
                Activator.statusHandler.handle(UFStatus.Priority.PROBLEM,
                        e.getLocalizedMessage() + filePath, e);
            }
            if (sources == null) {
                sources = new SoundingSource[0];
            }
        }
        return sources;
    }
}
