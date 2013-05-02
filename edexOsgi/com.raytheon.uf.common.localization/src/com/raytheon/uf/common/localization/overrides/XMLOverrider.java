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
package com.raytheon.uf.common.localization.overrides;

import java.io.File;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import org.apache.commons.configuration.CombinedConfiguration;
import org.apache.commons.configuration.ConfigurationException;
import org.apache.commons.configuration.XMLConfiguration;
import org.apache.commons.configuration.tree.NodeCombiner;
import org.apache.commons.configuration.tree.OverrideCombiner;

import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;

/**
 * Override a set of XML files incrementally.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 14, 2013            mnash     Initial creation
 * 
 * </pre>
 * 
 * @author mnash
 * @version 1.0
 */

public class XMLOverrider {
    private static final IUFStatusHandler handler = UFStatus
            .getHandler(XMLOverrider.class);

    /**
     * Takes the information needed to get a file from localization using an
     * {@link IPathManager}, and then combines them and returns.
     * 
     * @param type
     * @param name
     * @return
     */
    public static CombinedConfiguration override(LocalizationType type,
            String name) {
        IPathManager manager = PathManagerFactory.getPathManager();
        Map<LocalizationLevel, LocalizationFile> mapFiles = manager
                .getTieredLocalizationFile(type, name);

        List<File> files = new ArrayList<File>();
        for (LocalizationLevel level : manager.getAvailableLevels()) {
            files.add(mapFiles.get(level).getFile());
        }

        return override(files.toArray(new File[files.size()]));
    }

    /**
     * Takes an array of {@link LocalizationFile} and returns a combined version
     * of them.
     * 
     * @param lFiles
     * @return
     */
    public static CombinedConfiguration override(LocalizationFile... lFiles) {
        File[] files = convertToFiles(lFiles);
        return override(files);
    }

    /**
     * Provide the override functionality for XML. This takes the XML files and
     * integrates them together.
     */
    public static CombinedConfiguration override(File... files) {
        NodeCombiner combiner = new OverrideCombiner();
        // lowest level first
        XMLConfiguration[] configurations = new XMLConfiguration[files.length];
        for (int i = 0; i < files.length; i++) {
            try {
                configurations[i] = new XMLConfiguration(files[i]);
            } catch (ConfigurationException e) {
                handler.handle(Priority.ERROR,
                        "Unable to make a new XML configuration", e);
            }
        }

        CombinedConfiguration combinedConfiguration = new CombinedConfiguration(
                combiner);
        combinedConfiguration.setForceReloadCheck(true);
        // loop through each xml configuration
        for (XMLConfiguration config : configurations) {
            combinedConfiguration.addConfiguration(config);
        }

        return combinedConfiguration;
    }

    /**
     * Converts localization files to files for use later
     * 
     * @param lFiles
     * @return
     */
    private static File[] convertToFiles(LocalizationFile[] lFiles) {
        File[] files = new File[lFiles.length];
        for (int i = 0; i < files.length; i++) {
            File file = lFiles[i].getFile();
            if (file != null && file.exists()) {
                files[i] = file;
            }
        }
        return files;
    }
}
