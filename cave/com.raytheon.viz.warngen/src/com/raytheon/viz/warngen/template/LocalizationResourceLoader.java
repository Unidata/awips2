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
package com.raytheon.viz.warngen.template;

import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.Reader;
import java.util.HashMap;
import java.util.Map;

import org.apache.velocity.exception.ResourceNotFoundException;
import org.apache.velocity.runtime.resource.Resource;
import org.apache.velocity.runtime.resource.loader.FileResourceLoader;
import org.apache.velocity.util.ExtProperties;

import com.raytheon.uf.common.dataplugin.warning.util.WarnFileUtil;
import com.raytheon.uf.common.localization.FileUpdatedMessage;
import com.raytheon.uf.common.localization.ILocalizationFileObserver;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.LocalizationUtil;
import com.raytheon.uf.common.localization.exception.LocalizationException;

/**
 * Loads the appropriate files in the localization for the Velocity Engine.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 18, 2011            mschenke     Initial creation
 * 06/01/2012   DR 14555   D. Friedman  Support new version of Velocity.
 * Apr 28, 2014 3033       jsanchez     Retrieved the site and back up from the extended properties.
 * Jan 06, 2023 8991       lsingh       Updated method signatures to support Velocity-engine 2.3 upgrade.
 *                                      Updated velocity property name.
 * Apr 21, 2023 2030407    mapeters     Restore site/backup property constants and update for clarity
 * </pre>
 */

public class LocalizationResourceLoader extends FileResourceLoader
        implements ILocalizationFileObserver {

    /*
     * resource.loader.file.<key>=<value> properties that are passed into the
     * Velocity engine are then passed into this resource loader as
     * <key>=<value>.
     */

    private static final String PROPERTY_SITE = "site";

    /**
     * Property key for passing the issuing site ID into the Velocity engine.
     */
    public static final String ENGINE_PROPERTY_SITE = "resource.loader.file."
            + PROPERTY_SITE;

    private static final String PROPERTY_BACKUP = "backup";

    /**
     * Property key for passing the backup site ID into the Velocity engine (if
     * applicable).
     */
    public static final String ENGINE_PROPERTY_BACKUP = "resource.loader.file."
            + PROPERTY_BACKUP;

    private String site;

    private Map<String, LocalizationFile> fileMap = new HashMap<>();

    private ExtProperties configuration;

    @Override
    public long getLastModified(Resource resource) {
        return getFile(resource.getName()).getTimeStamp().getTime();
    }

    @Override
    public void init(ExtProperties configuration) {
        if (this.configuration != configuration) {
            this.configuration = configuration;
        }
        super.init(configuration);
    }

    @Override
    public Reader getResourceReader(String templateName, String encoding)
            throws ResourceNotFoundException {
        try {
            Reader reader = buildReader(getFile(templateName).openInputStream(),
                    encoding);
            return reader;
        } catch (IOException | LocalizationException e) {
            throw new ResourceNotFoundException(e);
        }
    }

    private synchronized LocalizationFile getFile(String name)
            throws ResourceNotFoundException {
        if (configuration == null) {
            throw new RuntimeException("Unable to locate file: " + name
                    + ", resource loader has not been initialized");
        }
        String site = configuration.getString(PROPERTY_SITE);
        String backup = configuration.getString(PROPERTY_BACKUP);
        if (site == null || !site.equals(this.site)) {
            // We changed sites since last time, clear out cache
            for (LocalizationFile file : fileMap.values()) {
                file.removeFileUpdatedObserver(this);
            }
            fileMap.clear();
            this.site = site;
        }
        this.site = site;

        try {
            LocalizationFile file = fileMap.get(name);
            if (file == null || !file.exists()) {
                file = WarnFileUtil.findFileInLocalizationIncludingBackupSite(
                        name, site, backup);
                file.addFileUpdatedObserver(this);
                fileMap.put(name, file);
            }
            return file;
        } catch (FileNotFoundException e) {
            throw new ResourceNotFoundException(e);
        }
    }

    @Override
    public synchronized void fileUpdated(FileUpdatedMessage message) {
        fileMap.remove(LocalizationUtil.extractName(message.getFileName()));
    }

    @Override
    public boolean resourceExists(String name) {
        LocalizationFile file;
        try {
            file = getFile(name);
            return file.exists();
        } catch (ResourceNotFoundException e) {
            return false;
        }
    }
}
