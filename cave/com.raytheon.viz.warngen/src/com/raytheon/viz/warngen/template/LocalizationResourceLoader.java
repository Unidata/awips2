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
import java.io.InputStream;
import java.util.HashMap;
import java.util.Map;

import org.apache.commons.collections.ExtendedProperties;
import org.apache.velocity.exception.ResourceNotFoundException;
import org.apache.velocity.runtime.resource.Resource;
import org.apache.velocity.runtime.resource.loader.FileResourceLoader;

import com.raytheon.uf.common.dataplugin.warning.util.FileUtil;
import com.raytheon.uf.common.localization.FileUpdatedMessage;
import com.raytheon.uf.common.localization.ILocalizationFileObserver;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.LocalizationUtil;
import com.raytheon.uf.common.localization.exception.LocalizationException;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 18, 2011            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class LocalizationResourceLoader extends FileResourceLoader implements
        ILocalizationFileObserver {

    public static final String SITE_KEY = "SITE";

    private String site;

    private Map<String, LocalizationFile> fileMap = new HashMap<String, LocalizationFile>();

    @Override
    public long getLastModified(Resource resource) {
        return getFile(resource.getName()).getTimeStamp().getTime();
    }

    @Override
    public void init(ExtendedProperties configuration) {
        String site = configuration.getString(SITE_KEY);
        if (site != null && (site.equals(this.site) == false)) {
            // We changed sites since last time, clear out cache
            fileMap.clear();
        }
        this.site = site;
        super.init(configuration);
    }

    @Override
    public InputStream getResourceStream(String templateName)
            throws ResourceNotFoundException {
        try {
            return getFile(templateName).openInputStream();
        } catch (LocalizationException e) {
            throw new ResourceNotFoundException(e);
        }
    }

    private synchronized LocalizationFile getFile(String name) {
        LocalizationFile file = fileMap.get(name);
        if (file == null || file.exists() == false) {
            try {
                file = FileUtil.getLocalizationFile(name, site);
                file.addFileUpdatedObserver(this);
            } catch (FileNotFoundException e) {
                throw new RuntimeException("Error retrieving resource file", e);
            }
            fileMap.put(name, file);
        }
        return file;
    }

    @Override
    public synchronized void fileUpdated(FileUpdatedMessage message) {
        fileMap.remove(LocalizationUtil.extractName(message.getFileName()));
    }
}
