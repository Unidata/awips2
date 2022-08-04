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
package com.raytheon.uf.common.localization;

import java.io.File;
import java.util.List;

import com.google.common.collect.Lists;
import com.raytheon.uf.common.localization.TestPathManager.TestLocalizationAdapter;

/**
 * {@link ILocalizationAdapter} implementation for tests running in Eclipse. If
 * your common baseline repository name is not "AWIPS2_baseline" then set a
 * system property named "common.baseline.repo.name" to override it. i.e. in
 * your Eclipse launcher you would add a VM argument
 * "-Dcommon.baseline.repo.name=awips2_common_baseline" or something similar.
 * 
 * TODO: Changes will need to be made somehow when this moves to the common
 * baseline... How will we add work assignment specific tests while reusing old
 * tests, perhaps linking source directories from a work-assignment specific
 * project?
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 18, 2012 740        djohnson     Initial creation
 * Oct 23, 2012 1286       djohnson     Create Eclipse and command-line specific versions.
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */
public class EclipseTestLocalizationAdapter extends TestLocalizationAdapter {

    private static final String EDEX_OSGI = "edexOsgi";

    private static final File EDEX_OSGI_DIR = findEdexOsgiDir();

    /**
     * @param site
     * @param savedLocalizationFileDir
     */
    public EclipseTestLocalizationAdapter(String site,
            File savedLocalizationFileDir) {
        super(site, savedLocalizationFileDir);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    List<File> getDirectoriesWithPlugins() {

        List<File> retVal = Lists.newArrayList();
        retVal.add(EDEX_OSGI_DIR);
        
        File commonBaseline = TestPathManager
                .getCommonBaselineEnvironmentPath();

        if (commonBaseline != null) {
            File commonEdexOsgiDir = new File(commonBaseline, EDEX_OSGI);
            if (!retVal.contains(commonEdexOsgiDir)) {
                retVal.add(commonEdexOsgiDir);
            }
        }
        
        return retVal;
    }

    /**
     * Finds the edexOsgi directory.
     * 
     * @return the {@link File} reference to the edexOsgi directory
     */
    private static File findEdexOsgiDir() {
        File currentDir = new File(System.getProperty("user.dir"));
        File edexOsgiDir = new File(currentDir, EDEX_OSGI);

        while (!edexOsgiDir.isDirectory() && currentDir != null) {
            currentDir = currentDir.getParentFile();
            edexOsgiDir = new File(currentDir, EDEX_OSGI);
        }

        if (!edexOsgiDir.isDirectory()) {
            throw new IllegalStateException(
                    "Unable to find the edexOsgi directory!");
        }

        return edexOsgiDir;
    }

}
