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
import java.io.FileFilter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import com.raytheon.edex.utility.EDEXLocalizationAdapter;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.util.FileUtil;

/**
 * {@link IPathManager} implementation for tests.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 18, 2012 740        djohnson     Initial creation
 * Oct 23, 2012 1286       djohnson     Change to find more localization files.
 * Jan 16, 2013 1487       djohnson     Avoid adding new localization files to baseline utility directories.
 * May 31, 2013 1650       djohnson     Fix incorrect merge from vlab branch.
 * Sep 05, 2013 2307       dgilling     Better handling of directories in getPath().
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */
public class TestPathManager extends PathManager {

    /**
     * Construct a {@link TestPathManager} with the appropriate extension of
     * {@link TestLocalizationAdapter} to fit the environment tests are
     * executing in.
     * 
     * @param adapter
     *            the adapter
     */
    TestPathManager(TestLocalizationAdapter adapter) {
        super(adapter);
    }

    /**
     * {@link ILocalizationAdapter} implementation for running tests.
     * 
     */
    public static abstract class TestLocalizationAdapter extends
            EDEXLocalizationAdapter {

        private static final String UTILITY = "utility";

        private final File savedLocalizationFileDir;

        private final String site;

        private List<File> utilityDirs;

        /**
         * @param site
         * @param savedLocalizationFileDir
         * @param pluginDirectories
         */
        public TestLocalizationAdapter(String site,
                File savedLocalizationFileDir) {
            this.site = site;
            this.savedLocalizationFileDir = savedLocalizationFileDir;
        }

        /*
         * (non-Javadoc)
         * 
         * @see com.raytheon.edex.utility.EDEXLocalizationAdapter#getSiteName()
         */
        @Override
        protected String getSiteName() {
            return site;
        }

        /**
         * Duplicates the {@link EDEXLocalizationAdapter} version, adding the
         * ability to check multiple utility directories.
         */
        @Override
        public File getPath(LocalizationContext context, String fileName) {
            File foundFile = null;

            List<File> utilityDirs = getUtilityDirs();

            if (context.getLocalizationLevel() == LocalizationLevel.UNKNOWN) {
                throw new IllegalArgumentException(
                        "Unsupported localization level:"
                                + context.getLocalizationLevel());
                // } else if (false) {
                // TODO: Check for invalid type / level combinations
                // Change the above condition and add invalid type / level
                // checking
                // if needed
            }

            for (File utilityDir : utilityDirs) {
                File baseDir = new File(utilityDir, context.toPath());
                File toFind = new File(baseDir, fileName);

                if (toFind.exists()) {
                    if (!toFind.getAbsolutePath().startsWith(
                            savedLocalizationFileDir.getAbsolutePath())) {
                        foundFile = createTestIsolatedVersionOfLocalizationFile(
                                context, fileName, toFind);
                    } else {
                        foundFile = toFind;
                    }
                }

                // if we found an actual file, we can be reasonably sure there's
                // only one file in the whole project with that name and can
                // stop looking. However, if we found a directory there may be
                // multiple plugins contributing to that directory and we have
                // to search the whole collection of directories to be sure
                // we've got all files that belong in that dir.
                if (toFind.isFile()) {
                    break;
                }
            }

            if (foundFile != null) {
                return foundFile;
            } else {
                // Have to create a default File and the previous revision's
                // behavior was just to use the last File in utilityDirs. This
                // ensures that the path desired gets created as empty.
                return createTestIsolatedVersionOfLocalizationFile(context,
                        fileName,
                        new File(utilityDirs.get(utilityDirs.size() - 1),
                                fileName));
            }
        }

        /**
         * Creates a test isolated version of the localization file. Allows the
         * file to be written to, and changes to be read back, without affecting
         * the baselined version of the file.
         * 
         * @param context
         *            the context
         * @param fileName
         *            the file path
         * @param baselinedVersion
         *            the file reference
         * @return
         */
        private File createTestIsolatedVersionOfLocalizationFile(
                LocalizationContext context, String fileName,
                File baselinedVersion) {
            File savedFileBaseDir = new File(savedLocalizationFileDir,
                    context.toPath());
            File savedFile = new File(savedFileBaseDir, fileName);
            savedFile.getParentFile().mkdirs();

            try {
                if (baselinedVersion.exists()) {
                    if (baselinedVersion.isDirectory()) {
                        FileUtil.copyDirectory(baselinedVersion, savedFile);
                    } else {
                        FileUtil.copyFile(baselinedVersion, savedFile);
                    }
                }
            } catch (IOException e) {
                throw new RuntimeException(e);
            }
            return savedFile;
        }

        /*
         * (non-Javadoc)
         * 
         * @see
         * com.raytheon.edex.utility.EDEXLocalizationAdapter#getUtilityDir()
         */
        protected List<File> getUtilityDirs() {
            if (utilityDirs == null) {
                utilityDirs = new ArrayList<File>();

                // The directory which will have "saved" utility files
                utilityDirs.add(savedLocalizationFileDir);

                // Test overrides
                utilityDirs.add(new File(UTILITY));

                List<File> pluginDirectories = getDirectoriesWithPlugins();

                // Build.edex utility directory
                File buildEdexDir = null;
                for (File pluginDirectory : pluginDirectories) {
                    File potential = new File(pluginDirectory, "build.edex");
                    if (potential.isDirectory()) {
                        buildEdexDir = potential;
                        break;
                    }
                }

                File esbDataUtility = new File(buildEdexDir, "esb/data/utility");
                if (buildEdexDir != null && buildEdexDir.isDirectory()) {
                    utilityDirs.add(esbDataUtility);
                }

                // Add common baseline path if the test is in a work assignment
                // folder
                File commonBaselinePath = getCommonBaselineEnvironmentPath();
                if (commonBaselinePath != null) {
                    buildEdexDir = new File(commonBaselinePath, FileUtil.join(
                            "edexOsgi", "build.edex"));
                    esbDataUtility = new File(buildEdexDir, "esb/data/utility");

                    if (buildEdexDir.isDirectory()
                            && !utilityDirs.contains(esbDataUtility)) {
                        utilityDirs.add(esbDataUtility);
                    }
                }

                if (buildEdexDir == null) {
                    throw new RuntimeException(
                            "Unable to find the build.edex directory!");
                }

                // Plugin utility directories
                for (File pluginDir : pluginDirectories) {
                    File[] plugins = pluginDir.listFiles(new FileFilter() {
                        @Override
                        public boolean accept(File pathname) {
                            return pathname.isDirectory();
                        }
                    });
                    for (File plugin : plugins) {
                        File utilityDir = new File(plugin, UTILITY);
                        if (utilityDir.isDirectory()) {
                            utilityDirs.add(utilityDir);
                        }
                    }
                }
            }

            return utilityDirs;
        }

        /**
         * Return the list of directories that contain plugins. This will be
         * used to find the build.edex directory, along with checking for any
         * plugin-provided utility directories.
         * 
         * @return
         */
        abstract List<File> getDirectoriesWithPlugins();
    }

    /**
     * Get the common baseline repo path from the environment variable.
     * 
     * @return the file reference, or null if none was provided
     */
    public static File getCommonBaselineEnvironmentPath() {
        // work assignment workaround to for tests since we don't
        // find files if we don't know the baseline repo's location
        // THIS MEANS THAT FOR WORK ASSIGNMENTS WE MUST HAVE
        // BASELINE_DIR SET
        final String commonBaselineVersion = System.getenv("baseline_dir");
        if (commonBaselineVersion != null) {
            File dir = new File(commonBaselineVersion.replace("\n", ""));
            if (dir.isDirectory()) {
                return dir;
            }
        }
        return null;
    }
}
