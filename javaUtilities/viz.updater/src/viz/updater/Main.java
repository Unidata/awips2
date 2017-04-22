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
package viz.updater;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.InputStream;
import java.nio.charset.Charset;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Enumeration;
import java.util.List;
import java.util.Map;
import java.util.HashMap;
import java.util.jar.JarEntry;
import java.util.jar.JarInputStream;
import java.util.zip.ZipEntry;
import java.util.zip.ZipFile;

import org.eclipse.pde.internal.core.feature.ExternalFeatureModel;

/**
 * Utility to scan the currently installed viz features and the features
 * provided by an Eclipse repository to determine which viz features are
 * eligible for an upgrade.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 23, 2015 4759       bkowal      Initial creation
 * 
 * </pre>
 * 
 * @author bkowal
 * @version 1.0
 */

@SuppressWarnings({ "restriction" })
public class Main {

    /*
     * The directory that CAVE has been installed to.
     */
    private static final String CAVE_INSTALL_ARG = "caveInstall";

    /*
     * The directory that the latest version of the Eclipse repositories are in.
     * The expectation is that this directory will contain zip files.
     */
    private static final String ECLIPSE_REPO_ARG = "eclipseRepo";

    /*
     * The file to write the features that should be upgraded to.
     */
    private static final String OUTPUT_FILE_ARG = "outputFile";

    private static final String FEATURES_DIRECTORY = "features";

    private static final String FEATURE_XML = "feature.xml";

    private static final String BASE_PRODUCT_FEATURE = "com.raytheon.uf.viz.application.feature";

    private Path caveInstallPath;

    private Path eclipseRepositoryPath;

    private Path outputPath;

    protected Main() throws Exception {
        this.init();
    }

    public static void main(String[] args) {
        try {
            new Main().execute();
        } catch (Exception e) {
            e.printStackTrace();
            System.exit(1);
        }
        System.exit(0);
    }

    private void init() throws Exception {
        String caveInstallDirectory = System.getProperty(CAVE_INSTALL_ARG);
        String eclipseRepositoryDirectory = System
                .getProperty(ECLIPSE_REPO_ARG);
        String outputFile = System.getProperty(OUTPUT_FILE_ARG);

        if (caveInstallDirectory == null) {
            throw new Exception("Property " + CAVE_INSTALL_ARG
                    + " must be specified.");
        }
        if (eclipseRepositoryDirectory == null) {
            throw new Exception("Property " + ECLIPSE_REPO_ARG
                    + " must be specified.");
        }
        if (outputFile == null) {
            throw new Exception("Property " + OUTPUT_FILE_ARG
                    + " must be specified.");
        }

        this.caveInstallPath = Paths.get(caveInstallDirectory).resolve(
                FEATURES_DIRECTORY);
        this.eclipseRepositoryPath = Paths.get(eclipseRepositoryDirectory);
        this.outputPath = Paths.get(outputFile);

        if (Files.exists(this.caveInstallPath) == false) {
            throw new Exception("The specified " + CAVE_INSTALL_ARG
                    + " does not exist!");
        }
        if (Files.exists(this.eclipseRepositoryPath) == false) {
            throw new Exception("The specified " + CAVE_INSTALL_ARG
                    + " does not exist!");
        }
        if (Files.exists(outputPath)) {
            throw new Exception("The specified output file: "
                    + this.outputPath.toString() + " already exists!");
        }
    }

    private Map<String, String> getCAVEFeatureVersions() throws Exception {
        Map<String, String> eclipseFeatureVersionsMap = new HashMap<>();
        for (Path featureDirPath : Files
                .newDirectoryStream(this.caveInstallPath)) {
            final Path featurePath = featureDirPath.resolve(FEATURE_XML);
            if (Files.exists(featurePath) == false) {
                continue;
            }
            try (InputStream is = Files.newInputStream(featurePath)) {
                ExternalFeatureModel externalFeature = new ExternalFeatureModel();
                externalFeature.load(is, false);

                eclipseFeatureVersionsMap.put(externalFeature.getFeature()
                        .getId(), externalFeature.getFeature().getVersion());
            }
        }
        return eclipseFeatureVersionsMap;
    }

    private Map<String, String> getRepositoryFeatureVersions(
            final ZipFile zipFile, ZipEntry zipEntry) throws Exception {
        Map<String, String> featureVersionsMap = new HashMap<>();
        try (InputStream is = zipFile.getInputStream(zipEntry);
                JarInputStream jis = new JarInputStream(is)) {
            JarEntry jarEntry = jis.getNextJarEntry();
            if (jarEntry == null) {
                return Collections.emptyMap();
            }
            if (FEATURE_XML.equals(jarEntry.getName()) == false) {
                return Collections.emptyMap();
            }

            byte[] data = this.readAllFeatureBytes(jis);
            ByteArrayInputStream bais = new ByteArrayInputStream(data);
            ExternalFeatureModel externalFeature = new ExternalFeatureModel();
            externalFeature.load(bais, false);

            if (BASE_PRODUCT_FEATURE.equals(externalFeature.getFeature()
                    .getId())) {
                return Collections.emptyMap();
            }

            featureVersionsMap.put(externalFeature.getFeature().getId(),
                    externalFeature.getFeature().getVersion());
        }

        return featureVersionsMap;
    }

    private byte[] readAllFeatureBytes(JarInputStream jis) throws Exception {
        byte[] buffer = new byte[1024];
        ByteArrayOutputStream os = new ByteArrayOutputStream();

        int line = 0;
        while ((line = jis.read(buffer)) != -1) {
            os.write(buffer, 0, line);
        }

        return os.toByteArray();
    }

    private List<String> scanRepositoryFeatures(
            final Map<String, String> caveFeatureVersions) throws Exception {
        List<String> featuresToUpgrade = new ArrayList<>();
        for (Path repositoryZipPath : Files.newDirectoryStream(
                eclipseRepositoryPath, "*.zip")) {
            try (ZipFile zipFile = new ZipFile(repositoryZipPath.toFile(),
                    ZipFile.OPEN_READ)) {
                Enumeration<?> zipContents = zipFile.entries();
                while (zipContents.hasMoreElements()) {
                    ZipEntry zipEntry = (ZipEntry) zipContents.nextElement();
                    if (zipEntry.getName().startsWith("features/")
                            && "features/".equals(zipEntry.getName()) == false) {

                        Map<String, String> featureVersionsMap = this
                                .getRepositoryFeatureVersions(zipFile, zipEntry);
                        if (featureVersionsMap.isEmpty()) {
                            continue;
                        }

                        for (String feature : featureVersionsMap.keySet()) {
                            final String repoVersion = featureVersionsMap
                                    .get(feature);
                            final String caveVersion = caveFeatureVersions
                                    .get(feature);
                            if (caveVersion == null) {
                                /*
                                 * Not currently installed in CAVE.
                                 */
                                continue;
                            }

                            if (this.upgradeEligible(caveVersion, repoVersion)) {
                                featuresToUpgrade.add(feature);
                            }
                        }
                    }
                }
            }
        }

        return featuresToUpgrade;
    }

    private boolean upgradeEligible(final String installedVersion,
            final String repoVersion) throws Exception {
        String[] installedComponents = installedVersion.split("\\.");
        String[] repoComponents = repoVersion.split("\\.");

        if (installedComponents.length != repoComponents.length) {
            throw new Exception("Incompatible versions detected!");
        }

        for (int i = 0; i < installedComponents.length; i++) {
            final String iC = installedComponents[i];
            final String rC = repoComponents[i];
            if (iC.equals(rC)) {
                continue;
            }

            final int iVersion = Integer.parseInt(iC);
            final int rVersion = Integer.parseInt(rC);
            return rVersion > iVersion;
        }

        return false;
    }

    private void execute() throws Exception {
        /*
         * Get the cave feature versions.
         */
        Map<String, String> featureVersions = this.getCAVEFeatureVersions();
        List<String> featuresToUpgrade = this
                .scanRepositoryFeatures(featureVersions);

        if (featuresToUpgrade.isEmpty()) {
            return;
        }
        Files.write(this.outputPath, featuresToUpgrade,
                Charset.defaultCharset());
    }
}