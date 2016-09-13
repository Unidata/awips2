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
package awips.dependency.evaluator;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.HashMap;
import java.util.Set;
import java.util.jar.Attributes;
import java.util.jar.JarInputStream;
import java.util.jar.Manifest;
import java.io.File;
import java.io.FilenameFilter;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.PrintWriter;
import java.nio.charset.Charset;
import java.nio.file.DirectoryStream;
import java.nio.file.FileVisitResult;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.SimpleFileVisitor;
import java.nio.file.attribute.BasicFileAttributes;

import org.eclipse.osgi.util.ManifestElement;
import org.eclipse.pde.internal.core.feature.ExternalFeatureModel;
import org.eclipse.pde.internal.core.ifeature.IFeature;
import org.eclipse.pde.internal.core.ifeature.IFeatureChild;
import org.eclipse.pde.internal.core.ifeature.IFeaturePlugin;
import org.osgi.framework.Constants;
import org.eclipse.pde.internal.core.iproduct.IProductFeature;
import org.eclipse.pde.internal.core.iproduct.IProductModelFactory;
import org.eclipse.pde.internal.core.product.ProductModel;

/**
 * Utility to scan AWIPS features, verify that all dependencies have been
 * satisfied and write the dependencies out in the order that they should be
 * built.
 * 
 * This utility will eventually be extended to update the version information in
 * products and features so that we can enforce which version specific
 * components are compatible with.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 29, 2015 4759       bkowal      Initial creation
 * Oct 09, 2015 4759       bkowal      Added an exclude features parameter.
 * Oct 20, 2015 4759       bkowal      Handle non-Eclipse features that are
 *                                     embedded in other features.
 * Oct 23, 2015 4759       bkowal      Support fully upgradeable RCP products.
 * 
 * </pre>
 * 
 * @author bkowal
 * @version 1.0
 */

@SuppressWarnings("restriction")
public class Main {

    public static final String XML_EXTENSION = ".xml";

    private static final String PLUGINS_DIRECTORY = "plugins";

    private static final String FEATURES_DIRECTORY = "features";

    /*
     * The directory containing Eclipse.
     */
    private static final String BASE_LOCATION_ARG = "baseLocation";

    /*
     * The directory containing plugins and features sub-directories that the
     * baselined plugins and features will be copied into prior to the build.
     */
    private static final String BUILD_DIRECTORY_ARG = "buildDirectory";

    /*
     * The directory that all plugins and features that should be considered
     * during the build have been copied into. This property may eventually be
     * extended to allow for the specification of a delimiter-separated list of
     * directories to scan (however, the risk of modifying a product file would
     * be possible in this case).
     */
    private static final String STAGING_DIRECTORY_ARG = "stagingDirectory";

    /*
     * Argument specifying the minimum set of feature(s), as a comma-separated
     * list, that will need to be prepared for building. Set will be extended
     * with any features that are dependent on the features that are built. Use
     * '*' to specify all features that are discovered in the staging area.
     */
    private static final String BUILD_FEATURES_ARG = "buildFeatures";

    private static final String EXCLUDE_FEATURES_ARG = "excludeFeatures";

    /*
     * Argument specifying the product that will need to be prepared for
     * building. The existence of all features specified in the product will be
     * verified. Additionally, required dependencies will be added and the
     * features will be re-ordered if necessary.
     */
    private static final String BUILD_PRODUCT_ARG = "buildProduct";

    private static final String OUTPUT_FILE_ARG = "outputFile";

    /*
     * A single feature that will act as a base feature. Other features listed
     * in the product will be installed on top of this feature separately so
     * that they become upgradeable.
     */
    private static final String BASE_UPGRADE_FEATURE_ARG = "baseUpgrade";

    private Path eclipsePluginsPath;

    private Path eclipseFeaturesPath;

    private Path buildPluginsPath;

    private Path buildFeaturesPath;

    private Path stagingPath;

    private Path outputPath;

    private String buildFeatures;

    private String excludeFeatures;

    private String buildProduct;

    private String baseFeature;

    private static final String COMMON_BASE_FEATURE = "com.raytheon.uf.common.base.feature";

    private static final String VIZ = "viz";

    private static final String FEATURE = "feature";

    private static final String ALL_FEATURES = "*";

    private static final String CSV_SEPARATOR = ",";

    /*
     * Staged plugins and features that will be included in the build.
     */
    private final Map<String, BuildFeature> buildFeaturesMap = new HashMap<>();

    private final Map<String, BuildPlugin> buildPluginsMap = new HashMap<>();

    /*
     * Staged plugin metadata maps.
     */
    private final Map<String, List<String>> pluginExportedPackagesMap = new HashMap<>();

    private final Map<String, List<String>> pluginImportedPackagesMap = new HashMap<>();

    private final Map<String, List<String>> pluginRequiredBundlesMap = new HashMap<>();

    /*
     * Eclipse-provided plugin metadata.
     */
    private final List<String> eclipsePluginExportedPackages = new ArrayList<>();

    private final List<String> eclipsePlugins = new ArrayList<>();

    public static void main(String[] args) {
        try {
            new Main().execute();
        } catch (Exception e) {
            e.printStackTrace();
            System.exit(1);
        }

        System.exit(0);
    }

    protected Main() throws Exception {
        this.init();
    }

    private void init() throws Exception {
        String baseLocationDirectory = System.getProperty(BASE_LOCATION_ARG);
        if (baseLocationDirectory == null) {
            throw new Exception("Required property: " + BASE_LOCATION_ARG
                    + " has not been specified.");
        }
        String buildDirectory = System.getProperty(BUILD_DIRECTORY_ARG);
        if (buildDirectory == null) {
            throw new Exception("Required property: " + BUILD_DIRECTORY_ARG
                    + " has not been specified.");
        }
        String stagingDirectory = System.getProperty(STAGING_DIRECTORY_ARG);
        if (stagingDirectory == null) {
            throw new Exception("Required property: " + STAGING_DIRECTORY_ARG
                    + " has not been specified.");
        }
        this.buildFeatures = System.getProperty(BUILD_FEATURES_ARG);
        this.excludeFeatures = System.getProperty(EXCLUDE_FEATURES_ARG);
        this.buildProduct = System.getProperty(BUILD_PRODUCT_ARG);
        this.baseFeature = System.getProperty(BASE_UPGRADE_FEATURE_ARG);
        /*
         * At least one must be specified.
         */
        if (this.buildFeatures == null && this.buildProduct == null) {
            throw new Exception("Property " + BUILD_FEATURES_ARG
                    + " or property " + BUILD_PRODUCT_ARG
                    + " must be specified.");
        }
        /*
         * However, both cannot be specified.
         */
        if (this.buildFeatures != null
                && (this.buildProduct != null && ALL_FEATURES
                        .equals(this.buildFeatures) == false)) {
            throw new Exception("Both property " + BUILD_FEATURES_ARG
                    + " and property " + BUILD_PRODUCT_ARG
                    + " cannot be specified at the same time.");
        }
        /*
         * Both base feature and product must be specified.
         */
        if (this.baseFeature != null && this.buildProduct == null) {
            throw new Exception("Property " + BUILD_PRODUCT_ARG
                    + " must be specified when using the "
                    + BASE_UPGRADE_FEATURE_ARG + " property.");
        }

        Path baseLocationPath = Paths.get(baseLocationDirectory);
        if (Files.exists(baseLocationPath) == false) {
            throw new Exception("The specified " + BASE_LOCATION_ARG
                    + " does not exist!");
        }
        this.eclipsePluginsPath = baseLocationPath.resolve(PLUGINS_DIRECTORY);
        if (Files.exists(eclipsePluginsPath) == false) {
            throw new Exception(
                    "The Eclipse plugins directory does not exist at the expected location: "
                            + eclipsePluginsPath.toString() + "!");
        }
        this.eclipseFeaturesPath = baseLocationPath.resolve(FEATURES_DIRECTORY);
        if (Files.exists(eclipseFeaturesPath) == false) {
            throw new Exception(
                    "The Eclipse features directory does not exist at the expected location: "
                            + eclipsePluginsPath.toString() + "!");
        }
        this.stagingPath = Paths.get(stagingDirectory);
        if (Files.exists(stagingPath) == false) {
            throw new Exception("The specified " + STAGING_DIRECTORY_ARG
                    + " does not exist!");
        }
        final Path buildPath = Paths.get(buildDirectory);
        this.buildPluginsPath = buildPath.resolve(PLUGINS_DIRECTORY);
        this.buildFeaturesPath = buildPath.resolve(FEATURES_DIRECTORY);
        if (Files.exists(buildPluginsPath) == false) {
            /*
             * Attempt to create the directory.
             */
            Files.createDirectories(buildPluginsPath);
        }
        if (Files.exists(buildFeaturesPath) == false) {
            /*
             * Attempt to create the directory.
             */
            Files.createDirectories(buildFeaturesPath);
        }

        String outputFile = System.getProperty(OUTPUT_FILE_ARG);
        if (outputFile != null) {
            this.outputPath = Paths.get(outputFile);
            if (Files.exists(this.outputPath)) {
                throw new Exception("The specified output file: "
                        + this.outputPath.toString() + " already exists!");
            }

            /*
             * Verify that the intermediate directories exist.
             */
            if (Files.exists(this.outputPath.getParent()) == false) {
                /*
                 * Attempt to create the intermediate directories.
                 */
                Files.createDirectories(this.outputPath.getParent());
            }
        }
    }

    private void execute() throws Exception {
        List<BuildFeature> stagedFeatures = this.findFeatures();

        if (stagedFeatures.isEmpty()) {
            return;
        }

        for (BuildFeature buildFeature : stagedFeatures) {
            this.scanFeature(buildFeature, null);
        }

        this.determineDependencies();
        this.determineBuildOrder();
    }

    private void determineBuildOrder() throws Exception {
        /*
         * Determine the minimum set of features that should be included in the
         * build order.
         */
        Set<BuildFeature> featuresToBuild = new LinkedHashSet<>();
        boolean allFeatures = false;
        ProductModel productModel = null;
        Path productPath = null;
        // Do we need to read a product?
        if (this.buildProduct == null) {
            /*
             * Determine if all features should be built or if only a subset of
             * the features should be built.
             */
            if (ALL_FEATURES.equals(this.buildFeatures)) {
                featuresToBuild.addAll(this.buildFeaturesMap.values());
                allFeatures = true;
            } else {
                for (String id : this.buildFeatures.split(CSV_SEPARATOR)) {
                    BuildFeature buildFeature = this.buildFeaturesMap.get(id
                            .trim());
                    if (buildFeature == null) {
                        throw new Exception("Unable to find feature: "
                                + id.trim() + "!");
                    }
                    featuresToBuild.add(buildFeature);
                }
            }
        } else {
            /*
             * Read the product. If all features are requested to be built as
             * part of the product, we will skip reading the product because we
             * will eventually write a new product with all features in order.
             * This will allow products to only declare the high-level features
             * that differentiate them rather then needing to worry about all of
             * the core and common features in order.
             */
            productPath = this.stagingPath.resolve(this.buildProduct);
            if (Files.exists(productPath) == false) {
                throw new Exception("Unable to find the specified product: "
                        + productPath.toString() + "!");
            }

            productModel = new ProductModel();
            try (InputStream is = Files.newInputStream(productPath)) {
                productModel.load(is, false);
            }

            if (ALL_FEATURES.equals(this.buildFeatures)) {
                featuresToBuild.addAll(this.buildFeaturesMap.values());
                allFeatures = true;
            } else {
                IProductFeature[] features = productModel.getProduct()
                        .getFeatures();
                for (int i = 0; i < features.length; i++) {
                    IProductFeature feature = features[i];
                    BuildFeature buildFeature = this.buildFeaturesMap
                            .get(feature.getId());
                    if (buildFeature == null) {
                        throw new Exception("Unable to find feature: "
                                + feature.getId() + "!");
                    }
                    featuresToBuild.add(buildFeature);
                }
            }
            productModel.getProduct().removeFeatures(
                    productModel.getProduct().getFeatures());
        }

        // Ensure all dependencies are present for the features that need to be
        // built.
        if (allFeatures == false && this.buildProduct == null) {
            Set<BuildFeature> dependenciesToAdd = new HashSet<>();
            for (BuildFeature buildFeature : featuresToBuild) {
                this.addDependentFeatures(dependenciesToAdd, buildFeature);
            }

            featuresToBuild.addAll(dependenciesToAdd);
        }

        /*
         * Single dimensional sort is not enough. For, now as this is currently
         * the case, the features are in the order that they will need to be
         * built in the product.
         */
        List<BuildFeature> orderedFeatures = new LinkedList<>(featuresToBuild);

        if (productModel != null) {
            IProductModelFactory productModelFactory = productModel
                    .getFactory();
            List<IProductFeature> productFeatures = new LinkedList<>();
            if (baseFeature == null) {
                for (int i = 0; i < orderedFeatures.size(); i++) {
                    IProductFeature productFeature = productModelFactory
                            .createFeature();
                    productFeature.setId(orderedFeatures.get(i).getId());
                    productFeatures.add(productFeature);
                }
            } else {
                IProductFeature productFeature = productModelFactory
                        .createFeature();
                productFeature.setId(this.baseFeature);
                productFeatures.add(productFeature);
            }
            productModel.getProduct().addFeatures(
                    productFeatures.toArray(new IProductFeature[0]));

            try (OutputStream os = Files.newOutputStream(productPath);
                    PrintWriter writer = new PrintWriter(os)) {
                productModel.getProduct().write("", writer);
            }
        }

        if (this.outputPath != null) {
            List<String> orderedFeatureIds = new LinkedList<>();
            for (int i = 0; i < orderedFeatures.size(); i++) {
                orderedFeatureIds.add(orderedFeatures.get(i).getId());
            }
            Files.write(this.outputPath, orderedFeatureIds,
                    Charset.defaultCharset());
        }

        /*
         * Prepare the plugins and features for a build. No more mass copying of
         * all plugins and features in the baseline.
         */
        for (BuildFeature buildFeature : orderedFeatures) {
            if (buildFeature.isEclipse()) {
                /*
                 * Do not copy Eclipse features.
                 */
                continue;
            }
            final Path copyFeaturePath = this.buildFeaturesPath
                    .resolve(buildFeature.getId());
            Files.createDirectories(copyFeaturePath);
            this.copyResource(buildFeature.getFeaturePath().getParent(),
                    copyFeaturePath);

            for (BuildPlugin buildPlugin : buildFeature.getManagedPlugins()) {
                if (buildPlugin.isEclipse()) {
                    continue;
                }
                final Path copyPluginPath = this.buildPluginsPath
                        .resolve(buildPlugin.getId());
                Files.createDirectories(copyPluginPath);
                this.copyResource(buildPlugin.getPluginPath(), copyPluginPath);
            }
        }
    }

    private void copyResource(final Path sourcePath, final Path destinationPath)
            throws Exception {
        Files.walkFileTree(sourcePath, new SimpleFileVisitor<Path>() {
            @Override
            public FileVisitResult preVisitDirectory(Path dir,
                    BasicFileAttributes attrs) throws IOException {
                Path targetPath = destinationPath.resolve(sourcePath
                        .relativize(dir));
                if (Files.exists(targetPath) == false) {
                    Files.createDirectory(targetPath);
                }
                return FileVisitResult.CONTINUE;
            }

            @Override
            public FileVisitResult visitFile(Path file,
                    BasicFileAttributes attrs) throws IOException {
                Path targetPath = destinationPath.resolve(sourcePath
                        .relativize(file));
                if (Files.exists(targetPath) == false) {
                    Files.copy(file, destinationPath.resolve(sourcePath
                            .relativize(file)));
                }
                return FileVisitResult.CONTINUE;
            }
        });
    }

    private void topoSort(BuildFeature feature,
            List<BuildFeature> orderedFeatures,
            Set<BuildFeature> allowedFeatures) {
        if (feature.isTopoMark()) {
            return;
        }
        feature.mark();
        if (feature.getDependentFeatures().isEmpty() == false) {
            for (BuildFeature dependent : feature.getDependentFeatures()) {
                if (allowedFeatures.contains(dependent) == false) {
                    continue;
                }
                this.topoSort(dependent, orderedFeatures, allowedFeatures);
            }
        }
        orderedFeatures.add(0, feature);
    }

    private void addDependentFeatures(Set<BuildFeature> featuresSet,
            BuildFeature buildFeature) {
        if (buildFeature.getDependencyFeatures().isEmpty()) {
            return;
        }
        for (BuildFeature dependency : buildFeature.getDependencyFeatures()) {
            this.addDependentFeatures(featuresSet, dependency);
            featuresSet.add(dependency);
        }
    }

    private void determineDependencies() throws Exception {
        for (String pluginId : this.buildPluginsMap.keySet()) {
            BuildPlugin buildPlugin = this.buildPluginsMap.get(pluginId);
            if (buildPlugin.isEclipse()) {
                /*
                 * Allow Eclipse to manage its own dependencies.
                 */
                continue;
            }

            /*
             * First, examine the required bundles.
             */
            List<String> requiredBundles = this.pluginRequiredBundlesMap
                    .get(pluginId);
            if (requiredBundles != null) {
                for (String requiredBundle : requiredBundles) {
                    BuildPlugin buildPluginDependency = this.buildPluginsMap
                            .get(requiredBundle);
                    if (buildPluginDependency == null) {
                        throw new Exception(
                                "Unable to resolve the Required Bundle: "
                                        + requiredBundle
                                        + " dependency for plugin: " + pluginId
                                        + "!");
                    }
                    /*
                     * Is the plugin already in the associated feature? Will
                     * become a common scenario as we add certain plugins to
                     * every feature that requires it to reduce the dependency
                     * between features.
                     */
                    if (buildPluginDependency.isProvidedByFeatures(buildPlugin
                            .getContainingFeatures())) {
                        /*
                         * no further dependency resolution required for this
                         * bundle.
                         */
                        continue;
                    }

                    for (BuildFeature dependencyFeature : buildPluginDependency
                            .getContainingFeatures()) {
                        dependencyFeature.addDependentFeatures(
                                buildPlugin.getContainingFeatures(),
                                requiredBundle, pluginId);
                        for (BuildFeature containingFeature : buildPlugin
                                .getContainingFeatures()) {
                            containingFeature
                                    .addDependencyFeature(dependencyFeature);
                        }
                    }
                }
            }

            /*
             * Next, examine the imported packages.
             */
            List<String> importedPackages = this.pluginImportedPackagesMap
                    .get(pluginId);
            if (importedPackages != null) {
                for (String importedPackage : importedPackages) {
                    if (this.eclipsePluginExportedPackages
                            .contains(importedPackage)) {
                        continue;
                    }

                    List<String> packageProviders = this.pluginExportedPackagesMap
                            .get(importedPackage);
                    if (packageProviders == null || packageProviders.isEmpty()) {
                        throw new Exception(
                                "Unable to resolve the Imported Package: "
                                        + importedPackage
                                        + " dependency for plugin: " + pluginId
                                        + "!");
                    }
                    for (String packageProvider : packageProviders) {
                        BuildPlugin buildPluginDependency = this.buildPluginsMap
                                .get(packageProvider);
                        if (buildPluginDependency == null) {
                            /*
                             * Unlikely due to the previously completed plugin
                             * scanning.
                             */
                            throw new Exception("Failed to find plugin: "
                                    + packageProvider + "!");
                        }
                        /*
                         * Is the plugin already in the associated feature? Will
                         * become a common scenario as we add certain plugins to
                         * every feature that requires it to reduce the
                         * dependency between features.
                         */
                        if (buildPluginDependency
                                .isProvidedByFeatures(buildPlugin
                                        .getContainingFeatures())) {
                            /*
                             * no further dependency resolution required for
                             * this bundle.
                             */
                            continue;
                        }

                        for (BuildFeature dependencyFeature : buildPluginDependency
                                .getContainingFeatures()) {
                            dependencyFeature.addDependentFeatures(
                                    buildPlugin.getContainingFeatures(),
                                    importedPackage, pluginId);
                            for (BuildFeature containingFeature : buildPlugin
                                    .getContainingFeatures()) {
                                containingFeature
                                        .addDependencyFeature(dependencyFeature);
                            }
                        }
                    }
                }
            }
        }
    }

    /*
     * Find all *viz* features as well as the standard common base feature in
     * the staging directory.
     */
    private List<BuildFeature> findFeatures() {
        /*
         * Determine if there are any features that should be excluded ...
         */
        final List<String> excludedFeaturesList;
        if (this.excludeFeatures != null
                && this.excludeFeatures.isEmpty() == false) {
            excludedFeaturesList = new ArrayList<>();
            for (String excludedFeature : this.excludeFeatures
                    .split(CSV_SEPARATOR)) {
                excludedFeaturesList.add(excludedFeature);
            }
        } else {
            excludedFeaturesList = Collections.emptyList();
        }

        final List<BuildFeature> stagedFeatures = new ArrayList<>();
        this.stagingPath.toFile().list(new FilenameFilter() {
            @Override
            public boolean accept(File dir, String name) {
                if ((name.contains(FEATURE) && name.contains(VIZ))
                        || COMMON_BASE_FEATURE.equals(name)) {
                    if (excludedFeaturesList.contains(name)) {
                        return false;
                    }
                    final Path featurePath = stagingPath.resolve(name).resolve(
                            BuildFeature.FILENAME);
                    /*
                     * impossible for the {@link Path} not to exist based on how
                     * it is discovered?
                     */
                    stagedFeatures.add(new BuildFeature(featurePath));
                }

                return false;
            }
        });

        return stagedFeatures;
    }

    private void scanFeature(final BuildFeature buildFeature,
            BuildFeature containingBuildFeature) throws Exception {
        if (containingBuildFeature == null) {
            containingBuildFeature = buildFeature;
        }

        ExternalFeatureModel externalFeature = new ExternalFeatureModel();
        try (InputStream is = Files.newInputStream(buildFeature
                .getFeaturePath())) {
            externalFeature.load(is, false);
        }

        /*
         * Evaluate the included plugins.
         */
        IFeature feature = externalFeature.getFeature();
        if (buildFeature.getId() == null) {
            buildFeature.setId(feature.getId());
        }
        for (IFeaturePlugin featurePlugin : feature.getPlugins()) {
            BuildPlugin buildPlugin = this.buildPluginsMap.get(featurePlugin
                    .getId());
            if (buildPlugin != null) {
                buildPlugin.addContainingFeature(containingBuildFeature);
                continue;
            }

            /*
             * TODO: if we ever have to handle the case of a lazy rename in
             * which the plugin was renamed in metadata; however, the plugin was
             * never actually copied into a directory with the new name, we will
             * also need to pre-scan the plugins in staging to build a map of id
             * to plugin path.
             */
            /*
             * Determine if the plugin is available in staging.
             */
            Path potentialPluginPath = this.stagingPath.resolve(featurePlugin
                    .getId());
            if (Files.exists(potentialPluginPath)) {
                buildPlugin = new BuildPlugin(featurePlugin.getId(),
                        potentialPluginPath, false, containingBuildFeature);
                this.scanStagingPlugin(buildPlugin);
                this.buildPluginsMap.put(featurePlugin.getId(), buildPlugin);
            } else {
                this.scanEclipsePlugin(featurePlugin.getId(),
                        containingBuildFeature);
            }
        }

        /*
         * Evaluate the included features.
         */
        for (IFeatureChild featureChild : feature.getIncludedFeatures()) {
            final Path expectedFeaturePath = this.stagingPath.resolve(
                    featureChild.getId()).resolve(BuildFeature.FILENAME);
            BuildFeature embeddedBuildFeature = null;
            if (Files.exists(expectedFeaturePath)) {
                embeddedBuildFeature = new BuildFeature(expectedFeaturePath);
            } else {
                List<Path> resourcePaths = this.getEclipseResource(
                        featureChild.getId(), this.eclipseFeaturesPath);
                if (resourcePaths.isEmpty()) {
                    throw new Exception("Failed to find feature with id: "
                            + featureChild.getId() + " for "
                            + containingBuildFeature.getId() + "!");
                }
                embeddedBuildFeature = new BuildFeature(resourcePaths.get(0)
                        .resolve(BuildFeature.FILENAME), true);
            }
            this.scanFeature(embeddedBuildFeature, containingBuildFeature);
        }
        this.buildFeaturesMap.put(feature.getId(), buildFeature);
    }

    private void scanStagingPlugin(final BuildPlugin buildPlugin)
            throws Exception {
        if (buildPluginsMap.containsKey(buildPlugin.getId())) {
            /*
             * The plugin is only added to the build plugins map after it has
             * been scanned.
             */
            return;
        }
        final Path manifestMFPath = buildPlugin.getPluginPath().resolve(
                BuildPlugin.MANIFEST_MF);
        if (Files.exists(manifestMFPath) == false) {
            throw new Exception("Failed to find the manifest file for plugin: "
                    + buildPlugin.getId() + "! Expected manifest to be at: "
                    + manifestMFPath.toString() + ".");
        }

        try (InputStream is = Files.newInputStream(manifestMFPath)) {
            final Manifest manifest = new Manifest(is);
            Attributes mainAttributes = manifest.getMainAttributes();

            String attributeValue = mainAttributes
                    .getValue(Constants.EXPORT_PACKAGE);
            if (attributeValue != null) {
                ManifestElement[] elements = ManifestElement.parseHeader(
                        Constants.EXPORT_PACKAGE, attributeValue);
                for (ManifestElement element : elements) {
                    String exportedPackage = element.getValue();
                    if (this.pluginExportedPackagesMap
                            .containsKey(exportedPackage) == false) {
                        this.pluginExportedPackagesMap.put(exportedPackage,
                                new ArrayList<String>());
                    }
                    this.pluginExportedPackagesMap.get(exportedPackage).add(
                            buildPlugin.getId());
                }
            }

            attributeValue = mainAttributes.getValue(Constants.IMPORT_PACKAGE);
            if (attributeValue != null) {
                ManifestElement[] elements = ManifestElement.parseHeader(
                        Constants.IMPORT_PACKAGE, attributeValue);
                for (ManifestElement element : elements) {
                    String importedPackage = element.getValue();
                    if (this.pluginImportedPackagesMap.containsKey(buildPlugin
                            .getId()) == false) {
                        this.pluginImportedPackagesMap.put(buildPlugin.getId(),
                                new ArrayList<String>());
                    }
                    this.pluginImportedPackagesMap.get(buildPlugin.getId())
                            .add(importedPackage);
                }
            }

            attributeValue = mainAttributes.getValue(Constants.REQUIRE_BUNDLE);
            if (attributeValue != null) {
                ManifestElement[] elements = ManifestElement.parseHeader(
                        Constants.REQUIRE_BUNDLE, attributeValue);
                for (ManifestElement element : elements) {
                    String requiredBundle = element.getValue();
                    if (this.pluginRequiredBundlesMap.containsKey(buildPlugin
                            .getId()) == false) {
                        this.pluginRequiredBundlesMap.put(buildPlugin.getId(),
                                new ArrayList<String>());
                    }
                    this.pluginRequiredBundlesMap.get(buildPlugin.getId()).add(
                            requiredBundle);
                }
            }
        }
    }

    private void scanEclipsePlugin(final String id,
            final BuildFeature containingBuildFeature) throws Exception {
        if (buildPluginsMap.containsKey(id)) {
            /*
             * The plugin is only added to the build plugins map after it has
             * been scanned.
             */
            return;
        }

        /*
         * Verify that the plugin is actually an Eclipse plugin.
         */
        List<Path> resourcePaths = this.getEclipseResource(id,
                this.eclipsePluginsPath);
        if (resourcePaths.isEmpty()) {
            throw new Exception("Failed to find plugin with id: " + id + "!");
        }
        this.eclipsePlugins.add(id);

        for (Path pluginPath : resourcePaths) {
            BuildPlugin buildPlugin = new BuildPlugin(id, pluginPath, true,
                    containingBuildFeature);

            if (Files.isDirectory(pluginPath)) {
                /*
                 * The Eclipse plugin is unpacked.
                 */
                final Path manifestMFPath = pluginPath
                        .resolve(BuildPlugin.MANIFEST_MF);
                if (Files.exists(manifestMFPath) == false) {
                    throw new Exception(
                            "Failed to find the manifest file for plugin: "
                                    + id + "! Expected manifest to be at: "
                                    + manifestMFPath.toString() + ".");
                }

                try (InputStream is = Files.newInputStream(manifestMFPath)) {
                    final Manifest manifest = new Manifest(is);
                    Attributes mainAttributes = manifest.getMainAttributes();

                    String attributeValue = mainAttributes
                            .getValue(Constants.EXPORT_PACKAGE);
                    if (attributeValue != null) {
                        ManifestElement[] elements = ManifestElement
                                .parseHeader(Constants.EXPORT_PACKAGE,
                                        attributeValue);
                        for (ManifestElement element : elements) {
                            String exportedPackage = element.getValue();
                            this.eclipsePluginExportedPackages
                                    .add(exportedPackage);
                        }
                    }
                }
            } else {
                try (InputStream is = Files.newInputStream(pluginPath);
                        JarInputStream jis = new JarInputStream(is)) {
                    Manifest manifest = jis.getManifest();
                    if (manifest == null) {
                        throw new Exception(
                                "Failed to find the manifest for Eclipse plugin: "
                                        + id + "!");
                    }
                    /*
                     * Eclipse is capable of managing these dependencies, we
                     * just want to ensure that all build dependencies have
                     * either been satisfied by either plugins provided by
                     * Eclipse or plugins in the baseline that is being built.
                     * We only care about the plugin, itself and the packages
                     * exported by the plugin. Eclipse can handle managing
                     * dependencies between its own plugins.
                     */
                    Attributes mainAttributes = manifest.getMainAttributes();

                    String attributeValue = mainAttributes
                            .getValue(Constants.EXPORT_PACKAGE);
                    if (attributeValue != null) {
                        ManifestElement[] elements = ManifestElement
                                .parseHeader(Constants.EXPORT_PACKAGE,
                                        attributeValue);
                        for (ManifestElement element : elements) {
                            String exportedPackage = element.getValue();
                            this.eclipsePluginExportedPackages
                                    .add(exportedPackage);
                        }
                    }
                }
            }

            this.buildPluginsMap.put(id, buildPlugin);
        }
    }

    private List<Path> getEclipseResource(final String id, Path containingPath)
            throws Exception {
        final String resourceRegex = id + "_[0-9].*";
        List<Path> resourcePaths = new ArrayList<>(1);
        try (DirectoryStream<Path> directoryStream = Files.newDirectoryStream(
                containingPath, resourceRegex)) {
            Iterator<Path> iterator = directoryStream.iterator();
            while (iterator.hasNext()) {
                Path path = iterator.next();
                resourcePaths.add(path);
            }
        }

        return resourcePaths;
    }
}
