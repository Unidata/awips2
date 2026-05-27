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

import java.nio.file.Path;
import java.util.Set;
import java.util.HashSet;

/**
 * POJO representing an AWIPS feature and its associated dependencies.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 29, 2015 4759       bkowal      Initial creation
 * Oct 09, 2015 4759       bkowal      Build cycles now cause failure.
 * Oct 20, 2015 4759       bkowal      Added {@link #eclipse}.
 * 
 * </pre>
 * 
 * @author bkowal
 * @version 1.0
 */

public class BuildFeature {

    public static final String FILENAME = "feature" + Main.XML_EXTENSION;

    private String id;

    private final Path featurePath;

    /*
     * Indicates that this feature is provided by Eclipse.
     */
    private final boolean eclipse;

    /*
     * A {@link Set} of the {@link BuildFeature}s that are dependent on this
     * {@link BuildFeature}. This {@link BuildFeature} must be built before the
     * {@link BuildFeature}s in the {@link Set} can be built.
     */
    private final Set<BuildFeature> dependentFeatures = new HashSet<>();

    /*
     * A {@link Set} of the {@link BuildFeature}s that this feature is dependent
     * on. The {@link BuildFeature}s in this {@link Set} must be built before
     * this {@link BuildFeature} can be built.
     */
    private final Set<BuildFeature> dependencyFeatures = new HashSet<>();

    /*
     * The {@link Set} of all {@link BuildPlugin}s that this feature is
     * responsible for.
     */
    private final Set<BuildPlugin> managedPlugins = new HashSet<>();

    private boolean topoMark = false;

    public BuildFeature(final Path featurePath) {
        this(featurePath, false);
    }

    public BuildFeature(final Path featurePath, boolean eclipse) {
        this.featurePath = featurePath;
        this.eclipse = eclipse;
    }

    public void addDependentFeature(BuildFeature buildFeature) {
        this.dependentFeatures.add(buildFeature);
    }

    public void addDependentFeatures(Set<BuildFeature> buildFeatures,
            final String resource, String pluginId) {
        for (BuildFeature buildFeature : buildFeatures) {
            if (buildFeature.dependsOn(this)) {
                throw new IllegalStateException("Build cycle detected: "
                        + buildFeature.getId() + " and " + this.id
                        + " are dependent on each other! Linked by resource = "
                        + resource + " required by plugin " + pluginId + " ...");
            }
            this.addDependentFeature(buildFeature);
        }
    }

    public void addDependencyFeature(BuildFeature buildFeature) {
        this.dependencyFeatures.add(buildFeature);
    }

    public void addManagedPlugin(BuildPlugin buildPlugin) {
        this.managedPlugins.add(buildPlugin);
    }

    public boolean dependsOn(BuildFeature buildFeature) {
        return this.dependentFeatures.contains(buildFeature);
    }

    public void mark() {
        this.topoMark = true;
    }

    /**
     * @param id
     *            the id to set
     */
    public void setId(String id) {
        this.id = id;
    }

    /**
     * @return the id
     */
    public String getId() {
        return id;
    }

    /**
     * @return the featurePath
     */
    public Path getFeaturePath() {
        return featurePath;
    }

    /**
     * @return the dependentFeatures
     */
    public Set<BuildFeature> getDependentFeatures() {
        return dependentFeatures;
    }

    /**
     * @return the dependencyFeatures
     */
    public Set<BuildFeature> getDependencyFeatures() {
        return dependencyFeatures;
    }

    /**
     * @return the managedPlugins
     */
    public Set<BuildPlugin> getManagedPlugins() {
        return managedPlugins;
    }

    /**
     * @return the topoMark
     */
    public boolean isTopoMark() {
        return topoMark;
    }

    /**
     * @return the eclipse
     */
    public boolean isEclipse() {
        return eclipse;
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#equals(java.lang.Object)
     */
    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (obj == null)
            return false;
        if (getClass() != obj.getClass())
            return false;
        BuildFeature other = (BuildFeature) obj;
        if (id == null) {
            if (other.id != null)
                return false;
        } else if (!id.equals(other.id))
            return false;
        return true;
    }

    @Override
    public String toString() {
        return this.id;
    }
}
