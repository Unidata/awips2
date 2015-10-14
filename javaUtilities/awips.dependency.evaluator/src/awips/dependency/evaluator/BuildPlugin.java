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

import java.util.Set;
import java.util.HashSet;
import java.nio.file.Path;
import java.nio.file.Paths;

/**
 * POJO representing an AWIPS / RCP plugin.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 30, 2015 4759       bkowal      Initial creation
 * 
 * </pre>
 * 
 * @author bkowal
 * @version 1.0
 */

public class BuildPlugin {

    public static final String MANIFEST_MF = Paths.get("META-INF",
            "MANIFEST.MF").toString();

    private final String id;

    private final Path pluginPath;

    /*
     * true if the plugin is provided by Eclipse.
     */
    private final boolean eclipse;

    private Set<BuildFeature> containingFeatures;

    public BuildPlugin(final String id, final Path pluginPath,
            final boolean eclipse, BuildFeature containingFeature) {
        this.id = id;
        this.pluginPath = pluginPath;
        this.eclipse = eclipse;
        this.containingFeatures = new HashSet<>(1, 1.0f);
        this.containingFeatures.add(containingFeature);
        containingFeature.addManagedPlugin(this);
    }

    public void addContainingFeature(BuildFeature containingFeature) {
        this.containingFeatures.add(containingFeature);
        containingFeature.addManagedPlugin(this);
    }

    public void addContainingFeatures(Set<BuildFeature> containingFeatures) {
        this.containingFeatures.addAll(containingFeatures);
        for (BuildFeature containingFeature : containingFeatures) {
            containingFeature.addManagedPlugin(this);
        }
    }

    public boolean isProvidedByFeature(BuildFeature buildFeature) {
        return this.containingFeatures.contains(buildFeature);
    }

    public boolean isProvidedByFeatures(Set<BuildFeature> buildFeatures) {
        for (BuildFeature buildFeature : buildFeatures) {
            if (this.containingFeatures.contains(buildFeature)) {
                return true;
            }
        }
        return false;
    }

    /**
     * @return the id
     */
    public String getId() {
        return id;
    }

    /**
     * @return the pluginPath
     */
    public Path getPluginPath() {
        return pluginPath;
    }

    /**
     * @return the eclipse
     */
    public boolean isEclipse() {
        return eclipse;
    }

    public Set<BuildFeature> getContainingFeatures() {
        return this.containingFeatures;
    }
}