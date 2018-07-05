package gov.noaa.gsd.viz.ensemble;

import org.eclipse.jface.preference.IPersistentPreferenceStore;
import org.eclipse.ui.plugin.AbstractUIPlugin;
import org.osgi.framework.BundleContext;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.viz.core.localization.HierarchicalPreferenceStore;

/**
 * 
 * TODO Add Description
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 1, 2014              polster     Initial creation
 *
 * </pre>
 *
 * @author polster
 */
public class Activator extends AbstractUIPlugin {

    public static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(Activator.class);

    // The plug-in ID
    public static final String PLUGIN_ID = "gov.noaa.gsd.viz.ensemble"; //$NON-NLS-1$

    // The shared instance
    private static Activator plugin;

    // pref store
    private HierarchicalPreferenceStore prefs = new HierarchicalPreferenceStore(
            this);

    /**
     * The constructor
     */
    public Activator() {
    }

    @Override
    public void start(BundleContext context) throws Exception {
        super.start(context);

        plugin = this;
    }

    @Override
    public void stop(BundleContext context) throws Exception {
        plugin = null;
        super.stop(context);
    }

    /**
     * Returns the shared instance
     * 
     * @return the shared instance
     */
    public static Activator getDefault() {
        return plugin;
    }

    @Override
    public IPersistentPreferenceStore getPreferenceStore() {
        return prefs;
    }

    // public static BundleContext getBundleContext() {
    // return bundleContext;
    // }

}
