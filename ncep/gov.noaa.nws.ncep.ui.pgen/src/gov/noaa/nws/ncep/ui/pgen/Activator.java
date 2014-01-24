package gov.noaa.nws.ncep.ui.pgen;

import gov.noaa.nws.ncep.ui.pgen.PgenUtil.PgenMode;

import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.ui.plugin.AbstractUIPlugin;
import org.osgi.framework.BundleContext;

/**
 * The activator class controls the plug-in life cycle
 */
public class Activator extends AbstractUIPlugin {

    // The plug-in ID
    public static final String PLUGIN_ID = "gov.noaa.nws.ncep.ui.pgen";

    // The shared instance
    private static Activator plugin;

    private IPreferenceStore myprefs = null;

    /**
     * The constructor
     */
    public Activator() {
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.ui.plugin.AbstractUIPlugin#start(org.osgi.framework.BundleContext
     * )
     */
    public void start(BundleContext context) throws Exception {
        super.start(context);
        plugin = this;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.ui.plugin.AbstractUIPlugin#stop(org.osgi.framework.BundleContext
     * )
     */
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

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.ui.plugin.AbstractUIPlugin#getPreferenceStore()
     */
    @Override
    public IPreferenceStore getPreferenceStore() {

        /*
         * First time, set defaults for the PGEN preference store
         */
        if (myprefs == null) {
            myprefs = super.getPreferenceStore();
            myprefs.setDefault(PgenPreferences.P_OPR_DIR,
                    PgenPreferences.V_OPR_DIR);
            myprefs.setDefault(PgenPreferences.P_WORKING_DIR,
                    PgenPreferences.V_WORKING_DIR);
            myprefs.setDefault(PgenPreferences.P_RECOVERY_DIR,
                    PgenPreferences.V_RECOVERY_DIR);
            myprefs.setDefault(PgenPreferences.P_AUTO_FREQ, 5);
            myprefs.setDefault(PgenPreferences.P_MAX_DIST, 30);
            myprefs.setDefault(PgenPreferences.P_PGEN_MODE,
                    PgenMode.SINGLE.toString());
            myprefs.setDefault(PgenPreferences.P_LAYER_LINK, false);
            myprefs.setDefault(PgenPreferences.P_COMP_COORD,
                    PgenPreferences.CED_COMP_COORD);
            myprefs.setDefault(PgenPreferences.P_AUTOPLACE_TEXT, false);
            myprefs.setDefault(PgenPreferences.P_LAYER_MERGE, 4);
        }

        return myprefs;
    }

}
