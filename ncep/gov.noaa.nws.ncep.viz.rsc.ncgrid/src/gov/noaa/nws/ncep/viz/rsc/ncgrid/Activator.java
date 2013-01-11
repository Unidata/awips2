package gov.noaa.nws.ncep.viz.rsc.ncgrid;
  
import gov.noaa.nws.ncep.viz.gempak.nativelib.LibraryLoader;    

import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.ui.plugin.AbstractUIPlugin;
import org.osgi.framework.BundleContext;

/**
 * The activator class controls the plug-in life cycle
 */
public class Activator extends AbstractUIPlugin {

	// The plug-in ID
	public static final String PLUGIN_ID = "gov.noaa.nws.ncep.viz.rsc.ncgrid";

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
        LibraryLoader.load("cnflib");
        // LibraryLoader.load("xml2");
        LibraryLoader.load("gempak");
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

	@Override
	public IPreferenceStore getPreferenceStore() {
		
		/*
		 * First time, set defaults for the Ncgrid logger store
		 */
		if ( myprefs == null ) {
			myprefs =  super.getPreferenceStore();
			myprefs.setDefault( NcgribLoggerPreferences.ENABLE_ALL_LOGGER, false);	
			myprefs.setDefault( NcgribLoggerPreferences.ENABLE_RSC_LOGGER, false);	
			myprefs.setDefault( NcgribLoggerPreferences.ENABLE_DGD_LOGGER, false);	
            myprefs.setDefault(NcgribLoggerPreferences.ENABLE_CNTR_LOGGER,
                    false);
            myprefs.setDefault(NcgribLoggerPreferences.ENABLE_FINAL_LOGGER,
                    false);
		}
		
		return myprefs;
	}
}
