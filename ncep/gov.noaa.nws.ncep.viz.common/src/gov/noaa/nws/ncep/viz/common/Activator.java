package gov.noaa.nws.ncep.viz.common;

//import gov.noaa.nws.ncep.viz.ui.display.DisplayNameManager;

import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.ui.plugin.AbstractUIPlugin;
import org.osgi.framework.BundleContext;

/**
 * The activator class controls the plug-in life cycle
 */
public class Activator extends AbstractUIPlugin {

	// The plug-in ID
	public static final String PLUGIN_ID = "gov.noaa.nws.ncep.viz.common";

	// The shared instance
	private static Activator plugin;

	private IPreferenceStore myprefs = null;
	/**
	 * The constructor
	 */
	public Activator() {
		EditorManager.initEditorNumList();
		
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.ui.plugin.AbstractUIPlugin#start(org.osgi.framework.BundleContext)
	 */
	public void start(BundleContext context) throws Exception {
		super.start(context);
		plugin = this;
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.ui.plugin.AbstractUIPlugin#stop(org.osgi.framework.BundleContext)
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
		 * First time, set defaults for the Ncgrid preference store
		 */
		if ( myprefs == null ) {
			myprefs =  super.getPreferenceStore();
			/*
			myprefs.setDefault( NcgridPreferences.LLLAT, "");
			myprefs.setDefault( NcgridPreferences.LLLON, "");
			myprefs.setDefault( NcgridPreferences.URLAT, "");
			myprefs.setDefault( NcgridPreferences.URLON, "");
			
			/*
			myprefs.setDefault( NcgridPreferences.CLIP_AREA_COM,  NcgridPreferences.CLIP_AREA_US);
			myprefs.setDefault( NcgridPreferences.GAREA, NcgridPreferences.CLIP_AREA_US);
			myprefs.setDefault( NcgridPreferences.PROJ, "STR/90;-97;0");
			myprefs.setDefault( NcgridPreferences.CUSTOM_AREA, false );
			*/
		}
		
		return myprefs;
	}

}
