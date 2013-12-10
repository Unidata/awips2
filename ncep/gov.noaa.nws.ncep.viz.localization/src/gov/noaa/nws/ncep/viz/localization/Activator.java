package gov.noaa.nws.ncep.viz.localization;

//import gov.noaa.nws.ncep.viz.localization.adapter.NcepCAVELocalizationAdapter;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.Plugin;
import org.osgi.framework.BundleContext;

import com.raytheon.uf.viz.core.ProgramArguments;
//import com.raytheon.uf.common.localization.IPathManager;

/**
 * The activator class controls the plug-in life cycle
 */
public class Activator extends Plugin {

	// The plug-in ID
	public static final String PLUGIN_ID = "gov.noaa.nws.ncep.viz.localization";

	public static final String DESK_ARG = "-desk"; 
//	public static final String DESK_ENV = "DESK"; 

	// The shared instance
	private static Activator plugin;

	private static String currentDesk="None";
	
	public Activator() {		
		/*
		 * Now the priority is: 1. command line argument, 2. from UI preference setting
		 * 3. system variable 
		 */
		String desk = ProgramArguments.getInstance().getString( DESK_ARG ); 
		
		if( desk != null && !desk.trim().isEmpty() ) {
			desk = desk.trim().toUpperCase();
			System.out.println("Setting Desk to "+ desk+" from Program Arguement.");
			currentDesk = desk;
		}
//		else {
//			desk = System.getenv( DESK_ENV );
//			
//			if( desk != null && !desk.trim().isEmpty() ) {
//				desk = desk.trim();
//				System.out.println("Setting Desk to "+ desk+" from Environment variable.");
//				currentDesk = desk;
//			}						
//		}
	
		
		// THE SITE is set by the LocalizationManager from either the  
		// preferences or the program args.

	}

	public static String getCurrentDesk() {
		return currentDesk;
	}
				
	private void locationTest() {
		String configurationLocation = Platform.getConfigurationLocation().getURL().getPath(); 
		System.out.println("Platform.getConfigurationLocation().getURL().getPath()="+configurationLocation); 
		
		String installLocation = Platform.getInstallLocation().getURL().getPath(); 
		System.out.println("Platform.getInstallLocation().getURL().getPath()="+installLocation); 
		
		String instanceLocation = Platform.getInstanceLocation().getURL().getPath(); 
		System.out.println("Platform.getInstanceLocation().getURL().getPath()="+instanceLocation); 
		
		String locationOSString = Platform.getLocation().toOSString(); 
		System.out.println("Platform.getLocation().toOSString()="+locationOSString); 
		
		String locationToString = Platform.getLocation().toString(); 
		System.out.println("Platform.getLocation().toString()="+locationToString); 
		
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
}
