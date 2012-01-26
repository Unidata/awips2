package gov.noaa.nws.ncep.viz.localization;

//import gov.noaa.nws.ncep.viz.localization.adapter.NcepCAVELocalizationAdapter;
import java.io.File;
import java.io.IOException;

import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.Plugin;
import org.osgi.framework.BundleContext;

//import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.viz.application.ProgramArguments;
import com.raytheon.uf.viz.core.localization.LocalizationManager;

/**
 * The activator class controls the plug-in life cycle
 */
public class Activator extends Plugin {

	// The plug-in ID
	public static final String PLUGIN_ID = "gov.noaa.nws.ncep.viz.localization";

	public static final String ACTIVE_DESK = "-ACTIVE_DESK"; 
	public static final String ACTIVE_SITE = "-ACTIVE_SITE"; 

	// The shared instance
	private static Activator plugin;

	// 
	private static String currentDesk="None";
	
	public Activator() {
		LocalizationManager localizationManager = LocalizationManager.getInstance();
		
		/*
		 * Now the priority is: 1. command line argument, 2. from UI preference setting
		 * 3. system variable 
		 */
		String desk = ProgramArguments.getInstance().getString( ACTIVE_DESK ); 
		if(!isValidDesk(desk)) 
			//desk = localizationManager.getCurrentDesk(); 
		if(!isValidDesk(desk))
			currentDesk = System.getenv("ACTIVE_DESK");
		if(!isValidDesk(desk))
			currentDesk = "None"; 
		else {
			/*
			 * set the valid current desk value back to localization store 
			 * through LocalizationManager
			 */
//			localizationManager.setCurrentDesk(desk); 
		}
//		System.out.println("===================, the active_desk value is:" + desk); 

		
		/*
		 * Now the priority is: 1. command line argument, 2. from UI preference setting
		 * 3. system variable 
		 */
		String site = ProgramArguments.getInstance().getString( ACTIVE_SITE ); 
		if(!isValidSite(site)) 
			site = localizationManager.getCurrentSite(); 
		if(!isValidSite(site))
			site = System.getenv("ACTIVE_SITE"); 
		if(!isValidSite(site))
			site = "none"; 
		else {
			/*
			 * set the valid current site value back to localization store 
			 * through LocalizationManager
			 */
			localizationManager.setCurrentSite(site); 
		}
//		System.out.println("===================, the active_site value is:" + site); 
//		initializeCurrentUser(localizationManager); 		
	}

	public static String getCurrentDesk() {
		return currentDesk;
	}

	private boolean isValidDesk(String deskName) {
		boolean isValidDesk = false; 
		if(!isStringEmpty(deskName) && !deskName.equalsIgnoreCase("none"))
			isValidDesk = true;
		return isValidDesk; 
	}
	
	
	
	private boolean isValidSite(String siteName) {
		boolean isValidSite = false; 
		if(!isStringEmpty(siteName) && !siteName.equalsIgnoreCase("none"))
			isValidSite = true;
		return isValidSite; 
	}
		
	public  boolean isStringEmpty(String str) {
		boolean result = false; 
		if(str == null || str.trim().length() == 0)
			result = true; 
		return result; 
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
