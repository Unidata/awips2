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
 * 
 *  #1051 Greg Hull - moved desk code to NmapCommon and trigger via spring
 */
public class Activator extends Plugin {

	// The plug-in ID
	public static final String PLUGIN_ID = "gov.noaa.nws.ncep.viz.localization";

	// The shared instance
	private static Activator plugin;
	
	public Activator() {		

	}

//	public static String getCurrentDesk() {
//		return currentDesk;
//	}
//				
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
