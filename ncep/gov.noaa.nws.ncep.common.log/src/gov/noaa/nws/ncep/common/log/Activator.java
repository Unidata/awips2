package gov.noaa.nws.ncep.common.log;

//import org.osgi.framework.BundleActivator;
import org.osgi.framework.BundleContext;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.Plugin;
//import org.osgi.framework.BundleContext;

public class Activator extends Plugin {

	// The plug-in ID
	public static final String PLUGIN_ID = "gov.noaa.nws.ncep.common.log";

	private static BundleContext context;

	// The shared instance
	private static Activator plugin;

	private String caveDataDir; 

	/**
	 * The constructor
	 */
	public Activator() {
		initialization(); 
	}

	private void initialization() {
		caveDataDir = Platform.getUserLocation().getURL().getPath();
	}
	
	public String getCaveDataDir() {
		return caveDataDir;
	}

	static BundleContext getContext() {
		return context;
	}

	/*
	 * (non-Javadoc)
	 * @see org.osgi.framework.BundleActivator#start(org.osgi.framework.BundleContext)
	 */
	public void start(BundleContext bundleContext) throws Exception {
		super.start(bundleContext);
		Activator.context = bundleContext;
		plugin = this;
	}

	/*
	 * (non-Javadoc)
	 * @see org.osgi.framework.BundleActivator#stop(org.osgi.framework.BundleContext)
	 */
	public void stop(BundleContext bundleContext) throws Exception {
		plugin = null;
		super.stop(context);
		Activator.context = null;
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
