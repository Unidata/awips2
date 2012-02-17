package meteolib.jni;

import java.io.IOException;
import java.net.URL;
import java.util.Enumeration;

import org.eclipse.core.runtime.FileLocator;
import org.eclipse.ui.plugin.AbstractUIPlugin;
import org.osgi.framework.Bundle;
import org.osgi.framework.BundleContext;
import org.apache.commons.lang.SystemUtils;

/**
 * The activator class controls the plug-in life cycle
 */
public class Activator extends AbstractUIPlugin {

    // The plug-in ID
    public static final String PLUGIN_ID = "meteolib.jni";
    
    private static final String METEOLIB = "meteoLib";
    private static final String METEOLIB_LIBRARY_NAME = 
    	System.mapLibraryName(METEOLIB);

    // The shared instance
    private static Activator plugin;

    private static String nativeLibraryPath;

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
    @Override
    public void start(BundleContext context) throws Exception {
        super.start(context);
        plugin = this;
        nativeLibraryPath = findNativeLibraryPath();
    }

    /**
     * 
     * @return the location of the meteolib native library
     * @throws IOException
     */
    public static String getNativeLibraryPath() throws IOException {

        if (nativeLibraryPath == null) {
            nativeLibraryPath = findNativeLibraryPath();
        }

        return nativeLibraryPath;
    }

    private static String findNativeLibraryPath() throws IOException {
        Bundle b = Activator.getDefault().getBundle();
        @SuppressWarnings("unchecked")
		Enumeration<URL> enumeration = 
			b.findEntries("/", METEOLIB_LIBRARY_NAME, true);
        
        /*
         * We should have one element.
         */
        if (!enumeration.hasMoreElements())
        {
        	return null;
        }
        
        /*
         * Extract the location of the shared library.
         */
		URL url = 
        	FileLocator.resolve((URL) enumeration.nextElement());
		/*
		 * Will be the empty string if it does
		 * not exist.
		 */
        String nativeLibraryPath = 
        	url.getFile();
        if (nativeLibraryPath == null || nativeLibraryPath.equalsIgnoreCase(""))
        {
        	return null;
        }
        
        /*
         * We do not need to reformat the path for Windows here because
         * Java does not care.
         */
        System.load(nativeLibraryPath);
        
        // Re-format for Windows.
        if (SystemUtils.IS_OS_WINDOWS) 
        {
        	/*
        	 * We reformat here because this is the path that will be
        	 * returned to python scripts that use the meteoLib library.
        	 */
        	return formatWindowsPath(nativeLibraryPath);
        }
        
        return nativeLibraryPath;
    }
    
    private static String formatWindowsPath(String nativeLibraryPath) {
    	/*
    	 * Remove the leading "/" if there is one.
    	 */
    	if (nativeLibraryPath.startsWith("/"))
    	{
    		int length = nativeLibraryPath.length();
    		nativeLibraryPath = nativeLibraryPath.substring(1, length);
    	}
    	
    	/*
    	 * Replace all file path separators with the Windows version of
    	 * a file path separator.
    	 */
    	if (nativeLibraryPath.contains("/"))
    	{
    		nativeLibraryPath = nativeLibraryPath.replace("/", "\\");
    	}
    	
    	return nativeLibraryPath;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.ui.plugin.AbstractUIPlugin#stop(org.osgi.framework.BundleContext
     * )
     */
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
}