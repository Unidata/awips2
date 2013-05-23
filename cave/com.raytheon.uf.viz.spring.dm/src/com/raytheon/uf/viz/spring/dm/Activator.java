package com.raytheon.uf.viz.spring.dm;

import java.io.IOException;
import java.net.URL;
import java.util.ArrayList;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.regex.Pattern;

import org.eclipse.core.runtime.FileLocator;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Platform;
import org.osgi.framework.Bundle;
import org.osgi.framework.BundleActivator;
import org.osgi.framework.BundleContext;
import org.osgi.framework.Constants;

/**
 * 
 * Custom version of spring osgi ContextLoaderListener to turn off xml
 * validation
 * 
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 12, 2010            mschenke     Initial creation
 * Jan 24, 2013 1522       bkowal       Halt initialization if a p2 installation 
 *                                      has been started
 * Mar 05, 2013 1754       djohnson     Catch exceptions and allow as much of the Spring container to boot as possible.
 * May 23, 2013 2005       njensen      Added springSuccess flag
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */
public class Activator implements BundleActivator {

    // The plug-in ID
    public static final String PLUGIN_ID = "com.raytheon.uf.viz.spring.dm";

    private static final String SPRING_PATH = "res" + IPath.SEPARATOR
            + "spring";

    private static final String SPRING_FILE_EXT = "*.xml";

    private static final Pattern COMMA_SPLIT = Pattern.compile("[,]");

    private static final Pattern SEMICOLON_SPLIT = Pattern.compile("[;]");

    // The shared instance
    private static Activator plugin;

    private boolean springSuccess = true;

    /**
     * The constructor
     */
    public Activator() {
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.core.runtime.Plugins#start(org.osgi.framework.BundleContext)
     */
    @Override
    public void start(BundleContext context) throws Exception {
        if (this.isInstallOperation()) {
            return;
        }
        plugin = this;

        Map<String, OSGIXmlApplicationContext> contextMap = new HashMap<String, OSGIXmlApplicationContext>();
        Set<String> processing = new HashSet<String>();
        Bundle[] bundles = context.getBundles();
        Map<String, Bundle> bundleMap = new HashMap<String, Bundle>();
        for (Bundle b : bundles) {
            bundleMap.put(b.getSymbolicName(), b);
        }
        for (Bundle b : bundles) {
            createContext(bundleMap, contextMap, b, processing);
        }
    }

    private OSGIXmlApplicationContext createContext(
            Map<String, Bundle> bundles,
            Map<String, OSGIXmlApplicationContext> contextMap, Bundle bundle,
            Set<String> processing) {
        String bundleName = bundle.getSymbolicName();
        OSGIXmlApplicationContext appCtx = contextMap.get(bundleName);
        if (contextMap.containsKey(bundleName) == false
                && bundleName.contains(".edex.") == false) {
            if (processing.contains(bundleName)) {
                springSuccess = false;
                throw new RuntimeException(
                        "Found recursive spring dependency while processing plugins: "
                                + bundleName);
            }
            processing.add(bundleName);

            // No context created yet and not edex project, check for files
            Enumeration<?> entries = bundle.findEntries(SPRING_PATH,
                    SPRING_FILE_EXT, true);
            if (entries != null) {
                List<String> files = new ArrayList<String>();
                while (entries.hasMoreElements()) {
                    URL url = (URL) entries.nextElement();
                    try {
                        url = FileLocator.toFileURL(url);
                        files.add(url.toString());
                    } catch (IOException e) {
                        throw new RuntimeException(
                                "Error resolving spring file: " + url, e);
                    }
                }
                if (files.size() > 0) {
                    // Files found, check for dependencies
                    String requiredBundlesHeader = (String) bundle.getHeaders()
                            .get(Constants.REQUIRE_BUNDLE);
                    // Split comma separated string from MANIFEST
                    String[] requiredBundles = COMMA_SPLIT
                            .split(requiredBundlesHeader);
                    List<OSGIXmlApplicationContext> parentContexts = new ArrayList<OSGIXmlApplicationContext>();
                    for (String requiredBndl : requiredBundles) {
                        // Extract bundle name which is first item in
                        // semicolon
                        // split list
                        String[] bndlParts = SEMICOLON_SPLIT
                                .split(requiredBndl);
                        Bundle reqBndl = bundles.get(bndlParts[0]);
                        if (reqBndl != null) {
                            // Found bundle, process context for bundle
                            OSGIXmlApplicationContext parent = createContext(
                                    bundles, contextMap, reqBndl, processing);
                            if (parent != null) {
                                // Context found, add to list
                                parentContexts.add(parent);
                            }
                        }
                    }

                    try {
                        if (parentContexts.size() > 0) {
                            // Context with parent context
                            appCtx = new OSGIXmlApplicationContext(
                                    new OSGIGroupApplicationContext(
                                            parentContexts),
                                    files.toArray(new String[0]), bundle);
                        } else {
                            // No parent context required
                            appCtx = new OSGIXmlApplicationContext(
                                    files.toArray(new String[0]), bundle);
                        }
                    } catch (Throwable t) {
                        // No access to the statusHandler yet, so print the
                        // stack trace to the console. By catching this, we also
                        // allow as many beans as possible to continue to be
                        // created
                        System.err
                                .println("Errors booting the Spring container.  CAVE will not be fully functional.");
                        t.printStackTrace();
                        springSuccess = false;
                    }
                }
            }
            contextMap.put(bundleName, appCtx);
        }
        processing.remove(bundleName);
        return appCtx;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.core.runtime.Plugin#stop(org.osgi.framework.BundleContext)
     */
    @Override
    public void stop(BundleContext context) throws Exception {
        plugin = null;
    }

    /**
     * Returns the shared instance
     * 
     * @return the shared instance
     */
    public static Activator getDefault() {
        return plugin;
    }

    /**
     * Based on the command line arguments, determine whether or not an Eclipse
     * p2 repository will be installed
     * 
     * @return true if an Eclipse p2 repository is going to be installed, false
     *         otherwise
     */
    private boolean isInstallOperation() {
        final String P2_DIRECTOR = "org.eclipse.equinox.p2.director";

        /**
         * We look at the command line arguments instead of the program
         * arguments (com.raytheon.uf.viz.application.ProgramArguments) because
         * the command line arguments include almost everything that was passed
         * as an argument to the Eclipse executable instead of just what CAVE is
         * interested in.
         */
        for (String argument : Platform.getCommandLineArgs()) {
            if (P2_DIRECTOR.equals(argument)) {
                return Boolean.TRUE;
            }
        }

        return Boolean.FALSE;
    }

    public boolean isSpringInitSuccessful() {
        return springSuccess;
    }
}
