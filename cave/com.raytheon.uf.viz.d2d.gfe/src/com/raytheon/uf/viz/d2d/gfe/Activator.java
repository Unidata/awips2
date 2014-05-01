package com.raytheon.uf.viz.d2d.gfe;

import org.osgi.framework.BundleActivator;
import org.osgi.framework.BundleContext;

import com.raytheon.uf.viz.d2d.gfe.browser.GFEVbDataCatalog;
import com.raytheon.viz.volumebrowser.datacatalog.DataCatalogManager;

public class Activator implements BundleActivator {

    private static BundleContext context;

    static BundleContext getContext() {
        return context;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.osgi.framework.BundleActivator#start(org.osgi.framework.BundleContext
     * )
     */
    public void start(BundleContext bundleContext) throws Exception {
        Activator.context = bundleContext;
        DataCatalogManager.getDataCatalogManager().addDataCatalog(
                new GFEVbDataCatalog());
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.osgi.framework.BundleActivator#stop(org.osgi.framework.BundleContext)
     */
    public void stop(BundleContext bundleContext) throws Exception {
        Activator.context = null;
    }

}
