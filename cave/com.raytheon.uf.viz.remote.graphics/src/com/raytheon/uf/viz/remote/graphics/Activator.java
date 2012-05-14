package com.raytheon.uf.viz.remote.graphics;

import java.awt.image.BufferedImage;

import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.ui.plugin.AbstractUIPlugin;
import org.osgi.framework.BundleContext;

import com.raytheon.uf.common.serialization.DynamicSerializationManager;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.viz.core.PixelExtent;
import com.raytheon.uf.viz.remote.graphics.adapters.PixelExtentSerializationAdapter;
import com.raytheon.uf.viz.remote.graphics.adapters.RGBSerializationAdapter;
import com.raytheon.uf.viz.remote.graphics.adapters.RectangleAdapter;
import com.raytheon.uf.viz.remote.graphics.adapters.RenderedImageAdapter;

/**
 * The activator class controls the plug-in life cycle
 */
public class Activator extends AbstractUIPlugin {

    public static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(Activator.class);

    // The plug-in ID
    public static final String PLUGIN_ID = "com.raytheon.uf.viz.remote.graphics"; //$NON-NLS-1$

    // The shared instance
    private static Activator plugin;

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
        DynamicSerializationManager.registerAdapter(PixelExtent.class,
                new PixelExtentSerializationAdapter());
        DynamicSerializationManager.registerAdapter(RGB.class,
                new RGBSerializationAdapter());
        DynamicSerializationManager.registerAdapter(BufferedImage.class,
                new RenderedImageAdapter());
        DynamicSerializationManager.registerAdapter(Rectangle.class,
                new RectangleAdapter());
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

    public static IUFStatusHandler getStatusHandler() {
        return statusHandler;
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
