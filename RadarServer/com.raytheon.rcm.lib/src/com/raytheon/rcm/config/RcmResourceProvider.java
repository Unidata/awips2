package com.raytheon.rcm.config;

import java.io.InputStream;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * This provides a resource to access configuration files that may exist
 * locally (when running in RadarServer) or in Localization (when running
 * in CAVE.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 2015-09-08   DR 17944   D. Friedman Initial creation
 * </pre>
 */
public abstract class RcmResourceProvider {
    private static RcmResourceProvider instance;

    private Map<String, List<Runnable>> resourceChangeListeners;

    public abstract InputStream getResourceAsStream(String resource)/* throws IOException*/;

    public void addResourceChangeListener(String resource, Runnable callback) {
        synchronized (this) {
            if (resourceChangeListeners == null) {
                resourceChangeListeners = new HashMap<String, List<Runnable>>();
            }
            List<Runnable> list = resourceChangeListeners.get(resource);
            if (list == null) {
                list = new ArrayList<Runnable>();
                resourceChangeListeners.put(resource, list);
            }
            list.add(callback);
        }
    }

    public void removeResourceChangeListener(String resource, Runnable callback) {
        synchronized (this) {
            if (resourceChangeListeners != null) {
                List<Runnable> list = resourceChangeListeners.get(resource);
                if (list != null) {
                    list.remove(callback);
                }
            }
        }
    }

    protected void notifyResourceChanged(String resource) {
        ArrayList<Runnable> runnables = null;
        synchronized (this) {
            if (resourceChangeListeners != null) {
                List<Runnable> list = resourceChangeListeners.get(resource);
                if (list != null) {
                    runnables = new ArrayList<Runnable>(list);
                }
            }
        }
        if (runnables != null) {
            for (Runnable r : runnables) {
                r.run();
            }
        }
    }

    public static void setInstance(RcmResourceProvider instance) {
        RcmResourceProvider.instance = instance;
    }

    public static RcmResourceProvider getInstance() {
        return instance;
    }
}
