package com.raytheon.uf.viz.core.drawables.ext;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IExtension;
import org.eclipse.core.runtime.IExtensionPoint;
import org.eclipse.core.runtime.IExtensionRegistry;
import org.eclipse.core.runtime.Platform;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.drawables.ext.GraphicsExtension.IGraphicsExtensionInterface;
import com.raytheon.uf.viz.core.exception.VizException;

public class GraphicsExtensionManager {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(GraphicsExtensionManager.class);

    private static final String CLASS_ATTR = "class";

    private static List<Class<?>> extensions = new ArrayList<Class<?>>();

    static {
        IExtensionRegistry registry = Platform.getExtensionRegistry();
        IExtensionPoint point = registry
                .getExtensionPoint("com.raytheon.uf.viz.core.graphicsExtension");

        if (point != null) {
            for (IExtension extension : point.getExtensions()) {
                for (IConfigurationElement config : extension
                        .getConfigurationElements()) {
                    try {
                        GraphicsExtension<?> impl = (GraphicsExtension<?>) config
                                .createExecutableExtension(CLASS_ATTR);
                        extensions.add(impl.getClass());
                    } catch (Throwable t) {
                        statusHandler.handle(
                                Priority.PROBLEM,
                                "Error getting extension "
                                        + config.getAttribute(CLASS_ATTR)
                                        + ": " + t.getLocalizedMessage());
                    }

                }
            }
        }
    }

    private IGraphicsTarget target;

    private Map<Object, Object> cached = new HashMap<Object, Object>();

    public GraphicsExtensionManager(IGraphicsTarget target) {
        this.target = target;
    }

    /**
     * Lookup the best extension implementation for the extension class
     * 
     * @param <T>
     * @param extensionClass
     * @return
     * @throws VizException
     */
    public synchronized <T extends IGraphicsExtensionInterface> T getExtension(
            Class<T> extensionClass) throws VizException {
        if (cached.containsKey(extensionClass)) {
            return extensionClass.cast(cached.get(extensionClass));
        }
        T bestExt = null;
        int bestVal = -1;
        for (Class<?> eClass : extensions) {
            if (extensionClass.isAssignableFrom(eClass)) {
                try {
                    GraphicsExtension<?> graphicsExt = GraphicsExtension.class.cast(eClass
                            .newInstance());
                    int val = graphicsExt.setTarget(target);
                    if (val > bestVal) {
                        bestVal = val;
                        bestExt = extensionClass.cast(graphicsExt);
                    }
                } catch (InstantiationException e) {
                    statusHandler.handle(Priority.PROBLEM,
                            e.getLocalizedMessage(), e);
                    continue;
                } catch (IllegalAccessException e) {
                    statusHandler.handle(Priority.PROBLEM,
                            e.getLocalizedMessage(), e);
                    continue;
                }
            }
        }
        if (bestExt != null) {
            cached.put(extensionClass, bestExt);
            return extensionClass.cast(bestExt);
        } else {
            throw new VizException("Could not find valid extension for class: "
                    + extensionClass);
        }
    }

    public synchronized void dispose() {
        for (Object obj : cached.values()) {
            if (obj instanceof GraphicsExtension<?>) {
                ((GraphicsExtension<?>) obj).dispose();
            }
        }
        cached.clear();
    }
}
