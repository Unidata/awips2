/**
 * Copyright 09/24/12 Raytheon Company.
 *
 * Unlimited Rights
 * This software was developed pursuant to Contract Number 
 * DTFAWA-10-D-00028 with the US Government. The US Government’s rights 
 * in and to this copyrighted software are as specified in DFARS
 * 252.227-7014 which was made part of the above contract. 
 */
package com.raytheon.uf.edex.wcs.reg;

import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

import org.springframework.context.ApplicationContext;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.edex.core.EDEXUtil;
import com.raytheon.uf.edex.ogc.common.spatial.VerticalSpatialFactory;

/**
 * Factory for getting coverage field for plugin data object
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 5, 2013            bclement     Initial creation
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */
public class FieldAdapterFactory {
    private static Map<Class<?>, IFieldAdapted<?>> REGISTRY = null;

    private static final IUFStatusHandler log = UFStatus
            .getHandler(FieldAdapterFactory.class);

    private static final class Init implements Runnable {
        @Override
        public void run() {
            while (!EDEXUtil.isRunning()) {
                try {
                    Thread.sleep(5000);
                } catch (InterruptedException e) {
                }
            }
            try {
                ApplicationContext ctx = EDEXUtil.getSpringContext();
                String[] beans = ctx.getBeanNamesForType(IFieldAdapted.class);
                HashMap<Class<?>, IFieldAdapted<?>> map = new HashMap<Class<?>, IFieldAdapted<?>>(
                        beans.length);
                for (String bean : beans) {
                    IFieldAdapted<?> fa = (IFieldAdapted<?>) ctx.getBean(bean);
                    map.put(fa.getSupportedClass(), fa);
                }
                REGISTRY = Collections.unmodifiableMap(map);
            } catch (Throwable e) {
                log.error("Unable to init " + VerticalSpatialFactory.class, e);
            }
        }
    }

    static {
        new Thread(new Init()).start();
    }

    /**
     * @param c
     * @return null if factory isn't initialized or no adapter found for c
     */
    @SuppressWarnings("unchecked")
    public static <T> IFieldAdapted<T> getAdapted(Class<T> c) {
        if (REGISTRY == null) {
            return null;
        }
        return (IFieldAdapted<T>) REGISTRY.get(c);
    }

    public static boolean isInitialized() {
        return REGISTRY != null;
    }

}
