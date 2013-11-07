/**
 * Copyright 09/24/12 Raytheon Company.
 *
 * Unlimited Rights
 * This software was developed pursuant to Contract Number 
 * DTFAWA-10-D-00028 with the US Government. The US Government’s rights 
 * in and to this copyrighted software are as specified in DFARS
 * 252.227-7014 which was made part of the above contract. 
 */
package com.raytheon.uf.edex.ogc.common.spatial;

import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

import org.springframework.context.ApplicationContext;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.edex.core.EDEXUtil;

/**
 * Factory for retrieving vertical coordinate objects from vertically enabled
 * classes. Uses VerticalEnabled beans registered in spring.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 29, 2013            bclement     Initial creation
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */
public class VerticalSpatialFactory {
    
    private static Map<Class<?>, VerticalEnabled<?>> REGISTRY = null;

	private static final IUFStatusHandler log = UFStatus
			.getHandler(VerticalSpatialFactory.class);

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
                String[] beans = ctx.getBeanNamesForType(VerticalEnabled.class);
                HashMap<Class<?>, VerticalEnabled<?>> map = new HashMap<Class<?>, VerticalEnabled<?>>(
                        beans.length);
                for (String bean : beans) {
                    VerticalEnabled<?> ve = (VerticalEnabled<?>) ctx
                            .getBean(bean);
                    map.put(ve.getSupportedClass(), ve);
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
     * @return null if factory isn't initialized or no vertical enabled found
     *         for c
     */
    @SuppressWarnings("unchecked")
    public static <T> VerticalEnabled<T> getEnabled(Class<T> c) {
        if (REGISTRY == null) {
            return null;
        }
        return (VerticalEnabled<T>) REGISTRY.get(c);
    }

    public static boolean isInitialized() {
        return REGISTRY != null;
    }

}
