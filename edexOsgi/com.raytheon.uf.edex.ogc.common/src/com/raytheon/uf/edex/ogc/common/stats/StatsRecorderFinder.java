/**
 * Copyright 09/24/12 Raytheon Company.
 *
 * Unlimited Rights
 * This software was developed pursuant to Contract Number 
 * DTFAWA-10-D-00028 with the US Government. The US Government’s rights 
 * in and to this copyrighted software are as specified in DFARS
 * 252.227-7014 which was made part of the above contract. 
 */
package com.raytheon.uf.edex.ogc.common.stats;

import org.springframework.context.ApplicationContext;

import com.raytheon.uf.edex.core.EDEXUtil;

/**
 * Retrieves stats recorder for system
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 25, 2013            bclement     Initial creation
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */
public class StatsRecorderFinder {

    private static volatile IStatsRecorder recorder = null;

    private static final Object recorderMutex = new Object();

    /**
     * Find stats recorder, defaults to no-op service if none found
     * 
     * @return
     */
    public static IStatsRecorder find() {
        if (recorder == null) {
            synchronized (recorderMutex) {
                if (recorder == null) {
                    ApplicationContext ctx = EDEXUtil.getSpringContext();
                    String[] beans = ctx
                            .getBeanNamesForType(IStatsRecorder.class);
                    if (beans.length == 0) {
                        recorder = new NoopStatsRecorder();
                    } else {
                        recorder = (IStatsRecorder) ctx.getBean(beans[0]);
                    }
                }
            }
        }

        return recorder;
    }

}
