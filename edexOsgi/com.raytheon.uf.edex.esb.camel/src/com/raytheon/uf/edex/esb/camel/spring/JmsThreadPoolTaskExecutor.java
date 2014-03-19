/**
 * This software was developed and / or modified by Raytheon Company,
 * pursuant to Contract DG133W-05-CQ-1067 with the US Government.
 * 
 * U.S. EXPORT CONTROLLED TECHNICAL DATA
 * This software product contains export-restricted data whose
 * export/transfer/disclosure is restricted by U.S. law. Dissemination
 * to non-U.S. persons whether in the United States or abroad requires
 * an export license or other authorization.
 * 
 * Contractor Name:        Raytheon Company
 * Contractor Address:     6825 Pine Street, Suite 340
 *                         Mail Stop B8
 *                         Omaha, NE 68106
 *                         402.291.0100
 * 
 * See the AWIPS II Master Rights File ("Master Rights File.pdf") for
 * further licensing information.
 **/
package com.raytheon.uf.edex.esb.camel.spring;

import java.util.concurrent.ThreadPoolExecutor;

import org.springframework.scheduling.concurrent.ThreadPoolTaskExecutor;

/**
 * Overrides prefersShortLivedTasks so that JMS connection don't keep
 * disconnecting/reconnecting
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 15, 2010            rjpeter     Initial creation
 * Apr 10, 2014 2726       rjpeter     Updated to create/initialize the ThreadPoolExecutor on demand.
 * </pre>
 * 
 * @author rjpeter
 * @version 1.0
 */

public class JmsThreadPoolTaskExecutor extends ThreadPoolTaskExecutor {
    private static final long serialVersionUID = 1L;

    private volatile boolean needToInitialize = true;

    @Override
    public ThreadPoolExecutor getThreadPoolExecutor()
            throws IllegalStateException {
        // don't initialize until first needed
        if (needToInitialize) {
            synchronized (this) {
                if (needToInitialize) {
                    afterPropertiesSet();
                    needToInitialize = false;
                }
            }
        }
        return super.getThreadPoolExecutor();
    }

    @Override
    public boolean prefersShortLivedTasks() {
        return false;
    }
}
