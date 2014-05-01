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
package com.raytheon.uf.viz.core.jobs;

import java.util.concurrent.LinkedBlockingQueue;

import org.eclipse.core.runtime.jobs.Job;

/**
 * Abstract extension of job for queuing up actions to execute on this job. This
 * is primarily to support having a never-ending job thread that run
 * continuously executes on, just sleeps until it receives a request.
 * 
 * Designed to support python jobs, since JEP/JNI requires the same thread that
 * initialized the python interpreter to be the same one that executes the
 * python code. This concept keeps that thread around permanently, but is
 * limited to one execution at a time.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 9, 2009            njensen     Initial creation
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public abstract class AbstractQueueJob<T extends QueueJobRequest<?>> extends
        Job {

    protected AbstractQueueJob(String name) {
        super(name);
    }

    protected LinkedBlockingQueue<T> queue = new LinkedBlockingQueue<T>();

    /**
     * Adds a request to the queue.
     * 
     * @param request
     * @return
     */
    public boolean enqueue(T request) {
        return queue.offer(request);
    }

}
