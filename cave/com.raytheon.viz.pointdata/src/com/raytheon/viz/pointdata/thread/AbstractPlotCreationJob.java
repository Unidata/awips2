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
package com.raytheon.viz.pointdata.thread;

import org.eclipse.core.runtime.jobs.Job;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.viz.pointdata.IPlotModelGeneratorCaller;

/**
 * Abstract job associated with one of the many details of plot creation.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 21, 2014 2868       njensen     Initial creation
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public abstract class AbstractPlotCreationJob extends Job {

    protected static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(PlotModelDataRequestJob.class);

    protected PlotThreadOverseer overseer;

    protected IPlotModelGeneratorCaller listener;

    public AbstractPlotCreationJob(String name, PlotThreadOverseer parent,
            IPlotModelGeneratorCaller caller) {
        super(name);
        this.overseer = parent;
        this.listener = caller;
        this.setSystem(false);
    }

    public boolean isDone() {
        return getState() != Job.RUNNING && getState() != Job.WAITING;
    }

    public boolean shutdown() {
        return super.cancel();
    }

}
