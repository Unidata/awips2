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
package com.raytheon.viz.gfe.core;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;

import com.raytheon.viz.gfe.core.internal.SampleSetManager;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 25, 2011            njensen     Initial creation
 * Sep 09, 2014  #3592     randerso    Removed unused DataManager parameter to SampleSetManager constructor
 * Nov 19, 2014  #5129     dgilling    Pass IFPClient into constructor.
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class SampleSetMgrInitJob extends Job {

    private DataManager dataMgr;

    public SampleSetMgrInitJob(DataManager dm) {
        super("GFE SampleSet Mgr Init Job");
        this.dataMgr = dm;
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.core.runtime.jobs.Job#run(org.eclipse.core.runtime.
     * IProgressMonitor)
     */
    @Override
    protected IStatus run(IProgressMonitor monitor) {
        dataMgr.sampleSetManager = new SampleSetManager(dataMgr.getClient());
        dataMgr.getInitStatus().setSampleSetMgrDone(true);

        return Status.OK_STATUS;
    }

}
