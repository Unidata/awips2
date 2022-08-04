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
package com.raytheon.uf.viz.pointset.rsc;

import java.util.Deque;
import java.util.concurrent.LinkedBlockingDeque;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;

/**
 * Stages point set frames in the background.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------
 * Aug 28, 2015  4709     bsteffen  Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public class PointSetStageJob extends Job {

    private Deque<PointSetFrame> frames = new LinkedBlockingDeque<>();

    public PointSetStageJob() {
        super("Staging PointSet data");
    }

    public void schedule(PointSetFrame frame) {
        if (!frames.contains(frame)) {
            frames.add(frame);
        }
        this.schedule();
    }

    @Override
    protected IStatus run(IProgressMonitor monitor) {
        PointSetFrame frame = frames.poll();
        while (frame != null) {
            frame.stage();
            frame = frames.poll();
        }
        return Status.OK_STATUS;
    }

}
