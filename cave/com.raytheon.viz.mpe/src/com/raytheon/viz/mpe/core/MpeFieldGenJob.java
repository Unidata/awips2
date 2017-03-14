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

package com.raytheon.viz.mpe.core;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;

import com.raytheon.uf.common.mpe.fieldgen.MpeFieldGenRequest;
import com.raytheon.uf.common.mpe.fieldgen.MpeFieldGenResponse;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.requests.ThriftClient;
import com.raytheon.viz.mpe.Activator;

/**
 * Job that executes the MPE Field Gen Job
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 1/08/09      1674       bphillip    Initial creation
 * 08/09/12     15307      snaples     Updated job to use postStreamingByteArray.
 * 03/28/14      2952      mpduff      Changed to use ThriftSrv.
 * </pre>
 * 
 * @author bphillip
 * @version 1.0
 */
public class MpeFieldGenJob extends Job {
    private final IUFStatusHandler log = UFStatus
            .getHandler(MpeFieldGenJob.class);

    /** The argument to run the MPE Field Gen with */
    private final String fieldGenArg;

    /**
     * Constructs a new MpeFieldGenJob
     * 
     * @param fieldGenArg
     *            The argument to run the MPE Field Gen with
     */
    public MpeFieldGenJob(String fieldGenArg) {
        super("Running Mpe Field Gen");
        this.fieldGenArg = fieldGenArg;
    }

    @Override
    protected IStatus run(IProgressMonitor monitor) {
        final Integer[] mpeExitValue = new Integer[1];

        MpeFieldGenRequest req = new MpeFieldGenRequest();
        req.setArgs(fieldGenArg);

        MpeFieldGenResponse response;
        try {
            response = (MpeFieldGenResponse) ThriftClient.sendRequest(req);
            int exitValue = response.getExitValue();

            if (exitValue != 0) {
                return new Status(Status.ERROR, Activator.PLUGIN_ID,
                        "MPE Field Gen execution failed with exit code: "
                                + mpeExitValue[0]);
            }
        } catch (VizException e) {
            log.handle(Priority.ERROR, "Error executing MPE FieldGen", e);
        }

        return Status.OK_STATUS;
    }
}
