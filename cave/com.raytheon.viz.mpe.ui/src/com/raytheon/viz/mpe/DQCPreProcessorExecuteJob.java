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
package com.raytheon.viz.mpe;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.ui.PlatformUI;

import com.raytheon.uf.common.mpe.dqcpreprocessor.DQCPreProcRunConfiguration;
import com.raytheon.uf.common.mpe.dqcpreprocessor.DQCPreProcessorExecuteRequest;
import com.raytheon.uf.common.mpe.dqcpreprocessor.DQCPreProcessorExecuteResponse;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.requests.ThriftClient;
import com.raytheon.viz.mpe.ui.MPEDisplayManager;

/**
 * Initiates the execution of the DQC PreProcessor in the background as an
 * Eclipse {@link Job}.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 6, 2018  7184       bkowal      Initial creation
 *
 * </pre>
 *
 * @author bkowal
 */

public class DQCPreProcessorExecuteJob extends Job {

    private final IUFStatusHandler statusHandler = UFStatus
            .getHandler(getClass());

    private static final String NAME = "Running DQC PreProcessor ...";

    private final DQCPreProcRunConfiguration runConfig;

    public DQCPreProcessorExecuteJob(
            final DQCPreProcRunConfiguration runConfig) {
        super(NAME);
        this.runConfig = runConfig;
    }

    @Override
    protected IStatus run(IProgressMonitor monitor) {
        final DQCPreProcessorExecuteRequest request = new DQCPreProcessorExecuteRequest(
                runConfig);

        DQCPreProcessorExecuteResponse response = null;
        try {
            response = (DQCPreProcessorExecuteResponse) ThriftClient
                    .sendRequest(request);
            if (response != null) {
                /*
                 * Offer to reload DailyQC if it is currently loaded ...
                 */
                checkDailyQCReload();
            } else {
                statusHandler.error(
                        "Received unexpected NULL response from the execution of the DQC PreProcessor.");
            }
        } catch (VizException e) {
            statusHandler.error("Failed to execute the DQC PreProcessor.", e);
        }

        return Status.OK_STATUS;
    }

    private void checkDailyQCReload() {
        VizApp.runAsync(new Runnable() {
            @Override
            public void run() {
                if (MPEDisplayManager.getCurrent().isQpf()
                        || MPEDisplayManager.getCurrent().isMaxmin()) {
                    MessageBox messageBox = new MessageBox(
                            PlatformUI.getWorkbench().getActiveWorkbenchWindow()
                                    .getShell(),
                            SWT.ICON_INFORMATION | SWT.OK);
                    messageBox.setText("DQC PreProcessor");
                    messageBox.setMessage(
                            "The requested run of DQC PreProcessor has successfully finished. Please reload the displayed data for the latest updates.");
                    messageBox.open();
                }
            }
        });
    }
}