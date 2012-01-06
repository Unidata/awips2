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
package com.raytheon.viz.aviation.monitor;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;

import com.raytheon.uf.common.time.SimulatedTime;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.viz.aviation.observer.TafMonitorDlg;

/**
 * Checks all monitored sites for all data types
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 10, 2009            njensen     Initial creation
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class CheckNowJob extends Job {

    private TafMonitorDlg dlg;

    public CheckNowJob(String name, TafMonitorDlg dlg) {
        super(name);
        this.dlg = dlg;
    }

    /*
     * (non-Javadoc)
     * 
     * @seeorg.eclipse.core.runtime.jobs.Job#run(org.eclipse.core.runtime.
     * IProgressMonitor)
     */
    @Override
    protected IStatus run(IProgressMonitor monitor) {
        VizApp.runSync(new Runnable() {

            @Override
            public void run() {
                dlg.getMessageBar().setMessageText("Checking all data...",
                        MonitorObserver.GREEN);
            }
        });

        // long t0 = System.currentTimeMillis();
        LtgDataMgr.clearAllData();
        long timeAgo = SimulatedTime.getSystemTime().getTime().getTime()
                - LtgDataMgr.age * 60 * 1000;
        LtgDataMgr.updateSiteData(MonitorDataUtil.getLightningData(timeAgo));
        // long t1 = System.currentTimeMillis();

        CcfpData.checkData();
        // long t2 = System.currentTimeMillis();

        // System.out.println("Lightning monitoring initialization time: "
        // + (t1 - t0));
        // System.out.println("CCFP monitoring initialization time: " + (t2 -
        // t1));

        for (TafSiteComp tsc : dlg.getTafSiteComps()) {
            tsc.checkNow();
        }

        return Status.OK_STATUS;
    }

}
