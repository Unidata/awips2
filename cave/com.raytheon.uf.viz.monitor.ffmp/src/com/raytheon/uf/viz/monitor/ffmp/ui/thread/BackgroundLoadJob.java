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
package com.raytheon.uf.viz.monitor.ffmp.ui.thread;

import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.List;
import java.util.NavigableMap;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.SubMonitor;

import com.raytheon.uf.viz.monitor.ffmp.ui.rsc.FFMPResourceData;

/**
 * Retrieves and loads FFMP data in the background. Used as the secondary and
 * tertiary loaders.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 04, 2013 2075       njensen     Initial creation
 * Jun 07, 2013 2075       njensen     Added progress monitoring
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class BackgroundLoadJob extends AbstractLoadJob {

    private static final SimpleDateFormat sdf = new SimpleDateFormat(
            "yyyy-MM-dd HH:mm:ss");

    protected boolean preloadAvailableUris = false;

    public BackgroundLoadJob(String name, FFMPResourceData resourceData,
            Date timeBack, Date mostRecentTime, List<String> hucsToLoad) {
        super(name, resourceData, timeBack, mostRecentTime, hucsToLoad);
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.core.runtime.jobs.Job#run(org.eclipse.core.runtime.
     * IProgressMonitor)
     */
    @Override
    protected IStatus run(IProgressMonitor monitor) {
        System.out.println("FFMP background load job running for: "
                + sdf.format(startTime) + " to " + sdf.format(endTime));

        try {
            SubMonitor smonitor = SubMonitor.convert(monitor, "Loading Data",
                    2500);
            long t0 = System.currentTimeMillis();

            // preload available URIs
            smonitor.subTask("Preloading URIs...");
            if (preloadAvailableUris) {
                preloadAvailableUris();
            }
            smonitor.worked(100);
            if (!this.shouldRun()) {
                return Status.CANCEL_STATUS;
            }

            // QPE
            smonitor.subTask("Processing QPE...");
            NavigableMap<Date, List<String>> qpeURIs = getQpeUris();
            smonitor.worked(100);
            doQpe(qpeURIs, smonitor.newChild(1000));
            if (!this.shouldRun()) {
                return Status.CANCEL_STATUS;
            }

            // QPF
            smonitor.subTask("Processing QPF...");
            List<NavigableMap<Date, List<String>>> qpfs = getQpfUris(startTime);
            smonitor.worked(100);
            SubMonitor qpfmonitor = smonitor.newChild(1000);
            qpfmonitor.beginTask(null, qpfs.size() * PROGRESS_FACTOR);
            int i = 0;
            for (NavigableMap<Date, List<String>> qpfURIs : qpfs) {
                doQpf(qpfURIs, product.getQpf(i),
                        qpfmonitor.newChild(PROGRESS_FACTOR));
                i++;
            }
            if (!this.shouldRun()) {
                return Status.CANCEL_STATUS;
            }

            // Virtual
            smonitor.subTask("Processing Virtual...");
            doVirtual(smonitor.newChild(200));
            if (!this.shouldRun()) {
                return Status.CANCEL_STATUS;
            }

            // Guidance
            smonitor.subTask("Processing Guidance...");
            doGuidance(startTime, smonitor.newChild(200));
            if (!this.shouldRun()) {
                return Status.CANCEL_STATUS;
            }

            smonitor.done();
            System.out.println(this.getName() + " took: "
                    + (System.currentTimeMillis() - t0));

        } catch (Exception e) {
            statusHandler.error("Couldn't complete " + this.getName(), e);
        }

        return Status.OK_STATUS;

    }

    public void setPreloadAvailableUris(boolean preload) {
        this.preloadAvailableUris = preload;
    }

}
