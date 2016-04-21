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

import java.util.Date;
import java.util.List;
import java.util.NavigableMap;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.SubMonitor;

import com.raytheon.uf.common.dataplugin.ffmp.FFMPRecord;
import com.raytheon.uf.viz.monitor.ffmp.ui.rsc.FFMPResourceData;

/**
 * An FFMP load job for when updates arrive.
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

public class UpdateLoadJob extends AbstractLoadJob {

    public UpdateLoadJob(FFMPResourceData resourceData, Date timeBack,
            Date mostRecentTime, List<String> hucsToLoad) {
        super("Update FFMP", resourceData, timeBack, mostRecentTime, hucsToLoad);
        this.setPriority(INTERACTIVE);
        hucsToLoad.remove(FFMPRecord.VIRTUAL);
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.core.runtime.jobs.Job#run(org.eclipse.core.runtime.
     * IProgressMonitor)
     */
    @Override
    protected IStatus run(IProgressMonitor monitor) {
        SubMonitor smonitor = SubMonitor.convert(monitor, "Loading Data", 1100);
        // Rate
        doRate();
        smonitor.worked(200);

        // QPE
        NavigableMap<Date, List<String>> qpeURIs = getQpeUris();
        smonitor.worked(25);
        doQpe(qpeURIs, smonitor.newChild(250));

        // QPF
        List<NavigableMap<Date, List<String>>> qpfs = getQpfUris(startTime);
        smonitor.worked(25);
        SubMonitor qpfMonitor = smonitor.newChild(225);
        int i = 0;
        qpfMonitor.beginTask(null, qpfs.size() * PROGRESS_FACTOR);
        for (NavigableMap<Date, List<String>> qpfURIs : qpfs) {
            doQpf(qpfURIs, product.getQpf(i),
                    qpfMonitor.newChild(PROGRESS_FACTOR));
            i++;
        }

        // Virtual
        doVirtual(smonitor.newChild(200));

        // Guidance
        doGuidance(startTime, smonitor.newChild(200));

        return Status.OK_STATUS;
    }

    @Override
    protected NavigableMap<Date, List<String>> getQpfUris(String sourceName,
            Date qpfTime) {
        return super.getQpfUris(sourceName, ffmpMonitor.getPreviousQueryTime(
                resourceData.siteKey, sourceName));
    }

    @Override
    protected NavigableMap<Date, List<String>> getGuidURIs(String sourceName,
            Date guidTime) {
        NavigableMap<Date, List<String>> retVal = null;
        Date prevTime = ffmpMonitor.getPreviousQueryTime(resourceData.siteKey,
                sourceName);
        if (prevTime != null) {
            retVal = ffmpMonitor.getAvailableUris(resourceData.siteKey,
                    resourceData.dataKey, sourceName, prevTime);
        }
        return retVal;
    }

}
