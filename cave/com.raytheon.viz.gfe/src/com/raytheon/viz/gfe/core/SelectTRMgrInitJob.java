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

import java.util.TimeZone;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;

import com.raytheon.uf.common.dataplugin.gfe.server.message.ServerResponse;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.viz.gfe.core.internal.SelectTimeRangeManager;

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
 * Dec 02, 2015  #5129     dgilling    Support modified constructor.
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class SelectTRMgrInitJob extends Job {

    private final IUFStatusHandler statusHandler = UFStatus
            .getHandler(getClass());

    private DataManager dataMgr;

    public SelectTRMgrInitJob(DataManager dm) {
        super("GFE SelectTR Mgr Init Job");
        this.dataMgr = dm;
    }

    @Override
    protected IStatus run(IProgressMonitor monitor) {
        ServerResponse<String> sr = dataMgr.getClient().getSiteTimeZone(
                dataMgr.getSiteID());
        TimeZone tz;
        if (sr.isOkay()) {
            tz = TimeZone.getTimeZone(sr.getPayload());
        } else {
            statusHandler.error("Unable to retrieve GFE time zone, using GMT");
            tz = TimeZone.getTimeZone("GMT");
        }

        dataMgr.selectTimeRangeManager = new SelectTimeRangeManager(tz);
        dataMgr.getInitStatus().setSelectTRMgrDone(true);

        return Status.OK_STATUS;
    }

}
