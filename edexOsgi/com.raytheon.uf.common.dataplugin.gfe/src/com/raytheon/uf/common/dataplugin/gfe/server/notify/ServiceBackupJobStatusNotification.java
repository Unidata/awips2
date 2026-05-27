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
package com.raytheon.uf.common.dataplugin.gfe.server.notify;

import com.raytheon.uf.common.dataplugin.gfe.svcbu.JobProgress;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * A {@code GfeNotification} for reporting status of asynchronous jobs utilized
 * during the service backup process.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 17, 2015  #4103     dgilling     Initial creation
 * 
 * </pre>
 * 
 * @author dgilling
 * @version 1.0
 */
@DynamicSerialize
public class ServiceBackupJobStatusNotification extends GfeNotification {

    @DynamicSerializeElement
    private String name;

    @DynamicSerializeElement
    private JobProgress state;

    /**
     * Default constructor--should only be used by DynamicSerialize.
     */
    public ServiceBackupJobStatusNotification() {
        super();
    }

    /**
     * Construct a new notification.
     * 
     * @param name
     *            The name of the job to report status for.
     * @param state
     *            A {@code JobProgress} instance containing the state of the
     *            job.
     * @param siteId
     *            The site identifier for the site the job was running for.
     */
    public ServiceBackupJobStatusNotification(String name, JobProgress state,
            String siteId) {
        super(siteId);
        this.name = name;
        this.state = state;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public JobProgress getState() {
        return state;
    }

    public void setState(JobProgress state) {
        this.state = state;
    }
}
