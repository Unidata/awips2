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
package com.raytheon.uf.common.backupsvc.response;

import java.util.List;

import com.raytheon.uf.common.dataplugin.backupsvc.database.BackupSvcIncoming;
import com.raytheon.uf.common.dataplugin.backupsvc.database.BackupSvcOutgoing;
import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * A Response Object that allows users to receive incoming Localization files
 * from other sites.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 24, 2021 84473     Amanuel Challa  Initial creation
 * Jun 08, 2021 92508     Robert.Blum  Added helper method to check for successful
 *                                     response.
 *
 * </pre>
 *
 * @author Amanuel.Challa
 * @version 1.0
 */
@DynamicSerialize
public class BackupSvcIncomingResponse implements ISerializableObject {

    public List<BackupSvcOutgoing> getFailedJobs() {
        return failedOutgoingJobs;
    }

    public void setFailedJobs(List<BackupSvcOutgoing> failedJobs) {
        this.failedOutgoingJobs = failedJobs;
    }

    @DynamicSerializeElement
    private List<BackupSvcIncoming> incomingJobs;

    @DynamicSerializeElement
    private List<BackupSvcIncoming> failedIncomingJobs;

    @DynamicSerializeElement
    private List<BackupSvcOutgoing> failedOutgoingJobs;

    @DynamicSerializeElement
    private List<BackupSvcOutgoing> outgoingJobs;

    public List<BackupSvcOutgoing> getOutgoingJobs() {
        return outgoingJobs;
    }

    public void setOutgoingJobs(List<BackupSvcOutgoing> outgoingJobs) {
        this.outgoingJobs = outgoingJobs;
    }

    @DynamicSerializeElement
    private Exception exceptions;

    public BackupSvcIncomingResponse() {

    }

    public List<BackupSvcOutgoing> getFailedOutgoingJobs() {
        return failedOutgoingJobs;
    }

    public List<BackupSvcIncoming> getIncomingJobs() {
        return incomingJobs;
    }

    public void setIncomingJobs(List<BackupSvcIncoming> incomingjobs) {
        this.incomingJobs = incomingjobs;
    }

    public Exception getExceptions() {
        return exceptions;
    }

    public void setExceptions(Exception exceptions) {
        this.exceptions = exceptions;
    }

    public List<BackupSvcIncoming> getFailedIncomingJobs() {
        return failedIncomingJobs;
    }

    public void setFailedIncomingJobs(
            List<BackupSvcIncoming> failedIncomingJobs) {
        this.failedIncomingJobs = failedIncomingJobs;
    }

    public boolean isSuccessful() {
        return exceptions == null
                && (failedIncomingJobs == null || failedIncomingJobs.isEmpty());
    }

    public void setFailedOutgoingJobs(List<BackupSvcOutgoing> failedJobs) {
        this.failedOutgoingJobs = failedJobs;
    }
}
