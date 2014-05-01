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

package com.raytheon.uf.common.dataplugin.gfe.server.request;

import com.raytheon.uf.common.dataplugin.gfe.db.objects.DatabaseID;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.ParmID;
import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.common.time.TimeRange;

/**
 * Placeholder for a CommitGridRequest
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 04/08/08     #875       bphillip    Initial Creation
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1.0
 */

@DynamicSerialize
public class CommitGridRequest implements ISerializableObject {

    /** The parmID of the commit request, if this is a parm commit request */

    @DynamicSerializeElement
    private ParmID parmId;

    /**
     * The databaseID of the commit request, if this is a database commit
     * request
     */

    @DynamicSerializeElement
    private DatabaseID dbId;

    /** The timerange of the commit */

    @DynamicSerializeElement
    private TimeRange timeRange;

    /**
     * Denotes whether to send ISC grids on publish based on the client's
     * preferences
     */

    @DynamicSerializeElement
    private boolean clientSendStatus;

    public CommitGridRequest() {

    }

    /**
     * Constructs a new CommitGridRequest
     * 
     * @param parmId
     *            The parmID to commit
     * @param commitTime
     *            The time to commit
     */
    public CommitGridRequest(ParmID parmId, TimeRange commitTime) {
        this.parmId = parmId;
        this.timeRange = commitTime;
    }

    public CommitGridRequest(ParmID parmId, TimeRange commitTime,
            boolean clientSendStatus) {
        this(parmId, commitTime);
        this.clientSendStatus = clientSendStatus;
    }

    /**
     * Constructs a new CommitGridRequest
     * 
     * @param databaseId
     *            The databaseID to commit
     * @param commitTime
     *            The time to commit
     */
    public CommitGridRequest(DatabaseID databaseId, TimeRange commitTime) {
        this.dbId = databaseId;
        this.timeRange = commitTime;
    }

    /**
     * Returns true if this is a parm-oriented request
     * 
     * @return True if this is a parm-oriented request
     */
    public boolean isParmRequest() {
        if (parmId == null) {
            return false;
        } else {
            return parmId.isValid();
        }
    }

    /**
     * Returns true if this is a database-oriented request
     * 
     * @return True if this is a database-oriented request
     */
    public boolean isDatabaseRequest() {
        if (dbId == null) {
            return false;
        } else {
            return dbId.isValid();
        }
    }

    public ParmID getParmId() {
        return parmId;
    }

    public void setParmId(ParmID parmId) {
        this.parmId = parmId;
    }

    public DatabaseID getDbId() {
        return dbId;
    }

    public void setDbId(DatabaseID dbId) {
        this.dbId = dbId;
    }

    public TimeRange getTimeRange() {
        return timeRange;
    }

    public void setTimeRange(TimeRange timeRange) {
        this.timeRange = timeRange;
    }

    @Override
    public String toString() {
        StringBuffer buf = new StringBuffer();
        if (isParmRequest()) {
            buf.append("ParmId: ").append(parmId.toString());
        } else if (this.isDatabaseRequest()) {
            buf.append("DbId: ").append(dbId.toString());
        }

        // valid request
        if (this.isParmRequest() || this.isDatabaseRequest()) {
            // Time Range
            buf.append(" CommitTR: ").append(this.timeRange);
        }
        // empty request
        else {
            buf.append(" <EmptyRequest>");
        }
        return buf.toString();
    }

    /**
     * @return the clientSendStatus
     */
    public boolean isClientSendStatus() {
        return clientSendStatus;
    }

    /**
     * @param clientSendStatus
     *            the clientSendStatus to set
     */
    public void setClientSendStatus(boolean clientSendStatus) {
        this.clientSendStatus = clientSendStatus;
    }

}
