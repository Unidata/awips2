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
import com.raytheon.uf.common.dataplugin.gfe.server.lock.LockTable;
import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.common.time.TimeRange;

/**
 * Encapsulates a request to change a lock
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 04/08/08     #875       bphillip    Initial Creation
 * 06/17/08     #940       bphillip    Implemented GFE Locking
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1.0
 */

@DynamicSerialize
public class LockRequest implements ISerializableObject {

    /** The time range for the lock change */

    @DynamicSerializeElement
    private TimeRange timeRange;

    /** The parm ID of the grid for which to change the lock */

    @DynamicSerializeElement
    private ParmID parmId;

    /** The databaseID of the grid for which to change the lock */

    @DynamicSerializeElement
    private DatabaseID dbId;

    /** The mode to change the lock to */

    @DynamicSerializeElement
    private LockTable.LockMode mode;

    /**
     * Creates a new LockRequest
     */
    public LockRequest() {

    }

    /**
     * Creates a new ParmID LockRequest
     * 
     * @param parmId
     *            The parm ID of the grid for which to change the lock
     * @param tr
     *            The time range for the lock change
     * @param mode
     *            The mode to change the lock to
     */
    public LockRequest(ParmID parmId, TimeRange tr, LockTable.LockMode mode) {
        this.parmId = parmId;
        this.timeRange = tr;
        this.mode = mode;
    }

    /**
     * Creates a new DatabaseID LockRequest
     * 
     * @param dbId
     *            The databaseID of the grids for which to change the lock
     * @param tr
     *            The time range for the lock change
     * @param mode
     *            The mode to change thelock to
     */
    public LockRequest(DatabaseID dbId, TimeRange tr, LockTable.LockMode mode) {
        this.dbId = dbId;
        this.timeRange = tr;
        this.mode = mode;
    }

    public String toString() {
        StringBuilder builder = new StringBuilder();

        // if we have a parm request
        if (isParmRequest()) {
            builder.append("ParmId: " + parmId);
        }
        // if we have a database request
        else {
            builder.append("DbId: " + dbId);
        }

        // valid request
        if (isParmRequest() || isDatabaseRequest()) {
            builder.append(" TR: " + timeRange);
            builder.append(" Mode: " + mode);
        }

        // empty request
        else {
            builder.append(" <EmptyRequest>\n");
        }

        return builder.toString();
    }

    public boolean isParmRequest() {
        return parmId != null && parmId.isValid();
    }

    public boolean isDatabaseRequest() {
        return dbId != null && dbId.isValid();
    }

    public TimeRange getTimeRange() {
        return timeRange;
    }

    public void setTimeRange(TimeRange timeRange) {
        this.timeRange = timeRange;
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

    public LockTable.LockMode getMode() {
        return mode;
    }

    public void setMode(LockTable.LockMode mode) {
        this.mode = mode;
    }

}
