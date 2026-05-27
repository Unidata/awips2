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

/**
 * Encapsulates a request for a LockTable
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
public class LockTableRequest implements ISerializableObject {

    /** The parmID of the lock table being requested */

    @DynamicSerializeElement
    private ParmID parmId;

    /** The databaseID of the lock table being requested */

    @DynamicSerializeElement
    private DatabaseID dbId;

    /**
     * Creates a new LockTableRequest
     */
    public LockTableRequest() {

    }

    /**
     * Creates a new LockTableRequest
     * 
     * @param parmId
     *            The parmID of the lock table being requested
     */
    public LockTableRequest(ParmID parmId) {
        this.parmId = parmId;
    }

    /**
     * Creates a new LockTableRequest
     * 
     * @param databaseId
     *            The databaseID of the lock table being requested
     */
    public LockTableRequest(DatabaseID databaseId) {
        this.dbId = databaseId;
    }

    /**
     * Checks if this is a parm type request
     * 
     * @return True if this is a parm type request, false if not
     */
    public boolean isParmRequest() {

        if (parmId == null) {
            return false;
        } else {
            return parmId.isValid();
        }
    }

    /**
     * Checks if this is a database type request
     * 
     * @return True if this is a database type request, false if not
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

}
