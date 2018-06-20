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
package com.raytheon.uf.common.dataplugin.gfe.request;

import com.raytheon.uf.common.dataplugin.gfe.db.objects.DatabaseID;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * Request object for getting the latest insert time for a given database ID.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 16, 2010  6349      bphillip     Initial creation
 * May 22, 2013  2025      dgilling     Add DynamicSerialize support.
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1.0
 */

@DynamicSerialize
public class GetLatestDbTimeRequest extends AbstractGfeRequest {

    @DynamicSerializeElement
    /** The database ID to get the latest insert time for */
    private DatabaseID dbId;

    public GetLatestDbTimeRequest() {
        // no-op
    }

    /**
     * Creates a new GetLatestDbTimeRequest
     * 
     * @param dbId
     *            The database ID to get the latest insert time for
     */
    public GetLatestDbTimeRequest(DatabaseID dbId) {
        super();
        this.dbId = dbId;
    }

    /**
     * Creates a new GetLatestDbTimeRequest
     * 
     * @param dbId
     *            The database ID to get the latest insert time for
     */
    public GetLatestDbTimeRequest(String dbId) {
        super();
        this.dbId = new DatabaseID(dbId);
    }

    public DatabaseID getDbId() {
        return dbId;
    }

    public void setDbId(DatabaseID dbId) {
        this.dbId = dbId;
    }
}
