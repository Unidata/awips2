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
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 23, 2010            dgilling     Initial creation
 * Mar 07, 2013   1759     dgilling     Refactored to use more sensible
 *                                      fields.
 * 
 * </pre>
 * 
 * @author dgilling
 * @version 1.0
 */

@DynamicSerialize
public class PurgeGfeGridsRequest extends AbstractGfeRequest {

    @DynamicSerializeElement
    private DatabaseID databaseID;

    public PurgeGfeGridsRequest() {
        // no-op, for serialization
    }

    public PurgeGfeGridsRequest(DatabaseID dbId) {
        this.databaseID = dbId;
        this.siteID = databaseID.getSiteId();
    }

    @Override
    public String toString() {
        StringBuilder builder = new StringBuilder();
        builder.append("PurgeGfeGridsRequest [databaseID=");
        builder.append(databaseID);
        builder.append(", workstationID=");
        builder.append(workstationID);
        builder.append(", siteID=");
        builder.append(siteID);
        builder.append("]");
        return builder.toString();
    }

    public void setDatabaseID(DatabaseID databaseID) {
        this.databaseID = databaseID;
    }

    public DatabaseID getDatabaseID() {
        return databaseID;
    }
}
