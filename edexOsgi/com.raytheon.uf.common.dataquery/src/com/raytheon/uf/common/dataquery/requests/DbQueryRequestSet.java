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
package com.raytheon.uf.common.dataquery.requests;

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.common.serialization.comm.IServerRequest;

/**
 * Send multiple {@link DbQueryRequestSet}s at once. This can be more efficient
 * than sending multiple requests individually because it reduces the network
 * overhead.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Jun 30, 2011           rjpeter     Initial creation
 * Dec 18, 2013  2579     bsteffen    Class javadoc
 * 
 * </pre>
 * 
 * @author rjpeter
 * @version 1.0
 * @see DbQueryRequest
 */
@DynamicSerialize
public class DbQueryRequestSet implements IServerRequest {

    @DynamicSerializeElement
    private DbQueryRequest[] queries;

    public DbQueryRequest[] getQueries() {
        return queries;
    }

    public void setQueries(DbQueryRequest[] queries) {
        this.queries = queries;
    }
}
