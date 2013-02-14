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
package com.raytheon.uf.common.dataaccess.grid;

import com.raytheon.uf.common.dataaccess.IDataRequest;
import com.raytheon.uf.common.datastorage.Request;

/**
 * A request for gridded data.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 9, 2012            njensen     Initial creation
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public interface IGridRequest extends IDataRequest<IGridData> {

    /**
     * Sets a storage request as part of the request. If null, the entire
     * dataset will be retrieved. Useful for slab requests to avoid retrieving
     * the entire dataset and boost performance in some scenarios.
     * 
     * @param request
     *            the {@link Request} to limit the data returned
     */
    public void setStorageRequest(Request request);

    /**
     * Gets the storage request set on the request.
     * 
     * @return the {@link Request} set on the IGridRequest
     */
    public Request getStorageRequest();

}
