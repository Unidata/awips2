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
package com.raytheon.uf.common.pypies.request;

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * Pypies request object used for deleting data from a pypies managed HDF5 data
 * store.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 27, 2010            njensen     Initial creation
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

@DynamicSerialize
public class DeleteFilesRequest extends AbstractRequest {

    /**
     * The formatted dates to delete. If the HDF5 file name contains on of these
     * dates, the file is deleted
     */
    @DynamicSerializeElement
    private String[] datesToDelete;

    /**
     * Creates a new DeleteFileRequest
     */
    public DeleteFilesRequest() {

    }

    /**
     * Creates a new DeleteFileRequest with the provided data store path and a
     * list of dates to delete
     * 
     * @param dataStorePath
     *            The data store path.
     * @param datesToDelete
     *            The dates to be deleted.
     */
    public DeleteFilesRequest(String dataStorePath, String[] datesToDelete) {
        this.datesToDelete = datesToDelete;
    }

    /**
     * @return the datesToDelete
     */
    public String[] getDatesToDelete() {
        return datesToDelete;
    }

    /**
     * @param datesToDelete
     *            the datesToDelete to set
     */
    public void setDatesToDelete(String[] datesToDelete) {
        this.datesToDelete = datesToDelete;
    }

}
