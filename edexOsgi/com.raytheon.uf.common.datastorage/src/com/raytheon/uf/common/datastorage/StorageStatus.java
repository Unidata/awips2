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
package com.raytheon.uf.common.datastorage;

import com.raytheon.uf.common.datastorage.IDataStore.StoreOp;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * Provides a mechanism to return exceptions that occurred while executing batch
 * inserts. Also allows certain state information to be returned to the caller.
 * 
 * This allows some of the operations to succeed even if some fail.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 30, 2008            chammack     Initial creation
 * Jan 09, 2014 1998       bclement     added hasExceptions method, removed ISerializableObject
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1.0
 */
@DynamicSerialize
public class StorageStatus {

    private StorageException[] exceptions;

    /**
     * Lets the caller know if an update or insert, etc was performed
     */
    @DynamicSerializeElement
    private StoreOp operationPerformed;

    /**
     * If an append occurred, lets the caller know where the append started.
     */
    @DynamicSerializeElement
    private long[] indexOfAppend;

    /**
     * @return the exceptions
     */
    public StorageException[] getExceptions() {
        return exceptions;
    }

    /**
     * @param exceptions
     *            the exceptions to set
     */
    public void setExceptions(StorageException[] exceptions) {
        this.exceptions = exceptions;
    }

    /**
     * @return the operationPerformed
     */
    public StoreOp getOperationPerformed() {
        return operationPerformed;
    }

    /**
     * @param operationPerformed
     *            the operationPerformed to set
     */
    public void setOperationPerformed(StoreOp operationPerformed) {
        this.operationPerformed = operationPerformed;
    }

    /**
     * @return the indexOfAppend
     */
    public long[] getIndexOfAppend() {
        return indexOfAppend;
    }

    /**
     * @param indexOfAppend
     *            the indexOfAppend to set
     */
    public void setIndexOfAppend(long[] indexOfAppend) {
        this.indexOfAppend = indexOfAppend;
    }

    /**
     * @return true if exceptions field is populated
     */
    public boolean hasExceptions() {
        return exceptions != null && exceptions.length > 0;
    }

}
