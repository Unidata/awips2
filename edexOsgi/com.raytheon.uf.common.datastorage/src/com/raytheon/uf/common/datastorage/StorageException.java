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

import com.raytheon.uf.common.datastorage.records.IDataRecord;

/**
 * Indicates an error in persistence occurred.
 * 
 * A standard exception, but also contains the record that caused the exception
 * as payload.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * Feb 8, 2007              chammack    Initial Creation.
 * Dec 31, 2008             chammack    Added IDataStore payload
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1
 */
public class StorageException extends Exception {

    protected IDataRecord record;

    public StorageException(String arg0, IDataRecord record) {
        super(arg0);
        this.record = record;
    }

    public StorageException(String arg0, IDataRecord record, Exception e) {
        super(arg0, e);
        this.record = record;
    }

    /**
     * @return the record
     */
    public IDataRecord getRecord() {
        return record;
    }

    private static final long serialVersionUID = 1L;

}
