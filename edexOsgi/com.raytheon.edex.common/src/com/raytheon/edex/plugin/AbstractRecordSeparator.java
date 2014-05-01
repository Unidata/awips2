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

package com.raytheon.edex.plugin;

import java.util.Iterator;

import com.raytheon.edex.esb.Headers;

/**
 * Standard interface for separating the contents of a multi-record data file
 * into single records for processing.
 * <P>
 * AbstractRecordSeparator is implemented by an AWIPS Data Type plug-in to allow
 * a consistant interface for sequentially obtaining each record in a data file.
 * The exact meaning of a data record is data type dependent.
 * <P>
 * Plug-ins handling data types having uni-record files should return the entire
 * file the first time {@link #next()} is called. Subsequent calls to
 * {@link #next()} should return {@code (byte[])null}.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 17Jan2007    138         MW Fegan    Initial Creation.
 * 06Nov2008                chammack    Convert to follow java Iterator
 * 
 * </pre>
 * 
 * @author mfegan
 * @version 1
 */
public abstract class AbstractRecordSeparator implements Iterator<Object> {
    /**
     * Sets the data to be iterated. The implementing class is responsible
     * converting the input into an appropriate data type for separation.
     * 
     * @param data
     *            the contents of the file to separate
     */
    public abstract void setData(byte[] data, Headers headers);

    /*
     * (non-Javadoc)
     * 
     * @see java.util.Iterator#remove()
     */
    @Override
    public void remove() {
        throw new UnsupportedOperationException("Remove not supported");
    }

}
