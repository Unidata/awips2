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

import com.raytheon.edex.esb.Headers;

/**
 * Provides a basic implementation of the {@code AbstractRecordSeparator}
 * interface. In this implementation, a valid call to {@link #next()} returns
 * the entire contents of the file. This class is provided as a convenience to
 * plug-in writers writing a plug-in to handle data having a single record in a
 * file.
 * <P>
 * In this implementation, calling {@link #hasNext()} has the following returns:
 * <DL>
 * <DT><B>false</B>
 * <DD>returned prior to calling {@link #setData(byte[])}.
 * <DT><B>true</B>
 * <DD>returned after calling {@link #setData(byte[])} and prior to calling
 * {@link #next()}.
 * <DT><B>false</B>
 * <DD>returned after calling {@link #setData(byte[])} and after calling
 * {@link #next()}.
 * </DL>
 * <P>
 * In this implementation, {@link #next()} returns {@code null} any time calling
 * {@link #hasNext()} would return {@code false}.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 19Jan2007    138         MW Fegan    Initial Creation.
 * 
 * </pre>
 * 
 * @author mfegan
 * @version 1
 */

public class RecordSeparatorImpl extends AbstractRecordSeparator {

    private boolean hasData = false;

    private byte[] separatorData = null;

    /**
     * 
     */
    public RecordSeparatorImpl() {
        // TODO Auto-generated constructor stub
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.edex.plugin.AbstractRecordSeparator#getRecord()
     */
    public byte[] next() {
        if (hasData) {
            hasData = false;
            return separatorData;
        } else {
            return (byte[]) null;
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.edex.plugin.AbstractRecordSeparator#hasNext()
     */
    public boolean hasNext() {
        return hasData;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.edex.plugin.AbstractRecordSeparator#setData(byte[])
     */
    public void setData(byte[] messageData, Headers headers) {
        separatorData = messageData;
        hasData = true;
    }

}
