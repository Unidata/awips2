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
package com.raytheon.hprof.data;

import com.raytheon.hprof.BigByteBuffer;
import com.raytheon.hprof.HprofFile;

/**
 * 
 * Base class for the different types of records in an {@link HprofFile}.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Jan 08, 2014  2648     bsteffen    Initial doc
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public abstract class AbstractHprofRecord {

    protected final int time;

    public AbstractHprofRecord(BigByteBuffer buffer, int idSize) {
        time = buffer.getInt();
        long size = buffer.getInt();
        if (size < 0) {
            /* size is really an unsigned int. */
            size = ((int) size) & 0xFFFFFFFFL;
        }
        BigByteBuffer safeBuffer = buffer.slice();
        safeBuffer.limit(size);
        init(safeBuffer, idSize);
        buffer.position(buffer.position() + size);
    }

    protected abstract void init(BigByteBuffer buffer, int idSize);
}
