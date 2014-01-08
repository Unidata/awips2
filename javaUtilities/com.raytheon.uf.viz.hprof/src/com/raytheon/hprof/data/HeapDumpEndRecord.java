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
 * Object for the heap dump end record in an {@link HprofFile}, this type of
 * record has no content.
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
 * @see MergedHeapDumpSegmentRecord
 */
public class HeapDumpEndRecord extends AbstractHprofRecord {

    public static final int TAG = 0x2c;

    public HeapDumpEndRecord(BigByteBuffer buffer, int idSize) {
        super(buffer, idSize);
    }

    @Override
    protected void init(BigByteBuffer buffer, int idSize) {
        /* This record has no content */
    }

}
