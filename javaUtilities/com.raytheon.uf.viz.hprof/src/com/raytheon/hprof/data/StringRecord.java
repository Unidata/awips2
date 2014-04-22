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
import com.raytheon.hprof.Id;

/**
 * 
 * Represents a string within an {@link HprofFile}.
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
public class StringRecord extends AbstractHprofRecord {

    public static final int TAG = 0x01;

    private Id id;

    private String string;

    public StringRecord(BigByteBuffer buffer, int idSize) {
        super(buffer, idSize);
    }

    @Override
    protected void init(BigByteBuffer buffer, int idSize) {
        id = new Id(buffer, idSize);
        byte[] string = new byte[(int) buffer.remaining()];
        buffer.get(string);
        this.string = new String(string).intern();
    }

    public Id getId() {
        return id;
    }

    public String getString() {
        return string;
    }

}
