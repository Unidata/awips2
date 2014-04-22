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
package com.raytheon.hprof;

import java.nio.ByteBuffer;
import java.util.Arrays;

/**
 * 
 * Represents an Id of an object in an hprof file. The id is generally either 4
 * or 8 bytes which is essentially an int or a long but the hprof format allows
 * arbitrary id lengths.
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
public class Id implements Comparable<Id> {
    
    private final byte[] id;

    public Id(BigByteBuffer buffer, int idSize) {
        id = new byte[idSize];
        buffer.get(id);
    }

    public Id(ByteBuffer buffer, int idSize) {
        id = new byte[idSize];
        buffer.get(id);
    }

    public boolean isNull() {
        for (byte b : id) {
            if (b != 0x00) {
                return false;
            }
        }
        return true;
    }

    public int size() {
        return id.length;
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result + Arrays.hashCode(id);
        return result;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (obj == null)
            return false;
        if (getClass() != obj.getClass())
            return false;
        Id other = (Id) obj;
        if (!Arrays.equals(id, other.id))
            return false;
        return true;
    }

    @Override
    public String toString() {
        StringBuilder str = new StringBuilder();
        str.append("id=0x");
        for (byte b : id) {
            str.append(String.format("%02x", b & 0xFF));
        }
        return str.toString();
    }

    @Override
    public int compareTo(Id that) {
        for (int i = 0; i < id.length; i++) {
            int diff = id[i] - that.id[i];
            if (diff != 0) {
                return diff;
            }
        }
        return 0;
    }

}