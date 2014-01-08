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
 * Record of a loaded class in an {@link HprofFile}. Only really used for
 * correlating class ids in the dump to their names.
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
public class LoadClassRecord extends AbstractHprofRecord {

    public static final int TAG = 0x02;

    private int classSerialNumber;

    private Id classId;

    private int stackSerialNumber;

    private Id classNameId;

    public LoadClassRecord(BigByteBuffer buffer, int idSize) {
        super(buffer, idSize);
    }

    @Override
    protected void init(BigByteBuffer buffer, int idSize) {
        classSerialNumber = buffer.getInt();
        classId = new Id(buffer, idSize);
        stackSerialNumber = buffer.getInt();
        classNameId = new Id(buffer, idSize);
    }

    public static int getTag() {
        return TAG;
    }

    public int getClassSerialNumber() {
        return classSerialNumber;
    }

    public Id getClassId() {
        return classId;
    }

    public int getStackSerialNumber() {
        return stackSerialNumber;
    }

    public Id getClassNameId() {
        return classNameId;
    }

}
