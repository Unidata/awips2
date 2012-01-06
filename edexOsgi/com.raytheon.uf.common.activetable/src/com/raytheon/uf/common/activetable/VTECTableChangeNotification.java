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
package com.raytheon.uf.common.activetable;

import java.util.Arrays;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * VTEC Table Change Notification message
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 17, 2010     #2866  randerso     Initial creation
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */

@DynamicSerialize
public class VTECTableChangeNotification {
    // Table for disabling editors
    // Key is pil of product in editor
    // Value is list of pils whose active table changes will disable editor
    // Default (no need for table entry) action is to disable when
    // site and pil match.

    public static final Map<String, List<String>> DisableTable;
    static {
        DisableTable = new HashMap<String, List<String>>();
        DisableTable.put("CWF", Arrays.asList("CWF", "NSH", "WCN", "TCV"));
        DisableTable.put("NSH", Arrays.asList("CWF", "NSH", "WCN", "TCV"));
        DisableTable.put("WCN", Arrays.asList("CWF", "NSH", "WCN"));
    }

    @DynamicSerializeElement
    private ActiveTableMode mode;

    @DynamicSerializeElement
    private Date modTime;

    @DynamicSerializeElement
    private String modSource;

    @DynamicSerializeElement
    private VTECChange[] changes;

    public VTECTableChangeNotification() {
    }

    public VTECTableChangeNotification(ActiveTableMode mode, Date modTime,
            String modSource, VTECChange[] changes) {
        this.mode = mode;
        this.modTime = modTime;
        this.modSource = modSource;
        this.changes = changes;
    }

    public ActiveTableMode getMode() {
        return mode;
    }

    public Date getModTime() {
        return modTime;
    }

    public String getModSource() {
        return modSource;
    }

    public VTECChange[] getChanges() {
        return changes;
    }

    public void setMode(ActiveTableMode mode) {
        this.mode = mode;
    }

    public void setModTime(Date modTime) {
        this.modTime = modTime;
    }

    public void setModSource(String modSource) {
        this.modSource = modSource;
    }

    public void setChanges(VTECChange[] changes) {
        this.changes = changes;
    }

    @Override
    public String toString() {
        // TODO Auto-generated method stub
        return "mode:" + mode + " modTime:" + modTime.toGMTString()
                + " source:'" + modSource + "' changes:"
                + Arrays.toString(changes);
    }
}
