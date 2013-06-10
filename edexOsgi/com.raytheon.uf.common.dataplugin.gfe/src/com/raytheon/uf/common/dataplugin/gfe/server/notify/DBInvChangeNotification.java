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

package com.raytheon.uf.common.dataplugin.gfe.server.notify;

import java.util.Collections;
import java.util.List;

import com.raytheon.uf.common.dataplugin.gfe.db.objects.DatabaseID;
import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * Notification that the grid inventory has been changed.<br>
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 08/17/10     #6742      bphillip    Initial Creation
 * 05/02/13     #1969      randerso    Removed inventory field, general cleanup
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1.0
 */

@DynamicSerialize
public class DBInvChangeNotification extends GfeNotification implements
        ISerializableObject {

    /** The additions to the database inventory */
    @DynamicSerializeElement
    private List<DatabaseID> additions = Collections.emptyList();

    /** The deletions to the database inventory */
    @DynamicSerializeElement
    private List<DatabaseID> deletions = Collections.emptyList();

    /**
     * Creates a new DBInvChangeNotification
     */
    public DBInvChangeNotification() {

    }

    /**
     * Creates a new DBInvChangeNotification
     * 
     * @param additions
     *            The DatabaseIDs that have been added to the inventory. Can be
     *            null if none.
     * @param deletions
     *            The DatabaseIDs that have been deleted from the inventory Can
     *            be null if none.
     */
    public DBInvChangeNotification(List<DatabaseID> additions,
            List<DatabaseID> deletions, String siteId) {
        super(siteId);

        if (additions != null) {
            this.additions = additions;
        }

        if (deletions != null) {
            this.deletions = deletions;
        }
    }

    @Override
    public boolean equals(Object obj) {
        if (!(obj instanceof DBInvChangeNotification)) {
            return false;
        }
        DBInvChangeNotification rhs = (DBInvChangeNotification) obj;

        if (additions.containsAll(rhs.getAdditions())
                && deletions.containsAll(rhs.getDeletions())
                && siteID.equals(rhs.getSiteID())) {
            return true;
        }

        return false;
    }

    @Override
    public String toString() {
        StringBuilder str = new StringBuilder();
        str.append("Additions: ").append(this.additions).append("\n");
        str.append("Deletions: ").append(this.deletions).append("\n");
        return str.toString();
    }

    /**
     * @return the additions
     */
    public List<DatabaseID> getAdditions() {
        return additions;
    }

    /**
     * @param additions
     *            the additions to set
     */
    public void setAdditions(List<DatabaseID> additions) {
        this.additions = additions;
    }

    /**
     * @return the deletions
     */
    public List<DatabaseID> getDeletions() {
        return deletions;
    }

    /**
     * @param deletions
     *            the deletions to set
     */
    public void setDeletions(List<DatabaseID> deletions) {
        this.deletions = deletions;
    }

}
