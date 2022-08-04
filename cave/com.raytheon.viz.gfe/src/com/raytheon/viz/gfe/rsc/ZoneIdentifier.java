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
package com.raytheon.viz.gfe.rsc;

/**
 * TODO Add Description ZoneIdentifier.java May 27, 2008
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 	May 27, 2008					Eric Babin Initial Creation
 * 
 * </pre>
 * 
 * @author ebabin
 * @version 1.0
 */

public class ZoneIdentifier implements Comparable<ZoneIdentifier> {
    private int group = -1;

    private String identifier = "";

    public ZoneIdentifier(int group, String identifier) {
        this.group = group;
        this.identifier = identifier;
    }

    public String getIdentifier() {
        return identifier;
    }

    public void setIdentifier(String identifier) {
        this.identifier = identifier;
    }

    public int getGroup() {
        return group;
    }

    public void setGroup(int group) {
        this.group = group;
    }

    public void clearZone() {
        this.setGroup(-1);
    }

    @Override
    public String toString() {
        // TODO Auto-generated method stub
        return this.identifier;
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Comparable#compareTo(java.lang.Object)
     */
    @Override
    public int compareTo(ZoneIdentifier o) {
        // first compare the zoneIndex's...
        if (this.getGroup() > o.getGroup()) {
            return 1;
        } else if (this.getGroup() < o.getGroup()) {
            return -1;
        } else if (o.getGroup() == this.getGroup()) {
            return this.getIdentifier().compareTo(o.getIdentifier());
        }
        return 0;
    }
}
