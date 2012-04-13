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
package com.raytheon.uf.viz.collaboration.data;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 22, 2012            mnash     Initial creation
 * 
 * </pre>
 * 
 * @author mnash
 * @version 1.0
 */

public class CollaborationNode implements Comparable<CollaborationNode> {

    private String text;

    private boolean local;

    String id;

    public CollaborationNode(String id) {
        super();
        this.id = id;
    }

    public String getId() {
        return id;
    }

    public String getImageKey() {
        return null;
    }

    /**
     * @return the name
     */
    public String getText() {
        return text;
    }

    /**
     * @param name
     *            the name to set
     */
    public void setText(String text) {
        this.text = text;
    }

    /**
     * @return the buddy
     */
    public boolean isLocal() {
        return local;
    }

    /**
     * @param buddy
     *            the buddy to set
     */
    public void setLocal(boolean local) {
        this.local = local;
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Comparable#compareTo(java.lang.Object)
     */
    @Override
    public int compareTo(CollaborationNode o) {
        return id.compareTo(o.id);
    }
}
