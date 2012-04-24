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

import java.util.ArrayList;
import java.util.List;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 23, 2012            mnash     Initial creation
 * 
 * </pre>
 * 
 * @author mnash
 * @version 1.0
 */

public class CollaborationGroupContainer {
    private List<Object> objects;

    /**
     * 
     */
    public CollaborationGroupContainer() {
        objects = new ArrayList<Object>();
    }

    /**
     * @param objects
     *            the objects to set
     */
    public void setObjects(List<Object> objects) {
        this.objects = objects;
    }

    /**
     * @return the objects
     */
    public List<Object> getObjects() {
        return objects;
    }

    public void addObject(Object o) {
        objects.add(o);
    }

    public void removeObject(Object o) {
        objects.remove(o);
    }

    public void clear() {
        objects.clear();
    }
}
