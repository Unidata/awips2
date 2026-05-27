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
package com.raytheon.uf.common.dataplugin.gfe.point;

import java.util.List;

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * List of GFE Point Data containers.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar  6, 2013 1735       rferrel     Initial creation
 * Apr 23, 2014  #3006     randerso    Added toString to aid in debugging
 * 
 * </pre>
 * 
 * @author rferrel
 * @version 1.0
 */
@DynamicSerialize
public class GFEPointDataContainers {
    /** Results for a list of coordinates. */
    @DynamicSerializeElement
    private List<GFEPointDataContainer> containers;

    /**
     * Get list of containers.
     * 
     * @return containers
     */
    public List<GFEPointDataContainer> getContainers() {
        return containers;
    }

    /**
     * Set list of containers.
     * 
     * @param containers
     */
    public void setContainers(List<GFEPointDataContainer> containers) {
        this.containers = containers;
    }

    /**
     * Get the number of containers in the list.
     * 
     * @return size
     */
    public int getSize() {
        int size = 0;
        if (containers != null) {
            size = containers.size();
        }
        return size;
    }

    /**
     * Obtain specific container.
     * 
     * @param index
     * @return container
     */
    public GFEPointDataContainer getContainer(int index) {
        return containers.get(index);
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#toString()
     */
    @Override
    public String toString() {
        return containers.toString();
    }

}
