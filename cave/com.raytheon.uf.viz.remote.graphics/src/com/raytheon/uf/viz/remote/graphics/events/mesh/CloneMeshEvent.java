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
package com.raytheon.uf.viz.remote.graphics.events.mesh;

import org.geotools.coverage.grid.GeneralGridGeometry;

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.viz.remote.graphics.events.AbstractDispatchingObjectEvent;
import com.raytheon.uf.viz.remote.graphics.events.ICreationEvent;

/**
 * 
 * Event for cloning a mesh in a different projection
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Feb 21, 2014  2817     bsteffen    Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
@DynamicSerialize
public class CloneMeshEvent extends AbstractDispatchingObjectEvent implements
        ICreationEvent {

    @DynamicSerializeElement
    private GeneralGridGeometry targetGeometry;

    @DynamicSerializeElement
    private int sourceObjectId;

    /**
     * @return the targetGeometry
     */
    public GeneralGridGeometry getTargetGeometry() {
        return targetGeometry;
    }

    /**
     * @param targetGeometry
     *            the targetGeometry to set
     */
    public void setTargetGeometry(GeneralGridGeometry targetGeometry) {
        this.targetGeometry = targetGeometry;
    }

    public int getSourceObjectId() {
        return sourceObjectId;
    }

    public void setSourceObjectId(int sourceObjectId) {
        this.sourceObjectId = sourceObjectId;
    }

}
