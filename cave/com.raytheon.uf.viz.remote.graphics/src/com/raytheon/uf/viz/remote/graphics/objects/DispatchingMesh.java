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
package com.raytheon.uf.viz.remote.graphics.objects;

import org.geotools.coverage.grid.GeneralGridGeometry;

import com.raytheon.uf.viz.core.IExtent;
import com.raytheon.uf.viz.core.IMesh;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.remote.graphics.Dispatcher;
import com.raytheon.uf.viz.remote.graphics.DispatchingObject;
import com.raytheon.uf.viz.remote.graphics.events.DisposeObjectEvent;
import com.raytheon.uf.viz.remote.graphics.events.RemoteGraphicsEventFactory;

/**
 * Dispatching mesh object created from graphics mesh and forwards key events to
 * remote clients
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 5, 2012            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class DispatchingMesh extends DispatchingObject<IMesh> implements IMesh {

    /**
     * @param targetObject
     * @param dispatcher
     */
    public DispatchingMesh(IMesh targetObject, Dispatcher dispatcher) {
        super(targetObject, dispatcher);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.IMesh#dispose()
     */
    @Override
    public void dispose() {
        wrappedObject.dispose();
        // Send event to dispose mesh
        dispatch(RemoteGraphicsEventFactory.createEvent(
                DisposeObjectEvent.class, this));
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.IMesh#intersects(com.raytheon.uf.viz.core.IExtent
     * )
     */
    @Override
    public boolean intersects(IExtent extent) {
        return wrappedObject.intersects(extent);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.IMesh#reproject(org.geotools.coverage.grid.
     * GeneralGridGeometry)
     */
    @Override
    public IMesh reproject(GeneralGridGeometry targetGeometry)
            throws VizException {
        return clone(targetGeometry);
    }

    @Override
    public IMesh clone(GeneralGridGeometry targetGeometry) throws VizException {
        return new DispatchingMesh(wrappedObject.clone(targetGeometry),
                getDispatcher());
    }

}
