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
package com.raytheon.uf.viz.collaboration.display.roles.dataprovider.rsc;

import org.opengis.referencing.crs.CoordinateReferenceSystem;

import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.collaboration.comm.identity.CollaborationException;
import com.raytheon.uf.viz.collaboration.comm.identity.ISharedDisplaySession;
import com.raytheon.uf.viz.collaboration.comm.identity.info.IVenueInfo;
import com.raytheon.uf.viz.collaboration.display.Activator;
import com.raytheon.uf.viz.collaboration.display.data.SessionContainer;
import com.raytheon.uf.viz.collaboration.display.data.SharedDisplaySessionMgr;
import com.raytheon.uf.viz.collaboration.display.editor.ReprojectRemoteDisplay;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.LoadProperties;

/**
 * A resource that is added to an editor that the Data Provider is sharing. It
 * captures some events and also displays to the Data Provider that the editor
 * is currently shared.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 13, 2012            njensen     Initial creation
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class DataProviderRsc extends
        AbstractVizResource<DataProviderRscData, IDescriptor> {

    private String roomName;

    private String subject;

    private ISharedDisplaySession session;

    public DataProviderRsc(DataProviderRscData resourceData,
            LoadProperties loadProperties) {
        super(resourceData, loadProperties);
        SessionContainer container = SharedDisplaySessionMgr
                .getSessionContainer(resourceData.getSessionId());
        if (container != null) {
            session = container.getSession();
            IVenueInfo info = session.getVenue().getInfo();
            roomName = info.getVenueDescription();
            subject = info.getVenueDescription();
        }
    }

    @Override
    protected void disposeInternal() {

    }

    @Override
    protected void paintInternal(IGraphicsTarget target,
            PaintProperties paintProps) throws VizException {

    }

    @Override
    protected void initInternal(IGraphicsTarget target) throws VizException {

    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.rsc.AbstractVizResource#project(org.opengis.
     * referencing.crs.CoordinateReferenceSystem)
     */
    @Override
    public void project(CoordinateReferenceSystem crs) throws VizException {
        ReprojectRemoteDisplay event = new ReprojectRemoteDisplay();
        event.setTargetGeometry(descriptor.getGridGeometry());
        event.setDisplayId(resourceData.getDisplayId());
        try {
            session.sendObjectToVenue(event);
        } catch (CollaborationException e) {
            Activator.statusHandler.handle(
                    Priority.PROBLEM,
                    "Error sending reprojection event: "
                            + e.getLocalizedMessage(), e);
        }
    }

    public String getName() {
        String text = "Sharing with " + roomName;
        if (!"".equals(subject)) {
            text += " (" + subject + ")";
        }
        return text;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.rsc.AbstractVizResource#okToUnload()
     */
    @Override
    public boolean okToUnload() {
        // Though I hate this methods exists, it serves its purpose
        return false;
    }

}
