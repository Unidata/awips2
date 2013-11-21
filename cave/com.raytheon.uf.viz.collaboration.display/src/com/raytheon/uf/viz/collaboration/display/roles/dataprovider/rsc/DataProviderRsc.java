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

import org.eclipse.swt.graphics.RGB;
import org.opengis.referencing.crs.CoordinateReferenceSystem;

import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.collaboration.comm.identity.CollaborationException;
import com.raytheon.uf.viz.collaboration.comm.identity.ISharedDisplaySession;
import com.raytheon.uf.viz.collaboration.comm.identity.info.IVenueInfo;
import com.raytheon.uf.viz.collaboration.display.Activator;
import com.raytheon.uf.viz.collaboration.display.data.SessionColorManager;
import com.raytheon.uf.viz.collaboration.display.data.SessionContainer;
import com.raytheon.uf.viz.collaboration.display.data.SharedDisplaySessionMgr;
import com.raytheon.uf.viz.collaboration.display.editor.ReprojectRemoteDisplay;
import com.raytheon.uf.viz.core.DrawableString;
import com.raytheon.uf.viz.core.IExtent;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.IGraphicsTarget.HorizontalAlignment;
import com.raytheon.uf.viz.core.IGraphicsTarget.TextStyle;
import com.raytheon.uf.viz.core.IGraphicsTarget.VerticalAlignment;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.drawables.IFont;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.remote.graphics.DispatchGraphicsTarget;

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

    private SessionColorManager colorManager;

    private IFont font;

    public DataProviderRsc(DataProviderRscData resourceData,
            LoadProperties loadProperties) {
        super(resourceData, loadProperties);
        SessionContainer container = SharedDisplaySessionMgr
                .getSessionContainer(resourceData.getSessionId());
        if (container != null) {
            session = container.getSession();
            colorManager = container.getColorManager();
            IVenueInfo info;
            try {
                info = session.getVenue().getInfo();
                roomName = info.getVenueDescription();
                subject = info.getVenueSubject();
            } catch (CollaborationException e) {
                statusHandler.error(e.getLocalizedMessage(), e);
                roomName = session.getVenue().getName();
            }

        }
    }

    @Override
    protected void disposeInternal() {
        font.dispose();
    }

    @Override
    protected void paintInternal(IGraphicsTarget target,
            PaintProperties paintProps) throws VizException {
        if (target instanceof DispatchGraphicsTarget) {
            target = ((DispatchGraphicsTarget) target).getWrappedObject();
        }
        target.clearClippingPlane();
        IExtent extent = paintProps.getView().getExtent();
        RGB color = colorManager.getColorFromUser(session.getUserID());
        target.drawRect(extent, color, 3.0f, 1.0f);

        DrawableString string = new DrawableString(getName(), color);
        string.horizontalAlignment = HorizontalAlignment.CENTER;
        string.verticallAlignment = VerticalAlignment.BOTTOM;
        string.setCoordinates(extent.getMinX() + extent.getWidth() / 2,
                extent.getMaxY());
        string.font = font;
        string.textStyle = TextStyle.BLANKED;
        target.drawStrings(string);

        target.setupClippingPlane(paintProps.getClippingPane());
    }

    @Override
    protected void initInternal(IGraphicsTarget target) throws VizException {
        if (target instanceof DispatchGraphicsTarget) {
            target = ((DispatchGraphicsTarget) target).getWrappedObject();
        }
        font = target.getDefaultFont().deriveWithSize(11.0f);
        font.setScaleFont(true);
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
        if (subject.isEmpty() == false) {
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
