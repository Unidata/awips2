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

import java.util.ArrayList;

import org.opengis.referencing.crs.CoordinateReferenceSystem;

import com.raytheon.uf.common.dataplugin.gfe.reference.ReferenceData;
import com.raytheon.uf.common.dataplugin.gfe.reference.ReferenceData.CoordinateType;
import com.raytheon.uf.common.geospatial.MapUtil;
import com.raytheon.uf.common.geospatial.ReferencedGeometry;
import com.raytheon.uf.common.geospatial.ReferencedObject.Type;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.drawables.FillPatterns;
import com.raytheon.uf.viz.core.drawables.IShadedShape;
import com.raytheon.uf.viz.core.drawables.IWireframeShape;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.map.IMapDescriptor;
import com.raytheon.uf.viz.core.rsc.AbstractResourceData;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorableCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.OutlineCapability;
import com.raytheon.viz.core.rsc.jts.JTSCompiler;
import com.raytheon.viz.gfe.core.IReferenceSetManager;
import com.raytheon.viz.gfe.core.msgs.IReferenceSetChangedListener;
import com.raytheon.viz.gfe.core.msgs.Message;
import com.raytheon.viz.gfe.core.msgs.Message.IMessageClient;
import com.raytheon.viz.gfe.core.msgs.RefSetAppearanceChangedMsg;
import com.vividsolutions.jts.geom.Envelope;
import com.vividsolutions.jts.geom.MultiPolygon;

/**
 * Resource used to render the GFE edit area (reference set)
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date			Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * Mar 27, 2008		#1053	randerso	Initial creation
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */

public class GFEReferenceSetResource extends
        AbstractVizResource<AbstractResourceData, IMapDescriptor> implements
        IMessageClient, IReferenceSetChangedListener {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(GFEReferenceSetResource.class);

    private IReferenceSetManager refSetMgr;

    private boolean needsUpdate;

    private IWireframeShape outlineShape;

    private IShadedShape shadedShape;

    private byte[] fillPattern = FillPatterns.getGLPattern("SELECTED_AREA");

    public GFEReferenceSetResource(IReferenceSetManager refSetMgr) {
        super(new GFEResourceData(), new LoadProperties());
        this.refSetMgr = refSetMgr;
        this.needsUpdate = true;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.core.rsc.IVizResource#dispose()
     */
    @SuppressWarnings("unchecked")
    @Override
    public void disposeInternal() {
        this.refSetMgr.removeReferenceSetChangedListener(this);
        Message.unregisterInterest(this, RefSetAppearanceChangedMsg.class);
        disposeShapes();
    }

    /**
     * 
     */
    private void disposeShapes() {
        if (shadedShape != null) {
            shadedShape.dispose();
            shadedShape = null;
        }

        if (outlineShape != null) {
            outlineShape.dispose();
            outlineShape = null;
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.core.rsc.IVizResource#getName()
     */
    @Override
    public String getName() {
        return this.refSetMgr.getActiveRefSet().getId().getName()
                + " Edit Area";
    }

    /*
     * (non-Javadoc)
     * 
     * @seecom.raytheon.viz.core.rsc.IVizResource#init(com.raytheon.viz.core.
     * IGraphicsTarget)
     */
    @SuppressWarnings("unchecked")
    @Override
    public void initInternal(IGraphicsTarget target) {
        this.refSetMgr.addReferenceSetChangedListener(this);
        Message.registerInterest(this, RefSetAppearanceChangedMsg.class);
        receiveMessage(Message
                .inquireLastMessage(RefSetAppearanceChangedMsg.class));
        initRefSetData(target);
    }

    private void initRefSetData(IGraphicsTarget target) {
        disposeShapes();

        outlineShape = target.createWireframeShape(false, this.descriptor);

        shadedShape = target.createShadedShape(false, this.descriptor, true);

        JTSCompiler jtsCompiler = new JTSCompiler(shadedShape, outlineShape,
                this.descriptor);

        this.needsUpdate = false;
        ReferenceData refData = this.refSetMgr.evaluateActiveRefSet();

        refData.setGrid(refData.getGrid());

        MultiPolygon mp = (MultiPolygon) refData.getPolygons(
                CoordinateType.GRID).clone();

        ReferencedGeometry rc = new ReferencedGeometry(mp,
                MapUtil.getGridGeometry(refData.getGloc()), Type.GRID_CENTER);

        try {
            jtsCompiler.handle(rc, getCapability(ColorableCapability.class)
                    .getColor());
        } catch (VizException e) {
            statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(), e);
        }

        outlineShape.compile();
        shadedShape.compile();
        shadedShape.setFillPattern(fillPattern);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.core.drawables.IRenderable#paint(com.raytheon.viz.core
     * .IGraphicsTarget, com.raytheon.viz.core.drawables.PaintProperties)
     */
    @Override
    public void paintInternal(IGraphicsTarget aTarget,
            PaintProperties paintProps) throws VizException {
        if (this.needsUpdate) {
            initRefSetData(aTarget);
        }

        float alpha = paintProps.getAlpha();

        if (shadedShape != null && shadedShape.isDrawable()) {
            aTarget.drawShadedShape(shadedShape, alpha);
        }

        OutlineCapability outlineCapability = getCapability(OutlineCapability.class);
        if (outlineCapability.isOutlineOn() && outlineShape != null
                && outlineShape.isDrawable()) {
            aTarget.drawWireframeShape(outlineShape,
                    getCapability(ColorableCapability.class).getColor(),
                    outlineCapability.getOutlineWidth(),
                    outlineCapability.getLineStyle());
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.core.rsc.capabilities.IProjectableResource#project(org
     * .opengis.referencing.crs.CoordinateReferenceSystem)
     */
    @Override
    public void project(CoordinateReferenceSystem mapData) throws VizException {
        this.needsUpdate = true;
        issueRefresh();
    }

    @Override
    public void receiveMessage(Message message) {
        RefSetAppearanceChangedMsg msg = (RefSetAppearanceChangedMsg) message;
        getCapability(ColorableCapability.class).setColor(msg.getColor());
        getCapability(OutlineCapability.class).setOutlineWidth(
                msg.getLineWidth());
        this.needsUpdate = true;
        issueRefresh();
    }

    /*
     * (non-Javadoc)
     * 
     * @seecom.raytheon.viz.gfe.core.msgs.IReferenceSetChangedListener#
     * referenceSetChanged
     * (com.raytheon.uf.common.dataplugin.gfe.reference.ReferenceData,
     * java.util.ArrayList)
     */
    @Override
    public void referenceSetChanged(ReferenceData refSet,
            ArrayList<Envelope> domains) {
        this.needsUpdate = true;
        issueRefresh();
    }
}
