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
package com.raytheon.viz.gfe.core;

import java.util.Date;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.map.MapDescriptor;
import com.raytheon.uf.viz.core.maps.display.PlainMapRenderableDisplay;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.viz.gfe.core.msgs.ISpatialEditorTimeChangedListener;
import com.raytheon.viz.gfe.core.msgs.Message;
import com.raytheon.viz.gfe.core.msgs.Message.IMessageClient;
import com.raytheon.viz.gfe.core.msgs.ShowQuickViewDataMsg;
import com.raytheon.viz.gfe.core.parm.ParmDisplayAttributes.VisMode;
import com.raytheon.viz.gfe.edittool.GridID;
import com.raytheon.viz.gfe.rsc.GFEPaintProperties;
import com.raytheon.viz.gfe.rsc.GFEResource;

/**
 * Performs map display duties for GFE.
 * 
 * Primarily installs the gfe time matcher
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 19, 2009            chammack     Initial creation
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1.0
 */
@XmlAccessorType(XmlAccessType.NONE)
@XmlRootElement
public class GFEMapRenderableDisplay extends PlainMapRenderableDisplay
        implements IMessageClient, ISpatialEditorTimeChangedListener {

    private GFEResource qvRsc;

    private DataTime qvTime;

    private DataManager dataMgr;

    public GFEMapRenderableDisplay() {
    }

    public GFEMapRenderableDisplay(MapDescriptor desc) {
        super(desc);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.drawables.AbstractRenderableDisplay#setDescriptor
     * (com.raytheon.uf.viz.core.drawables.IDescriptor)
     */
    @Override
    public void setDescriptor(IDescriptor desc) {
        super.setDescriptor(desc);
        if (desc.getTimeMatcher() == null) {
            desc.setTimeMatcher(new GFETimeMatcher());
        }
    }

    @SuppressWarnings("unchecked")
    public void setDataManager(DataManager dataManager) {
        if (this.dataMgr != null) {
            dataMgr.getSpatialDisplayManager()
                    .removeSpatialEditorTimeChangedListener(this);
        }
        dataMgr = dataManager;
        dataMgr.getSpatialDisplayManager().addSpatialEditorTimeChangedListener(
                this);
        Message.registerInterest(this, ShowQuickViewDataMsg.class);
    }

    @SuppressWarnings("unchecked")
    @Override
    public void dispose() {
        Message.unregisterInterest(this, ShowQuickViewDataMsg.class);
        if (dataMgr != null) {
            dataMgr.getSpatialDisplayManager()
                    .removeSpatialEditorTimeChangedListener(this);
        }
        super.dispose();
    }

    @Override
    protected PaintProperties calcPaintDataTime(PaintProperties paintProps,
            AbstractVizResource<?, ?> rsc) {
        if (dataMgr != null) {
            // Get time for resource from FramesInfo
            paintProps.setDataTime(paintProps.getFramesInfo()
                    .getTimeForResource(rsc));
        }

        GFEPaintProperties gfeProps = new GFEPaintProperties(paintProps);
        if (qvTime != null) {
            gfeProps.setDataTime(qvTime);
            gfeProps.setVisMode(VisMode.IMAGE);
        } else {
            if (rsc instanceof GFEResource) {
                gfeProps.setVisMode(((GFEResource) rsc).getParm()
                        .getDisplayAttributes().getVisMode());
            }
        }
        return gfeProps;
    }

    @Override
    protected boolean shouldDisplay(ResourcePair pair, int displayWidth) {
        boolean display = super.shouldDisplay(pair, displayWidth);
        AbstractVizResource<?, ?> rsc = pair.getResource();
        if (rsc instanceof GFEResource) {
            if (qvRsc != null) {
                display = qvRsc.equals(rsc);
            }
        }
        return display;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.gfe.core.msgs.Message.IMessageClient#receiveMessage(
     * com.raytheon.viz.gfe.core.msgs.Message)
     */
    @Override
    public void receiveMessage(Message message) {
        if (message instanceof ShowQuickViewDataMsg) {
            GridID gridId = ((ShowQuickViewDataMsg) message).getGridId();
            if (gridId != null) {
                ISpatialDisplayManager sdm = gridId.getParm().getDataManager()
                        .getSpatialDisplayManager();
                ResourcePair pair = sdm.getResourcePair(gridId.getParm());
                qvRsc = (GFEResource) pair.getResource();
                qvRsc.issueRefresh();
                qvTime = new DataTime(gridId.getDate());
            } else {
                qvRsc.issueRefresh();
                qvRsc = null;
                qvTime = null;
            }
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.gfe.core.msgs.ISpatialEditorTimeChangedListener#
     * spatialEditorTimeChanged(java.util.Date)
     */
    @Override
    public void spatialEditorTimeChanged(Date date) {
        this.refresh();
    }
}
