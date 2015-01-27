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
package gov.noaa.nws.ncep.viz.rsc.timeseries;

import gov.noaa.nws.ncep.viz.common.display.INatlCntrsDescriptor;
import gov.noaa.nws.ncep.viz.resources.time_match.NCTimeMatcher;
import gov.noaa.nws.ncep.viz.ui.display.NCTimeSeriesDescriptor;
import gov.noaa.nws.ncep.viz.ui.display.NCTimeSeriesRenderableDisplay;
import gov.noaa.nws.ncep.viz.ui.display.NcDisplayMngr;

import java.util.List;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.IDisplayPane;
import com.raytheon.uf.viz.core.PixelExtent;
import com.raytheon.uf.viz.core.drawables.AbstractDescriptor;
import com.raytheon.uf.viz.core.drawables.IRenderableDisplay;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.xy.graph.IGraph;
import com.raytheon.viz.ui.editor.AbstractEditor;

/**
 * Geomag descriptor, needed so loading bundles know what editor to load with
 * this descriptor.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer   Description
 * ------------ ---------- ---------- --------------------------
 * 06/13/2014   #1136      qzhou      Initial creation
 *                                    Added functions to set it up.
 * 07/28/2014   R4079      sgurung    Added new constructor
 * </pre>
 * 
 * @author qzhou
 * @version 1.0
 */
@XmlAccessorType(XmlAccessType.NONE)
public class GeoMagDescriptor extends NCTimeSeriesDescriptor implements
        INatlCntrsDescriptor {

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(AbstractDescriptor.class);

    public GeoMagDescriptor() {
        super();
    }

    public GeoMagDescriptor(PixelExtent pixelExtent) {
        super(pixelExtent);
    }

    public GeoMagDescriptor(NCTimeSeriesDescriptor desc) throws VizException {
        super();

        List<ResourcePair> rlist = desc.getResourceList();
        if (rlist != null) {
            ResourcePair[] rp = rlist.toArray(new ResourcePair[rlist.size()]);
            this.setSerializableResources(rp);
        }

        IRenderableDisplay rendDisp = desc.getRenderableDisplay();
        if (!(rendDisp instanceof NCTimeSeriesRenderableDisplay)) {
            throw new VizException("Error: Renderable display is not of type "
                    + rendDisp.getClass().getName());
        }

        rendDisp.setDescriptor(desc);
        this.setRenderableDisplay(rendDisp);

        NCTimeMatcher tm = (NCTimeMatcher) desc.getTimeMatcher();

        if (tm == null) {
            tm = (NCTimeMatcher) desc.getRenderableDisplay().getDescriptor()
                    .getTimeMatcher();
        }

        desc.setTimeMatcher(tm);

        this.setAutoUpdate(true);
        // this.setDataTimes(desc.getDataTimes());
        this.setFramesInfo(desc.getFramesInfo());
        this.setNumberOfFrames(desc.getNumberOfFrames());
        this.setGridGeometry(desc.getGridGeometry());

    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.xy.graph.XyGraphDescriptor#constructGraph()
     */
    @Override
    public IGraph constructGraph() {
        return new GeoMagGraph(this);
    }

    public static IDisplayPane[] getDisplayPane() {
        // get the pane of the selected resource.
        AbstractEditor editor = NcDisplayMngr.getActiveNatlCntrsEditor();

        return (IDisplayPane[]) editor.getDisplayPanes();

    }

    public void addDescriptor(GeoMagDescriptor desc, IDisplayPane pane) {

        NCTimeSeriesRenderableDisplay rendDisp = (NCTimeSeriesRenderableDisplay) pane
                .getRenderableDisplay();

        if (!(rendDisp instanceof NCTimeSeriesRenderableDisplay)) {
            try {
                throw new VizException(
                        "Error: can't zoom to resource in the renderable display : "
                                + rendDisp.getClass().getName());
            } catch (VizException e) {
                statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(),
                        e);
            }
        }

        rendDisp.setDescriptor(desc);

    }

    public void addDescriptor(NCTimeSeriesDescriptor desc,
            IRenderableDisplay rendDisp) {

        if (!(rendDisp instanceof NCTimeSeriesRenderableDisplay)) {
            try {
                throw new VizException(
                        "Error: can't zoom to resource in the renderable display : "
                                + rendDisp.getClass().getName());
            } catch (VizException e) {
                statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(),
                        e);
            }
        }

        rendDisp.setDescriptor(desc);

    }

    public void setResourcePair(GeoMagDescriptor desc, IDisplayPane pane) {

        List<ResourcePair> rlist = pane.getRenderableDisplay().getDescriptor()
                .getResourceList();

        if (rlist != null) {

            ResourcePair[] rp = rlist.toArray(new ResourcePair[rlist.size()]);
            desc.setSerializableResources(rp);
        }

    }

    public void setResourcePair(List<ResourcePair> rlist) {

        if (rlist != null) {

            ResourcePair[] rp = rlist.toArray(new ResourcePair[rlist.size()]);
            this.setSerializableResources(rp);
        }

    }

    public void setNCTimeMatcher(GeoMagDescriptor desc, IDisplayPane pane) {

        NCTimeMatcher tm = (NCTimeMatcher) pane.getDescriptor()
                .getTimeMatcher();

        desc.setTimeMatcher(tm);
    }

    public void setNCTimeMatcher(NCTimeMatcher tm) {
        this.setTimeMatcher(tm);
    }

}
