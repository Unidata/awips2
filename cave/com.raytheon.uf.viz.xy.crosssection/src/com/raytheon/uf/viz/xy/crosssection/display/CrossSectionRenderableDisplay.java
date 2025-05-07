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
package com.raytheon.uf.viz.xy.crosssection.display;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.locationtech.jts.geom.LineString;

import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.PixelExtent;
import com.raytheon.uf.viz.core.drawables.IRenderableDisplay;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.ResourceList;
import com.raytheon.uf.viz.core.rsc.ResourceProperties;
import com.raytheon.uf.viz.d2d.core.ImageCombiner;
import com.raytheon.uf.viz.d2d.ui.AbstractHeightDisplay;
import com.raytheon.uf.viz.xy.crosssection.rsc.AbstractCrossSectionResource;
import com.raytheon.uf.viz.xy.map.rsc.GraphResourceData;
import com.raytheon.uf.viz.xy.map.rsc.GraphResourceData.OverlayMode;
import com.raytheon.uf.viz.xy.scales.HeightScale;
import com.raytheon.uf.viz.xy.scales.HeightScales;
import com.raytheon.viz.awipstools.IBaselineChangedListener;
import com.raytheon.viz.awipstools.ToolsDataManager;

/**
 * {@link IRenderableDisplay} for {@link AbstractCrossSectionResource}s.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- ----------------------------------
 * Jun 28, 2010           bsteffen  Initial creation
 * Nov 10, 2016  5976     bsteffen  Move HeightScales and add javadoc
 * May 24, 2021  8452     randerso  Added capability to update when the
 *                                  associated baseline is updated.
 * Dec 20, 2023  2036519  mapeters  Don't construct the graph resource in
 *                                  customizeResourceList()
 * May 24, 2024  2037092  mapeters  Redo time matching in updateBaseline
 *
 * </pre>
 *
 * @author bsteffen
 */
@XmlAccessorType(XmlAccessType.NONE)
@XmlRootElement
public class CrossSectionRenderableDisplay extends AbstractHeightDisplay
        implements IBaselineChangedListener {

    /**
     * Delay time to wait for additional baseline updates before re-displaying.
     * Setting this value < 0 will disable automatic updates based on baseline
     * changes.
     */
    private static final long delayTime = Long
            .getLong("cross.section.baseline.change.delay", 7_000L);

    private class DelayJob extends Job {
        private String baselineName;

        private LineString baseline;

        private Long timeToRun;

        public DelayJob() {
            super("Awaiting further baseline changes");
        }

        public void startDelay(String name, LineString baseline) {
            if (delayTime < 0) {
                return;
            }

            this.cancel();
            this.baselineName = name;
            this.baseline = baseline;
            this.timeToRun = System.currentTimeMillis() + delayTime;
            this.schedule();
        }

        @Override
        protected IStatus run(IProgressMonitor monitor) {
            while (System.currentTimeMillis() < timeToRun) {
                try {
                    Thread.sleep(timeToRun - System.currentTimeMillis());
                } catch (InterruptedException e) {
                    // do nothing
                }
            }

            updateBaseline(baselineName, baseline);
            return Status.OK_STATUS;
        }

    }

    private DelayJob delayJob = new DelayJob();

    public CrossSectionRenderableDisplay() {
        this(new PixelExtent(0, 1000, 0, 1000));
    }

    public CrossSectionRenderableDisplay(PixelExtent aPixelExtent) {
        super(aPixelExtent, new CrossSectionDescriptor(aPixelExtent));
    }

    @Override
    public String getScale() {
        if (getDescriptor() != null
                && getDescriptor().getHeightScale() != null) {
            return getDescriptor().getHeightScale().getName();
        }
        return null;
    }

    @Override
    public void setScale(String scale) {
        setHeightScale(HeightScales.fromName(scale));
    }

    @Override
    public void setHeightScale(HeightScale scale) {
        getDescriptor().setHeightScale(scale);

    }

    @Override
    public CrossSectionDescriptor getDescriptor() {
        return (CrossSectionDescriptor) super.getDescriptor();
    }

    @Override
    protected void customizeResourceList(ResourceList resourceList) {
        super.customizeResourceList(resourceList);

        // Add cross section graph resource
        GraphResourceData grd = new GraphResourceData(
                "Cross Section Background");
        LoadProperties lprops = new LoadProperties();
        ResourceProperties rprops = new ResourceProperties();
        rprops.setMapLayer(true);
        grd.setOverlayMode(OverlayMode.OVERLAY);
        ResourcePair rp = new ResourcePair();
        rp.setResourceData(grd);
        rp.setProperties(rprops);
        rp.setLoadProperties(lprops);
        resourceList.add(rp);
        resourceList.addPostAddListener(new ImageCombiner(getDescriptor()));
    }

    @Override
    public void setup(IGraphicsTarget target) {
        super.setup(target);
        ToolsDataManager.getInstance().addBaselinesChangedListener(this);
    }

    @Override
    public void dispose() {
        super.dispose();
        ToolsDataManager.getInstance().removeBaselinesChangedListener(this);
    }

    @Override
    public void baselineChanged(String name, LineString baseline) {
        if (name.equals(getDescriptor().getBaseLine())) {
            delayJob.startDelay(name, baseline);
        }
    }

    /**
     * @param name
     *            baseline name
     * @param baseline
     *            baseline LineString, if null will be retrieved from
     *            ToolsDataManager
     */
    public void updateBaseline(String name, LineString baseline) {
        CrossSectionDescriptor csd = getDescriptor();
        csd.setBaseLine(name);
        if (baseline == null) {
            baseline = ToolsDataManager.getInstance().getBaseline(name);
        }
        csd.setBaseLineString(baseline);
        setDescriptor(csd);
        for (ResourcePair pair : csd.getResourceList()) {
            AbstractVizResource<?, ?> rsc = pair.getResource();
            if (rsc instanceof AbstractCrossSectionResource) {
                /*
                 * Tell the time matcher to update this resource's time info
                 * whenever time matching is re-done (cached times will have
                 * wrong levelType set on them)
                 */
                csd.getTimeMatcher().redoTimeMatching(rsc);
                ((AbstractCrossSectionResource) rsc).setDescriptor(csd);
            }
        }

        try {
            // Actually re-do time matching
            csd.redoTimeMatching();
        } catch (VizException e) {
            statusHandler.error("Error updating cross section baseline", e);
        }

    }

}
