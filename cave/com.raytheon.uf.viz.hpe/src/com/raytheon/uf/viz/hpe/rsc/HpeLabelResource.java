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
package com.raytheon.uf.viz.hpe.rsc;

import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.swt.graphics.RGB;

import com.raytheon.uf.common.dataplugin.grid.GridRecord;
import com.raytheon.uf.common.plugin.hpe.request.HpeLabelDataRequest;
import com.raytheon.uf.common.plugin.hpe.request.HpeLabelDataResponse;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.DrawableString;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.IGraphicsTarget.HorizontalAlignment;
import com.raytheon.uf.viz.core.IGraphicsTarget.VerticalAlignment;
import com.raytheon.uf.viz.core.drawables.IFont;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.map.MapDescriptor;
import com.raytheon.uf.viz.core.requests.ThriftClient;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.IResourceDataChanged;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.ResourceList;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorableCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.MagnificationCapability;
import com.raytheon.uf.viz.hpe.util.HpeUtils;
import com.raytheon.viz.grid.rsc.general.D2DGridResource;

/**
 * A resource to display HPE label text in the upper left corner of the display
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 5, 2014    3026     mpduff      Initial creation
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class HpeLabelResource extends
        AbstractVizResource<HpeLabelResourceData, MapDescriptor> implements
        IResourceDataChanged {

    private final IUFStatusHandler logger = UFStatus
            .getHandler(HpeLabelResource.class);

    private final Map<Date, String> hpeTextCache = Collections
            .synchronizedMap(new HashMap<Date, String>());

    private DrawableString drawableString = null;

    private IFont font = null;

    private final HpeSourceDataJob dataJob = new HpeSourceDataJob();

    protected HpeLabelResource(HpeLabelResourceData resourceData,
            LoadProperties loadProperties) {
        super(resourceData, loadProperties);
    }

    @Override
    public void resourceChanged(ChangeType type, Object object) {
        if (type == ChangeType.DATA_REMOVE) {
            if (object instanceof DataTime) {
                hpeTextCache.remove(((DataTime) object).getRefTime());
            }
        }
    }

    @Override
    protected void disposeInternal() {
        if (font != null) {
            font.dispose();
        }
    }

    @Override
    protected void paintInternal(IGraphicsTarget target,
            PaintProperties paintProps) throws VizException {
        ResourceList rscList = this.descriptor.getResourceList();
        if (rscList != null) {
            StringBuilder sb = new StringBuilder();

            // Find all D2DGridResources
            List<D2DGridResource> list = rscList
                    .getResourcesByTypeAsType(D2DGridResource.class);
            if (!list.isEmpty()) {
                double[] pixel = paintProps.getView().getDisplayCoords(
                        new double[] { 125, 50 }, target);
                RGB color = getCapability(ColorableCapability.class).getColor();
                for (D2DGridResource rsc : list) {
                    GridRecord currentGridRec = rsc.getCurrentGridRecord();
                    color = rsc.getCapability(ColorableCapability.class)
                            .getColor();
                    if (HpeUtils.isHpe(currentGridRec)) {
                        // this is HPE so display the bias information
                        String text = getText(currentGridRec.getDataTime()
                                .getRefTime(), currentGridRec.getSecondaryId());
                        if (text != null) {
                            sb.append(text);
                        }
                    }
                }

                if (sb.length() > 0) {
                    drawableString.setText(sb.toString(), color);
                    drawableString.setCoordinates(pixel[0], pixel[1]);
                    target.drawStrings(drawableString);
                }
            }
        }
    }

    @Override
    protected void initInternal(IGraphicsTarget target) throws VizException {
        if (font == null) {
            font = target.initializeFont("Dialog", 11, null);
        }
        font.setMagnification(getCapability(MagnificationCapability.class)
                .getMagnification().floatValue());

        drawableString = new DrawableString("", getCapability(
                ColorableCapability.class).getColor());
        drawableString.font = font;
        drawableString.horizontalAlignment = HorizontalAlignment.CENTER;
        drawableString.verticallAlignment = VerticalAlignment.MIDDLE;
    }

    private String getText(Date date, String productId) {
        String text = hpeTextCache.get(date);
        if (text == null) {
            dataJob.scheduleRetrieval(date, productId);
        }

        return text;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.rsc.AbstractVizResource#resourceDataChanged(
     * com.raytheon.uf.viz.core.rsc.IResourceDataChanged.ChangeType,
     * java.lang.Object)
     */
    @Override
    protected void resourceDataChanged(ChangeType type, Object updateObject) {
        super.resourceDataChanged(type, updateObject);
    }

    private class HpeSourceDataJob extends Job {
        private volatile String productId;

        private volatile Date date;

        public HpeSourceDataJob() {
            super("Get HPE Source");
        }

        protected void scheduleRetrieval(Date date, String productId) {
            this.productId = productId;
            this.date = date;
            if (this.getState() == Job.RUNNING
                    || this.getState() == Job.SLEEPING
                    || this.getState() == Job.WAITING) {
                return;
            }
            this.schedule();
        }

        @Override
        protected IStatus run(IProgressMonitor monitor) {
            // Request the text from edex
            try {
                HpeLabelDataRequest req = new HpeLabelDataRequest(productId,
                        date);
                HpeLabelDataResponse response = (HpeLabelDataResponse) ThriftClient
                        .sendRequest(req);
                Map<Date, String> data = response.getData();
                for (Date d : data.keySet()) {
                    hpeTextCache.put(d, data.get(d));
                }
            } catch (VizException e) {
                statusHandler.error(e.getLocalizedMessage(), e);
            }

            return Status.OK_STATUS;
        }
    }
}
