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
package com.raytheon.uf.viz.d2d.ui.ncephydro.pdc;

import java.util.List;
import java.util.Map;

import org.eclipse.swt.graphics.RGB;

import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.drawables.IImage;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorMapCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorableCapability;
import com.raytheon.uf.viz.pdc.PointDataControlManager;
import com.raytheon.viz.hydrocommon.HydroDisplayManager;
import com.raytheon.viz.hydrocommon.data.GageData;
import com.raytheon.viz.hydrocommon.resource.AbstractMultiPointResource;
import com.raytheon.viz.hydrocommon.resource.AbstractMultiPointResourceData;

/**
 * Resource to display the Hydro gages in D2D
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 2, 2018   7379      mpduff      Initial creation
 *
 * </pre>
 *
 * @author mpduff
 */

public class D2DMultiPointResource extends AbstractMultiPointResource {
    private HydroDisplayManager manager = HydroDisplayManager.getInstance();

    public D2DMultiPointResource(AbstractMultiPointResourceData resourceData,
            LoadProperties loadProperties) {
        super(resourceData, loadProperties);
        // Hide the change color and colormap menu items
        getCapability(ColorMapCapability.class).setSuppressingMenuItems(true);
        getCapability(ColorableCapability.class).setSuppressingMenuItems(true);

    }

    @Override
    public void unmap() {
        /*
         * No resources implemented that need to be removed. The
         * D2DMultiPointResource is already removed. New resources that will
         * need to be removed should be removed here.
         */
    }

    @Override
    protected void disposeInternal() {
        for (Map<RGB, IImage> colorMap : imageMap.values()) {
            for (IImage image : colorMap.values()) {
                image.dispose();
            }
            colorMap.clear();
        }
        imageMap.clear();
        font.dispose();
        resetDataMap();

        manager.setDrawStation(false);
        PointDataControlManager.getInstance().cancelRunningJobs();
    }

    @Override
    protected void paintInternal(IGraphicsTarget target,
            PaintProperties paintProps) throws VizException {
        setScaleValues(paintProps);

        // Check the font size
        font.setMagnification((manager.getFontSize() / (float) fontSize), true);
        resetDataMap();
        paintInternalHelper(target, paintProps);
    }

    @Override
    protected void initInternal(IGraphicsTarget target) throws VizException {
        fontSize = 9;
        font = target.initializeFont("Dialog", fontSize, null);
        font.setSmoothing(false);

        List<GageData> data = manager.getObsReportList();

        if (data != null) {
            resetDataMap();
            for (GageData gage : data) {
                /* Get the point color for this location */
                if ((gage.getLid() != null) && gage.isUse()) {
                    addPoint(gage);
                }
            }
        }
    }

    @Override
    public String getName() {
        return ((D2DMultiPointResourceData) resourceData).getName();
    }
}
