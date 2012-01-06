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

package com.raytheon.viz.core.topo;

import java.text.DecimalFormat;

import javax.measure.converter.UnitConverter;
import javax.measure.quantity.Length;
import javax.measure.unit.NonSI;
import javax.measure.unit.SI;
import javax.measure.unit.Unit;
import javax.measure.unit.UnitFormat;

import org.apache.commons.lang.Validate;
import org.opengis.referencing.crs.CoordinateReferenceSystem;

import com.raytheon.uf.common.geospatial.ReferencedCoordinate;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.drawables.ColorMapParameters;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.map.IMapDescriptor;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorMapCapability;
import com.raytheon.viz.core.drawables.ColorMapParameterFactory;

/**
 * Provides an SRTM hdf5-backed topographic map
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * Feb 14, 2007             chammack    Initial Creation.
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1
 */
public class TopoResource extends
        AbstractVizResource<TopoResourceData, IMapDescriptor> {

    private TopoTileSet topoTileSet;

    protected TopoResource(TopoResourceData topoData,
            LoadProperties loadProperties) throws VizException {
        super(topoData, loadProperties);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.rsc.AbstractVizResource#dispose()
     */
    @Override
    protected void disposeInternal() {
        if (topoTileSet != null) {
            topoTileSet.dispose();
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.rsc.AbstractVizResource#init(com.raytheon.uf
     * .viz.core.IGraphicsTarget)
     */
    @Override
    protected void initInternal(IGraphicsTarget target) throws VizException {
        Unit<Length> dataUnit = SI.METER;

        // TODO: create topo style rules for topo and bathymetric topo

        ColorMapParameters parameters = ColorMapParameterFactory.build(
                (Object) null, "topo", dataUnit, null);
        parameters.setDataMin(Short.MIN_VALUE);
        parameters.setDataMax(Short.MAX_VALUE);

        parameters.setColorMapMin(-19);
        parameters.setColorMapMax(5000);

        String colorMapName = parameters.getColorMapName();
        if (colorMapName == null) {
            colorMapName = "topo";
        }
        parameters.setColorMap(target.buildColorMap(colorMapName));

        if (parameters.getDisplayUnit() == null) {
            parameters.setDisplayUnit(NonSI.FOOT);
        }
        parameters.setFormatString("0");

        getCapability(ColorMapCapability.class).setColorMapParameters(
                parameters);

        topoTileSet = new TopoTileSet(this, target.getViewType());
        topoTileSet.setMapDescriptor(descriptor);
        topoTileSet.init(target);

    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.drawables.IRenderable#paint(com.raytheon.uf.
     * viz.core.IGraphicsTarget,
     * com.raytheon.uf.viz.core.drawables.PaintProperties)
     */
    @Override
    protected void paintInternal(IGraphicsTarget target,
            PaintProperties paintProps) throws VizException {

        if (topoTileSet != null) {
            topoTileSet.paint(target, paintProps);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.rsc.AbstractVizResource#setDescriptor(com.raytheon
     * .uf.viz.core.drawables.IDescriptor)
     */
    @Override
    public void setDescriptor(IMapDescriptor descriptor) {
        super.setDescriptor(descriptor);
        Validate.isTrue(descriptor instanceof IMapDescriptor,
                "Resource expects map descriptor");

        if (topoTileSet != null) {
            topoTileSet.setMapDescriptor(this.descriptor);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.rsc.AbstractVizResource#project(org.opengis.
     * referencing.crs.CoordinateReferenceSystem)
     */
    @Override
    public void project(CoordinateReferenceSystem mapData) throws VizException {
        if (topoTileSet != null) {
            topoTileSet.reproject();
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.rsc.AbstractVizResource#inspect(com.raytheon
     * .uf.viz.core.geospatial.ReferencedCoordinate)
     */
    @Override
    public String inspect(ReferencedCoordinate coord) throws VizException {
        double height;
        try {
            // height = TopoQuery.getInstance().getHeight(coord.asLatLon());
            height = topoTileSet.interrogate(coord.asLatLon(), true);
        } catch (Exception e) {
            throw new VizException("Error transforming", e);
        }
        if (!Double.isNaN(height)) {
            ColorMapParameters parameters = getCapability(
                    ColorMapCapability.class).getColorMapParameters();
            UnitConverter cvt = parameters.getDataToDisplayConverter();

            DecimalFormat df = new DecimalFormat("0.00");
            return String.format(
                    "%s %s ",
                    df.format(cvt.convert(height)),
                    UnitFormat.getUCUMInstance().format(
                            parameters.getDisplayUnit()));
        }
        return "NO DATA";
    }

}
