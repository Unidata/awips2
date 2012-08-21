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
package com.raytheon.viz.grid.rsc.general;

import java.nio.FloatBuffer;
import java.util.List;

import javax.measure.Measure;
import javax.measure.unit.Unit;

import org.geotools.coverage.grid.GeneralGridEnvelope;
import org.geotools.coverage.grid.GridGeometry2D;
import org.geotools.geometry.jts.ReferencedEnvelope;
import org.opengis.coverage.grid.GridEnvelope;
import org.opengis.referencing.FactoryException;
import org.opengis.referencing.operation.TransformException;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.geospatial.ReferencedCoordinate;
import com.raytheon.uf.common.geospatial.interpolation.BilinearInterpolation;
import com.raytheon.uf.common.geospatial.interpolation.GridReprojection;
import com.raytheon.uf.common.geospatial.interpolation.Interpolation;
import com.raytheon.uf.common.geospatial.interpolation.data.DataSource;
import com.raytheon.uf.common.geospatial.interpolation.data.FloatArrayWrapper;
import com.raytheon.uf.common.geospatial.interpolation.data.FloatBufferWrapper;
import com.raytheon.uf.common.time.CombinedDataTime;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.IResourceGroup;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.ResourceList;
import com.raytheon.uf.viz.core.rsc.capabilities.GroupNamingCapability;
import com.raytheon.uf.viz.core.style.ParamLevelMatchCriteria;
import com.raytheon.viz.core.rsc.ICombinedResourceData.CombineOperation;
import com.raytheon.viz.core.rsc.ICombinedResourceData.CombineUtil;

/**
 * 
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 16, 2011            bsteffen     Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public class DifferenceGridResource extends
        AbstractGridResource<DifferenceGridResourceData> implements
        IResourceGroup {

    // Defines a constant size for the difference grid
    private static final GridEnvelope GRID_ENVELOPE = new GeneralGridEnvelope(
            new int[] { 0, 0 }, new int[] { 512, 512 });

    private final AbstractGridResource<?> one;

    private final AbstractGridResource<?> two;

    private GridReprojection oneInterpolation;

    private GridReprojection twoInterpolation;

    public DifferenceGridResource(DifferenceGridResourceData resourceData,
            LoadProperties loadProperties, AbstractGridResource<?> one,
            AbstractGridResource<?> two) {
        super(resourceData, loadProperties);
        this.one = one;
        this.two = two;
        this.dataTimes = TIME_AGNOSTIC;
    }

    @Override
    protected void initInternal(IGraphicsTarget target) throws VizException {
        one.init(target);
        two.init(target);
        super.initInternal(target);
    }

    @Override
    protected void initCapabilities() {
        getCapability(GroupNamingCapability.class);
        super.initCapabilities();
    }

    @Override
    protected void paintInternal(IGraphicsTarget target,
            PaintProperties paintProps) throws VizException {
        paintProps.setDataTime(null);
        super.paintInternal(target, paintProps);
    }

    @Override
    public ParamLevelMatchCriteria getMatchCriteria() {
        return one.getMatchCriteria();
    }

    @Override
    public GridGeometry2D getGridGeometry() {
        if (oneInterpolation == null) {
            GridGeometry2D oneGeometry = one.getGridGeometry();
            GridGeometry2D twoGeometry = two.getGridGeometry();
            GridGeometry2D newGeometry = null;
            if (oneGeometry.equals(twoGeometry)) {
                newGeometry = oneGeometry;
            } else {
                ReferencedEnvelope oneEnv = new ReferencedEnvelope(
                        oneGeometry.getEnvelope());
                ReferencedEnvelope twoEnv = new ReferencedEnvelope(
                        oneGeometry.getEnvelope());
                if (!oneEnv.getCoordinateReferenceSystem().equals(
                        twoEnv.getCoordinateReferenceSystem())) {
                    // If they aren't the same crs just go to screen space.
                    try {
                        oneEnv = oneEnv.transform(descriptor.getCRS(), true);
                        twoEnv = twoEnv.transform(descriptor.getCRS(), true);
                    } catch (TransformException e) {
                        throw new RuntimeException(e);
                    } catch (FactoryException e) {
                        throw new RuntimeException(e);
                    }
                }
                ReferencedEnvelope newEnv = new ReferencedEnvelope(
                        oneEnv.intersection(twoEnv), descriptor.getCRS());
                newGeometry = new GridGeometry2D(GRID_ENVELOPE, newEnv);
            }
            oneInterpolation = new GridReprojection(oneGeometry, newGeometry);
            twoInterpolation = new GridReprojection(twoGeometry, newGeometry);
        }
        return GridGeometry2D.wrap(oneInterpolation.getTargetGeometry());
    }

    @Override
    public GeneralGridData getData(DataTime time, List<PluginDataObject> pdos)
            throws VizException {
        if (!(time instanceof CombinedDataTime)) {
            throw new VizException("Unexpected single time in diff resource");
        }
        CombinedDataTime cTime = (CombinedDataTime) time;
        DataTime oneTime = cTime.getPrimaryDataTime();
        DataTime twoTime = cTime.getAdditionalDataTime();
        if (oneInterpolation == null || twoInterpolation == null) {
            getGridGeometry();
        }
        GeneralGridData oneData = one.requestData(oneTime);
        GeneralGridData twoData = two.requestData(twoTime);
        if (oneData == null || twoData == null) {
            return null;
        }
        Unit<?> dataUnit = Unit.ONE;
        if (stylePreferences != null) {
            dataUnit = stylePreferences.getDisplayUnits();
            oneData.convert(dataUnit);
            twoData.convert(dataUnit);
        } else if (oneData.getDataUnit().isCompatible(twoData.getDataUnit())) {
            dataUnit = oneData.getDataUnit();
            twoData.convert(dataUnit);
        }
        GeneralGridData newData = null;
        try {
            Interpolation interp = new BilinearInterpolation();
            if (oneData.isVector() && twoData.isVector()) {
                DataSource oneSourceU = new FloatBufferWrapper(
                        oneData.getUComponent(),
                        oneInterpolation.getSourceGeometry());
                DataSource oneSourceV = new FloatBufferWrapper(
                        oneData.getVComponent(),
                        oneInterpolation.getSourceGeometry());
                DataSource twoSourceU = new FloatBufferWrapper(
                        twoData.getUComponent(),
                        twoInterpolation.getSourceGeometry());
                DataSource twoSourceV = new FloatBufferWrapper(
                        twoData.getVComponent(),
                        twoInterpolation.getSourceGeometry());
                float[] oneU = oneInterpolation.reprojectedGrid(interp,
                        oneSourceU, new FloatArrayWrapper(getGridGeometry()))
                        .getArray();
                float[] oneV = oneInterpolation.reprojectedGrid(interp,
                        oneSourceV, new FloatArrayWrapper(getGridGeometry()))
                        .getArray();
                float[] twoU = twoInterpolation.reprojectedGrid(interp,
                        twoSourceU, new FloatArrayWrapper(getGridGeometry()))
                        .getArray();
                float[] twoV = twoInterpolation.reprojectedGrid(interp,
                        twoSourceV, new FloatArrayWrapper(getGridGeometry()))
                        .getArray();
                float[] newU = new float[oneU.length];
                float[] newV = new float[oneV.length];
                for (int i = 0; i < newU.length; i++) {
                    newU[i] = oneU[i] - twoU[i];
                    newV[i] = oneV[i] - twoV[i];
                }
                newData = GeneralGridData.createVectorDataUV(
                        FloatBuffer.wrap(newU), FloatBuffer.wrap(newV),
                        dataUnit);
            } else {
                DataSource oneSource = new FloatBufferWrapper(
                        oneData.getScalarData(),
                        oneInterpolation.getSourceGeometry());
                DataSource twoSource = new FloatBufferWrapper(
                        twoData.getScalarData(),
                        twoInterpolation.getSourceGeometry());
                float[] oneScalar = oneInterpolation.reprojectedGrid(interp,
                        oneSource, new FloatArrayWrapper(getGridGeometry()))
                        .getArray();
                float[] twoScalar = twoInterpolation.reprojectedGrid(interp,
                        twoSource, new FloatArrayWrapper(getGridGeometry()))
                        .getArray();
                float[] newScalar = new float[oneScalar.length];
                for (int i = 0; i < newScalar.length; i++) {
                    newScalar[i] = oneScalar[i] - twoScalar[i];
                }
                newData = GeneralGridData.createScalarData(
                        FloatBuffer.wrap(newScalar), dataUnit);
            }
        } catch (FactoryException e) {
            throw new VizException(e);
        } catch (TransformException e) {
            throw new VizException(e);
        }
        return newData;
    }

    @Override
    protected DataTime getTimeForResource() {
        DataTime oneTime = descriptor.getTimeForResource(one);
        DataTime twoTime = descriptor.getTimeForResource(two);
        if (oneTime == null || twoTime == null) {
            return null;
        }
        return new CombinedDataTime(oneTime, twoTime);
    }

    @Override
    public Measure<Float, ?> inspectValue(ReferencedCoordinate coord)
            throws VizException {
        Measure<Float, ?> oneVal = one.inspectValue(coord);
        Measure<Float, ?> twoVal = two.inspectValue(coord);
        if (oneVal == null || twoVal == null) {
            return null;
        }
        Unit<?> dataUnit = Unit.ONE;
        if (stylePreferences != null) {
            dataUnit = stylePreferences.getDisplayUnits();
            if (oneVal.getUnit().isCompatible(dataUnit)) {
                oneVal = Measure.valueOf((float) oneVal.getUnit()
                        .getConverterTo(dataUnit).convert(oneVal.getValue()),
                        dataUnit);
            }
            if (twoVal.getUnit().isCompatible(dataUnit)) {
                twoVal = Measure.valueOf((float) twoVal.getUnit()
                        .getConverterTo(dataUnit).convert(twoVal.getValue()),
                        dataUnit);
            }
        } else if (oneVal.getUnit().isCompatible(twoVal.getUnit())) {
            dataUnit = oneVal.getUnit();
            twoVal = Measure.valueOf(
                    (float) twoVal.getUnit().getConverterTo(dataUnit)
                            .convert(twoVal.getValue()), dataUnit);
        }
        return Measure.valueOf(oneVal.getValue() - twoVal.getValue(), dataUnit);
    }

    @Override
    public String getName() {
        DataTime oneTime = descriptor.getTimeForResource(one);
        DataTime twoTime = descriptor.getTimeForResource(two);
        if (oneTime == null || twoTime == null) {
            return "No Data Available";
        }
        return CombineUtil.getName(one.getName(), two.getName(),
                CombineOperation.DIFFERENCE) + " " + oneTime.getLegendString();
    }

    @Override
    public ResourceList getResourceList() {
        return resourceData.getResourceList();
    }

}
