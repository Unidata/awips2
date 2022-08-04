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

import java.util.ArrayList;
import java.util.List;

import javax.measure.Quantity;
import javax.measure.Unit;

import org.geotools.coverage.grid.GeneralGridEnvelope;
import org.geotools.coverage.grid.GridGeometry2D;
import org.geotools.geometry.jts.ReferencedEnvelope;
import org.opengis.coverage.grid.GridEnvelope;
import org.opengis.referencing.FactoryException;
import org.opengis.referencing.operation.TransformException;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.geospatial.ReferencedCoordinate;
import com.raytheon.uf.common.geospatial.interpolation.BilinearInterpolation;
import com.raytheon.uf.common.numeric.source.DataSource;
import com.raytheon.uf.common.style.ParamLevelMatchCriteria;
import com.raytheon.uf.common.time.CombinedDataTime;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.drawables.IRenderable;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.grid.rsc.AbstractGridResource;
import com.raytheon.uf.viz.core.grid.rsc.data.GeneralGridData;
import com.raytheon.uf.viz.core.grid.rsc.data.ScalarGridData;
import com.raytheon.uf.viz.core.grid.rsc.data.VectorGridData;
import com.raytheon.uf.viz.core.rsc.IResourceGroup;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.ResourceList;
import com.raytheon.uf.viz.core.rsc.capabilities.GroupNamingCapability;
import com.raytheon.uf.viz.core.rsc.groups.ICombinedResourceData.CombineOperation;
import com.raytheon.uf.viz.core.rsc.groups.ICombinedResourceData.CombineUtil;
import com.raytheon.uf.viz.core.rsc.interrogation.InterrogateMap;
import com.raytheon.uf.viz.core.rsc.interrogation.InterrogationKey;
import com.raytheon.uf.viz.core.rsc.interrogation.Interrogator;

import tec.uom.se.AbstractUnit;
import tec.uom.se.quantity.Quantities;

/**
 *
 * Resource which calculates the difference of two grid resources and displays
 * the value as a grid.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Mar 16, 2011  10751    bsteffen  Initial creation
 * Feb 28, 2013  2791     bsteffen  Use DataSource instead of FloatBuffers for
 *                                  data access
 * May 15, 2015  4079     bsteffen  Use publicly accessible display unit.
 * Aug 30, 2016  3240     bsteffen  Implement Interrogatable
 * Nov 28, 2017  5863     bsteffen  Change dataTimes to a NavigableSet
 * Feb 15, 2018  6902     njensen   Replace usage of deprecated interrogation key
 * Aug 29, 2019  67962    tjensen   Update for GeneralGridData refactor
 *
 * </pre>
 *
 * @author bsteffen
 */
public class DifferenceGridResource
        extends AbstractGridResource<DifferenceGridResourceData>
        implements IResourceGroup {

    // Defines a constant size for the difference grid
    private static final GridEnvelope GRID_ENVELOPE = new GeneralGridEnvelope(
            new int[] { 0, 0 }, new int[] { 512, 512 });

    private final AbstractGridResource<?> one;

    private final AbstractGridResource<?> two;

    public DifferenceGridResource(DifferenceGridResourceData resourceData,
            LoadProperties loadProperties, AbstractGridResource<?> one,
            AbstractGridResource<?> two) {
        super(resourceData, loadProperties);
        this.one = one;
        this.two = two;
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
    public InterrogateMap interrogate(ReferencedCoordinate coordinate,
            DataTime time, InterrogationKey<?>... keys) {
        if (!(time instanceof CombinedDataTime)) {
            return super.interrogate(coordinate, time, keys);
        }
        CombinedDataTime cTime = (CombinedDataTime) time;
        InterrogateMap oneMap = one.interrogate(coordinate,
                cTime.getPrimaryDataTime(), keys);
        InterrogateMap twoMap = two.interrogate(coordinate,
                cTime.getAdditionalDataTime(), keys);
        if (oneMap == null || oneMap.isEmpty() || twoMap == null
                || twoMap.isEmpty()) {
            return super.interrogate(coordinate, time, keys);
        }
        InterrogateMap myMap = new InterrogateMap();
        for (InterrogationKey<?> key : keys) {
            if (Interrogator.VALUE.equals(key)) {
                /*
                 * Get the values from the internal resources and difference
                 * them. If coordinate is not perfectly on grid points then this
                 * causes the interpolation to occur in the sources rather than
                 * in this resource which produces results that are more
                 * consistent with the interrogation of the internal resources.
                 */
                Quantity<?> oneMeasure = oneMap
                        .get(Interrogator.VALUE);
                Quantity<?> twoMeasure = twoMap
                        .get(Interrogator.VALUE);

                double oneVal = oneMeasure.getValue().doubleValue();
                double twoVal = twoMeasure.getValue().doubleValue();
                double myVal = oneVal - twoVal;
                Quantity<?> myMeasure = null;
                if (oneMeasure.getUnit().equals(twoMeasure.getUnit())) {
                    myMeasure = Quantities.getQuantity(myVal, oneMeasure.getUnit());
                } else {
                    myMeasure = Quantities.getQuantity(myVal, AbstractUnit.ONE);
                }
                myMap.put(Interrogator.VALUE, myMeasure);
            } else if (UNIT_STRING_INTERROGATE_KEY.equals(key)) {
                String oneUnit = oneMap.get(UNIT_STRING_INTERROGATE_KEY);
                String twoUnit = twoMap.get(UNIT_STRING_INTERROGATE_KEY);
                if (oneUnit.equals(twoUnit)) {
                    myMap.put(UNIT_STRING_INTERROGATE_KEY, oneUnit);
                } else {
                    myMap.put(UNIT_STRING_INTERROGATE_KEY,
                            "(" + oneUnit + "-" + twoUnit + ")");
                }
            } else if (DIRECTION_FROM_INTERROGATE_KEY.equals(key)) {
                Number oneDir = oneMap.get(DIRECTION_FROM_INTERROGATE_KEY);
                Number twoDir = twoMap.get(DIRECTION_FROM_INTERROGATE_KEY);
                if (oneDir != null && twoDir != null) {
                    myMap.put(DIRECTION_FROM_INTERROGATE_KEY,
                            oneDir.doubleValue() - twoDir.doubleValue());
                } else {
                    myMap.putAll(super.interrogate(coordinate, time, key));
                }
            } else {
                myMap.putAll(super.interrogate(coordinate, time, key));
            }
        }
        return myMap;

    }

    @Override
    public ParamLevelMatchCriteria getMatchCriteria() {
        return one.getMatchCriteria();
    }

    @Override
    public List<GeneralGridData> getData(DataTime time,
            List<PluginDataObject> pdos) throws VizException {
        if (!(time instanceof CombinedDataTime)) {
            throw new VizException("Unexpected single time in diff resource");
        }
        CombinedDataTime cTime = (CombinedDataTime) time;
        DataTime oneTime = cTime.getPrimaryDataTime();
        DataTime twoTime = cTime.getAdditionalDataTime();
        List<GeneralGridData> oneDataList = one.requestData(oneTime);
        List<GeneralGridData> twoDataList = two.requestData(twoTime);
        if (oneDataList == null || oneDataList.isEmpty() || twoDataList == null
                || twoDataList.isEmpty()) {
            return null;
        }
        List<GeneralGridData> newDataList = new ArrayList<>();
        for (GeneralGridData oneData : oneDataList) {
            for (GeneralGridData twoData : twoDataList) {
                GeneralGridData newData = difference(oneData, twoData);
                if (newData != null) {
                    newDataList.add(newData);
                }
            }
        }
        return newDataList;
    }

    private GeneralGridData difference(GeneralGridData oneData,
            GeneralGridData twoData) throws VizException {
        Unit<?> newUnit = oneData.getDataUnit();
        if (stylePreferences != null) {
            newUnit = stylePreferences.getDisplayUnits();
        }
        if (!oneData.convert(newUnit)) {
            // if oneData is somehow incompatible with our style rule then just
            // convert to its style units
            Unit<?> oneUnit = one.getDisplayUnit();
            if (oneUnit != null) {
                oneData.convert(oneUnit);
            }
        }
        if (!twoData.convert(newUnit)) {
            // if twoData is somehow incompatible with our style rule then just
            // convert to its style units
            Unit<?> twoUnit = two.getDisplayUnit();
            if (twoUnit != null) {
                twoData.convert(twoUnit);
            }
        }
        GridGeometry2D newGeom = oneData.getGridGeometry();
        if (!newGeom.equals(twoData.getGridGeometry())) {
            ReferencedEnvelope oneEnv = new ReferencedEnvelope(
                    oneData.getGridGeometry().getEnvelope());
            ReferencedEnvelope twoEnv = new ReferencedEnvelope(
                    twoData.getGridGeometry().getEnvelope());
            if (!oneEnv.getCoordinateReferenceSystem()
                    .equals(twoEnv.getCoordinateReferenceSystem())) {
                // If they aren't the same crs just go to screen space.
                try {
                    oneEnv = oneEnv.transform(descriptor.getCRS(), true);
                    twoEnv = twoEnv.transform(descriptor.getCRS(), true);
                } catch (FactoryException | TransformException e) {
                    throw new RuntimeException(e);
                }
            }
            ReferencedEnvelope newEnv = new ReferencedEnvelope(
                    oneEnv.intersection(twoEnv),
                    oneEnv.getCoordinateReferenceSystem());
            if (newEnv.isEmpty()) {
                return null;
            }
            newGeom = new GridGeometry2D(GRID_ENVELOPE, newEnv);
        }
        GeneralGridData newData = null;
        try {
            oneData = oneData.reproject(newGeom, new BilinearInterpolation());
            twoData = twoData.reproject(newGeom, new BilinearInterpolation());
            if (oneData instanceof VectorGridData
                    && twoData instanceof VectorGridData) {
                VectorGridData vectorOneData = (VectorGridData) oneData;
                VectorGridData vectorTwoData = (VectorGridData) twoData;
                DifferenceDataSource uComponent = new DifferenceDataSource(
                        vectorOneData.getUComponent(),
                        vectorTwoData.getUComponent());
                DifferenceDataSource vComponent = new DifferenceDataSource(
                        vectorOneData.getVComponent(),
                        vectorTwoData.getVComponent());
                newData = VectorGridData.createVectorDataUV(newGeom, uComponent,
                        vComponent, newUnit);
            } else if (oneData instanceof ScalarGridData
                    && twoData instanceof ScalarGridData) {
                ScalarGridData scalarOneData = (ScalarGridData) oneData;
                ScalarGridData scalarTwoData = (ScalarGridData) twoData;
                DifferenceDataSource data = new DifferenceDataSource(
                        scalarOneData.getScalarData(),
                        scalarTwoData.getScalarData());
                newData = ScalarGridData.createScalarData(newGeom, data,
                        newUnit);
            } else {
                throw new VizException(
                        "Unexpected grid data types in difference: "
                                + oneData.getClass() + " and "
                                + twoData.getClass());
            }
            // TODO: Add check for other GridData types as added
        } catch (TransformException | FactoryException e) {
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

    @Override
    protected boolean projectRenderable(IRenderable renderable) {
        // Always rebuild the renderables in case we projected into descriptor
        // space.
        return false;

    }

    protected static class DifferenceDataSource implements DataSource {

        private final DataSource one;

        private final DataSource two;

        private DifferenceDataSource(DataSource one, DataSource two) {
            super();
            this.one = one;
            this.two = two;
        }

        @Override
        public double getDataValue(int x, int y) {
            double oneVal = one.getDataValue(x, y);
            double twoVal = two.getDataValue(x, y);
            return oneVal - twoVal;
        }

    }

}
