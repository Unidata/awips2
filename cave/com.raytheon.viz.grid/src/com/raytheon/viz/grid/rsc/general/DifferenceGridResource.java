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
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.measure.unit.Unit;

import org.geotools.coverage.grid.GeneralGridEnvelope;
import org.geotools.coverage.grid.GridGeometry2D;
import org.geotools.geometry.jts.ReferencedEnvelope;
import org.opengis.coverage.grid.GridEnvelope;
import org.opengis.referencing.FactoryException;
import org.opengis.referencing.crs.CoordinateReferenceSystem;
import org.opengis.referencing.operation.TransformException;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.geospatial.ReferencedCoordinate;
import com.raytheon.uf.common.geospatial.interpolation.BilinearInterpolation;
import com.raytheon.uf.common.time.CombinedDataTime;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.drawables.IRenderable;
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
 * Resource which calculates the difference of two grid resources and displays
 * the value as a grid.
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
    public Map<String, Object> interrogate(ReferencedCoordinate coord)
            throws VizException {
        Map<String, Object> oneMap = one.interrogate(coord);
        Map<String, Object> twoMap = two.interrogate(coord);
        if (oneMap == null || twoMap == null) {
            return super.interrogate(coord);
        }
        Map<String, Object> myMap = new HashMap<String, Object>();
        float oneVal = (Float) oneMap.get(INTERROGATE_VALUE);
        float twoVal = (Float) twoMap.get(INTERROGATE_VALUE);
        myMap.put(INTERROGATE_VALUE, oneVal - twoVal);
        if (oneMap.get(INTERROGATE_UNIT).equals(twoMap.get(INTERROGATE_UNIT))) {
            myMap.put(INTERROGATE_UNIT, oneMap.get(INTERROGATE_UNIT));
        } else {
            myMap.put(INTERROGATE_UNIT, "(" + oneMap.get(INTERROGATE_UNIT)
                    + "-" + twoMap.get(INTERROGATE_UNIT) + ")");
        }
        if (myMap.containsKey(INTERROGATE_DIRECTION)
                && twoMap.containsKey(INTERROGATE_DIRECTION)) {
            float oneDir = (Float) oneMap.get(INTERROGATE_DIRECTION);
            float twoDir = (Float) twoMap.get(INTERROGATE_DIRECTION);
            myMap.put(INTERROGATE_DIRECTION, oneDir - twoDir);
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
        List<GeneralGridData> newDataList = new ArrayList<GeneralGridData>();
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
            if (one.stylePreferences != null) {
                oneData.convert(one.stylePreferences.getDisplayUnits());
            }
        }
        if (!twoData.convert(newUnit)) {
            // if twoData is somehow incompatible with our style rule then just
            // convert to its style units
            if (two.stylePreferences != null) {
                twoData.convert(two.stylePreferences.getDisplayUnits());
            }
        }
        GridGeometry2D newGeom = oneData.getGridGeometry();
        if (!newGeom.equals(twoData.getGridGeometry())) {
            ReferencedEnvelope oneEnv = new ReferencedEnvelope(oneData
                    .getGridGeometry().getEnvelope());
            ReferencedEnvelope twoEnv = new ReferencedEnvelope(twoData
                    .getGridGeometry().getEnvelope());
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
            if (oneData.isVector() && twoData.isVector()) {
                float[] oneU = oneData.getUComponent().array();
                float[] oneV = oneData.getVComponent().array();
                float[] twoU = twoData.getUComponent().array();
                float[] twoV = twoData.getVComponent().array();
                float[] newU = new float[oneU.length];
                float[] newV = new float[oneV.length];
                for (int i = 0; i < newU.length; i++) {
                    newU[i] = oneU[i] - twoU[i];
                    newV[i] = oneV[i] - twoV[i];
                }
                newData = GeneralGridData
                        .createVectorDataUV(newGeom, FloatBuffer.wrap(newU),
                                FloatBuffer.wrap(newV), newUnit);
            } else {
                float[] oneScalar = oneData.getScalarData().array();
                float[] twoScalar = twoData.getScalarData().array();
                float[] newScalar = new float[oneScalar.length];
                for (int i = 0; i < newScalar.length; i++) {
                    newScalar[i] = oneScalar[i] - twoScalar[i];
                }
                newData = GeneralGridData.createScalarData(newGeom,
                        FloatBuffer.wrap(newScalar), newUnit);
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
    protected boolean projectRenderable(IRenderable renderable,
            CoordinateReferenceSystem crs) throws VizException {
        // Always rebuild the renderables in case we projected into descriptor
        // space.
        return false;

    }

}
