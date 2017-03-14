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
package com.raytheon.uf.viz.xy.varheight.adapter;

import java.util.Collections;
import java.util.Comparator;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Set;

import javax.measure.converter.UnitConverter;
import javax.measure.unit.Unit;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.xy.varheight.rsc.VarHeightResourceData;
import com.raytheon.viz.core.graphing.xy.XYData;
import com.raytheon.viz.core.graphing.xy.XYWindImageData;
import com.raytheon.viz.core.slice.request.HeightScale;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 7, 2010            bsteffen     Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */

public abstract class AbstractVarHeightAdapter<T extends PluginDataObject> {

    protected HeightScale heightScale;

    protected VarHeightResourceData resourceData;

    protected Set<T> records = new HashSet<T>();

    /**
     * @param heightScale
     *            the heightScale to set
     */
    public void setHeightScale(HeightScale heightScale) {
        this.heightScale = heightScale;
    }

    /**
     * @param resourceData
     *            the resourceData to set
     */
    public void setResourceData(VarHeightResourceData resourceData) {
        this.resourceData = resourceData;
    }

    public abstract List<XYData> loadData(DataTime currentTime)
            throws VizException;

    public abstract Unit<?> getXUnit();

    public abstract Unit<?> getYUnit();

    public abstract String getParameterName();

    @SuppressWarnings("unchecked")
    public void addRecord(PluginDataObject pdo) {
        synchronized (records) {
            records.add((T) pdo);
        }
    }

    public void remove(DataTime time) {
        synchronized (records) {
            Iterator<T> itr = records.iterator();
            while (itr.hasNext()) {
                if (itr.next().getDataTime().equals(time)) {
                    itr.remove();
                }
            }
        }
    }

    public void sortData(List<XYData> data) {
        Collections.sort(data, new Comparator<XYData>() {

            @Override
            public int compare(XYData o1, XYData o2) {
                return Float.compare((Float) o1.getY(), (Float) o2.getY());
            }

        });
    }

    public void convertData(List<XYData> data, Unit<?> xDisplayUnit) {
        Unit<?> xDataUnit = getXUnit();
        Unit<?> yDataUnit = getYUnit();
        Unit<?> yDisplayUnit = heightScale.getParameterUnit();

        UnitConverter xConverter = UnitConverter.IDENTITY;
        if (xDataUnit != null && xDisplayUnit != null
                && xDataUnit.isCompatible(xDisplayUnit)) {
            xConverter = xDataUnit.getConverterTo(xDisplayUnit);
        }
        UnitConverter yConverter = UnitConverter.IDENTITY;
        if (yDataUnit != null && yDisplayUnit != null
                && yDataUnit.isCompatible(yDisplayUnit)) {
            yConverter = yDataUnit.getConverterTo(yDisplayUnit);
        }

        for (XYData xy : data) {
            if (xy instanceof XYWindImageData) {
                ((XYWindImageData) xy).convert(xConverter);
            }
            Number x = (Number) xy.getX();
            xy.setX(xConverter.convert(x.doubleValue()));

            Number y = (Number) xy.getY();
            xy.setY(yConverter.convert(y.doubleValue()));
        }
    }

}
