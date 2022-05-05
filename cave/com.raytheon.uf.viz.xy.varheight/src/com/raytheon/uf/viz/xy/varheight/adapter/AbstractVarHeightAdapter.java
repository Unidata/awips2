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

import javax.measure.Unit;
import javax.measure.UnitConverter;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.common.topo.CachedTopoQuery;
import com.raytheon.uf.common.units.UnitConv;
import com.raytheon.uf.common.wxmath.Hgt2Pres;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.xy.scales.HeightScale;
import com.raytheon.uf.viz.xy.varheight.rsc.VarHeightResourceData;
import com.raytheon.viz.core.graphing.xy.XYData;
import com.raytheon.viz.core.graphing.xy.XYWindImageData;

import tec.uom.se.AbstractConverter;

/**
 * Abstract base class for writing var height adapters
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * May 07, 2010           bsteffen  Initial creation
 * Feb 06, 2018  6829     njensen   Added variable wind
 * Feb 19, 2018  6666     bsteffen  Add trimBelowSurface and loadPreparedData.
 * Apr 15, 2019  7596     lsingh    Updated units framework to JSR-363.
 *                                  Handled unit conversion.
 *                                    
 * 
 * </pre>
 * 
 * @author bsteffen
 */

public abstract class AbstractVarHeightAdapter<T extends PluginDataObject> {

    protected HeightScale heightScale;

    protected VarHeightResourceData resourceData;

    protected boolean wind;

    protected Set<T> records = new HashSet<>();

    public void setHeightScale(HeightScale heightScale) {
        this.heightScale = heightScale;
    }

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

    protected void trimBelowSurface(List<XYData> dataList) {
        /*
         * The value for surface, data will be removed above or below this
         * value.
         */
        double cutoff = 0;
        /*
         * Normally remove values that are numerically lower than the cutoff,
         * but for pressure higher values are physically lower so do the inverse
         * and remove values that are numerically higher.
         */
        boolean inverse = false;
        switch (heightScale.getHeightType()) {
        case HEIGHT_MSL:
            cutoff = CachedTopoQuery.getInstance()
                    .getHeight(resourceData.getPoint());
            break;
        case HEIGHT_AGL:
            break;
        case PRESSURE:
            double topo = CachedTopoQuery.getInstance()
                    .getHeight(resourceData.getPoint());
            cutoff = Hgt2Pres.hgt2pres((float) topo);
            inverse = true;
            break;
        default:
            return;
        }

        double aboveY = Double.POSITIVE_INFINITY;
        XYData above = null;
        double belowY = Double.NEGATIVE_INFINITY;
        XYData below = null;
        Iterator<XYData> iter = dataList.iterator();
        while (iter.hasNext()) {
            XYData data = iter.next();
            double y = ((Number) data.getY()).doubleValue();
            if (y > cutoff) {
                if (inverse) {
                    iter.remove();
                }
                if (y < aboveY) {
                    aboveY = y;
                    above = data;
                }
            } else if (y < cutoff) {
                if (!inverse) {
                    iter.remove();
                }
                if (y > belowY) {
                    belowY = y;
                    below = data;
                }
            }
        }
        if (!wind && above != null && below != null) {
            double aboveX = ((Number) above.getX()).doubleValue();
            double belowX = ((Number) below.getX()).doubleValue();
            double interpX = belowX + (cutoff - belowY)
                    * ((aboveX - belowX) / (aboveY - belowY));
            dataList.add(new XYData((float) interpX, (float) cutoff));
        }
    }

    public void convertData(List<XYData> data, Unit<?> xDisplayUnit) {
        Unit<?> xDataUnit = getXUnit();
        Unit<?> yDataUnit = getYUnit();
        Unit<?> yDisplayUnit = heightScale.getParameterUnit();

        UnitConverter xConverter = AbstractConverter.IDENTITY;
        if (xDataUnit != null && xDisplayUnit != null
                && xDataUnit.isCompatible(xDisplayUnit)) {
            xConverter = UnitConv.getConverterToUnchecked(xDataUnit,
                    xDisplayUnit);
        }
        UnitConverter yConverter = AbstractConverter.IDENTITY;
        if (yDataUnit != null && yDisplayUnit != null
                && yDataUnit.isCompatible(yDisplayUnit)) {
            yConverter = UnitConv.getConverterToUnchecked(yDataUnit,
                    yDisplayUnit);
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

    public boolean isWind() {
        return wind;
    }

    /**
     * Similar to {@link #loadData(DataTime)}, but uses several of the utility
     * methods of this class to prepare the data for display. The returned data
     * will be sorted, converted to the units passed in and trimmed to remove
     * data below the surface.
     * 
     * @param time
     *            which time to load data for.
     * @param xDisplayUnit
     *            the units used to convert the data.
     * @return a list of prepared data.
     * @throws VizException
     */
    public List<XYData> loadPreparedData(DataTime time, Unit<?> xDisplayUnit)
            throws VizException {
        List<XYData> data = loadData(time);
        trimBelowSurface(data);
        sortData(data);
        convertData(data, xDisplayUnit);
        return data;
    }

}
