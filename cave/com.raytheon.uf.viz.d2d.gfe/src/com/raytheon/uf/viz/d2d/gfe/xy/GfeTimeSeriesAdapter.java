package com.raytheon.uf.viz.d2d.gfe.xy;

import java.util.ArrayList;
import java.util.Iterator;

import javax.measure.unit.Unit;

import org.geotools.geometry.DirectPosition2D;
import org.opengis.geometry.MismatchedDimensionException;
import org.opengis.metadata.spatial.PixelOrientation;
import org.opengis.referencing.operation.MathTransform;
import org.opengis.referencing.operation.TransformException;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.gfe.dataaccess.GFEDataAccessUtil;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.GFERecord;
import com.raytheon.uf.common.dataplugin.gfe.slice.IGridSlice;
import com.raytheon.uf.common.dataplugin.gfe.slice.ScalarGridSlice;
import com.raytheon.uf.common.dataplugin.gfe.slice.VectorGridSlice;
import com.raytheon.uf.common.geospatial.MapUtil;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.style.level.Level;
import com.raytheon.uf.viz.core.style.level.SingleLevel;
import com.raytheon.uf.viz.xy.timeseries.adapter.AbstractTimeSeriesAdapter;
import com.raytheon.viz.core.graphing.xy.XYData;
import com.raytheon.viz.core.graphing.xy.XYDataList;
import com.raytheon.viz.core.graphing.xy.XYWindImageData;

public class GfeTimeSeriesAdapter extends AbstractTimeSeriesAdapter<GFERecord> {

    private Unit<?> unit = Unit.ONE;

    @Override
    public XYDataList loadData() throws VizException {
        GFERecord[] recordsToLoad = null;
        synchronized (records) {
            recordsToLoad = new GFERecord[records.size()];
            Iterator<GFERecord> iter = records.iterator();
            for (int i = 0; i < recordsToLoad.length; ++i) {
                recordsToLoad[i] = iter.next();
            }
        }

        return loadInternal(recordsToLoad);
    }

    private XYDataList loadInternal(GFERecord[] recordsToLoad)
            throws VizException {
        ArrayList<XYData> data = new ArrayList<XYData>();

        DirectPosition2D point = null;
        for (GFERecord gfeRecord : recordsToLoad) {
            IGridSlice slice = null;
            try {
                slice = GFEDataAccessUtil.getSlice(gfeRecord);
            } catch (Exception e) {
                throw new VizException(e);
            }
            if (slice != null) {
                unit = slice.getGridInfo().getUnitObject();
                if (point == null) {
                    MathTransform transform = MapUtil.getTransformFromLatLon(
                            PixelOrientation.CENTER, slice.getGridInfo()
                                    .getGridLoc());
                    point = new DirectPosition2D(
                            resourceData.getCoordinate().x,
                            resourceData.getCoordinate().y);
                    try {
                        transform.transform(point, point);
                    } catch (MismatchedDimensionException e) {
                        throw new VizException(e);
                    } catch (TransformException e) {
                        throw new VizException(e);
                    }
                    point.x = Math.round(point.x);
                    point.y = Math.round(point.y);
                }
            }
            if (slice instanceof VectorGridSlice) {
                VectorGridSlice vSlice = (VectorGridSlice) slice;
                float spd = vSlice.getMagGrid().get((int) point.x,
                        (int) point.y);
                float dir = vSlice.getDirGrid().get((int) point.x,
                        (int) point.y);
                data.add(new XYWindImageData(gfeRecord.getDataTime(), spd, spd,
                        dir));
            } else if (slice instanceof ScalarGridSlice) {
                ScalarGridSlice sSlice = (ScalarGridSlice) slice;
                float val = sSlice.getScalarGrid().get((int) point.x,
                        (int) point.y);
                data.add(new XYData(gfeRecord.getDataTime(), val));
            } else if (slice == null) {
                throw new VizException("Unable to load GFE Slice Data");
            } else {
                throw new VizException("Unable GFE Slice of type "
                        + slice.getClass().getSimpleName());
            }
        }
        XYDataList list = new XYDataList();

        list.setData(data);
        return list;
    }

    @Override
    public SingleLevel getLevel() {
        return new SingleLevel(Level.LevelType.SURFACE);
    }

    @Override
    public Unit<?> getDataUnit() {
        return unit;
    }

    @Override
    public String getParameterName() {
        return resourceData.getYParameter().name;
    }

    @Override
    public XYDataList loadRecord(PluginDataObject pdo) throws VizException {
        GFERecord[] recordsToLoad = new GFERecord[1];
        recordsToLoad[0] = (GFERecord) pdo;
        return loadInternal(recordsToLoad);
    }

}
