package gov.noaa.nws.ncep.viz.timeseries;

import gov.noaa.nws.ncep.common.dataplugin.geomag.GeoMagRecord;

import java.util.ArrayList;
import java.util.Iterator;

import javax.measure.unit.SI;
import javax.measure.unit.Unit;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.style.level.SingleLevel;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.xy.timeseries.adapter.AbstractTimeSeriesAdapter;
import com.raytheon.viz.core.graphing.xy.XYData;
import com.raytheon.viz.core.graphing.xy.XYDataList;

public class GeoMagTimeSeriesAdapter extends
        AbstractTimeSeriesAdapter<GeoMagRecord> {

    private static Unit<?> unit = SI.NANO(SI.TESLA);

    @Override
    public XYDataList loadData() throws VizException {
        GeoMagRecord[] recordsToLoad = null;
        synchronized (records) {
            recordsToLoad = new GeoMagRecord[records.size()];
            Iterator<GeoMagRecord> iter = records.iterator();
            for (int i = 0; i < recordsToLoad.length; ++i) {
                recordsToLoad[i] = iter.next();
            }
        }
        return loadInternal(recordsToLoad);
    }

    @Override
    public XYDataList loadRecord(PluginDataObject pdo) throws VizException {
        GeoMagRecord[] recordsToLoad = new GeoMagRecord[1];
        recordsToLoad[0] = (GeoMagRecord) pdo;
        return loadInternal(recordsToLoad);
    }

    private XYDataList loadInternal(GeoMagRecord[] recordsToLoad) {

        ArrayList<XYData> data = new ArrayList<XYData>();

        for (GeoMagRecord record : recordsToLoad) {
            DataTime x = record.getDataTime();
            Float y;
            if (resourceData.getSource().equalsIgnoreCase("H_Component"))
                y = record.getComponent_1();
            else
                y = record.getComponent_2();
            data.add(new XYData(x, y));
        }

        XYDataList list = new XYDataList();
        list.setData(data);

        return list;
    }

    @Override
    public SingleLevel getLevel() {
        SingleLevel level = new SingleLevel("SURFACE");
        level.setValue(0.0);
        return level;
    }

    @Override
    public Unit<?> getDataUnit() {
        return unit;
    }

    @Override
    public String getParameterName() {
        return resourceData.getYParameter().name;
    }

}
