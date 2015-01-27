/**
 * This code has unlimited rights, and is provided "as is" by the National Centers 
 * for Environmental Prediction, without warranty of any kind, either expressed or implied, 
 * including but not limited to the implied warranties of merchantability and/or fitness 
 * for a particular purpose.
 * 
 * This code has been developed by the NCEP-SIB for use in the AWIPS2 system.
 * 
 **/
package gov.noaa.nws.ncep.viz.rtkp;

import gov.noaa.nws.ncep.common.dataplugin.geomag.GeoMagRecord;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Collections;
import java.util.Comparator;
import java.util.Date;
import java.util.Iterator;
import java.util.List;
import java.util.TimeZone;

import javax.measure.unit.SI;
import javax.measure.unit.Unit;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.style.level.SingleLevel;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.xy.timeseries.adapter.AbstractTimeSeriesAdapter;
import com.raytheon.viz.core.graphing.xy.XYData;
import com.raytheon.viz.core.graphing.xy.XYDataList;

/**
 * RTKP Time Series adapter.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 5, 2014  1122       sgurung     Initial creation
 * 
 * </pre>
 * 
 * @author sgurung
 * @version 1.0
 */

public class GeoMagRTKpTimeSeriesAdapter extends
        AbstractTimeSeriesAdapter<GeoMagRecord> {

    private static Unit<?> unit = SI.NANO(SI.TESLA);

    static final long ONE_MINUTE_IN_MILLIS = 60000;// millisecs

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

        System.out.println(" ... recordsToLoad.length = "
                + recordsToLoad.length);
        ArrayList<XYData> data = new ArrayList<XYData>();

        XYDataList list = new XYDataList();
        list.setData(data);

        return list;
    }

    /**
     * 
     */
    private void sortRecordsToLoad(GeoMagRecord[] recordsToLoad) {
        List<GeoMagRecord> recordsList = Arrays.asList(recordsToLoad);
        Collections.sort(recordsList, new Comparator<GeoMagRecord>() {

            @Override
            public int compare(GeoMagRecord xy1, GeoMagRecord xy2) {
                DataTime t1 = (DataTime) xy1.getDataTime();
                DataTime t2 = (DataTime) xy2.getDataTime();
                return t1.compareTo(t2);
            }

        });

    }

    private Date getUtcTime(Calendar cal) {
        // cal in GMT timezone
        TimeZone z = TimeZone.getDefault();
        int offset = z.getOffset(cal.getTimeInMillis());

        Date date = new Date(cal.getTimeInMillis() - offset);

        return date;
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
