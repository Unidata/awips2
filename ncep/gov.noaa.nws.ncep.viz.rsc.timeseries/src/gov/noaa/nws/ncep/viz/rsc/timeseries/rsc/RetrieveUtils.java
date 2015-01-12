package gov.noaa.nws.ncep.viz.rsc.timeseries.rsc;

import gov.noaa.nws.ncep.common.dataplugin.geomag.GeoMagAvg;
import gov.noaa.nws.ncep.common.dataplugin.geomag.GeoMagK1min;
import gov.noaa.nws.ncep.common.dataplugin.geomag.GeoMagRecord;
import gov.noaa.nws.ncep.common.dataplugin.geomag.exception.GeoMagException;
import gov.noaa.nws.ncep.common.dataplugin.geomag.request.RetrieveHrAvgRequest;
import gov.noaa.nws.ncep.common.dataplugin.geomag.request.RetrieveK1minRequest;
import gov.noaa.nws.ncep.common.dataplugin.geomag.request.RetrieveSingleK1minRequest;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Date;
import java.util.List;
import java.util.TimeZone;

import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.requests.ThriftClient;

/**
 * 
 * Utility class containing static methods to retrieve and store PGEN Activities
 * in EDEX
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 2014/02/12            qzhou         initiate
 * 
 * </pre>
 * 
 * @author qzhou
 * @version 1.0
 */
public class RetrieveUtils {

    /**
     * Retrieves the k index
     * 
     * @param stationCode
     * @param startTime
     * @param endTime
     * @return GeoMagK1min record list
     * @throws GeoMagException
     */

    @SuppressWarnings("unchecked")
    public static List<GeoMagK1min> retrieveK1minRecord(String stationCode,
            Date startTime, Date endTime) throws GeoMagException {

        RetrieveK1minRequest req = new RetrieveK1minRequest(stationCode,
                startTime, endTime);
        List<GeoMagK1min> resultList = null;
        try {
            resultList = (List<GeoMagK1min>) ThriftClient.sendRequest(req);
        } catch (VizException e) {
            throw new GeoMagException(
                    "Unable to retrieve geomag k-index Activities.", e);
        }

        return resultList;
    }

    @SuppressWarnings("unchecked")
    public static List<GeoMagK1min> retrieveSingleK1minRecord(
            String stationCode, Date startTime) throws GeoMagException {

        RetrieveSingleK1minRequest req = new RetrieveSingleK1minRequest(
                stationCode, startTime);

        List<GeoMagK1min> resultList = null;
        try {
            resultList = (List<GeoMagK1min>) ThriftClient.sendRequest(req);

        } catch (VizException e) {
            throw new GeoMagException(
                    "Unable to retrieve geomag k-index Activities.", e);
        }

        return resultList;
    }

    /**
     * Retrieves the hHrAvg and dHrAvg
     * 
     * @param stationCode
     * @param startTime
     * @param endTime
     * @return avgList
     * @throws GeoMagException
     */
    @SuppressWarnings("unchecked")
    public static List<GeoMagAvg> retrieveHrAvgs(String stationCode,
            Date startTime, Date endTime) throws GeoMagException {

        List<GeoMagAvg> avgList = new ArrayList<GeoMagAvg>();

        try {
            RetrieveHrAvgRequest request = new RetrieveHrAvgRequest(
                    stationCode, startTime, endTime);// startTime-1
            avgList = (List<GeoMagAvg>) ThriftClient.sendRequest(request);

        } catch (Exception e) {
            throw new GeoMagException(
                    "Unable to retrieve geomag average Activities.", e);

        }

        return avgList;

    }

    // @SuppressWarnings("unchecked")
    // public static List<GeoMagRecord> retrieveGeoMag(String stationCode,
    // Date startTime, Date endTime) throws GeoMagException {
    //
    // List<GeoMagRecord> geoMagList = new ArrayList<GeoMagRecord>();
    //
    // try {
    // RetrieveGeoMagRequest request = new RetrieveGeoMagRequest(
    // stationCode, startTime, endTime);
    // geoMagList = (List<GeoMagRecord>) ThriftClient.sendRequest(request);
    //
    // } catch (Exception e) {
    // throw new GeoMagException(
    // "Unable to retrieve geomag average Activities.", e);
    //
    // }
    //
    // return geoMagList;
    //
    // }

    public static Date getUtcDate(Calendar cal) {
        // cal in GMT timezone
        TimeZone z = TimeZone.getDefault();
        int offset = z.getOffset(cal.getTimeInMillis());

        Date date = new Date(cal.getTimeInMillis() - offset);

        return date;
    }

    public static Calendar getUtcCalendar(Date date) {
        Calendar calendar = Calendar.getInstance();
        calendar.setTimeZone(TimeZone.getTimeZone("GMT"));

        calendar.setTimeInMillis(date.getTime());

        return calendar;
    }

    public static String hourTo2DigetStr(Calendar calendar) {
        int hour = calendar.get(Calendar.HOUR_OF_DAY);

        if (hour == 24)
            hour = 0;

        String hourStr = String.valueOf(hour);

        if (hourStr.length() == 1)
            hourStr = '0' + hourStr;

        return hourStr;
    }

    public static Calendar moveToNextSynop(Calendar cal, int synop) {
        Calendar calendar = (Calendar) cal.clone();
        calendar.add(Calendar.HOUR_OF_DAY, synop);

        return calendar;
    }

    public static DataTime moveToNextSynop(DataTime dataTime, int synop) {
        Calendar cal = dataTime.getValidTime();
        Calendar calendar = moveToNextSynop(cal, synop);
        DataTime newDataTime = new DataTime(calendar);

        return newDataTime;
    }

    public static float[] getMedian(List<GeoMagRecord> records) {

        if (records == null || records.size() == 0)
            return new float[] { 0f, 0f };

        int size = records.size();
        float[] hComps = new float[size];
        float[] dComps = new float[size];

        for (int i = 0; i < size; i++) {
            hComps[i] = records.get(i).getComponent_1();
            dComps[i] = records.get(i).getComponent_2();
        }

        Arrays.sort(hComps);
        Arrays.sort(dComps);

        float hMedian = 0f;
        float dMedian = 0f;

        if (size % 2 == 0) {
            hMedian = (hComps[hComps.length / 2] + hComps[hComps.length / 2 - 1]) / 2;
            dMedian = (dComps[dComps.length / 2] + dComps[dComps.length / 2 - 1]) / 2;

        } else {
            hMedian = hComps[(hComps.length - 1) / 2];
            dMedian = dComps[(dComps.length - 1) / 2];
        }

        return new float[] { hMedian, dMedian };

    }

}
