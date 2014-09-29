package gov.noaa.nws.ncep.common.dataplugin.geomag.request;

import gov.noaa.nws.ncep.common.dataplugin.geomag.GeoMagAvg;
import gov.noaa.nws.ncep.common.dataplugin.geomag.GeoMagK1min;
import gov.noaa.nws.ncep.common.dataplugin.geomag.GeoMagK3hr;
import gov.noaa.nws.ncep.common.dataplugin.geomag.GeoMagRecord;
import gov.noaa.nws.ncep.common.dataplugin.geomag.calculation.CalcUtil;
import gov.noaa.nws.ncep.common.dataplugin.geomag.dao.GeoMagAvgDao;
import gov.noaa.nws.ncep.common.dataplugin.geomag.dao.GeoMagDao;
import gov.noaa.nws.ncep.common.dataplugin.geomag.dao.GeoMagK1minDao;
import gov.noaa.nws.ncep.common.dataplugin.geomag.dao.GeoMagK3hrDao;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Comparator;
import java.util.Date;
import java.util.List;

import com.raytheon.uf.common.dataquery.db.QueryParam;
import com.raytheon.uf.edex.database.DataAccessLayerException;
import com.raytheon.uf.edex.database.query.DatabaseQuery;

/**
 * 
 * Retrieve data from database utility for a given dataURI
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 2014/02/12   #1123       qzhou       Moved from edex to here
 * 2014/06/27   #1136       qzhou       Change hour avg to 0-current time
 * 
 * </pre>
 * 
 * @author qzhou
 * @version 1.0
 */
public class DatabaseUtil {

    private static final int AVG_DATA_RANGE = 30;

    private static final float MISSING_VAL = 99999.99f;

    /*
     * from geomag
     */
    public static List<?> retrieveUriForAvg(GeoMagDao dao, String dataUri,
            Date time) {
        String station = CalcUtil.getStationFromUri(dataUri);

        DatabaseQuery query = new DatabaseQuery(GeoMagRecord.class.getName());
        query.addReturnedField("component_1");
        query.addReturnedField("component_2");
        query.addReturnedField("dataTime.refTime");
        query.addReturnedField("badDataPoint");
        query.addReturnedField("sourceId");

        // called only when time is 59min, so include it.
        query.addQueryParam("dataTime.refTime", time,
                QueryParam.QueryOperand.LESSTHANEQUALS);
        Calendar cal = Calendar.getInstance();
        cal.setTime(time);
        cal.set(Calendar.MINUTE, 0);
        // cal.add(Calendar.HOUR_OF_DAY, -1);

        query.addQueryParam("dataTime.refTime", cal.getTime(),
                QueryParam.QueryOperand.GREATERTHANEQUALS);
        query.addQueryParam("stationCode", station);

        List<?> resultsList = null;

        try {
            resultsList = dao.queryByCriteria(query); // 60
        } catch (DataAccessLayerException e1) {
            e1.printStackTrace();
        }

        return resultsList;
    }

    /*
     * from geomag_houravg
     */
    public static List<GeoMagAvg> retrieveSingleAvg(String dataUri, Date time) {
        GeoMagAvgDao avgDao = new GeoMagAvgDao();
        String station = CalcUtil.getStationFromUri(dataUri);

        List<GeoMagAvg> resultsList = null;
        resultsList = avgDao.getSingleAvg(station, time);

        return resultsList;

    }

    /*
     * from geomag_houravg
     */
    public static List<GeoMagAvg> retrieveUriBy3hr(String dataUri, Date spTime) {
        GeoMagAvgDao avgDao = new GeoMagAvgDao();
        String station = CalcUtil.getStationFromUri(dataUri);

        Calendar cal = Calendar.getInstance();
        cal.setTime(spTime);
        cal.add(Calendar.DAY_OF_YEAR, -AVG_DATA_RANGE); // at least one day is
                                                        // needed for gt, lt

        // since avg have min=30, cal.getTime() and spTime are not included
        List<GeoMagAvg> resultsList = null;
        resultsList = avgDao.getAvgForStation(station, cal.getTime(), spTime); // 720

        return resultsList;
    }

    /*
     * from geomag
     */
    public static List<?> retrieveUriForK1min(GeoMagDao dao, String dataUri,
            Date time) {
        String station = CalcUtil.getStationFromUri(dataUri);

        DatabaseQuery query = new DatabaseQuery(GeoMagRecord.class.getName());

        query.addReturnedField("component_1");
        query.addReturnedField("component_2");
        query.addReturnedField("dataTime.refTime");
        query.addReturnedField("badDataPoint");
        query.addReturnedField("sourceId");

        // Document uses epTime-1minute. Consider 3 sources, we use current time
        query.addQueryParam("dataTime.refTime", time,
                QueryParam.QueryOperand.LESSTHANEQUALS);

        Date epTime = CalcUtil.getEPTime(time);
        Calendar cal = Calendar.getInstance();
        cal.setTime(epTime);
        cal.add(Calendar.HOUR_OF_DAY, -48);

        // start time is epTime-48hour. So use GREATERTHANEQUALS
        query.addQueryParam("dataTime.refTime", cal.getTime(),
                QueryParam.QueryOperand.GREATERTHANEQUALS);
        query.addQueryParam("stationCode", station);

        List<?> resultsList = null;
        try {
            resultsList = dao.queryByCriteria(query); // 2880
        } catch (DataAccessLayerException e1) {
            e1.printStackTrace();
        }

        return resultsList;
    }

    /*
     * from geomag_k1min
     */
    public static List<GeoMagK1min> retrieveSingleK1min(String dataUri,
            Date time) {
        GeoMagK1minDao k1minDao = new GeoMagK1minDao();
        String station = CalcUtil.getStationFromUri(dataUri);

        List<GeoMagK1min> resultsList = null;
        resultsList = k1minDao.getSingleK1min(station, time);

        return resultsList;

    }

    /*
     * from geomag_k3hr
     */
    public static List<GeoMagK3hr> retrieveUriForK3hr(String dataUri,
            Date epTime) {
        GeoMagK3hrDao k3hrDao = new GeoMagK3hrDao();
        String station = CalcUtil.getStationFromUri(dataUri);

        Calendar cal = Calendar.getInstance();
        cal.setTime(epTime);
        cal.add(Calendar.DAY_OF_YEAR, -1);

        List<GeoMagK3hr> resultsList = null;
        resultsList = k3hrDao.getRangeK3hr(station, cal.getTime(), epTime); // 1

        return resultsList;
    }

    /*
     * from geomag_k3hr
     */
    public static List<GeoMagK3hr> retrieveSingleK3hr(String dataUri,
            Date epTime) {
        GeoMagK3hrDao k3hrDao = new GeoMagK3hrDao();
        String station = CalcUtil.getStationFromUri(dataUri);

        List<GeoMagK3hr> resultsList = null;
        resultsList = k3hrDao.getSingleK3hr(station, epTime);

        return resultsList;
    }

    /*
     * sort n lists
     */
    @SuppressWarnings({ "unchecked", "rawtypes" })
    public static void sort(List... lists) {
        assert lists.length > 0;

        Object[][] objects = new Object[lists[0].size()][lists.length];

        for (int i = 0; i < lists.length; i++) {
            int j = 0;
            for (Object object : lists[i]) {
                objects[j++][i] = object;
            }
        }

        Arrays.sort(objects, new Comparator<Object[]>() {

            public int compare(Object[] o1, Object[] o2) {
                return ((Comparable) o1[0]).compareTo(o2[0]);
            }
        });

        for (int i = 0; i < lists.length; i++) {
            lists[i].clear();
            for (Object[] tuple : objects) {
                lists[i].add(tuple[i]);
            }
        }
    }

    /*
     * fill time tag gaps, return fullBestList
     */
    public static void fillHrAvgTimeGaps(List<GeoMagAvg> dataList,
            List<Date> dateListFinal, List<Float> hHrAvgListFinal,
            List<Float> dHrAvgListFinal, Date spTime) {
        List<Date> dateList = new ArrayList<Date>();
        List<Float> hHrAvgList = new ArrayList<Float>();
        List<Float> dHrAvgList = new ArrayList<Float>();

        for (int i = 0; i < dataList.size(); i++) { // 1 extra

            GeoMagAvg row = dataList.get(i);

            dateList.add((Date) row.getAvgTime());
            hHrAvgList.add((Float) row.gethHrAvg());
            dHrAvgList.add((Float) row.getdHrAvg());

        }

        DatabaseUtil.sort(dateList, hHrAvgList, dHrAvgList);

        /*
         * fill missing
         */

        // fill missing in the beginning
        Date date = (Date) dateList.get(0);
        int hr0 = date.getHours();

        if (hr0 != spTime.getHours()) {
            for (int k = 0; k < hr0; k++) {

                Date dateNew = (Date) date.clone();
                dateNew.setHours(k); // change setMinutes to setHours

                dateListFinal.add(dateNew);
                hHrAvgListFinal.add(MISSING_VAL);
                dHrAvgListFinal.add(MISSING_VAL);
            }
        }

        // fill missing in the middle
        for (int i = 0; i < dateList.size(); i++) {
            Date date0 = dateList.get(i);
            dateListFinal.add(date0); // change from data to data0
            hHrAvgListFinal.add(hHrAvgList.get(i));
            dHrAvgListFinal.add(dHrAvgList.get(i));

            if (i + 1 < dateList.size()) {
                Date date1 = (Date) dateList.get(i + 1);
                int diffHr = (int) (date1.getTime() - date0.getTime())
                        / (3600 * 1000);

                if (diffHr != 1) {
                    for (int j = 0; j < diffHr - 1; j++) {
                        dateListFinal.add(new Date(date0.getTime() + 3600
                                * 1000 * (j + 1)));
                        // append after i, i+1
                        hHrAvgListFinal.add(MISSING_VAL);
                        dHrAvgListFinal.add(MISSING_VAL);

                    }
                }
            }
        }

        // fill missing in the end // changed ending from 24 * AVG_DATA_RANGE to
        // 23(end of the day)
        int latest = dateListFinal.size();
        if (latest < 24 * AVG_DATA_RANGE) {
            for (int k = latest; k < 24 * AVG_DATA_RANGE; k++) {
                dateListFinal.add(new Date(dateListFinal.get(latest - 1)
                        .getTime() + 3600 * 1000 * (k + 1)));
                hHrAvgListFinal.add(MISSING_VAL);
                dHrAvgListFinal.add(MISSING_VAL);
            }
        }

    }

}
