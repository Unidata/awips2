package gov.noaa.nws.ncep.viz.resources.time_match;

/**
 * Time matching for Natl Cntrs is based on the dominant source. The data times
 * defined by it are the times for all of the resources. Other resources will
 * need to time match their data to this list of data times.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 05/02/14     #1131      qzhou    Initial creation
 * 
 * </pre>
 * 
 * @author qzhou
 * @version 1.0
 */
import java.util.Calendar;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;

import com.raytheon.uf.common.time.DataTime;

public class GraphTimelineUtil {
    /*
     * Convert UTC calendar time to next hourSnap point
     */
    public static Calendar snapTimeToNext(Calendar refTimeCal, int snap) {

        Calendar dup = (Calendar) refTimeCal.clone();
        if (snap == 0)
            return dup;

        int hourOfDay = refTimeCal.get(Calendar.HOUR_OF_DAY);
        int hour = refTimeCal.get(Calendar.HOUR);
        int min = refTimeCal.get(Calendar.MINUTE);
        boolean flag = true;

        if (hourOfDay != hour)
            flag = false;

        if (!(hourOfDay % snap == 0 && min == 0))
            hourOfDay = (hourOfDay / snap) * snap + snap;

        dup.set(Calendar.HOUR_OF_DAY, hourOfDay);
        if (flag)
            dup.set(Calendar.HOUR, hourOfDay);
        else
            dup.set(Calendar.HOUR, hourOfDay - 12);

        dup.set(Calendar.MINUTE, 0);

        return dup;
    }

    /*
     * Convert UTC calendar time to next hourSnap point
     */
    public static Calendar snapTimeToPrevious(Calendar refTimeCal, int snap) {

        Calendar dup = (Calendar) refTimeCal.clone();
        if (snap == 0)
            return dup;

        int hourOfDay = refTimeCal.get(Calendar.HOUR_OF_DAY);
        int hour = refTimeCal.get(Calendar.HOUR);
        int min = refTimeCal.get(Calendar.MINUTE);
        boolean flag = true;

        if (hourOfDay != hour)
            flag = false;

        if (!(hourOfDay % snap == 0 && min == 0))
            hourOfDay = (hourOfDay / snap) * snap;

        dup.set(Calendar.HOUR_OF_DAY, hourOfDay);
        if (flag)
            dup.set(Calendar.HOUR, hourOfDay);
        else
            dup.set(Calendar.HOUR, hourOfDay - 12);

        dup.set(Calendar.MINUTE, 0);

        return dup;
    }

    /*
     * Convert UTC calendar time to closest hourSnap point
     */
    public static Calendar snapTimeToClosest(Calendar refTimeCal, int snap) {

        Calendar dup = (Calendar) refTimeCal.clone();
        if (snap == 0)
            return dup;

        int hourOfDay = refTimeCal.get(Calendar.HOUR_OF_DAY);
        int hour = refTimeCal.get(Calendar.HOUR);
        int min = refTimeCal.get(Calendar.MINUTE);
        boolean flag = true;

        if (hourOfDay != hour)
            flag = false;

        if (!(hourOfDay % snap == 0 && min == 0)) {
            if (hourOfDay % snap == 0 || (hourOfDay % snap == 1 && min < 30))
                hourOfDay = (hourOfDay / snap) * snap;
            else
                hourOfDay = (hourOfDay / snap) * snap + snap;
        }

        dup.set(Calendar.HOUR_OF_DAY, hourOfDay);
        if (flag)
            dup.set(Calendar.HOUR, hourOfDay);
        else
            dup.set(Calendar.HOUR, hourOfDay - 12);

        dup.set(Calendar.MINUTE, 0);

        return dup;
    }

    /*
     * Move UTC selected calendar time to previous hourSnap point
     */
    // public static Calendar slideSelectedTimeToRightBysnap(Calendar
    // refTimeCal,
    // int snap) {
    //
    // }

    /**
     * sort on datatime
     */
    public static void sortAvailableData(List<DataTime> timeList) {// quan
        Collections.sort(timeList, new Comparator<DataTime>() {

            @Override
            public int compare(DataTime t1, DataTime t2) {
                return t1.compareTo(t2);
            }
        });
    }

    /**
     * sort on datatime
     */
    public static void sortAvailableCalendar(List<Calendar> timeList) {// quan
        Collections.sort(timeList, new Comparator<Calendar>() {

            @Override
            public int compare(Calendar t1, Calendar t2) {
                return t1.compareTo(t2);
            }
        });
    }
}
