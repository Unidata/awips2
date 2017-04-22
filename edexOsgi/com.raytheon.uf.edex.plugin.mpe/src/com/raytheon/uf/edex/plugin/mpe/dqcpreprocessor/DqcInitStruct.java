package com.raytheon.uf.edex.plugin.mpe.dqcpreprocessor;

import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.List;

import com.raytheon.uf.common.time.util.TimeUtil;

/**
 * DQC preprocessing init structure.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 01, 2016 4623       skorolev    Initial creation
 * 
 * </pre>
 * 
 * @author skorolev
 */

public class DqcInitStruct {

    public int temperatureWindowTime = PreProcConstants.TEMPERATURE_WINDOW_DEFAULT;

    public int temperatureHourWindow = PreProcConstants.MAXMIN_TEMPERATURE_HOUR_WINDOW;

    public List<Date> DateArray = new ArrayList<>();

    public boolean dqcEnding6hourObstimeFlag = PreProcConstants.DQC_PREPROCESSOR_ENDING_OBSTIME_12Z;

    public int[][] hourSlotMap = new int[2][24];

    public int hourSlotX;

    public int localMidnightInZ;

    public int ending6HourObsTime = PreProcConstants.DEFAULT_ENDING_6HOUR_OBS_TIME;

    private static DqcInitStruct instance = new DqcInitStruct();

    /**
     * Constructor
     */
    public DqcInitStruct() {
        /*
         * Determine the what local time is in GMT. For example, in EST, local
         * midnight is 05Z.
         */
        Calendar gmtCal = TimeUtil.newGmtCalendar();
        int gmtHour = gmtCal.get(Calendar.HOUR);
        Calendar localCal = TimeUtil.newCalendar();
        int localHour = localCal.get(Calendar.HOUR);
        if (gmtHour < localHour) {
            gmtHour += 24;
        }
        // Difference should be positive
        this.localMidnightInZ = gmtHour - localHour;
        // Init hourSlotMap
        for (int i = 0; i < 2; i++) {
            for (int j = 0; j < 24; j++) {
                hourSlotMap[i][j] = -1;
            }
        }
        hourSlotX = dqcEnding6hourObstimeFlag ? 1 : 0;
    }

    public static DqcInitStruct getInstance() {
        if (instance == null) {
            instance = new DqcInitStruct();
        }
        return instance;
    }

    /**
     * @return temperatureWindowTime in minutes
     */
    public int getSearchWindow() {
        return temperatureWindowTime;
    }

    public void setWindowTime(int windowTime) {
        this.temperatureWindowTime = windowTime;
    }

    public int getLocalMidnightInZ() {
        return localMidnightInZ;
    }

    public void setLocalMidnightInZ(int localMidnightInZ) {
        this.localMidnightInZ = localMidnightInZ;
    }

    public boolean getDqcEnding6hourObstimeFlag() {
        return this.dqcEnding6hourObstimeFlag;
    }

    public void setDqcEnding6hourObstimeFlag(boolean dqcEnding6hourObstimeFlag) {
        this.dqcEnding6hourObstimeFlag = dqcEnding6hourObstimeFlag;
    }

    public int getTemperatureHourWindow() {
        return this.temperatureHourWindow;
    }

    public void setTemperatureHourWindow(int temperatureHourWindow) {
        this.temperatureHourWindow = temperatureHourWindow;
    }

    public List<Date> getDateArray() {
        return DateArray;
    }

    public void setDateArray(List<Date> dateArray) {
        DateArray = dateArray;
    }

    public int[][] getHourSlotMap() {
        return hourSlotMap;
    }

    public void setHourSlotMap(int[][] hourSlotMap) {
        this.hourSlotMap = hourSlotMap;
    }

    public int getHourSlotX() {
        return hourSlotX;
    }

    public void setHourSlotX(int hourSlotX) {
        this.hourSlotX = hourSlotX;
    }

}
