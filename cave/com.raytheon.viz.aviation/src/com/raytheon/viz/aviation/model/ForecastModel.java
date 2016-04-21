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

package com.raytheon.viz.aviation.model;

import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.Date;
import java.util.HashMap;
import java.util.Map;
import java.util.SortedMap;
import java.util.TimeZone;

import org.apache.commons.collections.MultiMap;
import org.apache.commons.collections.map.MultiValueMap;

import com.raytheon.uf.common.time.SimulatedTime;
import com.raytheon.viz.aviation.utility.IBackupRestart;
import com.raytheon.viz.aviation.xml.MonitoringRule;

/**
 * ForecastModel class is a singleton class that contains the forecast model for
 * aviation.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 3/14/2008    933         grichard    Initial creation.
 * 5/21/2008    1119        grichard    Added anvObservation to model.
 * 5/22/2008    937         grichard    Added sitesSelected attribute.
 * 5/29/2008    937         grichard    Taf refactor first cut.
 * 5/30/2008    937         grichard    Added Weather Group.
 * 6/3/2008     937         grichard    Added remarks to TAFs.
 * 6/3/2008     937         grichard    Added probability to TAFs.
 * 6/16/2008    937         grichard    Improved viewer/editor interaction.
 * 7/29/2008    1342        grichard    Get calendar instance in UTC time zone.
 * 7/30/2008    1139        grichard    Added TAF Valid Period support.
 * 9/19/2008    1444        grichard    Add Taf wx quality check capability.
 * 12/2/2008    1588        grichard    Updated metar guidance contents.
 * 12/3/2008    1588        grichard    Added wmo header display option for metars.
 * 12/3/2008    1515        grichard    Implement 30 hour TAF modifications.
 * 1/8/2009     1839        grichard    Correct the 30 hour TAF format.
 * 1/15/2009    1816        grichard    Correct TAF to conform to 10-813 NWSI.
 * 5/11/2009    1982        grichard    Added backup/restart monitor feature.
 * 5/13/2009    1982        grichard    Refactor findTheCurrentElement method.
 * 8/18/2009    2838        grichard    Display PROB groups same line as predecessor.
 * 8/19/2009    2836        grichard    PROB and TEMPO groups should not affect MetWatch.
 * 
 * </pre>
 * 
 * @author grichard
 * @version 1.0
 */
public final class ForecastModel {

    /**
     * The static singleton instance.
     */
    private static ForecastModel instance;

    /**
     * Aviation Forecast.
     */
    private Map<String, SortedMap<Calendar, TimeGroup>> avnForecast = new HashMap<String, SortedMap<Calendar, TimeGroup>>();

    /**
     * Aviation TAF Guidance.
     */
    private MultiMap avnTafGuidance = new MultiValueMap();

    /**
     * Aviation Observation.
     */
    private Map<String, TimeGroup> avnObservation = new HashMap<String, TimeGroup>();

    /**
     * Aviation Current Element of Forecast.
     */
    private Map<String, TimeGroup> avnCurrentElement = new HashMap<String, TimeGroup>();

    /**
     * Aviation Persist 1 Hour Element of Forecast.
     */
    private Map<String, TimeGroup> avnPersist1HrElement = new HashMap<String, TimeGroup>();

    /**
     * Aviation Persist 2 Hour Element of Forecast.
     */
    private Map<String, TimeGroup> avnPersist2HrElement = new HashMap<String, TimeGroup>();

    /**
     * Aviation Persist 3 Hour Element of Forecast.
     */
    private Map<String, TimeGroup> avnPersist3HrElement = new HashMap<String, TimeGroup>();

    /**
     * Backup/Restart Utility for backing-up or restarting the TAF Monitor GUI.
     */
    private IBackupRestart backupRestartUtility;

    /**
     * The rule map.
     */
    private final Map<String, Map<String, MonitoringRule>> ruleMap = new HashMap<String, Map<String, MonitoringRule>>();

    public static enum TafOptions {
        LoadTafs, LoadTafsWithHeaders, LoadMetars, LoadAllMetars, LoadMetarsWithHeaders, LoadAllMetarsWithHeaders, LoadDecodedMetars, AdjustTimes, CopyForecasts, UseMetarForPrevailing, WestFlow, LoadGfsMos, LoadGfsLamp, LoadEtaMos, LoadNgmMos, LoadTafGfsLamp, LoadNamWrf, LoadGrids
    }

    /**
     * Singleton constructor.
     * 
     * @return the forecast model.
     */
    public static synchronized ForecastModel getInstance() {
        if (instance == null) {
            instance = new ForecastModel();
        }

        return instance;
    }

    /**
     * Private constructor: Use getInstance().
     */
    private ForecastModel() {
    }

    /**
     * Getter/Accessor of Aviation Forecast.
     * 
     * @param icao
     *            the International Civil Aviation Organization identifier
     * @return avnForecast
     */
    public SortedMap<Calendar, TimeGroup> getAvnForecast(String icao) {
        return avnForecast.get(icao);
    }

    /**
     * Method gets the time now of a forecast.
     * 
     * @return String representing the time now in DDHH format
     */
    public String getTimeNowInDdHhFormat(int deltaHours) {
        Date now = SimulatedTime.getSystemTime().getTime();
        SimpleDateFormat formatter = new SimpleDateFormat("ddHH");
        formatter.setTimeZone(TimeZone.getTimeZone("UTC"));
        Calendar cal = Calendar.getInstance(TimeZone.getTimeZone("UTC"));
        cal.setTime(now);
        cal.add(Calendar.HOUR_OF_DAY, deltaHours);
        return (formatter.format(cal.getTime()));
    }

    /**
     * Method gets the time now of a forecast.
     * 
     * @return String representing the time now in DDHHMM format
     */
    public String getTimeNowInDdHhMmFormat(int deltaHours) {
        Date now = SimulatedTime.getSystemTime().getTime();
        SimpleDateFormat formatter = new SimpleDateFormat("ddHHmm");
        formatter.setTimeZone(TimeZone.getTimeZone("UTC"));
        Calendar cal = Calendar.getInstance(TimeZone.getTimeZone("UTC"));
        cal.setTime(now);
        cal.add(Calendar.HOUR_OF_DAY, deltaHours);
        return (formatter.format(cal.getTime()));
    }

    /**
     * Getter for backup/restart utility.
     * 
     * @return the backup/restart utility
     */
    public IBackupRestart getBackupRestartUtility() {
        return backupRestartUtility;
    }

    /**
     * Setter for backup/restart utility.
     * 
     * @param backupRestartUtility
     */
    public void setBackupRestartUtility(IBackupRestart backupRestartUtility) {
        this.backupRestartUtility = backupRestartUtility;
    }

}
