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
package com.raytheon.uf.edex.plugin.mpe.dqcpreprocessor;

import java.nio.file.Path;
import java.util.Calendar;
import java.util.Set;

import com.raytheon.uf.common.mpe.dqcpreprocessor.DQCPreProcRunConfiguration;
import com.raytheon.uf.edex.plugin.mpe.dqcpreprocessor.output.TemperatureBasetime;

/**
 * Container for configuration information that is used throughout the DQC
 * PreProcessor.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 24, 2018 7184       bkowal      Initial creation
 * Apr 08, 2018 7184       bkowal      {@link DQCPreProcRunConfiguration} moved to common.
 *
 * </pre>
 *
 * @author bkowal
 */

public class DQCPreProcConfig {

    private final DQCPreProcRunConfiguration runConfig;

    private final Path stationListPath;

    private final Path pointPrecipPath;

    private final Path pointTemperaturePath;

    private final String siteId;

    private final Set<String> areaNames;

    private final Boolean loadHourlyPC;

    /*
     * In minutes.
     */
    private final long temperatureWindow;

    private final TemperatureBasetime temperatureBasetime;

    private final Calendar startDate;

    private final Calendar endDate;

    public DQCPreProcConfig(final DQCPreProcRunConfiguration runConfig,
            final Path stationListPath, final Path pointPrecipPath,
            final Path pointTemperaturePath, final String siteId,
            final Set<String> areaNames,
            final Boolean loadHourlyPC, final long temperatureWindow,
            final TemperatureBasetime temperatureBasetime, Calendar startDate,
            Calendar endDate) {
        this.runConfig = runConfig;
        this.stationListPath = stationListPath;
        this.pointPrecipPath = pointPrecipPath;
        this.pointTemperaturePath = pointTemperaturePath;
        this.siteId = siteId;
        this.areaNames = areaNames;
        this.loadHourlyPC = loadHourlyPC;
        this.temperatureWindow = temperatureWindow;
        this.temperatureBasetime = temperatureBasetime;
        this.startDate = startDate;
        this.endDate = endDate;
    }

    /**
     * @return the runConfig
     */
    public DQCPreProcRunConfiguration getRunConfig() {
        return runConfig;
    }

    /**
     * @return the stationListPath
     */
    public Path getStationListPath() {
        return stationListPath;
    }

    /**
     * @return the pointPrecipPath
     */
    public Path getPointPrecipPath() {
        return pointPrecipPath;
    }

    /**
     * @return the pointTemperaturePath
     */
    public Path getPointTemperaturePath() {
        return pointTemperaturePath;
    }

    /**
     * @return the siteId
     */
    public String getSiteId() {
        return siteId;
    }

    /**
     * @return the areaNames
     */
    public Set<String> getAreaNames() {
        return areaNames;
    }

    /**
     * @return the loadHourlyPC
     */
    public Boolean getLoadHourlyPC() {
        return loadHourlyPC;
    }

    /**
     * @return the temperatureWindow
     */
    public long getTemperatureWindow() {
        return temperatureWindow;
    }

    /**
     * @return the temperatureBasetime
     */
    public TemperatureBasetime getTemperatureBasetime() {
        return temperatureBasetime;
    }

    /**
     * @return the startDate
     */
    public Calendar getStartDate() {
        return startDate;
    }

    /**
     * @return the endDate
     */
    public Calendar getEndDate() {
        return endDate;
    }
}