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
package com.raytheon.uf.edex.aviation.aag;

import java.time.LocalDateTime;
import java.time.ZoneOffset;
import java.time.format.DateTimeFormatter;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.StringJoiner;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

import javax.measure.UnitConverter;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.raytheon.uf.common.dataquery.db.QueryResult;
import com.raytheon.uf.common.dataquery.db.QueryResultRow;
import com.raytheon.uf.common.pointdata.spatial.ObStation;
import com.raytheon.uf.common.time.SimulatedTime;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.edex.database.dao.CoreDao;
import com.raytheon.uf.edex.database.dao.DaoConfig;

import si.uom.NonSI;
import si.uom.SI;
import systems.uom.common.USCustomary;
import tec.uom.se.unit.MetricPrefix;

/**
 * Generate plain-language Alaska Aviation Guidance (AAG) forecasts
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 16, 2017 6110       tgurney     Initial creation
 * May 23, 2017 6110       tgurney     Fix forecast times in truncate()
 * Oct 17, 2018 7539       tgurney     Allow products that have ceiling but no
 *                                     cloud info
 * Jan 21, 2018 7539       tgurney     Fix NPE when retrieving an ObStation with
 *                                     a null name. Added better logging
 *
 * </pre>
 *
 * @author tgurney
 */

public class AAGForecastGenerator {

    /** System property indicating whether AAG is enabled */
    protected static final String AAG_ENABLED_PROPERTY = "aviation.aag.enabled";

    /**
     * Decodes multiples of 10 degrees into a cardinal direction (e.g. 30
     * becomes "WNW")
     */
    private static final String[] WIND_DIRS = new String[] { " ", "N", "NNE",
            "NNE", "NE", "NE", "ENE", "ENE", "E", "E", "E", "ESE", "ESE", "SE",
            "SE", "SSE", "SSE", "S", "S", "S", "SSW", "SSW", "SW", "SW", "WSW",
            "WSW", "W", "W", "W", "WNW", "WNW", "NW", "NW", "NNW", "NNW", "N",
            "N" };

    private static final UnitConverter SM_TO_KM = USCustomary.MILE
            .getConverterTo(MetricPrefix.KILO(SI.METRE));

    private static final UnitConverter KT_TO_MPH = USCustomary.KNOT
            .getConverterTo(USCustomary.MILE_PER_HOUR);

    private static final UnitConverter KT_TO_MPS = USCustomary.KNOT
            .getConverterTo(SI.METRE_PER_SECOND);

    private static final Pattern windPattern = Pattern
            .compile("([0-3]\\d\\d|VRB)(\\d\\d)(?:G(\\d+))?KT");

    /** For vis with fraction only */
    private static final Pattern visPattern = Pattern
            .compile("([0-9])? ?([0-9])/([0-9])SM");

    private static final int NUM_FORECAST_HRS = 6;

    private final Logger logger = LoggerFactory.getLogger(getClass());

    private WxDescriptionProvider wxDescriptions;

    public AAGForecastGenerator(WxDescriptionProvider wxDescriptions) {
        /*
         * This just logs the AAG on/off status. Actual enable/disable is done
         * in the camel routing
         */
        if (Boolean.getBoolean(AAGForecastGenerator.AAG_ENABLED_PROPERTY)) {
            logger.info("AAG generation is enabled.");
        } else {
            logger.info(
                    "AAG is disabled. No AAG products will be created or issued.");
        }
        this.wxDescriptions = wxDescriptions;
    }

    /**
     * @param aagDatas
     * @param hours
     * @return List of forecasts truncated to specified number of hours into the
     *         future
     */
    private List<AAGData> truncate(List<AAGData> aagDatas, int hours) {
        long now = SimulatedTime.getSystemTime().getMillis()
                / TimeUtil.MILLIS_PER_SECOND;
        /*
         * startTime is the FROM time of the first forecast. It is the current
         * time rounded to the nearest hour
         */
        long startTime;
        long nowModuloHour = now % TimeUtil.SECONDS_PER_HOUR;
        if (nowModuloHour < TimeUtil.SECONDS_PER_HOUR / 2) {
            startTime = now - nowModuloHour;
        } else {
            startTime = now + TimeUtil.SECONDS_PER_HOUR - nowModuloHour;
        }
        long endTime = startTime + hours * TimeUtil.SECONDS_PER_HOUR;
        /* Filter out forecasts that are fully outside the allowed window */
        List<AAGData> rval = aagDatas.stream()
                .filter(aagData -> !(aagData.getTimeFromSeconds() < startTime
                        && aagData.getTimeToSeconds() < startTime))
                .filter(aagData -> !(aagData.getTimeFromSeconds() > endTime
                        && aagData.getTimeToSeconds() > endTime))
                .collect(Collectors.toList());
        /*
         * Adjust forecast times that overlap with the start/end of the window
         */
        for (AAGData aagData : aagDatas) {
            if (aagData.getTimeFromSeconds() < startTime) {
                aagData.setTimeFromSeconds(startTime);
            }
            if (aagData.getTimeToSeconds() > endTime) {
                aagData.setTimeToSeconds(endTime);
            }
        }
        /*
         * Remove forecasts with same start and end time. This is only possible
         * as a result of the above adjustments
         */
        rval = rval.stream()
                .filter(aagData -> (int) (aagData.getTimeFromSeconds()
                        / TimeUtil.SECONDS_PER_MINUTE) != (int) (aagData
                                .getTimeToSeconds()
                                / TimeUtil.SECONDS_PER_MINUTE))
                .collect(Collectors.toList());
        return rval;
    }

    /**
     * @param wind
     *            Wind text from TAF (e.g. "31005KT")
     * @return Plain language description
     */
    private String getWindText(String wind) {
        // direction of 0 = variable
        double direction = 0;
        double speedKts = 0;
        double gustKts = 0;
        String rval = "missing";
        if (wind == null) {
            return rval;
        }
        Matcher m = windPattern.matcher(wind);
        if (m.matches()) {
            if (!"VRB".equals(m.group(1))) {
                direction = Integer.parseInt(m.group(1));
            }
            speedKts = Integer.parseInt(m.group(2));
            if (m.groupCount() == 3 && m.group(3) != null) {
                gustKts = Integer.parseInt(m.group(3));
            }
        }

        if (speedKts == 0) {
            rval = "calm";
        } else {
            if (direction == 0) {
                rval = "variable direction winds";
            } else if (direction > 0 && direction <= 360) {
                int ind = (int) Math.floor(direction / 10);
                rval = String.format("from the %s (%.0f degrees)",
                        WIND_DIRS[ind], direction);
            }
            double speedMph = KT_TO_MPH.convert(speedKts);
            double speedMps = KT_TO_MPS.convert(speedKts);
            rval += String.format(" at %.0f MPH (%.0f knots; %.1f m/s)",
                    speedMph, speedKts, speedMps);
            if (gustKts > 0) {
                double gustMph = KT_TO_MPH.convert(gustKts);
                double gustMps = KT_TO_MPS.convert(gustKts);
                rval += String.format(
                        " gusting to %.0f MPH (%.0f knots; %.1f m/s)", gustMph,
                        gustKts, gustMps);
            }
        }
        return rval;
    }

    /**
     * @param string
     *            Visibility from TAF (e.g. "1 1/2SM")
     * @return Plain language description
     */
    private String getVisText(String string) {
        double parsedVis = 0.0;
        String rval = "missing";
        String visSm;
        String visKm;
        if ("P6SM".equals(string)) {
            rval = "6 or more sm (10+ km)";
        } else if (string != null) {
            if (string.contains("/")) {
                Matcher m = visPattern.matcher(string);
                if (m.matches()) {
                    if (m.group(1) != null) {
                        parsedVis = Integer.parseInt(m.group(1));
                    }
                    parsedVis += Double.parseDouble(m.group(2))
                            / Double.parseDouble(m.group(3));

                }
            } else {
                parsedVis = Integer.parseInt(string.substring(0, 1));
            }
            if (parsedVis >= 3) {
                visSm = String.format("%.0f sm", parsedVis);
                visKm = String.format("%.0f km", SM_TO_KM.convert(parsedVis));
                rval = String.format("%s (%s)", visSm, visKm);
            } else if (parsedVis > 0) {
                visSm = String.format("%.2f sm", parsedVis);
                visKm = String.format("%.2f km", SM_TO_KM.convert(parsedVis));
                rval = String.format("%s (%s)", visSm, visKm);
            } else {
                visSm = String.format("%.0f sm", 0.25);
                visKm = String.format("%.0f km", SM_TO_KM.convert(0.25));
                rval = String.format("less than %s (%s)", visSm, visKm);
            }
        }
        return rval;
    }

    /**
     * @param wx
     *            Weather phenomena from TAF (e.g. "-SN BR")
     * @return Plain language description
     */
    private String getWxText(String wx) {
        if (wx == null || wx.isEmpty()) {
            return "";
        }
        StringJoiner sj = new StringJoiner(", ");

        String[] chunks = wx.split("\\s+");

        for (String chunk : chunks) {
            sj.add(wxDescriptions.getWxDescription(chunk));
        }
        return String.format("%s (%s)", wx, sj.toString());
    }

    /**
     * @param sky
     *            Sky info from TAF (e.g. "BKN150"). One level only
     * @return Plain language description
     */
    private String[] getSkyText(String sky) {
        String cldsPart = "";
        String cldsText = "";
        String ceilPart = "";
        String ceilText = "";
        if (sky != null) {
            if (sky.length() <= 3) {
                // It's either just ceiling or just clouds, figure out which
                try {
                    ceilPart = Integer.toString(Integer.parseInt(sky) * 100);
                } catch (NumberFormatException e) {
                    cldsPart = sky;
                }
            } else {
                cldsPart = sky.substring(0, 3);
                /*
                 * Note that a 'CB' suffix (cumulonimbus), if it exists, is
                 * thrown out here. The original PHP code did not use it.
                 */
                ceilPart = Integer.toString(Integer.parseInt(
                        sky.substring(3, Math.min(6, sky.length()))) * 100);
            }
        }
        switch (cldsPart) {
        case "OVC":
            ceilText = String.format("%s feet AGL", ceilPart);
            cldsText = String.format("overcast cloud deck at %s", ceilText);
            break;

        case "BKN":
            ceilText = String.format("%s feet AGL", ceilPart);
            cldsText = String.format("broken clouds at %s", ceilText);
            break;

        case "SCT":
            cldsText = String.format("scattered clouds at %s feet AGL",
                    ceilPart);
            ceilText = "at least 12,000 feet AGL";
            break;

        case "FEW":
            cldsText = String.format("few clouds at %s feet AGL", ceilPart);
            ceilText = "at least 12,000 feet AGL";
            break;

        case "SKC":
            cldsText = "clear skies";
            ceilText = "at least 12,000 feet AGL";
            break;
        case "":
            cldsText = "missing";
            break;
        default:
            logger.warn("Got unknown clouds code: " + cldsPart);
        }

        if (ceilText.isEmpty()) {
            if (!ceilPart.isEmpty()) {
                if (Integer.parseInt(ceilPart) > 12_000) {
                    ceilText = "at least 12,000 feet AGL";
                } else {
                    ceilText = String.format("%s feet AGL", ceilPart);
                }
            } else {
                ceilText = "unknown";
            }
        }

        return new String[] { ceilText, cldsText };

    }

    private String[] getRowValid(String fcstChange, Long timeFrom,
            Long timeTo) {
        DateTimeFormatter timeFmtr = DateTimeFormatter.ofPattern("HHmm");
        DateTimeFormatter dateFmtr = DateTimeFormatter
                .ofPattern("dd MMMM yyyy");
        String toTime = timeFmtr
                .format(LocalDateTime.ofEpochSecond(timeTo, 0, ZoneOffset.UTC));
        String toDay = dateFmtr
                .format(LocalDateTime.ofEpochSecond(timeTo, 0, ZoneOffset.UTC));

        String toTimeStr = toTime + " UTC " + toDay;

        String fromTime = timeFmtr.format(
                LocalDateTime.ofEpochSecond(timeFrom, 0, ZoneOffset.UTC));
        String fromDay = dateFmtr.format(
                LocalDateTime.ofEpochSecond(timeFrom, 0, ZoneOffset.UTC));

        String fromTimeStr;
        if (fromDay.equals(toDay)) {
            fromTimeStr = fromTime;
        } else {
            fromTimeStr = fromTime + " UTC " + fromDay;
        }
        String fcstChangeStr = "FROM: standard forecast or significant change";
        if ("TEMPO".equals(fcstChange)) {
            fcstChangeStr = "TEMPORARY: The following changes expected for "
                    + "less than half the time period";
        } else if ("PROB".equals(fcstChange)) {
            // Always PROB30. Will not produce PROB40 forecasts
            fcstChangeStr = "PROBABLE: 30 percent likelihood "
                    + "of the following during this time";
        }
        // BECMG is not used for these forecasts
        return new String[] { fromTimeStr + " to " + toTimeStr, fcstChangeStr };
    }

    private String generateForecast(List<AAGData> aagParts) {
        StringJoiner sj = new StringJoiner("\n");
        for (AAGData part : aagParts) {
            String[] rowValid = getRowValid(part.getForecastType(),
                    part.getTimeFromSeconds(), part.getTimeToSeconds());
            sj.add(String.format("Forecast period: %s", rowValid[0]));
            sj.add(String.format("Forecast type: %s", rowValid[1]));
            sj.add(String.format("Winds: %s", getWindText(part.getWind())));
            sj.add(String.format("Visibility: %s",
                    getVisText(part.getVisibility())));
            String[] sky = getSkyText(part.getSky());
            sj.add(String.format("Ceiling: %s", sky[0]));
            sj.add(String.format("Clouds: %s", sky[1]));
            String wx = part.getWeather();
            if (wx != null && !wx.isEmpty()) {
                sj.add(String.format("Weather: %s", getWxText(wx)));
            }
            sj.add("");
        }
        return sj.toString();
    }

    /**
     * For each station ID get the friendly name. e.g. "PANC" becomes "Anchorage
     * Intl, AK"
     *
     * @param stationIds
     * @return Map of station ID to friendly name. If no friendly name was found
     *         for a particular station ID, then that station ID will not be in
     *         the map.
     */
    private Map<String, String> getFriendlyStationNames(
            Collection<String> stationIds) {
        Map<String, String> rval = new HashMap<>(stationIds.size());
        CoreDao dao = new CoreDao(DaoConfig.forClass(ObStation.class));
        Map<String, Object> paramMap = new HashMap<>(1, 1);
        paramMap.put("stationIds", stationIds);
        QueryResult qresult = dao.executeHQLQuery(
                "select stationId, name"
                        + " from ObStation where stationId in :stationIds",
                paramMap);
        for (QueryResultRow row : qresult.getRows()) {
            Object thisStationId = row.getColumn(0);
            Object friendlyName = row.getColumn(1);
            if (friendlyName != null) {
                rval.put(thisStationId.toString(), friendlyName.toString());
            }
        }
        return rval;
    }

    /**
     * @param aagParts
     *            Map of station id to forecast data
     * @return Map of station id to 0-6 hr plain language forecast
     */
    public Map<String, String> generateForecasts(
            Map<String, List<AAGData>> aagParts) {
        String timestamp = DateTimeFormatter.ofPattern("HHmm 'UTC' dd MMM YYYY")
                .format(LocalDateTime.now(ZoneOffset.UTC));
        Map<String, String> rval = new HashMap<>();
        if (!aagParts.isEmpty()) {
            Map<String, String> friendlyNames = getFriendlyStationNames(
                    aagParts.keySet());
            for (Entry<String, List<AAGData>> entry : aagParts.entrySet()) {
                String stationId = entry.getKey();
                String friendlyName = friendlyNames.getOrDefault(stationId, "");
                List<AAGData> truncatedData = truncate(entry.getValue(),
                        NUM_FORECAST_HRS);
                String forecastText = generateForecast(truncatedData);
                String firstLine;
                if (!"".equals(friendlyName)) {
                    String fmt = "Guidance for: %s (%s) issued at %s";
                    firstLine = String.format(fmt, stationId, friendlyName,
                            timestamp);
                } else {
                    String fmt = "Guidance for: %s issued at %s";
                    firstLine = String.format(fmt, stationId, timestamp);
                }
                forecastText = firstLine + "\n\n" + forecastText;
                if (!truncatedData.isEmpty()) {
                    rval.put(entry.getKey(), forecastText);
                } else {
                    logger.info("Threw away all received data for station "
                            + stationId + " because it was" + " too old.");
                }
            }
            if (!rval.isEmpty()) {
                logger.info(
                        "Generated products for " + rval.size() + " stations");
            } else {
                logger.warn(
                        "Threw away all received data because it was too old."
                                + " Has BUFR LAMP data stopped coming in?");
            }
        } else {
            logger.info("No data received");
        }
        return rval;
    }
}
