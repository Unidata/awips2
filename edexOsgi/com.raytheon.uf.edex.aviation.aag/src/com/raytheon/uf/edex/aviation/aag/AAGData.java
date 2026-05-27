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

/**
 * Data used to generate an Alaska Aviation Guidance (AAG) product. Roughly
 * equivalent to a single line in a TAF
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 23, 2017 6110       tgurney     Initial creation
 * Oct 17, 2018 7539       tgurney     Update description of sky part
 *
 * </pre>
 *
 * @author tgurney
 */

public class AAGData {
    private Long timeFromSeconds;

    private Long timeToSeconds;

    private String forecastType;

    private String wind;

    private String visibility;

    private String weather;

    private String sky;

    public Long getTimeFromSeconds() {
        return timeFromSeconds;
    }

    public void setTimeFromSeconds(Long timeFromSeconds) {
        this.timeFromSeconds = timeFromSeconds;
    }

    public Long getTimeToSeconds() {
        return timeToSeconds;
    }

    public void setTimeToSeconds(Long timeToSeconds) {
        this.timeToSeconds = timeToSeconds;
    }

    /**
     * @return The forecast type ("FM", "TEMPO" or "PROB"). "BECMG" is not used
     */
    public String getForecastType() {
        return forecastType;
    }

    public void setForecastType(String forecast) {
        this.forecastType = forecast;
    }

    /**
     * @return Wind string (e.g. "31005KT")
     */
    public String getWind() {
        return wind;
    }

    public void setWind(String wind) {
        this.wind = wind;
    }

    /**
     * @return Visibility string (e.g. "1 1/2SM")
     */
    public String getVisibility() {
        return visibility;
    }

    public void setVisibility(String visibility) {
        this.visibility = visibility;
    }

    /**
     * @return Space-separated list of significant weather (e.g. "-SN BR")
     */
    public String getWeather() {
        return weather;
    }

    public void setWeather(String weather) {
        this.weather = weather;
    }

    /**
     * @return Sky conditions (e.g. "BKN150"). Will not have multiple levels.
     *         Either cloud part or ceiling part may not be present (leaving
     *         only e.g. "BKN" or "150"
     */
    public String getSky() {
        return sky;
    }

    public void setSky(String sky) {
        this.sky = sky;
    }
}
