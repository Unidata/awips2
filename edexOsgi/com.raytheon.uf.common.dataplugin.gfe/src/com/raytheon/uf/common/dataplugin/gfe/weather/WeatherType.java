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
package com.raytheon.uf.common.dataplugin.gfe.weather;

import java.util.ArrayList;
import java.util.List;

import com.raytheon.uf.common.dataplugin.gfe.DiscreteTerm;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * WeatherType defining the contents of a WeatherType.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 14, 2011      #8156 randerso    Re-ported from AWIPS 1
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */

@DynamicSerialize
public class WeatherType extends DiscreteTerm {
    @DynamicSerializeElement
    private List<WeatherCoverage> weatherCoverages;

    @DynamicSerializeElement
    private List<WeatherIntensity> weatherIntensities;

    @DynamicSerializeElement
    private List<WeatherAttribute> weatherAttributes;

    public WeatherType() {
        this.weatherCoverages = new ArrayList<WeatherCoverage>();
        this.weatherIntensities = new ArrayList<WeatherIntensity>();
        this.weatherAttributes = new ArrayList<WeatherAttribute>();
    }

    // -- public
    // ------------------------------------------------------------------
    // WeatherType::WeatherType()
    // Constructor for WeatherType class taking a symbol, description,
    // SeqOf<TextString> weatherCoverages, SeqOf<TextString> weatherIntensities,
    // SeqOf<TextString> weatherAttributes.
    // -- implementation
    // ----------------------------------------------------------
    // -----------------------------------------------------------------------------
    public WeatherType(String symbol, String description,
            List<WeatherCoverage> weatherCoverages,
            List<WeatherIntensity> weatherIntensities,
            List<WeatherAttribute> weatherAttributes) {
        this.symbol = symbol;
        this.description = description;
        this.weatherCoverages = new ArrayList<WeatherCoverage>(weatherCoverages);
        this.weatherIntensities = new ArrayList<WeatherIntensity>(
                weatherIntensities);
        this.weatherAttributes = new ArrayList<WeatherAttribute>(
                weatherAttributes);
    }

    /**
     * @return the weatherCoverages
     */
    public List<WeatherCoverage> getWeatherCoverages() {
        return weatherCoverages;
    }

    /**
     * @return the weatherIntensities
     */
    public List<WeatherIntensity> getWeatherIntensities() {
        return weatherIntensities;
    }

    /**
     * @return the weatherAttributes
     */
    public List<WeatherAttribute> getWeatherAttributes() {
        return weatherAttributes;
    }

    /**
     * @param weatherCoverages
     *            the weatherCoverages to set
     */
    public void setWeatherCoverages(List<WeatherCoverage> weatherCoverages) {
        this.weatherCoverages = weatherCoverages;
    }

    /**
     * @param weatherIntensities
     *            the weatherIntensities to set
     */
    public void setWeatherIntensities(List<WeatherIntensity> weatherIntensities) {
        this.weatherIntensities = weatherIntensities;
    }

    /**
     * @param weatherAttributes
     *            the weatherAttributes to set
     */
    public void setWeatherAttributes(List<WeatherAttribute> weatherAttributes) {
        this.weatherAttributes = weatherAttributes;
    }

    @Override
    public boolean equals(Object obj) {
        throw new UnsupportedOperationException("Not implemented yet");
    }
}
