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
package com.raytheon.edex.plugin.gfe.config;

import java.util.List;

import com.raytheon.uf.common.dataplugin.gfe.weather.WeatherAttribute;
import com.raytheon.uf.common.dataplugin.gfe.weather.WeatherCoverage;
import com.raytheon.uf.common.dataplugin.gfe.weather.WeatherIntensity;

/**
 * Configuration class for Weather Types
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 03/12/08     #1030      randerso    Initial port
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */

public class SimpleWeatherTypeConfig {
    // type symbol
    String symbol;

    // type description
    String description;

    // type coverages
    List<WeatherCoverage> coverages;

    // type intensities
    List<WeatherIntensity> intensities;

    // type attributes
    List<WeatherAttribute> attributes;

    public SimpleWeatherTypeConfig(String symbol, String description,
            List<WeatherCoverage> coverages,
            List<WeatherIntensity> intensities,
            List<WeatherAttribute> attributes) {
        this.symbol = symbol;
        this.description = description;
        this.coverages = coverages;
        this.intensities = intensities;
        this.attributes = attributes;
    }
}
