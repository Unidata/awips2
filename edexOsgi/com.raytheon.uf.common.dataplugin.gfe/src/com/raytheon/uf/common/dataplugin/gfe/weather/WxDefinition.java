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

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * WxDefinition defining the contents of a WeatherKey.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 10, 2011      #8156 randerso    Re-ported from AWIPS 1
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */

@DynamicSerialize
public class WxDefinition {
    @DynamicSerializeElement
    private List<WeatherType> weatherTypes;

    @DynamicSerializeElement
    private List<WeatherVisibility> weatherVisibilities;

    /**
     * Constructor for WxDefinition class taking a SeqOf<WeatherType> and a
     * 
     * @param weatherTypes
     * @param weatherVisibilities
     */
    public WxDefinition(List<WeatherType> weatherTypes,
            List<WeatherVisibility> weatherVisibilities) {
        this.weatherTypes = weatherTypes;
        this.weatherVisibilities = weatherVisibilities;
    }

    /**
     * Default constructor for WxDefinition.
     * 
     */
    public WxDefinition() {
        this.weatherTypes = new ArrayList<WeatherType>();
        this.weatherVisibilities = new ArrayList<WeatherVisibility>();
    }

    /**
     * @return the weatherTypes
     */
    public List<WeatherType> getWeatherTypes() {
        return weatherTypes;
    }

    /**
     * @param weatherTypes
     *            the weatherTypes to set
     */
    public void setWeatherTypes(List<WeatherType> weatherTypes) {
        this.weatherTypes = weatherTypes;
    }

    /**
     * @return the weatherVisibilities
     */
    public List<WeatherVisibility> getWeatherVisibilities() {
        return weatherVisibilities;
    }

    /**
     * @param weatherVisibilities
     *            the weatherVisibilities to set
     */
    public void setWeatherVisibilities(
            List<WeatherVisibility> weatherVisibilities) {
        this.weatherVisibilities = weatherVisibilities;
    }

    /**
     * Function to find the index matching the given type.
     * 
     * @param wxType
     * @return -1 if not found
     */
    public int typeIndex(String wxType) {
        for (int i = 0; i < weatherTypes.size(); i++) {
            if (weatherTypes.get(i).getSymbol().equals(wxType)) {
                return i;
            }
        }
        return -1;
    }

    /**
     * Function to find the index matching the given coverage.
     * 
     * @param wxType
     * @param cov
     * @return -1 if not found.
     */
    public int coverageIndex(String wxType, String cov) {
        for (WeatherType weatherType : weatherTypes) {
            if (weatherType.getSymbol().equals(wxType)) {
                for (int j = 0; j < weatherType.getWeatherCoverages().size(); j++) {
                    if (weatherType.getWeatherCoverages().get(j).getSymbol()
                            .equals(cov)) {
                        return j;
                    }
                }
            }
        }
        return -1;
    }

    /**
     * Function to find the index matching the given intensity.
     * 
     * @param wxType
     * @param inten
     * @return -1 if not found.
     */
    public int intensityIndex(String wxType, String inten) {
        for (WeatherType weatherType : weatherTypes) {
            if (weatherType.getSymbol().equals(wxType)) {
                for (int j = 0; j < weatherType.getWeatherIntensities().size(); j++) {
                    if (weatherType.getWeatherIntensities().get(j).getSymbol()
                            .equals(inten)) {
                        return j;
                    }
                }
            }
        }
        return -1;
    }

    /**
     * Function to find the index matching the given attribute.
     * 
     * @param wxType
     * @param attr
     * @return -1 if not found.
     */
    public int attributeIndex(String wxType, String attr) {
        for (WeatherType weatherType : weatherTypes) {
            if (weatherType.getSymbol().equals(wxType)) {
                for (int j = 0; j < weatherType.getWeatherAttributes().size(); j++) {
                    if (weatherType.getWeatherAttributes().get(j).getSymbol()
                            .equals(attr)) {
                        return j;
                    }
                }
            }
        }
        return -1;
    }

    /**
     * Function to find the index matching the given visibility.
     * 
     * @param vis
     * @return -1 if not found.
     */
    public int visibilityIndex(String vis) {
        for (int i = 0; i < weatherVisibilities.size(); i++) {
            if (weatherVisibilities.get(i).getSymbol().equals(vis)) {
                return i;
            }
        }
        return -1;
    }

    /**
     * Function to find the symbol given a type index.
     * 
     * @param typeIndex
     * @return "<Invalid>" if not found.
     */
    public String typeSymbol(int typeIndex) {
        // The index is bigger or equal to the length of the array
        if (weatherTypes.size() <= typeIndex) {
            return "<Invalid>";
        } else {
            return weatherTypes.get(typeIndex).getSymbol();
        }
    }

    /**
     * Function to find the symbol given a coverage index.
     * 
     * @param typeIndex
     * @param covIndex
     * @return "<Invalid>" if not found.
     */
    public String coverageSymbol(int typeIndex, int covIndex) {
        // The index is bigger than the length of the array
        if (weatherTypes.size() <= typeIndex) {
            return "<Invalid>";
        } else {
            if (weatherTypes.get(typeIndex).getWeatherCoverages().size() <= covIndex) {
                return "<Invalid>";
            } else {
                return weatherTypes.get(typeIndex).getWeatherCoverages()
                        .get(covIndex).getSymbol();
            }
        }
    }

    /**
     * Function to find the symbol given a intensity index.
     * 
     * @param typeIndex
     * @param intenIndex
     * @return "<Invalid>" if not found.
     */
    public String intensitySymbol(int typeIndex, int intenIndex) {
        // The index is bigger than the length of the array
        if (weatherTypes.size() <= typeIndex) {
            return "<Invalid>";
        } else {
            if (weatherTypes.get(typeIndex).getWeatherIntensities().size() <= intenIndex) {
                return "<Invalid>";
            } else {
                return weatherTypes.get(typeIndex).getWeatherIntensities()
                        .get(intenIndex).getSymbol();
            }
        }
    }

    /**
     * Function to find the symbol given an attibute index.
     * 
     * @param typeIndex
     * @param attrIndex
     * @return "<Invalid>" if not found.
     */
    public String attributeSymbol(int typeIndex, int attrIndex) {
        // The index is bigger than the length of the array
        if (weatherTypes.size() <= typeIndex) {
            return "<Invalid>";
        } else {
            if (weatherTypes.get(typeIndex).getWeatherAttributes().size() <= attrIndex) {
                return "<Invalid>";
            } else {
                return weatherTypes.get(typeIndex).getWeatherAttributes()
                        .get(attrIndex).getSymbol();
            }
        }
    }

    /**
     * Function to find the symbol given a visibility index.
     * 
     * @param visIndex
     * @return "<Invalid>" if not found.
     */
    public String visibilitySymbol(int visIndex) {
        // The index is bigger than the length of the array
        if (weatherVisibilities.size() <= visIndex) {
            return "<Invalid>";
        } else {
            return weatherVisibilities.get(visIndex).getSymbol();
        }
    }

    /**
     * Function to find the type description matching the given type.
     * 
     * @param wxType
     * @return empty String if not found.
     */
    public String typeDesc(String wxType) {
        for (WeatherType weatherType : weatherTypes) {
            if (weatherType.getSymbol().equals(wxType)) {
                return weatherType.getDescription();
            }
        }
        return "";
    }

    /**
     * Function to find the description matching the given coverage
     * 
     * @param wxType
     * @param cov
     * @return empty String if not found.
     */
    public String coverageDesc(String wxType, String cov) {
        for (WeatherType weatherType : weatherTypes) {
            if (weatherType.getSymbol().equals(wxType)) {
                for (WeatherCoverage coverage : weatherType
                        .getWeatherCoverages()) {
                    if (coverage.getSymbol().equals(cov)) {
                        return coverage.getDescription();
                    }
                }
            }
        }
        return "";
    }

    /**
     * Function to find the description matching the given intensity.
     * 
     * @param wxType
     * @param inten
     * @return empty String if not found.
     */
    public String intensityDesc(String wxType, String inten) {
        for (WeatherType weatherType : weatherTypes) {
            if (weatherType.getSymbol().equals(wxType)) {
                for (WeatherIntensity intensity : weatherType
                        .getWeatherIntensities()) {
                    if (intensity.getSymbol().equals(inten)) {
                        return intensity.getDescription();
                    }
                }
            }
        }
        return "";
    }

    /**
     * Function to find the description matching the given attribute.
     * 
     * @param wxType
     * @param attr
     * @return empty String if not found.
     */
    public String attributeDesc(String wxType, String attr) {
        for (WeatherType weatherType : weatherTypes) {
            if (weatherType.getSymbol().equals(wxType)) {
                for (WeatherAttribute attribute : weatherType
                        .getWeatherAttributes()) {
                    if (attribute.getSymbol().equals(attr)) {
                        return attribute.getDescription();
                    }
                }
            }
        }
        return "";
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#hashCode()
     */
    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result
                + ((weatherTypes == null) ? 0 : weatherTypes.hashCode());
        result = prime
                * result
                + ((weatherVisibilities == null) ? 0 : weatherVisibilities
                        .hashCode());
        return result;
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#equals(java.lang.Object)
     */
    @Override
    public boolean equals(Object obj) {
        if (this == obj) {
            return true;
        }
        if (obj == null) {
            return false;
        }
        if (getClass() != obj.getClass()) {
            return false;
        }
        WxDefinition other = (WxDefinition) obj;
        if (weatherTypes == null) {
            if (other.weatherTypes != null) {
                return false;
            }
        } else if (!weatherTypes.equals(other.weatherTypes)) {
            return false;
        }
        if (weatherVisibilities == null) {
            if (other.weatherVisibilities != null) {
                return false;
            }
        } else if (!weatherVisibilities.equals(other.weatherVisibilities)) {
            return false;
        }
        return true;
    }

    /**
     * @return the noAttr String.
     */
    public static String noAttr() {
        return "<NoAttr>";
    }

}
