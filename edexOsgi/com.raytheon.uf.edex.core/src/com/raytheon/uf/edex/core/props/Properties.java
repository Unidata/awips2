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

package com.raytheon.uf.edex.core.props;

import java.util.List;

import org.apache.commons.configuration.Configuration;


public class Properties extends EnvProperties {

    protected Configuration thePluginConfiguration;

    public Properties(Configuration aPluginConfiguration,
            Configuration anAttrNamesConfiguration,
            Configuration anEnvConfiguration) {
        super(anAttrNamesConfiguration, anEnvConfiguration);
        this.thePluginConfiguration = aPluginConfiguration;
    }

    public String getPluginValue(String attributeName) {
        return getValue(attributeName, null);
    }

    public String getPluginValue(String attributeName, String qualifier) {
        return getValue(attributeName, qualifier);
    }

    private String getValue(String propertyName, String qualifier) {
        String value = null;
        String property = null;

        property = theAttrNamesConfiguration.getString(propertyName, "");
        if (!"".equals(property)) {
            if (qualifier != null)
                value = thePluginConfiguration.getString(property + "_"
                        + qualifier, "");
            else
                value = thePluginConfiguration.getString(property, "");
        } else {
            value = thePluginConfiguration.getString(propertyName, "");
        }

        return value;
    }

    public String getPluginProperty(String propertyName) {

        String property = thePluginConfiguration.getString(propertyName);

        return property;
    }

    @SuppressWarnings("unchecked")
    public List<String> getPluginPropertyList(String propertyName) {

        List<String> property = thePluginConfiguration.getList(propertyName);

        return property;
    }

    public void setPluginConfiguration(Configuration aPluginConfiguration) {
        thePluginConfiguration = aPluginConfiguration;
    }

}
