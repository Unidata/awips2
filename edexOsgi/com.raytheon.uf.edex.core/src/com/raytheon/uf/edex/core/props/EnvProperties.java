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

import org.apache.commons.configuration.Configuration;

public class EnvProperties {

    protected Configuration theAttrNamesConfiguration;

    protected Configuration theEnvConfiguration;

    public EnvProperties(Configuration anAttrNamesConfiguration,
            Configuration anEnvConfiguration) {
        this.theAttrNamesConfiguration = anAttrNamesConfiguration;
        this.theEnvConfiguration = anEnvConfiguration;
    }

    public String getAttributeValue(String propertyName) {
        String value = null;

        value = theAttrNamesConfiguration.getString(propertyName);

        return value;
    }

    public String getEnvValue(String propertyName) {
        String value = null;
        String property = null;

        property = theAttrNamesConfiguration.getString(propertyName);

        if (property != null)
            value = theEnvConfiguration.getString(property);

        return value;
    }

    public void setAttributeNamesConfiguration(
            Configuration anAttrNamesConfiguration) {
        theAttrNamesConfiguration = anAttrNamesConfiguration;
    }

    public void setEnvironmentConfiguration(Configuration anEnvConfiguration) {
        theEnvConfiguration = anEnvConfiguration;
    }

}
