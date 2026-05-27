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
package com.raytheon.viz.gfe.localization.util;

import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.LocalizationUtil;

/**
 * An implementation of AbstractScriptUtil for making new Parameter Info files.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Aug 11, 2016  5816     randerso  Inital creation
 * 
 * </pre>
 * 
 * @author mpduff
 */

public class ParameterInfoUtil extends AbstractScriptUtil {

    private static final String TEMPLATE_FILENAME = "parameterInfo.vm";

    private static final String SCRIPT_TYPE = "ParameterInfo";

    private static final String SCRIPT_PATH = LocalizationUtil.join("grid",
            "parameterInfo");

    /**
     * Constructor.
     */
    public ParameterInfoUtil() {
        super();
    }

    @Override
    public String getScriptType() {
        return SCRIPT_TYPE;
    }

    @Override
    public String getScriptTypePathPrefix() {
        return SCRIPT_PATH;
    }

    @Override
    protected String getVelocityTemplateName() {
        return TEMPLATE_FILENAME;
    }

    @Override
    public LocalizationType getLocalizationType() {
        return LocalizationType.COMMON_STATIC;
    }

    @Override
    public LocalizationLevel getLocalizationLevel() {
        return LocalizationLevel.SITE;
    }

    @Override
    /**
     * Add ".xml" to name if it doesn't end with ".xml" and return the result.
     * 
     * @param name
     *            the undecorated script name
     * @return the decorated name.
     */
    public String normalize(String name) {
        String result = null;
        if (name.endsWith(".xml")) {
            result = name;
        } else {
            result = name + ".xml";
        }
        return result;
    }

}