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

import com.raytheon.uf.common.dataplugin.gfe.python.GfePyIncludeUtil;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;

/**
 * An implementation of AbstractScriptUtil for making new Smart Tools.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Aug 11, 2016  5816     randerso  Initial creation
 * 
 * </pre>
 * 
 * @author randerso
 */

public class SmartToolUtil extends AbstractScriptUtil {

    private static final String TEMPLATE_FILENAME = "smartTool.vm";

    private static final String SCRIPT_TYPE = "Smart Tool";

    /**
     * Default Constructor
     */
    public SmartToolUtil() {
        super();
    }

    @Override
    public String getScriptType() {
        return SCRIPT_TYPE;
    }

    @Override
    public String getScriptTypePathPrefix() {
        return GfePyIncludeUtil.SMART_TOOLS;
    }

    @Override
    protected String getVelocityTemplateName() {
        return TEMPLATE_FILENAME;
    }

    @Override
    public LocalizationType getLocalizationType() {
        return LocalizationType.CAVE_STATIC;
    }

    @Override
    public LocalizationLevel getLocalizationLevel() {
        return LocalizationLevel.USER;
    }
}
