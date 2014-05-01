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
package com.raytheon.viz.gfe.dialogs;

import com.raytheon.uf.common.dataplugin.gfe.python.GfePyIncludeUtil;
import com.raytheon.viz.gfe.core.script.AbstractScriptUtil;

/**
 * TODO Add Description
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 8, 2010            mpduff     Initial creation
 *
 * </pre>
 *
 * @author mpduff
 * @version 1.0	
 */

public class ConfigUtil extends AbstractScriptUtil {
    private static final String TEMPLATE_FILENAME = "config.vm";

    private static final String scriptType = "Config";

    /**
     * Constructor.
     */
    public ConfigUtil() {
        super();
    }

    /**
     * @see com.raytheon.viz.gfe.core.script.IScriptUtil#getScriptType()
     */
    @Override
    public String getScriptType() {
        return scriptType;
    }

    /**
     * @see com.raytheon.viz.gfe.core.script.AbstractScriptUtil#getScriptTypePathPrefix
     *      ()
     */
    @Override
    public String getScriptTypePathPrefix() {
        return GfePyIncludeUtil.CONFIG;
    }

    /**
     * @see com.raytheon.viz.gfe.core.script.AbstractScriptUtil#getVelocityTemplateName
     *      ()
     */
    @Override
    protected String getVelocityTemplateName() {
        return TEMPLATE_FILENAME;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.gfe.core.script.IScriptUtil#getFileType()
     */
    @Override
    public String getFileType() {
        return "CONFIG";
    }
}