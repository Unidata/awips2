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
 * An implementation of AbstractScriptUtil for overriding server config files.
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

public class ServerConfigUtil extends AbstractScriptUtil {

    private String scriptType;

    private String scriptPathPrefix;

    private String scriptTemplate;

    private String overrideFilename;

    /**
     * Constructor
     * 
     * @param path
     *            the path of the Server Config file being overidden
     */
    public ServerConfigUtil(String path) {
        super();

        scriptPathPrefix = LocalizationUtil.getParent(path);
        String filename = LocalizationUtil.extractName(path);

        // Determine override file name
        if ("serverConfig.py".equals(filename)) {
            overrideFilename = "localConfig.py";
        } else {
            overrideFilename = "local" + filename;
        }

        scriptType = overrideFilename.replace(".py", "");
        scriptTemplate = overrideFilename.replace(".py", ".vm");

    }

    @Override
    public String getScriptType() {
        return scriptType;
    }

    @Override
    public String getScriptTypePathPrefix() {
        return scriptPathPrefix;
    }

    @Override
    protected String getVelocityTemplateName() {
        return scriptTemplate;
    }

    @Override
    public LocalizationType getLocalizationType() {
        return LocalizationType.COMMON_STATIC;
    }

    @Override
    public LocalizationLevel getLocalizationLevel() {
        return LocalizationLevel.SITE;
    }

    /**
     * @return the override file name
     */
    public String getOverrideFilename() {
        return overrideFilename;
    }

}
