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
/**
 * 
 */
package com.raytheon.viz.gfe.textproduct;

import com.raytheon.uf.common.dataplugin.gfe.python.GfePyIncludeUtil;
import com.raytheon.viz.gfe.core.script.AbstractScriptUtil;

/**
 * An implementation of PythonFileTemplate for making new Text Products.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * ???                     wldougher   Initial creation
 * Jan 19, 2016  4834      njensen     Removed dead method
 *
 * </pre>
 * 
 * @author wldougher
 * 
 */
public class TextProductUtil extends AbstractScriptUtil {

    private static final String TEMPLATE_FILENAME = "textProductSmart.vm";

    private static final String SCRIPT_TYPE = "Text Product";

    @Override
    public String getScriptType() {
        return SCRIPT_TYPE;
    }

    public static String getTextProductScriptType() {
        return SCRIPT_TYPE;
    }

    @Override
    public String getScriptTypePathPrefix() {
        return GfePyIncludeUtil.TEXT_PRODUCTS;
    }

    @Override
    protected String getVelocityTemplateName() {
        return TEMPLATE_FILENAME;
    }

}
