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
package com.raytheon.viz.gfe.localization.util;

/**
 * An implementation of AbstractScriptUtil for making new Text Products
 * 
 * This class creates new text product scripts from the "table" Velocity
 * template rather than the "smart" template.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer   Description
 * ------------- -------- ---------- -------------------------------------------
 * ???                    wldougher  Initial creation
 * Aug 11, 2016  5816     randerso   Moved to gfe.localization.util.
 *                                   Code cleanup
 * 
 * </pre>
 * 
 * @author wldougher
 */
public class TextProductTableUtil extends TextProductUtil {

    private static final String TEMPLATE_FILENAME = "textProductTable.vm";

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.gfe.textproduct.TextProductUtil#getVelocityTemplateName
     * ()
     */
    @Override
    protected String getVelocityTemplateName() {
        return TEMPLATE_FILENAME;
    }
}
