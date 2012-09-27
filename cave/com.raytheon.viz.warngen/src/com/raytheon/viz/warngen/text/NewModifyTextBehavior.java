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
package com.raytheon.viz.warngen.text;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.texteditor.util.VtecUtil;

/**
 * Updates the VTEC with a new ETN.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 24, 2012   15322    jsanchez     Initial creation
 * 
 * </pre>
 * 
 * @author jsanchez
 * @version 1.0
 */
public class NewModifyTextBehavior implements IModifyTextBehavior {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(NewModifyTextBehavior.class);

    @Override
    public String modify(String text) {
        try {
            text = VtecUtil.getVtec(text);
        } catch (VizException e) {
            statusHandler
                    .handle(Priority.SIGNIFICANT,
                            "WarnGen cannot update the ETN. Please verify the VTEC is valid.",
                            e);
        }

        return text;
    }

}
