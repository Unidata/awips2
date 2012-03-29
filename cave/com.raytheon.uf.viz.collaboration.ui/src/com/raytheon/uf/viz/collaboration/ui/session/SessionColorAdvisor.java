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
package com.raytheon.uf.viz.collaboration.ui.session;

import java.util.HashMap;
import java.util.Map;

import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.widgets.Display;

import com.raytheon.uf.viz.collaboration.data.DataUser.RoleType;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 6, 2012            mnash     Initial creation
 * 
 * </pre>
 * 
 * @author mnash
 * @version 1.0
 */

public class SessionColorAdvisor {
    private static Map<RoleType, Color> colors = null;

    public static Color getColor(RoleType[] type, boolean isSelf) {
        if (colors == null) {
            colors = new HashMap<RoleType, Color>();
            colors.put(RoleType.LEADER,
                    Display.getCurrent().getSystemColor(SWT.COLOR_BLUE));
            colors.put(RoleType.DATA_PROVIDER, Display.getCurrent()
                    .getSystemColor(SWT.COLOR_RED));
            colors.put(RoleType.PARTICIPANT, Display.getCurrent()
                    .getSystemColor(SWT.COLOR_DARK_GREEN));
        }
        if (isSelf) {
            return Display.getCurrent().getSystemColor(SWT.COLOR_BLACK);
        }
        RoleType rType = null;
        if (type == null || type.length == 0) {
            rType = RoleType.PARTICIPANT;
        } else if (type.length == 1) {
            rType = type[0];
        } else {
            rType = RoleType.PARTICIPANT;
            for (RoleType rt : type) {
                if (rt == RoleType.DATA_PROVIDER) {
                    rType = rt;
                    break;
                }
                if (rt == RoleType.LEADER) {
                    rType = rt;
                }
            }
        }

        return colors.get(rType);
    }
}
