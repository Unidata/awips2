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
package com.raytheon.uf.edex.site.notify;

import java.util.List;

import com.raytheon.uf.common.site.notify.SiteActivationNotification;

/**
 * TODO Add Description
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 9, 2011            bphillip     Initial creation
 * Feb 15, 2013 1638       mschenke    Moved site activation notifier class into edex.site
 *
 * </pre>
 *
 * @author bphillip
 * @version 1.0	
 */

public class SiteActivateNotifyFilter {

    public boolean isSiteActivateNotification(Object body) {
        Object obj = body;
        if (body instanceof List) {
            List<?> list = (List<?>) body;
            if (list.size() > 0) {
                obj = list.get(0);
            }
        }
        return obj instanceof SiteActivationNotification;
    }
}
