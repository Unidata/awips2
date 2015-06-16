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
package com.raytheon.uf.viz.collaboration.ui;

import org.jivesoftware.smack.RosterEntry;

import com.raytheon.uf.viz.collaboration.comm.identity.ISession;
import com.raytheon.uf.viz.collaboration.comm.provider.user.UserId;
import com.raytheon.viz.ui.widgets.AbstractVizTreeFilter;

/**
 * Filters contact list tree according to a substring filter.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 13, 2012            mnash     Initial creation
 * May 20, 2014 3172       bclement  fixed filtering for contacts and sessions
 * Jun 16, 2015 4401       bkowal    Updated to extend {@link AbstractVizTreeFilter}.
 * 
 * </pre>
 * 
 * @author mnash
 * @version 1.0
 */

public class UsersTreeFilter extends AbstractVizTreeFilter {

    @Override
    protected boolean shouldFilter(Object element) {
        return (element instanceof UserId || element instanceof RosterEntry || element instanceof ISession);
    }
}