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
package com.raytheon.edex.subscription;

import org.hamcrest.Description;
import org.jmock.api.Action;
import org.jmock.api.Invocation;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 	
 * 
 * </pre>
 * 
 * @author mfegan
 * @version 1
 */

public class AddScriptAction implements Action {
    // private Subscription object;
    public static Action addScript(Subscription sub, Object... elements) {
        return new AddScriptAction(sub);
    }

    public AddScriptAction(Subscription object) {
        // this.object = object;
    }

    public void describeTo(Description description) {
        description.appendText("adds a Script to a Subscription");
    }

    public Object invoke(Invocation invocation) throws Throwable {
        String id = (String) invocation.getParameter(1);
        Object script = (Object) invocation.getParameter(2);
        ((Subscription) invocation.getParameter(0)).addScript(id, script);
        return null;
    }
}
