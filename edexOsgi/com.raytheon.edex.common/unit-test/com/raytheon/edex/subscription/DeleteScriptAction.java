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

public class DeleteScriptAction implements Action {
    // private Subscription object;
    public static Action deleteScript(Subscription sub, Object... elements) {
        return new DeleteScriptAction(sub);
    }

    public DeleteScriptAction(Subscription object) {
        // this.object = object;
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.hamcrest.SelfDescribing#describeTo(org.hamcrest.Description)
     */
    public void describeTo(Description description) {
        description.appendText("deletes a Script from a Subscription");
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.jmock.api.Invokable#invoke(org.jmock.api.Invocation)
     */
    public Object invoke(Invocation invocation) throws Throwable {
        String id = (String) invocation.getParameter(1);
        ((Subscription) invocation.getParameter(0)).removeScript(id);
        return null;
    }

}
