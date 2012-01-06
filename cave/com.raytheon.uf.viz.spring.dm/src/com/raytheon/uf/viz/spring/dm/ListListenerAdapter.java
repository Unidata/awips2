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
package com.raytheon.uf.viz.spring.dm;

import java.util.Iterator;
import java.util.List;

import org.springframework.osgi.context.event.OsgiBundleApplicationContextEvent;
import org.springframework.osgi.context.event.OsgiBundleApplicationContextListener;
import org.springframework.util.Assert;

/**
 * Direct copy of
 * org.springframework.osgi.extender.internal.activator.ListListenerAdapter.
 * Needed for copy of ContextLoaderListener (Activator) since spring class is
 * package level visibility only
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 12, 2010            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

class ListListenerAdapter implements OsgiBundleApplicationContextListener {

    private final List listeners;

    /**
     * Constructs a new <code>ListListenerAdapter</code> instance.
     * 
     * @param listeners
     */
    public ListListenerAdapter(List listeners) {
        Assert.notNull(listeners);
        this.listeners = listeners;
    }

    public void onOsgiApplicationEvent(OsgiBundleApplicationContextEvent event) {
        for (Iterator iterator = listeners.iterator(); iterator.hasNext();) {
            OsgiBundleApplicationContextListener osgiListener = (OsgiBundleApplicationContextListener) iterator
                    .next();
            osgiListener.onOsgiApplicationEvent(event);
        }
    }
}
