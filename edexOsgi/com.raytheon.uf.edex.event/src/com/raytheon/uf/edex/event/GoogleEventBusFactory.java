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
package com.raytheon.uf.edex.event;

import java.util.List;

import com.google.common.eventbus.EventBus;

/**
 * Interface that defines how to get an {@link EventBus} for system use.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 11, 2012 1407       djohnson     Initial creation
 * May 28, 2013 1650       djohnson     Change to get a list of event bus instances.
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */
public interface GoogleEventBusFactory {

    /**
     * Get the Google {@link EventBus} instances that will be wrapped with an
     * AWIPS class.
     * 
     * @return the {@link EventBus} instances
     * 
     */
    List<EventBus> getEventBuses();

}
