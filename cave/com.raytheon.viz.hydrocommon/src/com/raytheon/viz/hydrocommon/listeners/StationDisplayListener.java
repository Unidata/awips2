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
package com.raytheon.viz.hydrocommon.listeners;

import com.raytheon.viz.hydrocommon.events.StationDisplayUpdateEvent;


/**
 * Interface for controlling the Hydrodisplay from PDC
 *  
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 08Nov2008    #1628          dhladky     Initial Creation
 * 
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 */

public interface StationDisplayListener {
   // implement to receive messages
   public void notifyUpdate(StationDisplayUpdateEvent sdue);

}
