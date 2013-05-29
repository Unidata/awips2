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
package com.raytheon.uf.edex.datadelivery.bandwidth.notification;

import com.raytheon.uf.common.event.IBaseEventBusHandler;

/**
 * Interface for the {@link BandwidthEventBus} handler functionality.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 11, 2012            djohnson     Initial creation
 * May 28, 2013 1650       djohnson     Reuse the general event bus handling.
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */
interface IBandwidthEventBusHandler extends IBaseEventBusHandler<Object> {
}
