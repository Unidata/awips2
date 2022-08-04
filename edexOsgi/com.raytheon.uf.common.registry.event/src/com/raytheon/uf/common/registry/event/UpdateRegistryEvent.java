package com.raytheon.uf.common.registry.event;

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

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;

/**
 * 
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 16, 2012            jsanchez     Initial creation
 * Nov 08, 2013 2506       bgonzale     Added constructors.
 * Mar 31, 2014 2889      dhladky      Added username for notification center tracking.
 * 
 * </pre>
 * 
 * @author jsanchez
 * @version 1.0
 */
@DynamicSerialize
public class UpdateRegistryEvent extends RegistryEvent {
    private static final long serialVersionUID = 1559450302310296001L;

    public UpdateRegistryEvent() {

    }

    public UpdateRegistryEvent(String id, String lid, String username, String objectType) {
        super(id, lid, objectType, username, Action.UPDATE);
    }
}
