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
package com.raytheon.uf.common.datadelivery.event.retrieval;

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;

/**
 * 
 * Event that occurs when an adhoc subscription request is made.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 08, 2012 1654       bgonzale    Initial creation
 * 
 * </pre>
 * 
 * @author bgonzale
 * @version 1.0
 */
@DynamicSerialize
public class AdhocSubscriptionRequestEvent extends SubscriptionRequestEvent {

    private static final long serialVersionUID = -8049206300420972922L;

}
