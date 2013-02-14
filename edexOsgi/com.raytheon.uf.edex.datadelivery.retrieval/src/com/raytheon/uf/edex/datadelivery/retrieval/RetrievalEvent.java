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
package com.raytheon.uf.edex.datadelivery.retrieval;

import com.raytheon.uf.common.datadelivery.event.INotifiableEvent;
import com.raytheon.uf.common.datadelivery.event.notification.NotificationRecord;
import com.raytheon.uf.common.event.Event;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * An {@link Event} that occurs during Retrieval.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 31, 2012            djohnson     Initial creation
 * Dec 07, 2012 1104       djohnson     Simplify event type hierarchy.
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */
@DynamicSerialize
public class RetrievalEvent extends Event implements
        INotifiableEvent {

    private static final long serialVersionUID = 2349884590400753667L;

    @DynamicSerializeElement
    private final String message;

    /**
     * Constructor.
     */
    public RetrievalEvent(String message) {
        super();
        this.message = message;
    }

    @Override
    public String toString() {
        return message;
    }

    @Override
    public NotificationRecord generateNotification() {
        NotificationRecord record = new NotificationRecord();
        record.setCategory("Retrieval");
        record.setUsername("N/A");
        record.setPriority(1);
        record.setMessage(toString());
        record.setDate(getDate());
        return record;
    }

}