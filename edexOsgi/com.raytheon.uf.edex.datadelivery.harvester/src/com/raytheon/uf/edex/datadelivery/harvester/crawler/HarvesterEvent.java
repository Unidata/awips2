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
package com.raytheon.uf.edex.datadelivery.harvester.crawler;

import com.raytheon.uf.common.datadelivery.event.INotifiableEvent;
import com.raytheon.uf.common.datadelivery.event.notification.NotificationRecord;
import com.raytheon.uf.common.event.Event;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * An event occurred with the {@link MainSequenceHarvester} component.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 13, 2012 740        djohnson     Initial creation
 * Aug 30, 2012 1123       djohnson     Renamed to HarvesterEvent.
 * Dec 07, 2012 1104       djohnson     Simplify event type hierarchy.
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */
@DynamicSerialize
public class HarvesterEvent extends Event implements
        INotifiableEvent {

    private static final long serialVersionUID = 8482638241844878779L;

    @DynamicSerializeElement
    private final String message;

    /**
     * Constructor.
     */
    public HarvesterEvent(String message) {
        super();
        this.message = message;
    }

    @Override
    public NotificationRecord generateNotification() {
        NotificationRecord record = new NotificationRecord();
        record.setCategory("Harvester");
        record.setUsername("N/A");
        record.setPriority(1);
        record.setMessage(toString());
        record.setDate(getDate());
        return record;
    }

    @Override
    public String toString() {
        return message;
    }
}
