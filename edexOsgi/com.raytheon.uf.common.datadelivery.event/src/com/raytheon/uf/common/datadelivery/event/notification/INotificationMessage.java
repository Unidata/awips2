package com.raytheon.uf.common.datadelivery.event.notification;

import java.util.Calendar;

/**
 * 
 * Identifies which events can be stored in the notification table
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 22, 2012            jsanchez     Initial creation
 * 
 * </pre>
 * 
 * @author jsanchez
 * @version 1.0
 */
public interface INotificationMessage {

    public Calendar getDate();

    public String getUsername();

    public Integer getPriority();

    public String getCategory();

    public String getMessage();
}
