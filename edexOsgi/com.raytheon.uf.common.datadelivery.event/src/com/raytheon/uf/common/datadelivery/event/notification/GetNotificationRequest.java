package com.raytheon.uf.common.datadelivery.event.notification;

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.common.serialization.comm.IServerRequest;

/**
 * 
 * The request object to retrieve data from the notification table
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 1, 2012            jsanchez     Initial creation
 * 
 * </pre>
 * 
 * @author jsanchez
 * @version 1.0
 */
@DynamicSerialize
public class GetNotificationRequest implements IServerRequest {

    @DynamicSerializeElement
    private String username;

    @DynamicSerializeElement
    private Integer hours;

    @DynamicSerializeElement
    private Integer maxResults;

    /**
     * 
     * @return The username(s) of the records to be retrieved
     */
    public String getUsername() {
        return username;
    }

    /**
     * Records can be retrieved based on username(s). Multiple users can be
     * concatenated together separated by a comma (i.e. "user1,user2,user3").
     * Set this to null to disregard this constraint
     * 
     * @param username
     *            The username(s) of the records to be retrieved
     */
    public void setUsername(String username) {
        this.username = username;
    }

    /**
     * 
     * @return The number of hours to set back from the current time
     */
    public Integer getHours() {
        return hours;
    }

    /**
     * Records can be retrieved based on a time range. This time range is from
     * current time to current - hours. Set this to null to disregard this
     * constraint
     * 
     * @param hours
     *            The number of hours to set back from the current time
     */
    public void setHours(Integer hours) {
        this.hours = hours;
    }

    /**
     * 
     * @return The max number records to retrieve
     */
    public Integer getMaxResults() {
        return maxResults;
    }

    /**
     * The max number of records to retrieve. Set to null to disregard this
     * constraint
     * 
     * @param maxResults
     *            The max number of records to retrieve
     */
    public void setMaxResults(Integer maxResults) {
        this.maxResults = maxResults;
    }

}
