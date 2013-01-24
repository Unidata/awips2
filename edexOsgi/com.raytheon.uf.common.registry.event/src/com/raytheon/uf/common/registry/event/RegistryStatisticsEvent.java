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

import java.util.Calendar;
import java.util.Map;
import java.util.TimeZone;
import java.util.regex.Pattern;

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.common.stats.StatisticsEvent;

/**
 * Event used for statistics from the registry
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 25, 2012  #1340     dhladky     Initial creation
 * 
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 */

@DynamicSerialize
public class RegistryStatisticsEvent extends StatisticsEvent {

    private static final long serialVersionUID = -492426634010607520L;

    private static final Pattern COLON_PATTERN = Pattern.compile(":");

    public RegistryStatisticsEvent(String type, String status, String owner,
            long duration) {

        this.setDate(Calendar.getInstance(TimeZone.getTimeZone("GMT")));
        this.setType(getParsedString(type));
        this.setStatus(getParsedString(status));
        this.setOwner(owner);
        this.setDuration(duration);

    }

    public RegistryStatisticsEvent() {

    }

    @Override
    protected Map<String, String> getFieldUnitMap() {

        return null;
    }

    public void setDuration(long duration) {
        this.duration = duration;
    }

    public long getDuration() {
        return duration;
    }

    public void setOwner(String owner) {
        this.owner = owner;
    }

    public String getOwner() {
        return owner;
    }

    public void setStatus(String status) {
        this.status = status;
    }

    public String getStatus() {
        return status;
    }

    public void setType(String type) {
        this.type = type;
    }

    public String getType() {
        return type;
    }

    @DynamicSerializeElement
    private String type;

    @DynamicSerializeElement
    private String status;

    @DynamicSerializeElement
    private String owner;

    @DynamicSerializeElement
    private long duration;

    /**
     * Remove the Ebxml header info from the registry message returning only a
     * "Pretty" string for the message.
     * 
     * @param registryObject
     * @return
     */
    private String getParsedString(String registryObject) {

        if (registryObject != null) {
            String[] chunks = COLON_PATTERN.split(registryObject);
            if (chunks != null && chunks.length > 0) {
                return chunks[chunks.length - 1];
            }
        }
        return registryObject;
    }

}