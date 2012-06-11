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
package com.raytheon.uf.common.tafqueue;

import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.common.serialization.comm.IServerRequest;

/**
 * This class is used by CAVE to request updates or obtain information on the
 * taf_queue table.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 3, 2012  14715      rferrel     Initial creation
 * 
 * </pre>
 * 
 * @author avarani
 * @version 1.0
 */
@DynamicSerialize
public class TafQueueRequest implements IServerRequest {

    public enum Type {
        UNKNOWN, CREATE, GET_LIST, GET_LOG, GET_TAFS, REMOVE_SELECTED, RETRANSMIT
    }

    @DynamicSerializeElement
    private Type type;

    @DynamicSerializeElement
    private TafQueueRecord.TafQueueState state;

    @DynamicSerializeElement
    private List<TafQueueRecord> records;

    @DynamicSerializeElement
    private Map<String, Object> arguments;

    @DynamicSerializeElement
    private Date xmitTime;

    public TafQueueRequest() {
        this.arguments = new HashMap<String, Object>();
        this.type = Type.UNKNOWN;
    }

    public void addArgument(String key, Object value) {
        arguments.put(key, value);
    }

    public Map<String, Object> getArguments() {
        return arguments;
    }

    public Type getType() {
        return type;
    }

    public Date getXmitTime() {
        return xmitTime;
    }

    public void setArguments(Map<String, Object> arguments) {
        this.arguments = arguments;
    }

    public void setType(Type type) {
        this.type = type;
    }

    public TafQueueRecord.TafQueueState getState() {
        return state;
    }

    public void setState(TafQueueRecord.TafQueueState state) {
        this.state = state;
    }

    public List<TafQueueRecord> getRecords() {
        return records;
    }

    public void setRecords(List<TafQueueRecord> records) {
        this.records = records;
    }

    public void setXmitTime(Date xmitTime) {
        this.xmitTime = xmitTime;
    }
}
