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
package com.raytheon.edex.plugin.gfe.smartinit;

import java.io.Serializable;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.TimeZone;

import javax.persistence.Column;
import javax.persistence.Embeddable;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.Transient;

import com.raytheon.uf.common.dataplugin.gfe.db.objects.DatabaseID;
import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * Primary key for a smart init record.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 23, 2010 #7277      rjpeter     Initial creation
 * 
 * </pre>
 * 
 * this
 * 
 * @author rjpeter
 * @version 1.0
 */
@Embeddable
@DynamicSerialize
public class SmartInitRecordPK implements ISerializableObject, Serializable,
        Cloneable {

    private static final long serialVersionUID = 1L;

    private static final ThreadLocal<SimpleDateFormat> dateFormat = new ThreadLocal<SimpleDateFormat>() {

        @Override
        protected SimpleDateFormat initialValue() {
            SimpleDateFormat df = new SimpleDateFormat(
                    DatabaseID.MODEL_TIME_FORMAT);
            df.setTimeZone(TimeZone.getTimeZone("GMT"));
            return df;
        }

    };

    public enum State {
        PENDING, RUNNING
    };

    @DynamicSerializeElement
    @Column(nullable = false, length = 64)
    private String initName;

    @DynamicSerializeElement
    @Column(nullable = false)
    private Date validTime;

    @DynamicSerializeElement
    @Column(nullable = false)
    @Enumerated(EnumType.STRING)
    private State state;

    @Transient
    private transient int hashCode;

    public SmartInitRecordPK() {
        this.state = State.PENDING;
    }

    public SmartInitRecordPK(String initName, Date validTime) {
        this();
        this.initName = initName;
        this.validTime = validTime;
        generateHashCode();
    }

    public SmartInitRecordPK(String initName, State state, Date validTime) {
        this.initName = initName;
        this.state = state;
        this.validTime = validTime;
        generateHashCode();
    }

    public String getInitName() {
        return initName;
    }

    public void setInitName(String initName) {
        this.initName = initName;
        generateHashCode();
    }

    public Date getValidTime() {
        return validTime;
    }

    public void setValidTime(Date validTime) {
        this.validTime = validTime;
        generateHashCode();
    }

    public State getState() {
        return state;
    }

    public void setState(State state) {
        this.state = state;
        generateHashCode();
    }

    private void generateHashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result
                + ((initName == null) ? 0 : initName.hashCode());
        result = prime * result + ((state == null) ? 0 : state.hashCode());
        result = prime * result
                + ((validTime == null) ? 0 : validTime.hashCode());
        hashCode = result;
    }

    @Override
    public int hashCode() {
        return hashCode;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) {
            return true;
        }
        if (obj == null) {
            return false;
        }
        if (getClass() != obj.getClass()) {
            return false;
        }
        SmartInitRecordPK other = (SmartInitRecordPK) obj;
        if (initName == null) {
            if (other.initName != null) {
                return false;
            }
        } else if (!initName.equals(other.initName)) {
            return false;
        }
        if (state == null) {
            if (other.state != null) {
                return false;
            }
        } else if (!state.equals(other.state)) {
            return false;
        }
        if (validTime == null) {
            if (other.validTime != null) {
                return false;
            }
        } else if (!validTime.equals(other.validTime)) {
            return false;
        }
        return true;
    }

    @Override
    public String toString() {
        StringBuilder tmp = new StringBuilder(120);
        tmp.append(initName);
        tmp.append(" ValidTime: ");
        if (validTime != null) {
            tmp.append(dateFormat.get().format(validTime));
        } else {
            tmp.append("null");
        }
        return tmp.toString();
    }

    @Override
    public Object clone() {
        return new SmartInitRecordPK(initName, state, (Date) validTime.clone());
    }
}
