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
import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.Table;
import javax.persistence.UniqueConstraint;

import org.hibernate.annotations.Index;

import com.raytheon.uf.common.dataplugin.persist.IPersistableDataObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * Smart Init Record represent a smart init for a single forecast hour and its
 * state.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- -----------------------------------------
 * Sep 23, 2010  7277     rjpeter   Initial creation
 * Jun 13, 2013  2044     randerso  Created proper constructor, code cleanup
 * Feb 20, 2018  6928     randerso  Added toString(), javaDoc cleanup.
 *
 * </pre>
 *
 * @author rjpeter
 */
@Entity
@Table(name = "smartinit", uniqueConstraints = {
        @UniqueConstraint(columnNames = { "initName", "validTime", "state" }) })
@DynamicSerialize
public class SmartInitRecord implements
        IPersistableDataObject<SmartInitRecordPK>, Serializable, Cloneable {

    /** priority for live smart init requests */
    public static final int LIVE_SMART_INIT_PRIORITY = 20;

    /** priority for manual smart init requests */
    public static final int MANUAL_SMART_INIT_PRIORITY = 2;

    /** priority for smart init requests generated at site activation */
    public static final int SITE_ACTIVATION_INIT_PRIORITY = 99;

    /** valid time value to indicate all forecast hours should be processed */
    public static final Date ALL_TIMES = new Date(0);

    private static final long serialVersionUID = 1L;

    private static final ThreadLocal<SimpleDateFormat> dateFormat = new ThreadLocal<SimpleDateFormat>() {

        @Override
        protected SimpleDateFormat initialValue() {
            SimpleDateFormat df = new SimpleDateFormat(
                    "yyyy-MM-dd HH:mm:ss.SSS");
            df.setTimeZone(TimeZone.getTimeZone("GMT"));
            return df;
        }

    };

    @Id
    @DynamicSerializeElement
    private SmartInitRecordPK id;

    @Column
    @DynamicSerializeElement
    @Index(name = "smartInitInsertTimeIndex")
    private Date insertTime;

    @Column
    @DynamicSerializeElement
    private boolean manual = false;

    @Column
    @DynamicSerializeElement
    private String smartInit;

    @Column
    @DynamicSerializeElement
    private String dbName;

    @Column
    @DynamicSerializeElement
    @Index(name = "smartInitPriorityIdx")
    private int priority = Integer.MAX_VALUE;

    /**
     * Constructor (only for serialization)
     */
    public SmartInitRecord() {
    }

    /**
     * Constructor
     *
     * @param initName
     * @param module
     * @param validTime
     * @param dbName
     * @param calcAll
     * @param priority
     */
    public SmartInitRecord(String initName, String module, Date validTime,
            String dbName, boolean calcAll, int priority) {
        SmartInitRecordPK pk = new SmartInitRecordPK(initName, validTime);
        setId(pk);
        setInsertTime(new Date());
        setSmartInit(module);
        setDbName(dbName);
        setManual(calcAll);
        setPriority(priority);
    }

    /**
     * @return the id
     */
    public SmartInitRecordPK getId() {
        return id;
    }

    /**
     * @param id
     *            the id to set
     */
    public void setId(SmartInitRecordPK id) {
        this.id = id;
    }

    /**
     * @return the insertTime
     */
    public Date getInsertTime() {
        return insertTime;
    }

    /**
     * @param insertTime
     *            the insertTime to set
     */
    public void setInsertTime(Date insertTime) {
        this.insertTime = insertTime;
    }

    /**
     * @return the manual
     */
    public boolean isManual() {
        return manual;
    }

    /**
     * @param manual
     *            the manual to set
     */
    public void setManual(boolean manual) {
        this.manual = manual;
    }

    @Override
    public SmartInitRecordPK getIdentifier() {
        return id;
    }

    /**
     * @return the smartInit
     */
    public String getSmartInit() {
        return smartInit;
    }

    /**
     * @param smartInit
     *            the smartInit to set
     */
    public void setSmartInit(String smartInit) {
        this.smartInit = smartInit;
    }

    /**
     * @return the dbName
     */
    public String getDbName() {
        return dbName;
    }

    /**
     * @param dbName
     *            the dbName to set
     */
    public void setDbName(String dbName) {
        this.dbName = dbName;
    }

    /**
     * @return the priority
     */
    public int getPriority() {
        return priority;
    }

    /**
     * @param priority
     *            the priority to set
     */
    public void setPriority(int priority) {
        this.priority = priority;
    }

    @Override
    public Object clone() {
        SmartInitRecord rval = new SmartInitRecord();
        rval.id = (SmartInitRecordPK) this.id.clone();
        rval.insertTime = (Date) insertTime.clone();
        rval.manual = this.manual;
        rval.smartInit = new String(this.smartInit);
        rval.dbName = new String(this.dbName);
        rval.priority = this.priority;
        return rval;
    }

    @Override
    public String toString() {
        StringBuilder builder = new StringBuilder();
        builder.append("SmartInitRecord [");
        builder.append(id);
        builder.append(", priority=");
        builder.append(priority);
        builder.append(", insertTime=");
        if (insertTime != null) {
            builder.append(dateFormat.get().format(insertTime));
        } else {
            builder.append("null");
        }
        builder.append(", manual=");
        builder.append(manual);
        builder.append("]");
        return builder.toString();
    }
}
