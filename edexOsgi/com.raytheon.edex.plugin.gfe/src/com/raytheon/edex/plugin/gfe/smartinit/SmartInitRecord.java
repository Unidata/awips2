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
import java.util.Date;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.Table;
import javax.persistence.UniqueConstraint;

import org.hibernate.annotations.Index;

import com.raytheon.uf.common.dataplugin.persist.IPersistableDataObject;
import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * Smart Init Record represent a smart init for a single forecast hour and its
 * state.
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
 * @author rjpeter
 * @version 1.0
 */
@Entity
@Table(name = "smartinit", uniqueConstraints = { @UniqueConstraint(columnNames = {
        "initName", "validTime", "state" }) })
@DynamicSerialize
public class SmartInitRecord implements IPersistableDataObject, Serializable,
        ISerializableObject, Cloneable {

    public static int LIVE_SMART_INIT_PRIORITY = 20;

    public static int MANUAL_SMART_INIT_PRIORITY = 2;

    public static int SITE_ACTIVATION_INIT_PRIORITY = 99;

    private static final long serialVersionUID = 1L;

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

    public SmartInitRecord() {
    }

    public SmartInitRecordPK getId() {
        return id;
    }

    public void setId(SmartInitRecordPK id) {
        this.id = id;
    }

    public Date getInsertTime() {
        return insertTime;
    }

    public void setInsertTime(Date insertTime) {
        this.insertTime = insertTime;
    }

    public boolean isManual() {
        return manual;
    }

    public void setManual(boolean isManual) {
        this.manual = isManual;
    }

    @Override
    public Object getIdentifier() {
        return id;
    }

    public String getSmartInit() {
        return smartInit;
    }

    public void setSmartInit(String smartInit) {
        this.smartInit = smartInit;
    }

    public void setDbName(String dbName) {
        this.dbName = dbName;
    }

    public String getDbName() {
        return dbName;
    }

    public void setPriority(int priority) {
        this.priority = priority;
    }

    public int getPriority() {
        return priority;
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
}
