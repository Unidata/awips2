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
package com.raytheon.edex.plugin.gfe.isc;

import java.io.Serializable;
import java.util.Date;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.Table;
import javax.persistence.UniqueConstraint;

import org.hibernate.annotations.Index;

import com.raytheon.edex.plugin.gfe.isc.IscSendRecordPK.IscSendState;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.ParmID;
import com.raytheon.uf.common.dataplugin.persist.IPersistableDataObject;
import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.common.time.TimeRange;

/**
 * ISC send job record that represents a send job for a single ParmID and its
 * state.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 20, 2011            dgilling     Initial creation
 * 
 * </pre>
 * 
 * @author dgilling
 * @version 1.0
 */

@Entity
@Table(name = "gfe_iscsend", uniqueConstraints = { @UniqueConstraint(columnNames = {
        "parmID", "rangeStart", "rangeEnd", "state", "xmlDest" }) })
@DynamicSerialize
public class IscSendRecord implements IPersistableDataObject, Serializable,
        ISerializableObject, Cloneable {

    private static final long serialVersionUID = 1L;

    @Id
    @DynamicSerializeElement
    private IscSendRecordPK id;

    @Column
    @DynamicSerializeElement
    @Index(name = "iscSendInsertTimeIndex")
    private Date insertTime;

    public IscSendRecord() {
        // no-op, needed for dynamic serialize/hibernate
    }

    public IscSendRecord(ParmID parmId, TimeRange sendTR, String xmlDest,
            boolean sendNow) {
        IscSendState state = (sendNow ? IscSendState.QUEUED
                : IscSendState.PENDING);
        this.id = new IscSendRecordPK(parmId, sendTR, state, xmlDest);
        this.insertTime = new Date();
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.common.dataplugin.persist.IPersistableDataObject#
     * getIdentifier()
     */
    @Override
    public Object getIdentifier() {
        return id;
    }

    @Override
    protected Object clone() throws CloneNotSupportedException {
        IscSendRecord rval = new IscSendRecord();
        rval.id = (IscSendRecordPK) this.id.clone();
        rval.insertTime = (Date) this.insertTime.clone();
        return rval;
    }

    public IscSendRecordPK getId() {
        return id;
    }

    public void setId(IscSendRecordPK id) {
        this.id = id;
    }

    public Date getInsertTime() {
        return insertTime;
    }

    public void setInsertTime(Date insertTime) {
        this.insertTime = insertTime;
    }
}
