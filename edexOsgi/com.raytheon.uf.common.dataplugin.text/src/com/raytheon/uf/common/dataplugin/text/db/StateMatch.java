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
package com.raytheon.uf.common.dataplugin.text.db;

import javax.persistence.EmbeddedId;
import javax.persistence.Entity;
import javax.persistence.Table;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;

import com.raytheon.uf.common.dataplugin.persist.PersistableDataObject;
import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 *                                     Initial Implementation
 * 15 Oct 2008  1538       jkorman     Core functions added.
 * 31 Aub 2010  2103       cjeanbap    Initialize member variable
 *                                      in constructor.    
 * 
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */
@Entity
@Table
@DynamicSerialize
@XmlAccessorType(XmlAccessType.NONE)
public class StateMatch extends PersistableDataObject implements
        ISerializableObject {

    private static final long serialVersionUID = 1L;

    @EmbeddedId
    @DynamicSerializeElement
    @XmlElement
    private StateMatchPK pk;

    /** full constructor */
    public StateMatch(String state, String xxx, String ccc) {
        pk = new StateMatchPK(state, xxx, ccc);
    }

    /** default constructor */
    public StateMatch() {
        pk = new StateMatchPK();
    }

    public StateMatchPK getPk() {
        return pk;
    }

    public void setPk(StateMatchPK pk) {
        this.pk = pk;
    }
}
