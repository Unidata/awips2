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
package com.raytheon.uf.edex.datadelivery.retrieval.db;

import java.io.Serializable;

import javax.persistence.Column;
import javax.persistence.Embeddable;

import com.raytheon.uf.common.dataplugin.persist.IPersistableDataObject;
import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * Retrieval Request Record
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 09, 2012            rjpeter     Initial creation
 * 
 * </pre>
 * 
 * @author rjpeter
 * @version 1.0
 */
@Embeddable
@DynamicSerialize
public class RetrievalRequestRecordPK implements IPersistableDataObject,
        Serializable, ISerializableObject {

    private static final long serialVersionUID = 1L;

    @Column
    @DynamicSerializeElement
    private String subscriptionName;

    @Column
    @DynamicSerializeElement
    private int index;

    // TODO: Subscription only unique per owner

    public RetrievalRequestRecordPK() {
    }

    public RetrievalRequestRecordPK(String subscriptionName, int index) {
        this.subscriptionName = subscriptionName;
        this.index = index;
    }

    public String getSubscriptionName() {
        return subscriptionName;
    }

    public void setSubscriptionName(String subscriptionName) {
        this.subscriptionName = subscriptionName;
    }

    public int getIndex() {
        return index;
    }

    public void setIndex(int index) {
        this.index = index;
    }

    @Override
    public Object getIdentifier() {
        return this;
    }

    @Override
    public String toString() {
        return subscriptionName + "/" + index;
    }
}
