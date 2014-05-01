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
package com.raytheon.uf.common.dataquery.requests;

import java.util.HashMap;
import java.util.Map.Entry;

import javax.xml.bind.annotation.adapters.XmlAdapter;

/**
 * Handles serializing metadata maps in JAXB
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 4, 2009             chammack    Initial creation
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1.0
 */

public class RequestableMetadataMarshaller
        extends
        XmlAdapter<RequestConstraintSerializable, HashMap<String, RequestConstraint>> {

    /*
     * (non-Javadoc)
     * 
     * @see
     * javax.xml.bind.annotation.adapters.XmlAdapter#marshal(java.lang.Object)
     */
    @Override
    public RequestConstraintSerializable marshal(
            HashMap<String, RequestConstraint> v) throws Exception {
        if (v == null) {
            return null;
        }
        RequestConstraintSerializable serializable = new RequestConstraintSerializable();
        RequestConstraintSerializable.RequestConstraintItem[] items = new RequestConstraintSerializable.RequestConstraintItem[v
                .size()];
        int i = 0;
        for (Entry<String, RequestConstraint> entry : v.entrySet()) {
            items[i] = new RequestConstraintSerializable.RequestConstraintItem();
            items[i].key = entry.getKey();
            items[i].constraint = entry.getValue();
            i++;
        }
        serializable.items = items;
        return serializable;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * javax.xml.bind.annotation.adapters.XmlAdapter#unmarshal(java.lang.Object)
     */
    @Override
    public HashMap<String, RequestConstraint> unmarshal(
            RequestConstraintSerializable v) throws Exception {
        HashMap<String, RequestConstraint> map = null;
        if (v.items == null) {
            map = new HashMap<String, RequestConstraint>(0);
        } else {
            map = new HashMap<String, RequestConstraint>(v.items.length);
            for (RequestConstraintSerializable.RequestConstraintItem item : v.items) {
                map.put(item.key, item.constraint);
            }
        }

        return map;
    }

}
