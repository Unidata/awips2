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
package com.raytheon.uf.viz.datadelivery.subscription.xml;

import javax.xml.bind.annotation.adapters.XmlAdapter;

import com.raytheon.uf.viz.datadelivery.system.Operator;
import com.raytheon.uf.viz.datadelivery.system.OperatorTypes;
import com.raytheon.uf.viz.datadelivery.utils.NameOperationItems;
import com.raytheon.uf.viz.datadelivery.utils.TypeOperationItems;

/**
 * Operator adapter class.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 7, 2013    1420     mpduff      Initial creation.
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class OperatorAdapter extends XmlAdapter<String, Operator<?>> {
    @Override
    public Operator<?> unmarshal(String v) throws Exception {
        for (OperatorTypes ot : OperatorTypes.values()) {
            if (ot.toString().equals(v)) {
                return ot;
            }
        }

        for (NameOperationItems noi : NameOperationItems.values()) {
            if (noi.toString().equals(v)) {
                return noi;
            }
        }

        for (TypeOperationItems toi : TypeOperationItems.values()) {
            if (toi.toString().equals(v)) {
                return toi;
            }
        }

        return null;
    }

    @Override
    public String marshal(Operator<?> v) throws Exception {
        return v.toString();
    }

}
