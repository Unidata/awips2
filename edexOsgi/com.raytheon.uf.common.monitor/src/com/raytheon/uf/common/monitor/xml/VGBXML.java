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
package com.raytheon.uf.common.monitor.xml;

import java.util.ArrayList;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlElements;

/**
 * VGB's
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 14, 2010     3739       dhladky    Initial creation
 * 
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 */
@XmlAccessorType(XmlAccessType.NONE)
public class VGBXML {

    @XmlElements({ @XmlElement(name = "vgb") })
    private ArrayList<String> lids = new ArrayList<String>();

    /**
     * @return the lIds
     */
    public ArrayList<String> getLids() {
        return lids;
    }

    /**
     * @param the
     *            lids to set
     */
    public void setLid(ArrayList<String> lids) {
        this.lids = lids;
    }

    /**
     * Add one
     * 
     * @param slid
     */
    public void add(String lid) {
        if (lids == null) {
            lids = new ArrayList<String>();
        }
        lids.add(lid);
    }
}
