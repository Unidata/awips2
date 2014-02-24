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
package com.raytheon.uf.viz.collaboration.ui.notifier;

import javax.xml.bind.annotation.XmlEnum;
import javax.xml.bind.annotation.XmlEnumValue;
import javax.xml.bind.annotation.XmlType;

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;

/**
 * Contact Notifier Types.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 20, 2014   2632     mpduff      Initial creation
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */
@XmlType(name = "notifier")
@XmlEnum
@DynamicSerialize
public enum Notifier {
    @XmlEnumValue("sendMessage")
    SendMessage("Sends Me A Message"), @XmlEnumValue("signOn")
    SignOn("Signs On"), @XmlEnumValue("signOff")
    SignOff("Signs Off"), @XmlEnumValue("away")
    Away("Becomes Unavailable"), @XmlEnumValue("return")
    Returns("Becomes Available");

    private String description;

    Notifier(String description) {
        this.description = description;
    }

    public String getDescription() {
        return description;
    }
}
