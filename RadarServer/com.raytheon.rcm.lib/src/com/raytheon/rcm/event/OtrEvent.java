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
package com.raytheon.rcm.event;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.rcm.request.Request;

@XmlRootElement
@XmlAccessorType(XmlAccessType.FIELD)
public class OtrEvent extends NotificationEvent implements Cloneable {
	public Request request;
	public String radarID;
	public boolean done;
	
	/** The product data.  For clients outside of the RadarServer, this 
	 * may be truncated after the PDB.
	 */
	public byte[] product;
	
    /* (non-Javadoc)
     * @see java.lang.Object#clone()
     */
    @Override
    public OtrEvent clone() {
        try {
            return (OtrEvent) super.clone();
        } catch (CloneNotSupportedException e) {
            throw new RuntimeException(e);
        }
    }
}
