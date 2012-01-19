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

package com.raytheon.edex.msg.collaboration;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.edex.msg.CoordinateXML;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * An XML object for a Symbol
 * 
 * A symbol consists of the symbol id and the center location
 * 
 * <pre>
 *  
 *   SOFTWARE HISTORY
 *  
 *   Date         Ticket#     Engineer    Description
 *   ------------ ----------  ----------- --------------------------
 *   Nov 16, 2006             chammack    Initial Creation.
 *   Aug 20, 2008 1502        dglazesk    Added XML annotations.
 * </pre>
 * 
 * @author chammack
 * @version 1
 */
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public class Symbol extends CollaborationObject {

    @XmlAttribute
    @DynamicSerializeElement
    private int symbolId;

    @XmlElement
    @DynamicSerializeElement
    private CoordinateXML centerPoint;

    /**
     * @return the centerPoints
     */
    public CoordinateXML getCenterPoint() {
        return centerPoint;
    }

    /**
     * @param centerPoints
     *            the centerPoints to set
     */
    public void setCenterPoint(CoordinateXML centerPoint) {
        this.centerPoint = centerPoint;
    }

    /**
     * @return the id
     */
    public int getSymbolId() {
        return symbolId;
    }

    /**
     * @param id
     *            the id to set
     */
    public void setSymbolId(int id) {
        this.symbolId = id;
    }

}
