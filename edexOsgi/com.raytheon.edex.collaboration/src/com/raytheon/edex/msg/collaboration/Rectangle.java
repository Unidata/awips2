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
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.edex.msg.CoordinateXML;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * Represents a rectangle in XML
 * 
 * <pre>
 * 
 *  SOFTWARE HISTORY
 * 
 *  Date         Ticket#     Engineer    Description
 *  ------------ ----------  ----------- --------------------------
 *  Nov 20, 2006             chammack    Initial Creation.
 *  Aug 20, 2008 1502        dglazesk    Added XML annotations.
 * </pre>
 * 
 * @author chammack
 * @version 1
 */
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public class Rectangle extends CollaborationObject {

    @XmlElement
    @DynamicSerializeElement
    private CoordinateXML upperLeft;

    @XmlElement
    @DynamicSerializeElement
    private CoordinateXML lowerRight;

    /**
     * @return the lowerRight
     */
    public CoordinateXML getLowerRight() {
        return lowerRight;
    }

    /**
     * @param lowerRight
     *            the lowerRight to set
     */
    public void setLowerRight(CoordinateXML lowerRight) {
        this.lowerRight = lowerRight;
    }

    /**
     * @return the upperLeft
     */
    public CoordinateXML getUpperLeft() {
        return upperLeft;
    }

    /**
     * @param upperLeft
     *            the upperLeft to set
     */
    public void setUpperLeft(CoordinateXML upperLeft) {
        this.upperLeft = upperLeft;
    }

}
