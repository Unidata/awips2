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
import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * 
 * Consists of a drawing layer state
 * 
 * <pre>
 * 
 *  SOFTWARE HISTORY
 * 
 *  Date         Ticket#     Engineer    Description
 *  ------------ ----------  ----------- --------------------------
 *  Nov 16, 2006            chammack    Initial Creation.
 *  Aug 20, 2008 1502        dglazesk    Added XML annotations.
 * </pre>
 * 
 * @author chammack
 * @version 1
 */
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public class DrawingState implements ISerializableObject {

    @XmlElement
    @DynamicSerializeElement
    private CollaborationObject[] objects;

    @XmlElement
    @DynamicSerializeElement
    private CoordinateXML ulScreenPosition;

    @XmlElement
    @DynamicSerializeElement
    private CoordinateXML lrScreenPosition;

    /**
     * @return the fronts
     */
    public CollaborationObject[] getObjects() {
        return objects;
    }

    /**
     * @param objects
     *            the objects to set
     */
    public void setObjects(CollaborationObject[] objects) {
        this.objects = objects;
    }

    /**
     * @return the lrScreenPosition
     */
    public CoordinateXML getLrScreenPosition() {
        return lrScreenPosition;
    }

    /**
     * @param lrScreenPosition
     *            the lrScreenPosition to set
     */
    public void setLrScreenPosition(CoordinateXML lrScreenPosition) {
        this.lrScreenPosition = lrScreenPosition;
    }

    /**
     * @return the ulScreenPosition
     */
    public CoordinateXML getUlScreenPosition() {
        return ulScreenPosition;
    }

    /**
     * @param ulScreenPosition
     *            the ulScreenPosition to set
     */
    public void setUlScreenPosition(CoordinateXML ulScreenPosition) {
        this.ulScreenPosition = ulScreenPosition;
    }

}
