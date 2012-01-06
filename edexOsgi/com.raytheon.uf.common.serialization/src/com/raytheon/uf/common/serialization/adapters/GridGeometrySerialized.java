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
package com.raytheon.uf.common.serialization.adapters;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;

import com.raytheon.uf.common.serialization.ISerializableObject;

/**
 * A serializable form of GeneralGridGeometry
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 11, 2009            chammack     Initial creation
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1.0
 */
@XmlAccessorType(XmlAccessType.NONE)
public class GridGeometrySerialized implements ISerializableObject {

    @XmlAttribute
    public Integer[] rangeX;

    @XmlAttribute
    public Integer[] rangeY;

    @XmlAttribute(required = false)
    public Integer[] rangeZ;

    @XmlAttribute
    public Double envelopeMinX;

    @XmlAttribute
    public Double envelopeMaxX;

    @XmlAttribute
    public Double envelopeMinY;

    @XmlAttribute
    public Double envelopeMaxY;

    @XmlAttribute(required = false)
    public Double envelopeMinZ;

    @XmlAttribute(required = false)
    public Double envelopeMaxZ;

    @XmlElement
    public String CRS;

}
