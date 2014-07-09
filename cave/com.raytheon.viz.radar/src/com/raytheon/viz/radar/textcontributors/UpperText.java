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
package com.raytheon.viz.radar.textcontributors;

import java.util.ArrayList;
import java.util.List;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlElements;

/**
 * 
 * A collection of contributors that make up the upper text
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 7, 2010            bsteffen     Initial creation
 * 03/05/2013   DCS51     zwang        Handle GFM product
 * 07/08/2013   DR17356   zwang        Tag SAILS product
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
@XmlAccessorType(XmlAccessType.NONE)
public class UpperText {

    @XmlElements({
            @XmlElement(name = "vcp", type = VCPTextContributor.class),
            @XmlElement(name = "resolution", type = ResolutionTextContributor.class),
            @XmlElement(name = "elevation", type = ElevationTextContributor.class),
            @XmlElement(name = "layer", type = LayerTextContributor.class),
            @XmlElement(name = "join", type = JoinTextContributor.class),
            @XmlElement(name = "nullPrecip", type = NullPrecipTextContributor.class),
            @XmlElement(name = "blank", type = BlankTextContributor.class),
            @XmlElement(name = "productDependent", type = ProdDepTextContributor.class),
            @XmlElement(name = "productDependentDate", type = ProdDepDateTextContributor.class),
            @XmlElement(name = "productDependentUSPMax", type = USPMaxTextContributor.class),
            @XmlElement(name = "productDependentDVLMax", type = DigitalVilMaxTextContributor.class),
            @XmlElement(name = "sails", type = SailsTextContributor.class),
            @XmlElement(name = "gfmCount", type = GfmTextContributor.class),
            @XmlElement(name = "srmMovement", type = SrmMovementTextContributor.class),
            @XmlElement(name = "srmSource", type = SrmSourceTextContributor.class) })
    protected List<IRadarTextContributor> lines = new ArrayList<IRadarTextContributor>();

    @XmlAttribute(name = "productCodes", required = true)
    protected List<Integer> codes = new ArrayList<Integer>();

}
