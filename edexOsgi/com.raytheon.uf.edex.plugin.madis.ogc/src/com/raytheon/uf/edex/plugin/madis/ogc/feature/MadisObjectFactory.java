package com.raytheon.uf.edex.plugin.madis.ogc.feature;

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

import javax.xml.bind.JAXBElement;
import javax.xml.bind.annotation.XmlElementDecl;
import javax.xml.bind.annotation.XmlRegistry;
import javax.xml.namespace.QName;

import com.raytheon.uf.edex.ogc.common.feature.ObsLocation;

/**
 * MadisObjectFactory
 * <pre>
 *                     
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#     Engineer    Description
 * -----------  ----------  ----------- --------------------------
 * 3/27/13      1746         dhladky    Initial creation
 * </pre>
 * 
 * @author dhladky
 * @version 1
 */
@XmlRegistry
public class MadisObjectFactory {

    
    private final static QName _MADIS_QNAME = new QName("http://madis.edex.uf.raytheon.com", "madis");
    
    public MadisObjectFactory() {
       
    }
    
    /**
     * Create an instance of {@link Madis}
     * 
     */
    public Madis create() {
        return new Madis();
    }
    
    /**
     * Create an instance of {@link ObsLocation }
     * 
     */
    public ObsLocation createObsLocation() {
        return new ObsLocation();
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link Madis }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://madis.edex.uf.raytheon.com", name = "madis", substitutionHeadNamespace = "http://www.opengis.net/gml", substitutionHeadName = "_Feature")
    public JAXBElement<Madis> create(Madis madis) {
        return new JAXBElement<Madis>(_MADIS_QNAME, Madis.class, null, madis);
    }

}
