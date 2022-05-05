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

package org.w3.v1999.xlink;

import javax.xml.bind.JAXBElement;
import javax.xml.bind.annotation.XmlElementDecl;
import javax.xml.bind.annotation.XmlRegistry;
import javax.xml.namespace.QName;

import com.raytheon.uf.common.registry.schemas.ebxml.util.EbxmlNamespaces;

/**
 * This object contains factory methods for each Java content interface and Java
 * element interface generated in the org.w3._1999.xlink package.
 * <p>
 * An ObjectFactory allows you to programatically construct new instances of the
 * Java representation for XML content. The Java representation of XML content
 * can consist of schema derived interfaces and classes representing the binding
 * of schema type definitions, element declarations and model groups. Factory
 * methods for each of these are provided in this class.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 2012                     bphillip    Initial implementation
 * 10/17/2013    1682       bphillip    Added software history
 * </pre>
 * 
 * @author bphillip
 * @version 1
 */
@XmlRegistry
public class ObjectFactory {

    private final static QName _Resource_QNAME = new QName(
            EbxmlNamespaces.XLINK_URI, "resource");

    private final static QName _Locator_QNAME = new QName(
            EbxmlNamespaces.XLINK_URI, "locator");

    private final static QName _Arc_QNAME = new QName(
            EbxmlNamespaces.XLINK_URI, "arc");

    private final static QName _Title_QNAME = new QName(
            EbxmlNamespaces.XLINK_URI, "title");

    /**
     * Create a new ObjectFactory that can be used to create new instances of
     * schema derived classes for package: org.w3._1999.xlink
     * 
     */
    public ObjectFactory() {
    }

    /**
     * Create an instance of {@link LocatorType }
     * 
     */
    public LocatorType createLocatorType() {
        return new LocatorType();
    }

    /**
     * Create an instance of {@link TitleEltType }
     * 
     */
    public TitleEltType createTitleEltType() {
        return new TitleEltType();
    }

    /**
     * Create an instance of {@link Simple }
     * 
     */
    public Simple createSimple() {
        return new Simple();
    }

    /**
     * Create an instance of {@link ArcType }
     * 
     */
    public ArcType createArcType() {
        return new ArcType();
    }

    /**
     * Create an instance of {@link Extended }
     * 
     */
    public Extended createExtended() {
        return new Extended();
    }

    /**
     * Create an instance of {@link ResourceType }
     * 
     */
    public ResourceType createResourceType() {
        return new ResourceType();
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link ResourceType }
     * {@code >}
     * 
     */
    @XmlElementDecl(namespace = EbxmlNamespaces.XLINK_URI, name = "resource")
    public JAXBElement<ResourceType> createResource(ResourceType value) {
        return new JAXBElement<ResourceType>(_Resource_QNAME,
                ResourceType.class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link LocatorType }
     * {@code >}
     * 
     */
    @XmlElementDecl(namespace = EbxmlNamespaces.XLINK_URI, name = "locator")
    public JAXBElement<LocatorType> createLocator(LocatorType value) {
        return new JAXBElement<LocatorType>(_Locator_QNAME, LocatorType.class,
                null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link ArcType }{@code >}
     * 
     */
    @XmlElementDecl(namespace = EbxmlNamespaces.XLINK_URI, name = "arc")
    public JAXBElement<ArcType> createArc(ArcType value) {
        return new JAXBElement<ArcType>(_Arc_QNAME, ArcType.class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link TitleEltType }
     * {@code >}
     * 
     */
    @XmlElementDecl(namespace = EbxmlNamespaces.XLINK_URI, name = "title")
    public JAXBElement<TitleEltType> createTitle(TitleEltType value) {
        return new JAXBElement<TitleEltType>(_Title_QNAME, TitleEltType.class,
                null, value);
    }

}
