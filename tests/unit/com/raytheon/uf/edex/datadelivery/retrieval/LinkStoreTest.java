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
package com.raytheon.uf.edex.datadelivery.retrieval;

import static org.junit.Assert.assertEquals;

import java.io.StringReader;
import java.io.StringWriter;
import java.io.Writer;
import java.util.Date;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;

import org.junit.Test;

/**
 * Test {@link LinkStore}.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 26, 2012 955        djohnson     Initial creation
 * Sep 11, 2012 1154       djohnson     Test JAXB marshall/unmarshall.
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */

public class LinkStoreTest {

    @Test
    public void testGetLinkKeyReturnsLastPortionOfUrlMinusExtension() {
        final String expected = "gep14_06z";
        final String url = "http://nomads.ncep.noaa.gov:9090/dods/gens/gens20120726/"
                + expected + ".info";
        assertEquals(expected, LinkStore.getLinkKey(url));
    }

    @Test
    public void testGetLinkKeyReturnsLastPortionIfNoExtension() {
        final String expected = "gep14_06z";
        final String url = "http://nomads.ncep.noaa.gov:9090/dods/gens/gens20120726/"
                + expected;
        assertEquals(expected, LinkStore.getLinkKey(url));
    }

    @Test
    public void testGetLinkKeyReturnsWholeThingIfNoForwardSlash() {
        final String expected = "gep14_06z";
        final String url = expected;
        assertEquals(expected, LinkStore.getLinkKey(url));
    }

    @Test
    public void testJaxbMarshallUnmarshall() throws JAXBException {
        JAXBContext context = JAXBContext.newInstance(LinkStore.class,
                Link.class);

        final String linkName = "linkName";
        final String linkUrl = "http://linkUrl";
        final String dateString = "20120910";
        final String linkStoreLink = "http://urlOne";
        final long creationTime = 1L;
        final Date date = new Date(creationTime);

        Link link = new Link();
        link.setName(linkName);
        link.setUrl(linkUrl);

        LinkStore linkStore = new LinkStore(dateString, date);
        linkStore.addLink(linkStoreLink, link);
        linkStore.setCreationTime(creationTime);

        Writer writer = new StringWriter();
        context.createMarshaller().marshal(linkStore, writer);

        final String expectedXml = "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>"
                + "<linkStore creationTime=\""
                + creationTime
                + "\" date=\"1969-12-31T18:00:00.00" + creationTime + "-06:00\" dateString=\""
                + dateString
                + "\">"
                + "<links><entry><key xsi:type=\"xs:string\" xmlns:xs=\"http://www.w3.org/2001/XMLSchema\" "
                + "xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\">urlOne</key>"
                + "<value xsi:type=\"link\" url=\""
                + linkUrl
                + "\" name=\""
                + linkName
                + "\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"/>"
                + "</entry></links></linkStore>";

        assertEquals("The marshalled XML did not match what was expected!",
                expectedXml, writer.toString());

        LinkStore restored = (LinkStore) context.createUnmarshaller()
                .unmarshal(
                new StringReader(writer.toString()));

        assertEquals(
                "The jaxb unmarshalled version should have been equal to the original!",
                linkStore, restored);
    }
}
