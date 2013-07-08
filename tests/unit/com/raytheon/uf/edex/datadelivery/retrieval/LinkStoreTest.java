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
 * Jul 08, 2013 2173       mpduff       Change to look at xml values, not the xml itself.
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

        LinkStore restored = (LinkStore) context.createUnmarshaller()
                .unmarshal(new StringReader(writer.toString()));

        Link link2 = restored.getLink(LinkStore.getLinkKey(linkStoreLink));

        assertEquals("Links do not match", link, link2);

        assertEquals("Names do not match", link.getName(), link2.getName());

        assertEquals("URLs do not match", link.getUrl(), link2.getUrl());

        assertEquals("Creation Times do not match",
                linkStore.getCreationTime(), restored.getCreationTime());

        assertEquals("Date Strings do not match", linkStore.getDateString(),
                restored.getDateString());
        assertEquals("Dates do not match", linkStore.getDate(),
                restored.getDate());

        assertEquals(
                "The jaxb unmarshalled version should have been equal to the original!",
                linkStore, restored);
    }
}
