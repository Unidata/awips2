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
package com.raytheon.uf.common.datadelivery.registry;

import static org.junit.Assert.assertEquals;

import java.io.StringReader;
import java.io.StringWriter;
import java.io.Writer;
import java.util.concurrent.TimeUnit;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;

import org.junit.Test;

import com.raytheon.uf.edex.datadelivery.harvester.config.HarvesterConfig;
import com.raytheon.uf.edex.datadelivery.harvester.config.HarvesterConfigFixture;

/**
 * Test {@link Provider}.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 11, 2012 1154       djohnson     Initial creation
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */

public class ProviderTest {

    @Test
    public void testSetPostedFileDelayAllowsSpacesSurrounding() {
        Provider provider = new Provider();
        provider.setPostedFileDelay(" 2 MICROSECONDS ");

        assertEquals(2, provider.getPostedFileDelayValue());
        assertEquals(TimeUnit.MICROSECONDS, provider.getPostedFileDelayUnits());
    }

    @Test
    public void testSetPostedFileDelayCanParseText() {
        Provider provider = new Provider();
        provider.setPostedFileDelay("5 HOURS");

        assertEquals(5, provider.getPostedFileDelayValue());
        assertEquals(TimeUnit.HOURS, provider.getPostedFileDelayUnits());
    }

    @Test
    public void testSetPostedFileDelayIsCalledOnJaxbUnmarshall()
            throws JAXBException {
        HarvesterConfig config = HarvesterConfigFixture.INSTANCE.get();
        Provider provider = config.getProvider();
        provider.setPostedFileDelay("3 DAYS");

        Writer writer = new StringWriter();
        JAXBContext ctx = JAXBContext.newInstance(HarvesterConfig.class);
        ctx.createMarshaller().marshal(config, writer);

        HarvesterConfig restored = (HarvesterConfig) ctx.createUnmarshaller()
                .unmarshal(new StringReader(writer.toString()));
        Provider restoredProvider = restored.getProvider();
        assertEquals(3, restoredProvider.getPostedFileDelayValue());
        assertEquals(TimeUnit.DAYS, restoredProvider.getPostedFileDelayUnits());
    }

    @Test(expected = IllegalArgumentException.class)
    public void testSetPostedFileDelayThrowsExceptionOnInvalidUnits() {
        Provider provider = new Provider();
        provider.setPostedFileDelay("5 HOUR");
    }

    @Test(expected = IllegalArgumentException.class)
    public void testSetPostedFileDelayThrowsExceptionOnValueLessThanZero() {
        Provider provider = new Provider();
        provider.setPostedFileDelay("-1 DAYS");
    }
}
