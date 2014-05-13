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
package com.raytheon.uf.edex.datadelivery.retrieval.handlers;

import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.is;
import static org.junit.Assert.assertThat;

import java.util.Date;

import org.junit.BeforeClass;
import org.junit.Test;

import com.raytheon.uf.common.wmo.WMOHeader;
import com.raytheon.uf.common.wmo.WMOMessage;

/**
 * Test {@link AlwaysSameWmoHeader}.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 04, 2013 1647       djohnson     Initial creation
 * Aug 09, 2013 1822       bgonzale     Added parameters to IWmoHeaderApplier.applyWmoHeader().
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */
public class AlwaysSameWmoHeaderTest {

    private static final AlwaysSameWmoHeader WRAPPER = new AlwaysSameWmoHeader(
            "SMYG10 LYBM 280000");

    private static final String wrapped = WRAPPER.applyWmoHeader(null, null,
            null, new Date(), "someData");

    private static WMOMessage message;

    private static WMOHeader wmoHeader;

    @BeforeClass
    public static void setUpClass() {
        message = new WMOMessage(wrapped.getBytes());
        wmoHeader = message.getWmoHeader();
    }

    @Test
    public void createsValidWmoHeader() {
        assertThat(wmoHeader.isValid(), is(true));
    }

    @Test
    public void createsParseableT1() {
        assertThat(wmoHeader.getT1(), is(equalTo('S')));
    }

    @Test
    public void createsParseableT2() {
        assertThat(wmoHeader.getT2(), is(equalTo('M')));
    }

    @Test
    public void createsParseableA1() {
        assertThat(wmoHeader.getA1(), is(equalTo('Y')));
    }

    @Test
    public void createsParseableA2() {
        assertThat(wmoHeader.getA2(), is(equalTo('G')));
    }

    @Test
    public void createsParseableIi() {
        assertThat(wmoHeader.getIi(), is(equalTo(10)));
    }

    @Test
    public void createsParseableCCCC() {
        assertThat(wmoHeader.getCccc(), is(equalTo("LYBM")));
    }

    @Test
    public void createsParseableYYGGgg() {
        assertThat(wmoHeader.getYYGGgg(), is(equalTo("280000")));
    }

    @Test
    public void createsParseableMessage() {
        assertThat(message.getMessageBody(), is(equalTo("someData".getBytes())));
    }
}
