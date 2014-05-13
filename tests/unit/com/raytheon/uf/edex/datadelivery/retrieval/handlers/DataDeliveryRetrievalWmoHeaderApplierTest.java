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

import java.io.IOException;
import java.util.Date;

import org.junit.Before;
import org.junit.Test;

import com.raytheon.uf.common.wmo.WMOHeader;
import com.raytheon.uf.common.wmo.WMOMessage;
import com.raytheon.uf.edex.core.props.PropertiesException;

/**
 * Test {@link DataDeliveryRetrievalWmoHeaderApplier}.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 08, 2013 1822       bgonzale    Initial creation
 * Oct 01, 2013 2267       bgonzale    Added test for null inputs.
 * Oct 09, 2013 2267       bgonzale    Fix Wmo header cr and lf formatting testing.
 * 
 * </pre>
 * 
 * @author bgonzale
 * @version 1.0
 */
public class DataDeliveryRetrievalWmoHeaderApplierTest {

    private DataDeliveryRetrievalWmoHeaderApplier applier;

    private WMOMessage message;

    private WMOHeader wmoHeader;

    @Before
    public void setUp() throws PropertiesException, IOException {
        applier = createApplier();
    }

    private void setHeader(String dataProvider, String dataFormat,
            String sourceType, Date date) {
        String dataWithHeader = applier.applyWmoHeader(dataProvider,
                dataFormat, sourceType, date, "someData");
        message = new WMOMessage(dataWithHeader.getBytes());
        wmoHeader = message.getWmoHeader();
    }

    private static DataDeliveryRetrievalWmoHeaderApplier createApplier()
            throws PropertiesException {
        String wmoHeader = "LZ{0}{1}9{2} KWBC {3,date,ddHH00}\r\r\n";
        String dataProviderMapping = "NOMADS:A,MADIS:B,PDA:C";
        String dataFormatMapping = "GRID:A,MADIS:B,NETCDF:C";
        String dataSourceMapping = "MODEL:1,OBSERVATION:2,SATELLITE:3";
        return new DataDeliveryRetrievalWmoHeaderApplier(wmoHeader,
                dataProviderMapping, dataFormatMapping, dataSourceMapping);
    }

    @Test
    public void createValidWmoHeader() {
        Date date = new Date();
        setHeader("NOMADS", "GRID", "MODEL", date);
        assertThat(wmoHeader.isValid(), is(true));
    }

    @Test
    public void createNomadsGridModel() {
        Date date = new Date();
        setHeader("NOMADS", "GRID", "MODEL", date);
        assertThat(wmoHeader.getT1(), is(equalTo('L')));
        assertThat(wmoHeader.getT2(), is(equalTo('Z')));
        assertThat(wmoHeader.getA1(), is(equalTo('A')));
        assertThat(wmoHeader.getA2(), is(equalTo('A')));
        assertThat(wmoHeader.getIi(), is(equalTo(91)));
        assertThat(wmoHeader.getCccc(), is(equalTo("KWBC")));
        assertThat(message.getMessageBody(), is(equalTo("someData".getBytes())));
    }

    @Test
    public void createMadisGridModel() {
        Date date = new Date();
        setHeader("MADIS", "GRID", "MODEL", date);
        assertThat(wmoHeader.getT1(), is(equalTo('L')));
        assertThat(wmoHeader.getT2(), is(equalTo('Z')));
        assertThat(wmoHeader.getA1(), is(equalTo('B')));
        assertThat(wmoHeader.getA2(), is(equalTo('A')));
        assertThat(wmoHeader.getIi(), is(equalTo(91)));
        assertThat(wmoHeader.getCccc(), is(equalTo("KWBC")));
        assertThat(message.getMessageBody(), is(equalTo("someData".getBytes())));
    }

    @Test
    public void createMadisPointModel() {
        Date date = new Date();
        setHeader("MADIS", "MADIS", "MODEL", date);
        assertThat(wmoHeader.getT1(), is(equalTo('L')));
        assertThat(wmoHeader.getT2(), is(equalTo('Z')));
        assertThat(wmoHeader.getA1(), is(equalTo('B')));
        assertThat(wmoHeader.getA2(), is(equalTo('B')));
        assertThat(wmoHeader.getIi(), is(equalTo(91)));
        assertThat(wmoHeader.getCccc(), is(equalTo("KWBC")));
        assertThat(message.getMessageBody(), is(equalTo("someData".getBytes())));
    }

    @Test
    public void createMadisPointSatellite() {
        Date date = new Date();
        setHeader("MADIS", "MADIS", "SATELLITE", date);
        assertThat(wmoHeader.getT1(), is(equalTo('L')));
        assertThat(wmoHeader.getT2(), is(equalTo('Z')));
        assertThat(wmoHeader.getA1(), is(equalTo('B')));
        assertThat(wmoHeader.getA2(), is(equalTo('B')));
        assertThat(wmoHeader.getIi(), is(equalTo(93)));
        assertThat(wmoHeader.getCccc(), is(equalTo("KWBC")));
        assertThat(message.getMessageBody(), is(equalTo("someData".getBytes())));
    }

    @Test
    public void createWithNullArgs() {
        Date date = new Date();
        setHeader(null, null, null, date);
        assertThat(wmoHeader.getT1(), is(equalTo('L')));
        assertThat(wmoHeader.getT2(), is(equalTo('Z')));
        assertThat(wmoHeader.getA1(), is(equalTo('A')));
        assertThat(wmoHeader.getA2(), is(equalTo('A')));
        assertThat(wmoHeader.getIi(), is(equalTo(91)));
        assertThat(wmoHeader.getCccc(), is(equalTo("KWBC")));
        assertThat(message.getMessageBody(), is(equalTo("someData".getBytes())));
    }

}
