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
import static org.junit.Assert.assertTrue;

import java.util.Arrays;

import javax.xml.bind.JAXBException;

import org.junit.Test;

import com.raytheon.uf.common.serialization.SerializationUtil;
import com.raytheon.uf.common.serialization.SerializationUtilTest;
import com.raytheon.uf.common.time.util.ImmutableDate;
import com.raytheon.uf.common.util.TestUtil;

/**
 * Test {@link DataSetMetaData}.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 31, 2012            djohnson     Initial creation
 * Sep 07, 2012 1102       djohnson     Add test for compareTo.
 * Sep 19, 2012 726        jspinks      Add test for Date serialization
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */
public class DataSetMetaDataTest {

    private final DataSetMetaData objectUnderTest = getDataSetMetaData(3L,
            "someUrl");

    private final DataSetMetaData equal = getDataSetMetaData(objectUnderTest
            .getDate().getTime(), objectUnderTest.getUrl());

    private final DataSetMetaData notEqual = getDataSetMetaData(4L,
            "someOtherUrl");

    @Test
    public void testEqualsAndHashCodeContract() {
        TestUtil.assertEqualsAndHashcodeContract(objectUnderTest,
                Arrays.asList(equal), Arrays.asList(notEqual));
    }

    @Test
    public void testDateComparator() {
        final DataSetMetaData lessThan = getDataSetMetaData(1L, "someUrl");
        final DataSetMetaData greaterThanObject = notEqual;

        assertTrue(DataSetMetaData.DATE_COMPARATOR.compare(objectUnderTest,
                lessThan) > 0);
        assertTrue(DataSetMetaData.DATE_COMPARATOR.compare(objectUnderTest,
                greaterThanObject) < 0);
        assertEquals(0,
                DataSetMetaData.DATE_COMPARATOR.compare(objectUnderTest, equal));
    }

    @Test
    public void testDateSerialization() throws JAXBException {
    	SerializationUtilTest.initSerializationUtil();
    	
    	OpenDapGriddedDataSetMetaData objectUnderTest = 
    		OpenDapGriddedDataSetMetaDataFixture.INSTANCE.get();

        final ImmutableDate original = objectUnderTest.getDate();
        final String xml = SerializationUtil.marshalToXml(objectUnderTest);

        OpenDapGriddedDataSetMetaData o = SerializationUtil.unmarshalFromXml(OpenDapGriddedDataSetMetaData.class, xml);
        assertEquals(original, o.getDate());
    }

    /**
     * Get a {@link DataSetMetaData} instance with the specified fields.
     * 
     * @param dateAsTime
     *            the date as milliseconds
     * @param url
     *            the url to use
     * @return the instance
     */
    private static DataSetMetaData getDataSetMetaData(long dateAsTime,
            String url) {
        final DataSetMetaData object = new DataSetMetaData() {
            @Override
            public void accept(IDataSetMetaDataVisitor visitor) {
            }
        };
        object.setDate(new ImmutableDate(dateAsTime));
        object.setUrl(url);

        return object;
    }
}
