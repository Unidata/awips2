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
package com.raytheon.uf.common.serialization;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;

import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.IOException;
import java.util.Arrays;
import java.util.List;

import javax.xml.bind.JAXBException;

import oasis.names.tc.ebxml.regrep.wsdl.registry.services.v4.MsgRegistryException;
import oasis.names.tc.ebxml.regrep.xsd.rs.v4.RegistryExceptionType;

import org.junit.BeforeClass;
import org.junit.Test;

import com.raytheon.uf.common.datadelivery.registry.Provider;
import com.raytheon.uf.common.datadelivery.registry.ProviderFixture;
import com.raytheon.uf.common.registry.OperationStatus;
import com.raytheon.uf.common.registry.RegistryResponse;
import com.raytheon.uf.common.util.FileUtil;
import com.raytheon.uf.common.util.TestUtil;

/**
 * Test {@link SerializationUtil}.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 11, 2012 1102       djohnson     Initial creation
 * Sep 14, 2012 1169       djohnson     Test dynamically serializing a throwable.
 * Sep 28, 2012 1187       djohnson     Test dynamically serializing with a field level adapter.
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */

public class SerializationUtilTest {

    private static Provider PROVIDER;

    private static String PROVIDER_XML;

    /**
     * An ever-growing list of JAXB-able classes. Basically this array should
     * duplicate the contents of the ISerializableContext files at some point
     * (as they are needed).
     */
    private static final Class<?>[] JAXB_CLASSES = {
            com.raytheon.uf.common.datadelivery.registry.AdhocSubscription.class,
            com.raytheon.uf.common.datadelivery.registry.Connection.class,
            com.raytheon.uf.common.datadelivery.registry.Coverage.class,
            com.raytheon.uf.common.datadelivery.registry.DataLevelType.class,
            com.raytheon.uf.common.datadelivery.registry.DataSet.class,
            com.raytheon.uf.common.datadelivery.registry.DataSetMetaData.class,
            com.raytheon.uf.common.datadelivery.registry.DataSetName.class,
            com.raytheon.uf.common.datadelivery.registry.Ensemble.class,
            com.raytheon.uf.common.datadelivery.registry.GriddedCoverage.class,
            com.raytheon.uf.common.datadelivery.registry.GriddedDataSet.class,
            com.raytheon.uf.common.datadelivery.registry.GriddedDataSetMetaData.class,
            com.raytheon.uf.common.datadelivery.registry.GriddedProjection.class,
            com.raytheon.uf.common.datadelivery.registry.GroupDefinition.class,
            com.raytheon.uf.common.datadelivery.registry.Levels.class,
            com.raytheon.uf.common.datadelivery.registry.OpenDapGriddedDataSet.class,
            com.raytheon.uf.common.datadelivery.registry.OpenDapGriddedDataSetMetaData.class,
            com.raytheon.uf.common.datadelivery.registry.Parameter.class,
            com.raytheon.uf.common.datadelivery.registry.ParameterLevel.class,
            com.raytheon.uf.common.datadelivery.registry.PendingSubscription.class,
            com.raytheon.uf.common.datadelivery.registry.Projection.class,
            com.raytheon.uf.common.datadelivery.registry.Provider.class,
            com.raytheon.uf.common.datadelivery.registry.Subscription.class,
            com.raytheon.uf.common.datadelivery.registry.SubscriptionBundle.class,
            com.raytheon.uf.common.datadelivery.registry.Time.class,
            com.raytheon.uf.edex.datadelivery.bandwidth.dao.DataSetMetaDataDao.class,
            com.raytheon.uf.edex.datadelivery.bandwidth.dao.SubscriptionDao.class,
            com.raytheon.uf.edex.datadelivery.bandwidth.dao.SubscriptionRetrieval.class,
            com.raytheon.uf.edex.datadelivery.bandwidth.dao.BandwidthAllocation.class,
            com.raytheon.uf.edex.datadelivery.bandwidth.retrieval.BandwidthMap.class,
            com.raytheon.uf.common.datadelivery.retrieval.xml.ServiceConfig.class,
            com.raytheon.uf.common.datadelivery.retrieval.xml.UnitLookup.class,
            com.raytheon.uf.common.datadelivery.retrieval.xml.LevelLookup.class };

    /**
     * Enables the use of {@link SerializationUtil} within tests.
     */
    public static void initSerializationUtil() {
        try {
            SerializationUtil.jaxbManager = new JAXBManager(JAXB_CLASSES);
        } catch (JAXBException e) {
            throw new IllegalStateException(
                    "Unable to install the jaxbManager instance in SerializationUtil!");
        }
    }

    @BeforeClass
    public static void staticSetup() throws JAXBException {
        SerializationUtilTest.initSerializationUtil();

        PROVIDER = ProviderFixture.INSTANCE.get();
        PROVIDER_XML = SerializationUtil.marshalToXml(PROVIDER);
    }

    @SuppressWarnings("deprecation")
    @Test
    public void testDeprecatedJaxbUnmarshalFromInputStream()
            throws SerializationException {
        assertNotNull(SerializationUtil
                .jaxbUnmarshalFromInputStream(new ByteArrayInputStream(
                        PROVIDER_XML.getBytes())));
    }

    @SuppressWarnings("deprecation")
    @Test
    public void testDeprecatedJaxbUnmarshalFromXmlFile() throws IOException,
            SerializationException {
        File testDir = TestUtil.setupTestClassDir(SerializationUtilTest.class);
        File file = new File(testDir, "test.xml");
        FileUtil.bytes2File(PROVIDER_XML.getBytes(), file);
        assertNotNull(SerializationUtil.jaxbUnmarshalFromXmlFile(file));
    }

    @SuppressWarnings("deprecation")
    @Test
    public void testDeprecatedJaxbUnmarshalFromXmlFileStringParameter()
            throws IOException, SerializationException {
        File testDir = TestUtil.setupTestClassDir(SerializationUtilTest.class);
        File file = new File(testDir, "test.xml");
        FileUtil.bytes2File(PROVIDER_XML.getBytes(), file);
        assertNotNull(SerializationUtil.jaxbUnmarshalFromXmlFile(file
                .getAbsolutePath()));
    }

    @SuppressWarnings("deprecation")
    @Test
    public void testDeprecatedTransformFromThrift()
            throws SerializationException {
        byte[] serialized = SerializationUtil.transformToThrift(PROVIDER);
        assertNotNull(SerializationUtil.transformFromThrift(serialized));
    }

    @SuppressWarnings("deprecation")
    @Test
    public void testDeprecatedUnmarshalFromXml() throws JAXBException {
        assertNotNull(SerializationUtil.unmarshalFromXml(PROVIDER_XML));
    }

    @Test
    public void testDynamicallySerializeThrowable()
            throws SerializationException {

        final IOException exceptionOne = new IOException("Some IO issue");
        final MsgRegistryException exceptionTwo = new MsgRegistryException(
                "blah", new RegistryExceptionType());

        RegistryResponse<String> response = new RegistryResponse<String>();
        response.setRegistryObjects(Arrays.asList("one", "two"));
        response.setErrors(Arrays
                .<Throwable> asList(exceptionOne, exceptionTwo));
        response.setStatus(OperationStatus.PARTIAL_SUCCESS);

        byte[] serialized = SerializationUtil.transformToThrift(response);
        @SuppressWarnings("unchecked")
        RegistryResponse<String> restored = SerializationUtil
                .transformFromThrift(RegistryResponse.class, serialized);
        List<String> registryObjects = restored.getRegistryObjects();
        assertEquals("Incorrect number of registry objects returned", 2,
                registryObjects.size());
        assertEquals("one", registryObjects.get(0));
        assertEquals("two", registryObjects.get(1));

        List<Throwable> throwables = restored.getErrors();
        assertEquals("Incorrect number of throwables returned!", 2,
                throwables.size());
        assertEquals("Incorrect throwable message!", exceptionOne.getMessage(),
                throwables.get(0).getMessage());
        assertEquals("Incorrect throwable message!", exceptionTwo.getMessage(),
                throwables.get(1).getMessage());
    }
}
