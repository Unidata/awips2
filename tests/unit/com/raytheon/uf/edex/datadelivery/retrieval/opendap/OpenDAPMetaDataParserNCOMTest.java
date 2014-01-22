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
package com.raytheon.uf.edex.datadelivery.retrieval.opendap;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.io.ByteArrayInputStream;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

import com.raytheon.uf.common.datadelivery.registry.Collection;
import com.raytheon.uf.common.datadelivery.registry.DataSet;
import com.raytheon.uf.common.datadelivery.registry.DataSetMetaData;
import com.raytheon.uf.common.datadelivery.registry.DataType;
import com.raytheon.uf.common.datadelivery.registry.OpenDapGriddedDataSet;
import com.raytheon.uf.common.datadelivery.registry.Parameter;
import com.raytheon.uf.common.datadelivery.registry.Projection;
import com.raytheon.uf.common.datadelivery.registry.Provider;
import com.raytheon.uf.common.datadelivery.registry.Projection.ProjectionType;
import com.raytheon.uf.common.datadelivery.registry.Provider.ServiceType;
import com.raytheon.uf.common.gridcoverage.LatLonGridCoverage;
import com.raytheon.uf.common.localization.PathManagerFactoryTest;
import com.raytheon.uf.common.registry.handler.RegistryObjectHandlersUtil;
import com.raytheon.uf.common.time.util.ImmutableDate;
import com.raytheon.uf.common.util.TestUtil;
import com.raytheon.uf.edex.datadelivery.retrieval.Link;
import com.raytheon.uf.edex.datadelivery.retrieval.LinkStore;
import com.raytheon.uf.edex.datadelivery.retrieval.opendap.OpenDAPMetaDataExtracter.DAP_TYPE;

import dods.dap.DAS;
import dods.dap.DASException;
import dods.dap.parser.ParseException;

/**
 * Test {@link OpenDAPMetaDataParser}.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 09, 2013  1466      dhladky     Unit test for NCOM
 * Feb 06, 2013 1543       djohnson    Remove test setup methods no longer necessary.
 * Jun 24, 2013 2106       djohnson    Use in-memory registry object handlers.
 * Sept 30, 2013 1797      dhladky     Generics
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */
public class OpenDAPMetaDataParserNCOMTest {
    private static final String DAS_FILE = "ncom_amseas_20130109.das";

    private static final String COLLECTION_NAME = "ncom";

    private static final String DATASET_NAME = "ncom_amseas";

    private static final String LINK_URL = "http://nomads.ncep.noaa.gov:9090/dods/ncom/ncom20130109/ncom_amseas_20130109";

    private static final String LINK2_URL = "http://nomads.ncep.noaa.gov:9090/dods/ncom/ncom20130109/useast_20130109";

    private static final Provider provider = new Provider();
    static {
        provider.setName("someProvider");

        Projection projection = new Projection();
        projection.setName(LatLonGridCoverage.PROJECTION_TYPE);
        projection.setType(ProjectionType.LatLon);
        ArrayList<Projection> projections = new ArrayList<Projection>();
        projections.add(projection);
        provider.setProjection(projections);
    }

    private static final Date DATASET_DATE;
    static {
        DATASET_DATE = new ImmutableDate(1357689600000L);
    }

    private static DAS DAS = new DAS();

    @BeforeClass
    public static void classSetUp() throws DASException, ParseException {
        PathManagerFactoryTest.initLocalization();

        ByteArrayInputStream bis = new ByteArrayInputStream(
                TestUtil.readResource(OpenDAPMetaDataParserNCOMTest.class, DAS_FILE));
        DAS.parse(bis);

        RegistryObjectHandlersUtil.initMemory();
    }

    private final OpenDAPMetaDataParser parser = new OpenDAPMetaDataParser() {
        // Override the method to store the result for interrogation
        @Override
        protected void storeDataSet(final DataSet dataSet) {
            OpenDAPMetaDataParserNCOMTest.this.dataSet = (OpenDapGriddedDataSet) dataSet;
        }

        /**
         * {@inheritDoc}
         */
        @Override
        public void storeMetaData(List<DataSetMetaData<?>> metaDatas,
                DataSet dataSet) {
            metadatas = metaDatas;
        }
    };

    private OpenDapGriddedDataSet dataSet;

    private List<DataSetMetaData<?>> metadatas;

    private void performParse() {
        Link link = new Link(COLLECTION_NAME, LINK_URL);
        link.getLinks().put(DAP_TYPE.DAS.getDapType(), DAS);
        String DateFormat = "HHddMMMyyyy";
        Collection coll = new Collection(COLLECTION_NAME, "ncom", "yyyyMMdd");
        coll.setProjection(ProjectionType.LatLon);

        LinkStore linkStore = new LinkStore();
        linkStore.addLink(LINK_URL, link);
        parser.parseMetaData(provider, linkStore, coll, DateFormat);
    }

    @Before
    public void setUp() throws DASException, ParseException {
        PathManagerFactoryTest.initLocalization();
    }

    @Test
    public void testDataSetMetaDataParameterDescriptionIsParsed() {
        performParse();

        Map<String, Parameter> parameters = dataSet.getParameters();
        assertEquals("Incorrect parameter description!",
                "salinity [psu] ",
                parameters.get("salinity").getDefinition());
        
        assertEquals("Incorrect parameter description!",
                "grid y surface wind stress [newton/meter2] ",
                parameters.get("swsgy").getDefinition());
        
        assertEquals("Incorrect parameter description!",
                "surface atmospherics pressure [pascals] ",
                parameters.get("surf_atm_press").getDefinition());
    }

    @Test
    public void testDataSetNameIsParsed() {
        performParse();

        assertEquals("Incorrect data set name!", DATASET_NAME,
                dataSet.getDataSetName());
    }

    @Test
    public void testDataSetTypeIsParsed() {
        performParse();

        assertEquals("Incorrect data type found!", DataType.GRID,
                dataSet.getDataSetType());
    }

    @Test
    public void testParserAddsAllCyclesToDataSet() throws DASException,
            ParseException {
        // Setup two links for the link store
        LinkStore linkStore = new LinkStore();

        Link link = new Link(COLLECTION_NAME, LINK_URL);
        link.getLinks().put(DAP_TYPE.DAS.getDapType(), DAS);
        linkStore.addLink(LINK_URL, link);

        DAS das2 = new DAS();
        das2.parse(new ByteArrayInputStream(TestUtil.readResource(
                OpenDAPMetaDataParserNCOMTest.class, "ncom_amseas_20130109.das")));
        Link link2 = new Link(COLLECTION_NAME, LINK2_URL);
        link2.getLinks().put(DAP_TYPE.DAS.getDapType(), das2);
        linkStore.addLink(LINK2_URL, link2);

        String DateFormat = "HHddMMMyyyy";
        Collection coll = new Collection(COLLECTION_NAME, "ncom", "yyyyMMdd");
        coll.setProjection(ProjectionType.LatLon);

        parser.parseMetaData(provider, linkStore, coll, DateFormat);

        // NCOM has no cylces
        Set<Integer> cycles = dataSet.getCycles();
        assertFalse(cycles.contains(Integer.valueOf(0)));
        assertFalse(cycles.contains(Integer.valueOf(1)));
    }

    @Test
    public void testParserAddsAllForecastHoursToDataSet() throws DASException,
            ParseException {
        // Setup two links for the link store
        LinkStore linkStore = new LinkStore();

        Link link = new Link(COLLECTION_NAME, LINK_URL);
        link.getLinks().put(DAP_TYPE.DAS.getDapType(), DAS);
        linkStore.addLink(LINK_URL, link);

        DAS das2 = new DAS();
        das2.parse(new ByteArrayInputStream(TestUtil.readResource(
                OpenDAPMetaDataParserNCOMTest.class, "ncom_useast_20130109.das")));
        Link link2 = new Link(COLLECTION_NAME, LINK2_URL);
        link2.getLinks().put(DAP_TYPE.DAS.getDapType(), das2);
        linkStore.addLink(LINK2_URL, link2);

        String DateFormat = "HHddMMMyyyy";
        Collection coll = new Collection(COLLECTION_NAME, "ncom", "yyyyMMdd");
        coll.setProjection(ProjectionType.LatLon);

        parser.parseMetaData(provider, linkStore, coll, DateFormat);

    }

    @Test
    public void testParserAddsCycleToDataSet() throws DASException,
            ParseException {
        // Setup two links for the link store
        LinkStore linkStore = new LinkStore();

        Link link = new Link(COLLECTION_NAME, LINK_URL);
        link.getLinks().put(DAP_TYPE.DAS.getDapType(), DAS);
        linkStore.addLink(LINK_URL, link);

        DAS das2 = new DAS();
        das2.parse(new ByteArrayInputStream(TestUtil.readResource(
                OpenDAPMetaDataParserNCOMTest.class, "ncom_amseas_20130109.das")));
        Link link2 = new Link(COLLECTION_NAME, LINK2_URL);
        link2.getLinks().put(DAP_TYPE.DAS.getDapType(), das2);
        linkStore.addLink(LINK2_URL, link2);

        String DateFormat = "HHddMMMyyyy";
        Collection coll = new Collection(COLLECTION_NAME, "ncom", "yyyyMMdd");
        coll.setProjection(ProjectionType.LatLon);

        parser.parseMetaData(provider, linkStore, coll, DateFormat);

        OpenDapGriddedDataSet griddedDataSet = dataSet;
        Set<Integer> keySet = griddedDataSet.getCyclesToUrls().keySet();
        assertFalse(keySet.contains(0));
        assertFalse(keySet.contains(1));
        // NCOM has no cyles
        //Iterator<Integer> iter = griddedDataSet.newestToOldestIterator();
        //assertEquals(1, iter.next().intValue());
        //assertEquals(0, iter.next().intValue());
    }

    @Test
    public void testParserMarksCycleAsUpdated() throws DASException,
            ParseException {
        performParse();

        assertTrue(dataSet instanceof OpenDapGriddedDataSet);
                
    }

    @Test
    public void testParserReturnsCorrectAmountOfDataSetMetaData()
            throws DASException, ParseException {
        // Setup two links for the link store
        LinkStore linkStore = new LinkStore();

        Link link = new Link(COLLECTION_NAME, LINK_URL);
        link.getLinks().put(DAP_TYPE.DAS.getDapType(), DAS);
        linkStore.addLink(LINK_URL, link);

        DAS das2 = new DAS();
        das2.parse(new ByteArrayInputStream(TestUtil.readResource(
                OpenDAPMetaDataParserNCOMTest.class, "ncom_useast_20130109.das")));
        Link link2 = new Link(COLLECTION_NAME, LINK2_URL);
        link2.getLinks().put(DAP_TYPE.DAS.getDapType(), das2);
        linkStore.addLink(LINK2_URL, link2);

        String DateFormat = "HHddMMMyyyy";
        Collection coll = new Collection(COLLECTION_NAME, "ncom", "yyyyMMdd");
        coll.setProjection(ProjectionType.LatLon);

        List<DataSetMetaData<?>> results = parser.parseMetaData(provider,
                linkStore, coll, DateFormat);

        assertEquals("Expected two DataSetMetaData objects to be parsed!", 2,
                results.size());
    }

    @Test
    public void testParserReturnsDataSetMetaDataWithDataDate() {
        performParse();

        assertEquals("Incorrect date set on MetaData object!", DATASET_DATE,
                metadatas.get(0).getDate());
    }

    @Test
    public void testParserReturnsDataSetMetaDataWithDataSetName() {
        performParse();

        assertEquals("Incorrect dataSetName set on MetaData object!",
                DATASET_NAME, metadatas.get(0).getDataSetName());
    }

    @Test
    public void testParserReturnsDataSetMetaDataWithLinkAsUrl() {
        performParse();

        assertEquals("Incorrect URL found for the data set metadata!",
                LINK_URL, metadatas.get(0).getUrl());
    }

    @Test
    public void testParserSetsServiceType() {
        performParse();

        assertEquals(ServiceType.OPENDAP, dataSet.getServiceType());
    }

    @Test
    public void testParserUpdatesCycleUrl() throws DASException, ParseException {
        performParse();

        assertTrue(dataSet instanceof OpenDapGriddedDataSet);

    }
}
