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

import static com.raytheon.uf.common.util.Matchers.hasNumberOfFiles;
import static org.junit.Assert.assertThat;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.io.File;
import java.util.Date;
import java.util.Random;

import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;
import org.mockito.Matchers;

import com.raytheon.uf.common.localization.PathManagerFactoryTest;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.util.TestUtil;
import com.raytheon.uf.edex.datadelivery.retrieval.db.IRetrievalDao;
import com.raytheon.uf.edex.datadelivery.retrieval.db.RetrievalRequestRecord;
import com.raytheon.uf.edex.datadelivery.retrieval.db.RetrievalRequestRecordPK;

/**
 * Test {@link SerializeRetrievedDataToDirectory}.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 01, 2013 1543       djohnson     Initial creation
 * Feb 15, 2013 1543       djohnson     Class renames.
 * Mar 05, 2013 1647       djohnson     Pass wmo header strategy to constructor.
 * Aug 09, 2013 1822       bgonzale     Added parameters to processRetrievedPluginDataObjects.
 * Oct 01, 2013 2267       bgonzale     Pass request parameter instead of components of request.
 *                                      Add test for wfs retrieval.
 * Nov 04, 2013 2506       bgonzale     removed IRetrievalDao and request parameters.
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */
public class SerializeRetrievedDataToDirectoryTest {

    private final File directory = TestUtil
            .setupTestClassDir(SerializeRetrievedDataToDirectoryTest.class);

    private final IRetrievalDao mockDao = mock(IRetrievalDao.class);

    private final SerializeRetrievedDataToDirectory service = new SerializeRetrievedDataToDirectory(
            directory, new AlwaysSameWmoHeader("SMYG10 LYBM 280000"), mockDao);

    @BeforeClass
    public static void classSetUp() {
        PathManagerFactoryTest.initLocalization();
    }

    @Before
    public void setup() {
        when(mockDao.getById((RetrievalRequestRecordPK) Matchers.anyObject()))
                .thenReturn(
                        RetrievalRequestRecordFixture.INSTANCE.getInstance(0,
                                new Random(0)));
    }

    @Test
    public void serializesRetrievedDataToAFileInTheTargetDirectory()
            throws SerializationException {

        RetrievalResponseXml retrievalPluginDataObjects = RetrievalPluginDataObjectsFixture.INSTANCE
                .get();
        RetrievalRequestRecord request = new RetrievalRequestRecord();
        request.setProvider("NOMADS");
        request.setPlugin("grid");
        // "Model"
        request.setInsertTime(new Date());

        service.processRetrievedPluginDataObjects(retrievalPluginDataObjects);

        assertThat(directory, hasNumberOfFiles(1));
    }

    @Test
    public void serializesWfsRetrievedDataToAFileInTheTargetDirectory()
            throws SerializationException {

        RetrievalResponseXml retrievalPluginDataObjects = WfsRetrievalPluginDataObjectsFixture.INSTANCE
                .get();
        RetrievalRequestRecord request = new RetrievalRequestRecord();
        request.setProvider("MADIS");
        request.setPlugin("MADIS");
        // "Model"
        request.setInsertTime(new Date());

        service.processRetrievedPluginDataObjects(retrievalPluginDataObjects);

        assertThat(directory, hasNumberOfFiles(1));
    }
}
