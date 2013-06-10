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

import static com.raytheon.uf.common.util.Matchers.hasNoFiles;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.notNullValue;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertThat;

import java.io.File;

import org.junit.Test;

import com.raytheon.uf.common.util.TestUtil;

/**
 * Test {@link DeserializeRetrievedDataFromDirectory}.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 01, 2013 1543       djohnson     Initial creation
 * Feb 12, 2013 1543       djohnson     Can only test the retrieval response is now not null.
 * Feb 15, 2013 1543       djohnson     Some renames.
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */
public class DeserializeRetrievedDataFromDirectoryTest {
    private final File directory = TestUtil
            .setupTestClassDir(DeserializeRetrievedDataFromDirectoryTest.class);

    private final DeserializeRetrievedDataFromDirectory service = new DeserializeRetrievedDataFromDirectory(
            directory);

    @Test
    public void deserializesRetrievedDataFromAFileInTheTargetDirectory()
            throws Exception {

        RetrievalResponseXml retrievalPluginDataObjects = RetrievalPluginDataObjectsFixture.INSTANCE
                .get();

        new SerializeRetrievedDataToDirectory(directory)
                .processRetrievedPluginDataObjects(retrievalPluginDataObjects);

        final RetrievalResponseXml restored = service
                .findRetrievals();

        // Just make sure the payload is present
        assertThat(restored.getRetrievalAttributePluginDataObjects().get(0)
                .getRetrievalResponse(), is(notNullValue()));
    }

    @Test
    public void deletesFileAfterRetrievingFromTheTargetDirectory()
            throws Exception {

        RetrievalResponseXml retrievalPluginDataObjects = RetrievalPluginDataObjectsFixture.INSTANCE
                .get();

        new SerializeRetrievedDataToDirectory(directory)
                .processRetrievedPluginDataObjects(retrievalPluginDataObjects);

        service.findRetrievals();

        assertThat(directory, hasNoFiles());
    }

    @Test
    public void ignoresSubDirectories() throws Exception {

        new File(directory, "subDir1").mkdirs();

        service.findRetrievals();
    }

    @Test
    public void returnsNullWhenNoFileInTheTargetDirectory() throws Exception {

        final RetrievalResponseXml restored = service
                .findRetrievals();

        assertNull(restored);
    }
}
