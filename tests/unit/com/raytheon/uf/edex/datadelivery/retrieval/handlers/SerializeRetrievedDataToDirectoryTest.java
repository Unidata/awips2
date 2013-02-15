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

import java.io.File;

import org.junit.Test;

import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.util.TestUtil;

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
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */
public class SerializeRetrievedDataToDirectoryTest {

    private final File directory = TestUtil
            .setupTestClassDir(SerializeRetrievedDataToDirectoryTest.class);

    private final SerializeRetrievedDataToDirectory service = new SerializeRetrievedDataToDirectory(
            directory);

    @Test
    public void serializesRetrievedDataToAFileInTheTargetDirectory()
            throws SerializationException {

        RetrievalResponseXml retrievalPluginDataObjects = RetrievalPluginDataObjectsFixture.INSTANCE
                .get();

        service.processRetrievedPluginDataObjects(retrievalPluginDataObjects);

        assertThat(directory, hasNumberOfFiles(1));
    }
}
