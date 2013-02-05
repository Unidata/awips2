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

import java.io.File;
import java.io.IOException;
import java.util.UUID;

import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.serialization.SerializationUtil;
import com.raytheon.uf.common.util.FileUtil;

/**
 * Serializes the retrieved data to a directory.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 01, 2013 1543       djohnson     Initial creation
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */
public class SerializeRetrievedDataToDirectory implements
        IRetrievalPluginDataObjectsProcessor {

    private final File targetDirectory;

    /**
     * @param directory
     */
    public SerializeRetrievedDataToDirectory(File directory) {
        this.targetDirectory = directory;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void processRetrievedPluginDataObjects(
            RetrievalPluginDataObjects retrievalPluginDataObjects)
            throws SerializationException {
        try {
            FileUtil.bytes2File(SerializationUtil
                    .transformToThrift(retrievalPluginDataObjects), new File(
                    targetDirectory, UUID.randomUUID().toString()));
        } catch (IOException e) {
            throw new SerializationException(e);
        }
    }

}
