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
package com.raytheon.uf.edex.archive.feeder;

import java.io.BufferedInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;

import com.raytheon.uf.common.serialization.DynamicSerializationManager;
import com.raytheon.uf.common.serialization.DynamicSerializationManager.SerializationType;
import com.raytheon.uf.common.serialization.SerializationException;

/**
 * This class uses the DynamicSerializationManager to create an instance of the
 * first serialized object in a file.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 25, 2014 2838       rferrel     Initial creation
 * 
 * </pre>
 * 
 * @author rferrel
 * @version 1.0
 */

public class ArchiveBinaryReader {

    /** File containing the serialized object. */
    private File binFile;

    /** Instance of the file's serialized object. */
    Object object;

    /**
     * Constructor
     * 
     * @param binFile
     *            - File containing serialized object.
     */
    public ArchiveBinaryReader(File binFile) {
        this.binFile = binFile;
    }

    /**
     * The object obtained from the binFile.
     * 
     * @return object
     * @throws FileNotFoundException
     * @throws SerializationException
     */
    public Object getObject() throws FileNotFoundException,
            SerializationException {
        if (object == null) {
            object = readObject();
        }
        return object;
    }

    /**
     * Create an instance of the file's serialized object.
     * 
     * @return object
     * @throws FileNotFoundException
     * @throws SerializationException
     */
    private Object readObject() throws FileNotFoundException,
            SerializationException {
        InputStream inputStream = null;
        Object object = null;
        try {
            DynamicSerializationManager dsm = DynamicSerializationManager
                    .getManager(SerializationType.Thrift);
            inputStream = new BufferedInputStream(new FileInputStream(binFile));
            object = dsm.deserialize(inputStream);
        } finally {
            if (inputStream != null) {
                try {
                    inputStream.close();
                } catch (IOException e) {
                    // Ignore
                }
                inputStream = null;
            }
        }
        return object;
    }
}