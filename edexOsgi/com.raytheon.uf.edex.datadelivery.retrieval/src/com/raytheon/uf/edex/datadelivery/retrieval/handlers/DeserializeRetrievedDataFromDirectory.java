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
import java.io.FileFilter;

import javax.xml.bind.JAXBException;

import com.raytheon.uf.common.datadelivery.registry.Coverage;
import com.raytheon.uf.common.serialization.JAXBManager;
import com.raytheon.uf.common.util.CollectionUtil;
import com.raytheon.uf.edex.datadelivery.retrieval.opendap.OpenDapRetrievalResponse;

/**
 * Deserializes the retrieved data in a directory.
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
public class DeserializeRetrievedDataFromDirectory implements
        IRetrievalsFinder {

    private static final FileFilter NO_DIRECTORIES = new FileFilter() {
        @Override
        public boolean accept(File pathname) {
            return pathname.isFile();
        }
    };

    private final File directory;

    private final JAXBManager jaxbManager;

    /**
     * @param directory
     */
    public DeserializeRetrievedDataFromDirectory(File directory) {
        this.directory = directory;
        try {
            this.jaxbManager = new JAXBManager(
                    RetrievalResponseXml.class,
                    OpenDapRetrievalResponse.class, Coverage.class);
        } catch (JAXBException e) {
            throw new ExceptionInInitializerError(e);
        }

    }

    /**
     * {@inheritDoc}
     */
    @Override
    public RetrievalResponseXml findRetrievals()
            throws Exception {

        final File[] files = directory.listFiles(NO_DIRECTORIES);

        if (CollectionUtil.isNullOrEmpty(files)) {
            return null;
        }

        final File file = files[0];
        try {
            return (RetrievalResponseXml) jaxbManager
                    .jaxbUnmarshalFromXmlFile(file);
        } finally {
            file.delete();
        }
    }

}
