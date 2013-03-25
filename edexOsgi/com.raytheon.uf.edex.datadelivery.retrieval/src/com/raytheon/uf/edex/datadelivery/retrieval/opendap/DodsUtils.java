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

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;

import com.raytheon.edex.util.Util;
import com.raytheon.uf.common.serialization.SerializationException;

import dods.dap.DConnect;
import dods.dap.DataDDS;

/**
 * Utilities for working with net.dods.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 12, 2013 1543       djohnson     Initial creation
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */

public class DodsUtils {

    /**
     * Prevent construction.
     */
    private DodsUtils() {
    }

    /**
     * Convert the DataDDS instance to a byte array.
     * 
     * @param dataDds
     *            the DataDDS instance
     * @return the byte array
     * @throws SerializationException
     *             on error converting to a byte array
     */
    public static byte[] convertDataDdsToByteArray(DataDDS dataDds)
            throws SerializationException {
        ByteArrayOutputStream os = null;

        try {
            os = new ByteArrayOutputStream(700);

            dataDds.externalize(os, true, true);

            return os.toByteArray();
        } catch (IOException e) {
            throw new SerializationException(
                    "Unable to externalize the DataDDS instance.", e);
        } finally {
            Util.close(os);
        }
    }

    /**
     * Restore the {@link DataDDS} from the byte array.
     * 
     * @param byteArray
     * @return the DataDDS instance
     * @throws SerializationException
     *             on error restoring the DataDDS
     */
    public static DataDDS restoreDataDdsFromByteArray(byte[] byteArray)
            throws SerializationException {
        DConnect dconnect = new DConnect(new ByteArrayInputStream(byteArray));
        DataDDS data = null;
        try {
            data = dconnect.getData(null);
            return data;
        } catch (Exception e) {
            throw new SerializationException(e);
        }
    }

}
