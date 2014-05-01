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
package com.raytheon.uf.common.pypies;

import java.io.File;

import com.raytheon.uf.common.datastorage.StorageProperties;
import com.raytheon.uf.common.datastorage.records.FloatDataRecord;
import com.raytheon.uf.common.datastorage.records.StringDataRecord;
import com.raytheon.uf.common.pypies.response.RetrieveResponse;
import com.raytheon.uf.common.serialization.DynamicSerializationManager;
import com.raytheon.uf.common.serialization.SerializationUtil;
import com.raytheon.uf.common.util.FileUtil;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 11, 2010            njensen     Initial creation
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class PyToJavaTest {

    /**
     * @param args
     */
    public static void main(String[] args) {
        File file = new File("/tmp/pyLight");
        try {
            byte[] bytes = FileUtil.file2bytes(file);
            // DynamicSerializationManager.inspect(TestStoreRequest.class);
            DynamicSerializationManager.inspect(RetrieveResponse.class);
            DynamicSerializationManager.inspect(StringDataRecord.class);
            DynamicSerializationManager.inspect(StorageProperties.class);
            DynamicSerializationManager.inspect(FloatDataRecord.class);
            long t0 = System.currentTimeMillis();
            // TestStoreRequest req = (TestStoreRequest) SerializationUtil
            // .transformFromThrift(bytes);
            Object obj = SerializationUtil.transformFromThrift(bytes);
            long t1 = System.currentTimeMillis();
            System.out
                    .println("Java deserializing time: " + (t1 - t0) / 1000.0);
            // System.out.println("floatValue " + req.getFloatValue());
            // System.out.println("groupName " + req.getGroupName());
            // System.out.println("nullTest " + req.getNullTest());
            // System.out.println("otherName " + req.getOtherName());
            // System.out.println("size " + req.getSize());
            // System.out.println("value " + req.getValue());
            // System.out.println("floatArray " + req.getFloatArray().length
            // + ", " + req.getFloatArray()[0]);
            // System.out.println("intArray " + req.getIntArray().length + ", "
            // + req.getIntArray()[0]);
            // System.out.println("testMap " + req.getTestMap());
            // System.out.println("testSet " + req.getTestSet());
            // System.out.println("someEnum " + req.getSomeEnum());
        } catch (Exception e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }

    }

}
