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

package com.raytheon.uf.common.datastorage.hdf5;

import java.io.File;
import java.lang.Thread.State;
import java.util.Random;

import com.raytheon.uf.common.datastorage.DataStoreFactory;
import com.raytheon.uf.common.datastorage.IDataStore;
import com.raytheon.uf.common.datastorage.records.FloatDataRecord;

/**
 * Test locking mechanism on HDF5
 * 
 * Start two instance of this program and check for errors
 * 
 * <pre>
 * 
 *  SOFTWARE HISTORY
 * 
 *  Date         Ticket#     Engineer    Description
 *  ------------ ----------  ----------- --------------------------
 *  Feb 20, 2007             chammack    Initial Creation.
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1
 */
public class TestHDF5Locking {

    private static File TEST_FILE = new File("/tmp/test.hdf");

    public static void main(String[] args) {
        Random rand = new Random(System.currentTimeMillis());
        int uniqueId = rand.nextInt();

        Thread thread1 = new Thread(new HDF5Writer(uniqueId, 1));
        Thread thread2 = new Thread(new HDF5Writer(uniqueId, 2));

        try {
            Thread.sleep(3000);
        } catch (InterruptedException e1) {
            // TODO Auto-generated catch block
            e1.printStackTrace();
        }

        thread1.start();
        thread2.start();
        while (thread1.getState() != State.TERMINATED
                || thread2.getState() != State.TERMINATED) {
            try {
                Thread.sleep(100);
            } catch (InterruptedException e) {
                // TODO Auto-generated catch block
                e.printStackTrace();
            }
        }

        System.out.println("Complete");
        System.exit(0);

    }

    public static class HDF5Writer implements Runnable {

        private int progId;

        private int instanceId;

        public HDF5Writer(int progId, int instanceId) {
            this.progId = progId;
            this.instanceId = instanceId;
        }

        /*
         * (non-Javadoc)
         * 
         * @see java.lang.Runnable#run()
         */
        public void run() {
            IDataStore dataStore = DataStoreFactory.getDataStore(TEST_FILE);

            float[] dummyFloatData = new float[1024];

            for (int i = 0; i < 5000; i++) {
                FloatDataRecord fdr = new FloatDataRecord("" + i, progId + "/"
                        + instanceId, dummyFloatData);
                try {
                    dataStore.addDataRecord(fdr);
                    dataStore.store();
                    // dataStore.store("/");
                } catch (Exception e) {
                    // TODO Auto-generated catch block
                    e.printStackTrace();
                }
            }
        }

    }

}
