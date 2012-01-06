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

package com.raytheon.edex.test;

import java.io.File;
import java.lang.Thread.State;

import com.raytheon.uf.common.datastorage.locking.ClusteredLockManager;
import com.raytheon.uf.common.datastorage.locking.LockException;

/**
 * Clustered version of TestLockManager. This requires multicast.
 * 
 * Run two copies of this program at the same time.
 * 
 * Should yield approximately 0.5 on each program.
 * 
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * Feb 20, 2007             chammack    Initial Creation.
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1
 */
public class TestLockManagerClustered {

    public static void main(String[] args) {

        try {
            // ClusteredLockManager lockMgr =
            // ClusteredLockManager.getInstance();
            ClusteredLockManager.getInstance();
            Thread.sleep(2000);
        } catch (LockException e1) {
            // TODO Auto-generated catch block
            e1.printStackTrace();
        } catch (InterruptedException e1) {
            // TODO Auto-generated catch block
            e1.printStackTrace();
        }

        // Test #1 Clustered
        System.out
                .println("Clustered test: Programs should both return approximately 0.5 load");

        LockTester lt1 = new LockTester(1);

        Thread thread1 = new Thread(lt1);

        thread1.start();

        System.out.println("Running test: This will take a few seconds...");
        while (thread1.getState() != State.TERMINATED) {
            try {
                Thread.sleep(100);
            } catch (InterruptedException e) {
                // TODO Auto-generated catch block
                e.printStackTrace();
            }
        }

        System.exit(0);
    }

    public static class LockTester implements Runnable {

        private int id;

        public LockTester(int id) {
            this.id = id;
        }

        public void run() {
            try {

                ClusteredLockManager lockMgr = ClusteredLockManager
                        .getInstance();

                int hits = 0;
                for (int i = 0; i < 1000; i++) {
                    boolean gotLock = lockMgr.getLock(new File("/tmp/foo"),
                            true);
                    Thread.sleep(10);
                    if (gotLock) {
                        lockMgr.releaseLock(new File("/tmp/foo"));
                        hits++;
                    }
                    Thread.sleep(10);

                }

                System.out.println("Thread #" + id + ":: " + (hits) / 1000.0);
            } catch (Exception e) {
                e.printStackTrace();
            }
        }

    }

}
