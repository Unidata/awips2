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

import junit.framework.Assert;

import com.raytheon.uf.common.datastorage.locking.ClusteredLockManager;

/**
 * A Lock Manager test:
 * 
 * Tests lock manager running inside the same JVM
 * 
 * <pre>
 * 
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
public class TestLockManager {

    public static void main(String[] args) {

        try {
            // Test #1: Single Thread

            ClusteredLockManager lockMgr = ClusteredLockManager.getInstance();

            boolean gotLock = lockMgr.getLock(new File("/tmp/foo"), true);
            if (gotLock == false) {
                System.out.println("ERROR: Lock was not granted initially");
            }

            gotLock = lockMgr.getLock(new File("/tmp/foo"), true);
            if (gotLock == true) {
                System.out.println("ERROR: Lock should not have been granted");
            }

            lockMgr.releaseLock(new File("/tmp/foo"));

            // Release and retry
            gotLock = lockMgr.getLock(new File("/tmp/foo"), true);
            if (gotLock == false) {
                System.out.println("ERROR: Lock was not granted after unlock");
            }

            lockMgr.releaseLock(new File("/tmp/foo"));
        } catch (Exception e) {
            e.printStackTrace();
            Assert.fail(e.getMessage());
        }

        // Test #2 Multi-thread
        System.out
                .println("Threaded test: Threads should both return approximately 0.5 load");

        LockTester lt1 = new LockTester(1);
        LockTester lt2 = new LockTester(2);

        Thread thread1 = new Thread(lt1);
        Thread thread2 = new Thread(lt2);

        thread1.start();
        thread2.start();

        System.out.println("Running test: This will take a few seconds...");
        while (thread1.getState() != State.TERMINATED
                || thread2.getState() != State.TERMINATED) {
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
