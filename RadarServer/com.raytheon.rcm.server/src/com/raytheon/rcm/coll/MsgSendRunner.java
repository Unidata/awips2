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
package com.raytheon.rcm.coll;

import java.io.File;
import java.io.IOException;
import java.util.Arrays;
import java.util.Scanner;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.ThreadFactory;

import com.raytheon.rcm.server.Log;

/**
 * Utility class to run {@code msg_send} in a separate thread
 * 
 */
public class MsgSendRunner implements Runnable {
    String[] args;

    File f;

    public MsgSendRunner(String[] args, File f) {
        this.args = args;
        this.f = f;
    }

    @Override
    protected void finalize() throws Throwable {
        super.finalize();
        cleanup();
    }

    protected void cleanup() {
        if (f != null) {
            try {
                f.delete();
                f = null;
            } catch (Exception e) {
                // nothing
            }
        }
    }

    // TODO: has to be daemon until there is a shutdown notification
    private static ExecutorService executorService = Executors
            .newSingleThreadExecutor(new ThreadFactory() {
                @Override
                public Thread newThread(Runnable r) {
                    Thread t = new Thread(r);
                    t.setDaemon(true);
                    return t;
                }
            });

    private static ExecutorService getExecutorService() {
        return executorService;
    }

    public void runAsync() {
        getExecutorService().submit(this);
    }

    @Override
    public void run() {
        try {
            String[] cmd = new String[args.length + 1];
            cmd[0] = "msg_send";

            String projectDir = System.getenv("PROJECT");
            if (projectDir != null)
                cmd[0] = (new File(new File(new File(projectDir), "bin"),
                        cmd[0])).getPath();
            System.arraycopy(args, 0, cmd, 1, args.length);

            Log.eventf("Running %s", Arrays.toString(cmd));

            StringBuilder output = new StringBuilder();
            Process p = null;
            try {
                ProcessBuilder pb = new ProcessBuilder(cmd);
                pb.redirectErrorStream(true);
                p = pb.start();
                Scanner s = new Scanner(p.getInputStream());
                while (s.hasNextLine()) {
                    if (output.length() > 0)
                        output.append('\n');
                    output.append(s.nextLine());
                }
                p.waitFor();
                if (p.exitValue() != 0) {
                    Log.errorf("msg_send exited with code %d\n%s",
                            p.exitValue(), output.toString());
                }
            } catch (IOException e) {
                Log.errorf("exec of msg_send failed: %s%s%s", e,
                        output.length() > 0 ? "\n" : "", output.toString());
            } catch (InterruptedException e) {
                Log.errorf("exec of msg_send failed: interrupted");
            } finally {
                // DR #10955
                if (p != null) {
                    p.destroy();
                }
            }
        } finally {
            cleanup();
        }
    }
}
