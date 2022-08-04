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
package com.raytheon.viz.texteditor.scripting.runner;

import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.atomic.AtomicInteger;

import com.raytheon.viz.texteditor.msgs.IScriptRunnerObserver;

/**
 * Manages creation and lifetime of TextWS script threads
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Jan 02, 2018  6804     tgurney   Initial creation
 * Feb 26, 2019  7746     randerso  Change to use Path instead of String for
 *                                  TextWS script path.
 * Mar 04, 2019  7601     tgurney   Include script path in the log when starting
 *                                  a script
 *
 * </pre>
 *
 * @author tgurney
 */

public class TextWsScriptThreadManager {
    private AtomicInteger threadCounter = new AtomicInteger();

    private final List<TextWsScriptThread> threads = new ArrayList<>();

    private static final TextWsScriptThreadManager instance = new TextWsScriptThreadManager();

    private TextWsScriptThreadManager() {
        // singleton
    }

    public static TextWsScriptThreadManager getInstance() {
        return instance;
    }

    /**
     * Start the script in a new thread.
     *
     * @param scriptPath
     *            Absolute path to the script on file system
     * @param observer
     * @param token
     * @return a controller object that can be used to stop the script.
     */
    public ITextWsScriptController runScript(Path scriptPath,
            IScriptRunnerObserver observer, String token) throws Exception {
        TextWsScriptThread thread = null;
        observer.clearErrBuffer();
        observer.writeText("--- Running Script: " + scriptPath + " ---\n");
        observer.showScriptStatus("Running script \"" + scriptPath + "\"");
        try {
            String threadName = "TextWsScriptThread-"
                    + threadCounter.getAndIncrement();
            thread = new TextWsScriptThread(threadName, scriptPath, observer,
                    token);
            synchronized (threads) {
                threads.add(thread);
            }
            thread.start();
            return thread;
        } catch (Exception e) {
            if (thread != null) {
                synchronized (threads) {
                    threads.remove(thread);
                }
            }
            observer.writeText("--- Script Error! ---\n");
            observer.showErrorMessage(
                    "Text WorkStation Scripting Error\n" + e.getMessage(), e);
            throw e;
        }
    }

    public void stopAllScripts() {
        synchronized (threads) {
            for (TextWsScriptThread thread : threads) {
                thread.cancel();
                thread.interrupt();
            }
        }
    }

    /** Called by a script thread before it dies */
    public void threadFinished(TextWsScriptThread thread) {
        synchronized (threads) {
            threads.remove(thread);
        }
    }
}
