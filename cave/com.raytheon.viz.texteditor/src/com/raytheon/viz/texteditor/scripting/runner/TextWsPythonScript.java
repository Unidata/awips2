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

import java.io.File;

import jep.Jep;
import jep.JepException;

import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.viz.texteditor.msgs.IScriptRunnerObserver;

/**
 * Implements a JEP based Script runner for the Text WS.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 29, 2009            mfegan     Initial creation
 *
 * </pre>
 *
 * @author mfegan
 * @version 1.0	
 */

public class TextWsPythonScript {
    private final static String BASE_PATH = "textws/scripting";
    private static final String BASE_SCRIPT = BASE_PATH + File.separator + "twsScripting.py";
    
    private static final String EDITOR_FMT = "editor = \"Text %s\"";

    private static final String SCRIPT_CANCELLED = "ScriptCancelled";
    
    /** the observer object - to pass on */
    private final IScriptRunnerObserver observer;
    /** the Script Edit Window ID Token */
    private final String token;
    /** the JEPP Python interpreter */
    private Jep jep = null;
    private boolean cancelled = false;
    
    /**
     * Constructor.
     * 
     * @param observer
     * @param token
     * @throws JepException
     */
    public TextWsPythonScript(IScriptRunnerObserver observer, String token) throws JepException {
        File bundle = PathManagerFactory.getPathManager().getStaticFile(BASE_SCRIPT);
        String path = bundle.getParent();
        this.observer = observer;
        this.token = token;
        jep = new Jep(false,path,TextWsPythonScript.class.getClassLoader());
        initializeJep(bundle);
        jep.setInteractive(true);
        jep.set("observer", observer);
        jep.eval(String.format(EDITOR_FMT, token));
    }
    /**
     * 
     * @param filePath
     * @throws JepException
     */
    public void execute(String filePath) throws JepException {
        if (jep != null) {
            try {
                jep.runScript(filePath);
                System.out.println("script ran");
                
            } catch (JepException e) {
                System.out.println("exception is "+e);
                if (e.getMessage().indexOf(SCRIPT_CANCELLED) == -1) {
                    throw e;
                } else {
                    cancelled = true;
                }
            }
        }
    }

    /**
     * 
     */
    public void dispose() {
        if (jep != null) {
            jep.close();
            jep = null;
        }
    }
    /**
     * 
     * @param filePath
     * @throws JepException
     */
    private void initializeJep(File path) throws JepException {
        String filePath = path.getPath();
        if (jep != null) {
            jep.eval("import JavaImporter");
            jep.runScript(filePath);
        }
    }
    public boolean isCancelled() {
        return cancelled;
    }
}
