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
package com.raytheon.edex.uengine.runners;

import java.util.List;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.raytheon.edex.uengine.exception.MicroEngineException;
import com.raytheon.uf.common.message.response.AbstractResponseMessage;

/**
 * Base class for &mu;Engine script runners. Provides default implementations of
 * IMicroEngine's {@link #setScript(String)}, {@link #executeScript(String)},
 * and {@link #getResult()} methods. It also provides the script and results
 * fields and the logger instance. As implemented here,
 * {@link #setScript(String)} and {@link #getResult()} are Bean style accessors;
 * {@link #executeScript(String)} and {@link #initialize(String)} call the
 * appropriate methods. {@link #initialize()} and {@link #release()} provide
 * safe, no-op implementations. All other methods are abstract and must be
 * implemented.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 12Nov2008    1709       MW Fegan    Initial Creation.
 * 25May2011    8686       cjeanbap    Add getter/setter for Triggers.
 * 12Dec2015    5166       kbisanz     Update logging to use SLF4J
 * 
 * </pre>
 * 
 * @author mfegan
 * @version 1.0
 */

public abstract class AMicroEngine implements IMicroEngine {
    /**
     * Logger instance for system logging.
     */
    protected transient Logger logger = LoggerFactory.getLogger(getClass());

    /**
     * The script to run.
     */
    protected String script = null;

    /**
     * Holds the results from running the &mu;Engine script.
     */
    protected List<AbstractResponseMessage> result = null;

    protected String trigger = null;

    /**
     * Constructor.
     */
    public AMicroEngine() {
        super();
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.edex.uengine.runners.IMicroEngine#execute()
     */
    @Override
    public abstract void execute() throws MicroEngineException;

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.edex.uengine.runners.IMicroEngine#initialize()
     */
    @Override
    public void initialize() throws MicroEngineException {
        // the default is to do no initialization
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.edex.uengine.runners.IMicroEngine#release()
     */
    @Override
    public void release() throws MicroEngineException {
        // the default is to not release any resources
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.edex.uengine.runners.IMicroEngine#executeScript(java.lang
     * .String)
     */
    @Override
    public List<AbstractResponseMessage> executeScript(String script)
            throws MicroEngineException {
        this.script = script;
        try {
            initialize();
            execute();
            return result;
        } finally {
            release();
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.edex.uengine.runners.IMicroEngine#getResult()
     */
    @Override
    public List<AbstractResponseMessage> getResult() {
        return result;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.edex.uengine.runners.IMicroEngine#initialize(java.lang.String
     * )
     */
    @Override
    public void initialize(String script) throws MicroEngineException {
        this.script = script;
        initialize();
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.edex.uengine.runners.IMicroEngine#setScript(java.lang.String
     * )
     */
    @Override
    public void setScript(String script) {
        this.script = script;
    }

    public void setTrigger(String trigger) {
        this.trigger = trigger;
    }

    public String getTrigger() {
        return this.trigger;
    }
}
