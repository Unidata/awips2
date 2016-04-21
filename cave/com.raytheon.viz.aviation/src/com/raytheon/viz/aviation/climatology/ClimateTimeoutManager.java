package com.raytheon.viz.aviation.climatology;

import java.io.File;

import javax.xml.bind.JAXB;

import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.viz.aviation.xml.ClimateTimeouts;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 4, 2011  8896       rferrel     Initial creation
 * Aug 9, 2013  2033       mschenke    Switched File.separator to IPathManager.SEPARATOR
 * 
 * </pre>
 * 
 * @author rferrel
 * @version 1.0
 */
public class ClimateTimeoutManager {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(ClimateTimeoutManager.class);

    private static ClimateTimeoutManager instance = null;

    private ClimateTimeouts timeouts;

    private ClimateTimeoutManager() {
    }

    public synchronized final static ClimateTimeoutManager getInstance() {
        if (instance == null) {
            instance = new ClimateTimeoutManager();
            instance.initTimeouts();
        }
        return instance;
    }

    public synchronized final static void disposeInstance() {
        instance = null;
    }

    private void initTimeouts() {
        StringBuilder path = new StringBuilder();
        path.append("aviation").append(IPathManager.SEPARATOR).append("config")
                .append(IPathManager.SEPARATOR).append("gui")
                .append(IPathManager.SEPARATOR).append("ClimateTimeouts.xml");
        IPathManager pm = PathManagerFactory.getPathManager();
        File fname = pm.getStaticFile(path.toString());
        try {
            timeouts = JAXB.unmarshal(fname, ClimateTimeouts.class);
        } catch (RuntimeException e) {
            statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(), e);
            timeouts = new ClimateTimeouts();
        }

        if (timeouts.getClimateMetarTimeout() <= 0) {
            timeouts.setClimateMetarTimeout(20);
        }

        if (timeouts.getWindRoseTimeout() <= 0) {
            timeouts.setWindRoseTimeout(20);
        }

        if (timeouts.getCigVisDistTimeout() <= 0) {
            timeouts.setCigVisDistTimeout(90);
        }

        if (timeouts.getCigVisTrendTimeout() <= 0) {
            timeouts.setCigVisTrendTimeout(300);
        }
    }

    public int getClimateMetarTimeout() {
        return timeouts.getClimateMetarTimeout();
    }

    public int getWindRoseTimeout() {
        return timeouts.getWindRoseTimeout();
    }

    public int getCigVisDistTimeout() {
        return timeouts.getCigVisDistTimeout();
    }

    public int getCigVisTrendTimeout() {
        return timeouts.getCigVisTrendTimeout();
    }
}
