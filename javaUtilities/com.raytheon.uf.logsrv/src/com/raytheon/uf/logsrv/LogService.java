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
package com.raytheon.uf.logsrv;

import java.io.File;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.Unmarshaller;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import ch.qos.logback.classic.LoggerContext;
import ch.qos.logback.classic.joran.JoranConfigurator;

import com.raytheon.uf.logsrv.config.LogSrvConfig;
import com.raytheon.uf.logsrv.derby.DerbyDao;
import com.raytheon.uf.logsrv.quartz.JobScheduler;

/**
 * The main class of the logging service that loads the config files and
 * therefore listens for incoming logging events.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 27, 2013            njensen     Initial creation
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class LogService {

    private static final String LOGBACK_CONFIG = "receiver.xml";

    private static final String SERVICE_CONFIG = "config.xml";

    private static final String ENV_CONF_DIR = "logSrvConf";

    private static final Logger logger = LoggerFactory
            .getLogger("InternalLogger");

    /**
     * @param args
     * @throws Exception
     */
    public static void main(String[] args) throws Exception {
        logger.info("Starting log analytics service");
        String confDir = System.getenv(ENV_CONF_DIR);
        if (confDir == null) {
            throw new LogServiceException("Environment variable "
                    + ENV_CONF_DIR
                    + " is not set! Unable to find configuration!");
        }

        JAXBContext context = JAXBContext.newInstance(LogSrvConfig.class);
        Unmarshaller m = context.createUnmarshaller();
        LogSrvConfig config = (LogSrvConfig) m.unmarshal(new File(confDir
                + SERVICE_CONFIG));
        config.validate();
        DerbyDao.getInstance().setConfig(config);
        logger.info("Logging events from " + config.getClusterName());

        logger.info("Starting socket listener");
        LoggerContext lc = (LoggerContext) LoggerFactory.getILoggerFactory();
        lc.reset();
        JoranConfigurator configurator = new JoranConfigurator();
        configurator.setContext(lc);
        configurator.doConfigure(confDir + LOGBACK_CONFIG);

        logger.info("Scheduling report generation");
        JobScheduler.scheduleJobs(config);
    }

    public static Logger getLogger() {
        return logger;
    }

}
