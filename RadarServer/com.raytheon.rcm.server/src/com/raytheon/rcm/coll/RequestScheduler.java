package com.raytheon.rcm.coll;

import java.io.InputStream;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.GregorianCalendar;
import java.util.List;
import java.util.Random;
import java.util.SimpleTimeZone;
import java.util.TimeZone;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Unmarshaller;

import org.quartz.CronTrigger;
import org.quartz.Job;
import org.quartz.JobDataMap;
import org.quartz.JobDetail;
import org.quartz.JobExecutionContext;
import org.quartz.JobExecutionException;
import org.quartz.Scheduler;
import org.quartz.SchedulerException;
import org.quartz.impl.StdSchedulerFactory;

import com.raytheon.rcm.config.Configuration;
import com.raytheon.rcm.config.RadarConfig;
import com.raytheon.rcm.config.Util;
import com.raytheon.rcm.event.OtrEvent;
import com.raytheon.rcm.event.RadarEventAdapter;
import com.raytheon.rcm.event.RadarEventListener;
import com.raytheon.rcm.message.Message;
import com.raytheon.rcm.otrmgr.OTRHandler;
import com.raytheon.rcm.otrmgr.OTRManager;
import com.raytheon.rcm.request.Request;
import com.raytheon.rcm.server.Log;
import com.raytheon.rcm.server.RadarServer;

public class RequestScheduler extends RadarEventAdapter {

    // Quartz job detail properties
    private static final String SCHEDULER = "SC";
    private static final String CRON_OTR = "CO";
    
    private RadarServer radarServer;
    private OTRManager otrManager;

    private CronOTRConfiguration cronConfig;
    
    private Scheduler scheduler;
    
    private Random random = new Random();
    
    private static JAXBContext jaxbContext;
    
    public RequestScheduler(RadarServer radarServer) {
        this.radarServer = radarServer;
        
        for (RadarEventListener l : radarServer.getListeners()) {
            if (l instanceof OTRManager) {
                otrManager = (OTRManager) l;
                break;
            }
        }
        
        if (otrManager == null) {
            Log.errorf("%s requires an %s which was not found", 
                    RequestScheduler.class.getSimpleName(),
                    OTRManager.class.getSimpleName());
            return;
        }
        
        try {
            scheduler = StdSchedulerFactory.getDefaultScheduler();
            scheduler.start();
        } catch (SchedulerException e) {
            Log.errorf("Failed to start cron-OTR scheduler: %s", e);            
            scheduler = null;
        }
        
        if (scheduler !=  null) {
            InputStream ins = null;
            try {
                ins = radarServer.getConfiguration().getDropInData("cronOTRs.xml");
                try {
                    cronConfig = (CronOTRConfiguration) createUnmarshaller().unmarshal(ins);
                } finally {
                    ins.close();
                }
            } catch (Exception e) {
                Log.errorf("Error loading cron-OTR configuration: %s", e);
            }
            
            if (cronConfig != null) {
                int jobIndex = 1; // Used to generate unique JobDetail names
                for (CronOTR cronOTR : cronConfig.cronOTRList) {
                    String name = "CronOTRcron" + jobIndex;
                    CronTrigger trigger = new CronTrigger(name, name);
                    try {
                        trigger.setCronExpression(cronOTR.getCron());
                    } catch (Exception e) {
                        Log.errorf("Error setting cron \"%s\": %s", 
                                cronOTR.getCron(), e);
                        continue;
                    }
    
                    JobDetail jd = new JobDetail(name, null, CronOTRJob.class);
                    JobDataMap jdm = jd.getJobDataMap();
                    jdm.put(SCHEDULER, this);
                    jdm.put(CRON_OTR, cronOTR);
                    try {
                        scheduler.scheduleJob(jd, trigger);
                    } catch (Exception e) {
                        Log.errorf("Error schedule cron \"%s\": %s",
                                cronOTR.getCron(), e);
                    }
                    
                    jobIndex++;
                }
            }
        }
    }

    // Package access for testing
    static JAXBContext getJAXBContext() throws JAXBException {
        if (jaxbContext == null)
            jaxbContext = JAXBContext.newInstance(CronOTRConfiguration.class);
        return jaxbContext;
    }
    
    private static synchronized Unmarshaller createUnmarshaller() throws JAXBException {
        return getJAXBContext().createUnmarshaller();
    }

    protected static final TimeZone gmt = new SimpleTimeZone(0, "GMT"); // TimeZone.getTimeZone("GMT+0");
    
    private void runCron(CronOTR cronOTR) {
        Calendar cal = new GregorianCalendar(gmt);
        
        MyOTRHandler handler = null;
        if (cronOTR.isSendSpecified())
            handler = new MyOTRHandler(cronOTR);
        // else, handled by Collector as specified by prodList.txt

        for (RadarConfig rc : getCollectedRadars()) {
            List<Request> requests = cronOTR.createRequests(Util.getRadarType(rc), cal);
            if (requests.size() > 0)
                otrManager.sendOneTimeRequests(Arrays.asList(rc.getRadarID()),
                        requests, handler);
        }
    }
    
    private List<RadarConfig> getCollectedRadars() {
        Configuration config = radarServer.getConfiguration();
        ArrayList<RadarConfig> radarIDs = new ArrayList<RadarConfig>();        
        for (String radarID : config.getConfiguredRadarList()) {
            RadarConfig rc = config.getConfigForRadar(radarID);
            if (isCollectedRadar(rc))
                radarIDs.add(rc);
        }
        return radarIDs;
    }
    
    private boolean isCollectedRadar(RadarConfig rc) {
        return rc != null && rc.isDedicated() && rc.isCollectionEnabled()
                && rc.isEnabled();
    }

    private void runJob(CronOTR cronOTR) {
        if (cronOTR.getRandomWait() != null) {
            int sleepSeconds = random.nextInt(cronOTR.getRandomWait());
            try {
                Thread.sleep(sleepSeconds * 1000);
            } catch (InterruptedException e) {
                // ignore
            }
        }
        
        runCron(cronOTR);
    }
    
    /**
     * This class declared public only so that it can be instantiated by
     * Quartz.
     */
    public static class CronOTRJob implements Job {

        @Override
        public void execute(JobExecutionContext context)
                throws JobExecutionException {
            JobDataMap jdm = context.getMergedJobDataMap();
            RequestScheduler scheduler = 
                (RequestScheduler) jdm.get(SCHEDULER);
            scheduler.runJob((CronOTR) jdm.get(CRON_OTR));
        }
        
    }

    private class MyOTRHandler implements OTRHandler {

        private CronOTR cronOTR;

        public MyOTRHandler(CronOTR cronOTR) {
            this.cronOTR = cronOTR;
        }

        @Override
        public void handleOtrEvent(OtrEvent event) {
            byte[] data = event.product;
            int code = Message.messageCodeOf(data);
            
            if (code == Message.REQUEST_RESPONSE)
                return; // OTR failed

            Distribution.sendProductMessage(event.product, cronOTR.getWmo(),
                    cronOTR.getNnn(), event.radarID, "cron", radarServer
                            .getConfiguration());
        }

    }
}
