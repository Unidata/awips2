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
package com.raytheon.uf.edex.backupsvc.database;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import org.hibernate.HibernateException;
import org.hibernate.Session;
import org.hibernate.StatelessSession;
import org.hibernate.Transaction;

import com.raytheon.uf.common.backupsvc.BackupHost;
import com.raytheon.uf.common.backupsvc.response.GetEDEXVersionResponse;
import com.raytheon.uf.common.dataplugin.backupsvc.database.BackupSvcJobInfo;
import com.raytheon.uf.common.dataplugin.backupsvc.database.BackupSvcOutgoing;
import com.raytheon.uf.common.dataplugin.backupsvc.database.DbStatus;
import com.raytheon.uf.common.util.SizeUtil;
import com.raytheon.uf.common.util.SystemUtil;
import com.raytheon.uf.common.util.app.AppInfo;
import com.raytheon.uf.edex.database.dao.CoreDao;
import com.raytheon.uf.edex.database.dao.DaoConfig;

/**
 * Data Access Object for Backup Service outgoing table which contains the
 * received backup service job information.
 * 
 * This class is intended to handle backup service jobs created in Edex versions
 * >= 21.4.1
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#     Engineer        Description
 * ------------ ----------  --------------- ----------------------------
 * Apr 20, 2021 88364       Amanuel Challa  Initial creation
 * May 25, 2021 91044       Robert.Blum     Update parameters to be set correctly.
 * Jun 22, 2021 92788       Gang Chen       Modify the createOutBSJ() parameters
 * Jul 01, 2021 93517       Amanuel Challa  Added new parameter to getAllOutgoingJobs() and
 *                                          Modified to return filtered jobs
 * Jul 13, 2021 93058       Amanuel Challa  Changed updatetime in updateStatus()/updateContent()
 *                                          from Integer to Long
 * Jul 15, 2021 88368       Gang Chen       Remove the outgoing BSJs over 2 weeks
 * Jul 07, 2021 84656       Amanuel Challa  Log messages clean up
 * Jul 27, 2021 84654       Robert.Blum     Set recipient version on Job creation.
 * Aug 13, 2021 92922       Robert.Blum     Cleanup of the job statuses and update DAO for new db fields.
 * Sep 17, 2021 96153       Gang Chen       Fix the purging bugs for obsolete Accepted / Rejected BSJs
 * Oct 08, 2021 97253       Robert.Blum     Fix wording of purge log msgs.
 * Jul 24, 2023 2035783     Lisa.Singh      Properly handle jobs when unable to reach recipient site.
 * Apr 23, 2026 2040631     Tim Jensen      Improved exception handling in createOutBSJ()
 *
 * </pre>
 *
 * @author achalla
 */
public class BackupSvcOutgoingDao extends CoreDao {
    private static final String DB_NAME = "metadata";

    private static final String UPDATE_STATUS_QUERY = "UPDATE awips.backup_svc_outgoing set status = :status,"
            + "updatetime = :updatetime WHERE bksvc_id in (:ids)";

    private static final String UPDATE_OUTGOING_QUERY = "UPDATE awips.backup_svc_outgoing set"
            + " updatetime=:updatetime WHERE jobinfo_id in (:ids) AND status='New'";

    private static final String UPDATE_JOBINFO_QUERY = "UPDATE awips.backup_svc_jobinfo set"
            + " blob=:blobt WHERE id in (:ids)";

    private static final String PURGE_OLD_JOBS_QUERY = "DELETE from awips.backup_svc_outgoing"
            + " where status in ('" + DbStatus.ACCEPTED + "','"
            + DbStatus.REJECTED + "') and updatetime < :updatetime";

    private static final String PURGE_OLD_JOBINFO_QUERY = "DELETE from awips.backup_svc_jobinfo WHERE id in (:ids)";

    private static final long MAX_FILE_SIZE = SizeUtil.BYTES_PER_MB
            * SizeUtil.BYTES_PER_KB;

    private static final int SUCCCESS = 1;

    private static final int FAIL = 0;

    public BackupSvcOutgoingDao() {
        super(DaoConfig.forDatabase(DB_NAME));
    }

    /**
     * Create a Database Data Entry at Sender Site
     *
     * @param blob
     *            The serialized request blob, limit up to 1GB
     * @param jobName
     *            The transferred AWIPS file path and name
     * @param component
     *            Edex component that AWIPS file is linked to
     * @param senderSite
     *            The site where an AWIPS file was added in the host server
     * @param recipientHosts
     *            List of BackupHosts that an AWIPS file is sent to
     * @param systemVersion
     *            The AWIPS system version when a Backup Service job was added
     * @param compatibleVersion
     *            The backward compatible AWIPS system version
     *
     *
     */
    public int createOutBSJ(byte[] blob, String jobFileLocation,
            String jobFilePath, String component, String senderSite,
            List<BackupHost> recipientHosts) {
        Transaction tx = null;

        Long now = System.currentTimeMillis();
        String edexVersion = AppInfo.getInstance().getVersion();
        if (edexVersion == null) {
            edexVersion = GetEDEXVersionResponse.UNDEFINED;
        }
        try (Session session = getSessionFactory().openSession()) {
            tx = session.beginTransaction();
            BackupSvcJobInfo requestBSJInfo = new BackupSvcJobInfo();
            List<BackupSvcOutgoing> failedJobs = new ArrayList<>();

            /*
             * The transferred IServerRequest must not be over the limit of of
             * 1GB
             */
            if (blob != null) {
                if (blob.length > MAX_FILE_SIZE) {
                    throw new IOException(
                            "The transferred IServerRequest limit is 5242880 bytes. The current request size is: "
                                    + SizeUtil.prettyByteSize(blob.length));
                }
            }
            requestBSJInfo.setRequestBlob(blob);
            requestBSJInfo.setJobFileLocation(jobFileLocation);
            requestBSJInfo.setJobFilePath(jobFilePath);
            session.persist(requestBSJInfo);

            for (BackupHost host : recipientHosts) {
                BackupSvcOutgoing outgoingJob = new BackupSvcOutgoing();
                outgoingJob.setComponent(component);
                outgoingJob.setBackupSvcJobInfo(requestBSJInfo);
                outgoingJob.setSenderSite(senderSite);
                outgoingJob.setRecipientSite(host.getSite());
                outgoingJob.setUpdateTime(now);
                outgoingJob.setSenderHostName(SystemUtil.getHostName());
                outgoingJob.setSenderPort(System.getenv("HTTP_PORT"));
                outgoingJob.setRecipientHostName(host.getHostName());
                outgoingJob.setSystemVersion(edexVersion);
                outgoingJob.setRecipientVersion(host.getEDEXVersion(true));
                // If no recipient version was retrieved, then the receiving
                // site is either down or does not exist. We need to record the
                // failure in the DB.
                if (outgoingJob.getRecipientVersion() == null) {
                    outgoingJob.setStatus(DbStatus.FAILED.toString());
                    failedJobs.add(outgoingJob);
                } else {
                    outgoingJob.setStatus(DbStatus.NEW.toString());
                }
                session.persist(outgoingJob);
            }

            if (failedJobs.isEmpty()) {
                logger.info(
                        "Successfully saved Outgoing Job content at Sender Site.");
            } else {
                // If numerous jobs are being sent to the same bad site, it
                // doesn't make sense to have an error for each failed job every
                // 5 minutes in the edex logs. Log one error per bad site.
                List<String> badHosts = new ArrayList<>();
                for (BackupSvcOutgoing job : failedJobs) {
                    if (!badHosts.contains(job.getRecipientSite())) {
                        logger.error("Failed to send job(s) to site '"
                                + job.getRecipientHostName() + "'.");
                        badHosts.add(job.getSenderSite());
                    }
                }
            }
            tx.commit();
        } catch (HibernateException e) {
            logger.error("Error saving database data entry at Sender Site: ",
                    e);
            if (tx != null) {
                tx.rollback();
            }

            return FAIL;
        } catch (Exception e) {
            logger.error("Error in Backup Service Outgoing Dao: ", e);
            if (tx != null) {
                tx.rollback();
            }
            return FAIL;
        }
        return SUCCCESS;
    }

    /**
     * Return all Backup Service Job information found as a list .
     */
    @SuppressWarnings("unchecked")
    public List<BackupSvcJobInfo> getAllJobInfo() {
        List<BackupSvcJobInfo> allJobInfos = null;
        Transaction tx = null;
        try (Session s = getSessionFactory().openSession()) {
            tx = s.beginTransaction();
            allJobInfos = s.createQuery("from BackupSvcJobInfo").list();
            logger.info("Successfully retrieved: " + allJobInfos.size()
                    + " Backup Service Job Info.");
            tx.commit();
        } catch (HibernateException e) {
            logger.error("Failed retrieving Backup Service Job Info. ", e);
            if (tx != null) {
                tx.rollback();
            }
        } catch (Exception e) {
            logger.error("Error in Backup Service Outgoing Dao: ", e);
            if (tx != null) {
                tx.rollback();
            }
        }
        return allJobInfos;
    }

    /**
     * Receive a BackUp Service Outgoing Job as a parameter and return the
     * related Job info
     *
     * @param BackupSvcOutgoing
     */
    public BackupSvcJobInfo getJobInfo(BackupSvcOutgoing outgoingJob) {
        BackupSvcJobInfo jobInfo = null;
        try (StatelessSession ss = getSessionFactory().openStatelessSession()) {
            jobInfo = (BackupSvcJobInfo) ss.get(BackupSvcJobInfo.class,
                    outgoingJob.getJobInfoId());
            logger.info("Successfully retrieved Backup Service Job Info.");
        } catch (HibernateException e) {
            logger.error("Failed to retrieve Backup Service Job Info: ", e);
        } catch (Exception e) {
            logger.error("Error in Backup Service Outgoing Dao: ", e);
        }
        return jobInfo;
    }

    /**
     * Return all Backup Service Outgoing Jobs found as a List.
     *
     * @return outJobs
     *
     *         A list of out Outgoing Backup Service Jobs
     */

    public List<BackupSvcOutgoing> getAllOutgoingJobs() {
        return getAllOutgoingJobs(null);
    }

    /**
     * Return all Backup Service Outgoing Jobs found as a List .
     *
     * @param queryStr
     *            SQL query generated in Backup Service Filter Dialog to filter
     *            jobs
     * @return outJobs
     *
     *         A list of out Outgoing Backup Service Jobs
     *
     */

    @SuppressWarnings("unchecked")
    public List<BackupSvcOutgoing> getAllOutgoingJobs(String queryStr) {

        /*
         * sql query example bksvc_id > 3 and component = 'Test Component' and
         * recipientsite = 'dev-01' and systemversion >= '50.1.1' and sendersite
         * = 'OAX'
         */
        // two call to jobinfo and combine it before
        // select * from back_svc_outgoing where sendersite = 'OAX' and
        // jobinfo_id in(select id from jobinfo where jobfilepath = 'xyz');
        List<BackupSvcOutgoing> outJobs = null;
        Transaction tx = null;

        if (queryStr != null && (!queryStr.isEmpty())) {
            // build the query string from parameter
            StringBuilder sb = new StringBuilder();
            sb.append("from BackupSvcOutgoing where ");
            sb.append(queryStr);
            String newQuery = sb.toString();

            try (Session s = getSessionFactory().openSession()) {
                tx = s.beginTransaction();
                outJobs = s.createQuery(newQuery).list();
                logger.info("Successfully retrieved: " + outJobs.size()
                        + " Outgoing Backup Service Job(s) with filtering Query: "
                        + queryStr);
                tx.commit();
            } catch (HibernateException e) {
                logger.error(
                        "Failed to retrieve Backup Service Outgoing Job(s): ",
                        e);
                if (tx != null) {
                    tx.rollback();
                }
            } catch (Exception e) {
                logger.error("Error in Backup Service Outgoing Dao:", e);
                if (tx != null) {
                    tx.rollback();
                }
            }
        } else {
            try (Session s = getSessionFactory().openSession()) {
                tx = s.beginTransaction();
                outJobs = s.createQuery("from BackupSvcOutgoing").list();
                logger.info("Successfully retrieved: " + outJobs.size()
                        + " Outgoing Backup Service Job(s).");
                tx.commit();
            } catch (HibernateException e) {
                logger.error(
                        "Failed to retrieve Backup Service Outgoing Jobs: ", e);
                if (tx != null) {
                    tx.rollback();
                }
            } catch (Exception e) {
                logger.error("Error in Backup Service Outgoing Dao:", e);
                if (tx != null) {
                    tx.rollback();
                }
            }
        }
        return outJobs;
    }

    /**
     * Receive a list of BackUp Service Outgoing Jobs and a status String as a
     * parameter and update all the related Outgoing Jobs status field
     *
     * @param BackupSvcOutgoing
     *            List of Outgoing Jobs that needs their status to be updated
     * @param status
     *            The new status field to replace the old status
     */
    public int updateStatus(List<BackupSvcOutgoing> outJobs, DbStatus status) {
        long updatetime = System.currentTimeMillis();

        try {
            List<Long> ids = new ArrayList<>();
            for (BackupSvcOutgoing jobs : outJobs) {
                ids.add(jobs.getBksvcId());
            }
            Map<String, Object> paramMap = new HashMap<>();
            paramMap.put("status", status.toString());
            paramMap.put("updatetime", updatetime);
            paramMap.put("ids", ids);
            int updateCount = executeSQLUpdate(UPDATE_STATUS_QUERY, paramMap);
            logger.info("Successfully updated: " + updateCount
                    + " Outgoing Jobs with ID's " + ids + " to " + status
                    + " status.");
        } catch (Exception e) {
            logger.error(
                    "Failed to update Backup Service Outgoing Jobs status:", e);
            return FAIL;
        }
        return SUCCCESS;
    }

    /*
     * This method updates the blob field in BackupSvcJobInfo when user modifies
     * the physical file content where the JobInfo is linked to a certain backup
     * service job.
     *
     * NOTE: Input params: (1) localization file path; (2) content BL: use
     * localization file path to query JobInfo table to find out JobInfo record
     * and Job ID;
     *
     * NOTE: if one JobInfo record is found, update its JobInfo.blob,
     * Outgoing.updatetime, Outgoing.status=new; if more JobInfo records are
     * found, find the job ID with JOB.status=new (TDB), update the
     * JobInfo.blob;
     */
    public void updateReqeustBlob(String jobname, byte[] blob) {
        try {
            Object[] id = executeSQLQuery(
                    "select id from backup_svc_jobinfo where jobname = :jobname",
                    "jobname", jobname);
            if (id.length == 1) {

                long updatetime = System.currentTimeMillis();

                List<Long> ids = Arrays.stream(id).map(Object::toString)
                        .map(Long::valueOf).collect(Collectors.toList());
                Map<String, Object> paramOutJobs = new HashMap<>();
                Map<String, Object> paramJobInfo = new HashMap<>();
                paramOutJobs.put("updatetime", updatetime);
                paramOutJobs.put("ids", ids);
                paramJobInfo.put("blob", blob);
                paramJobInfo.put("ids", ids);
                int OutJobsCount = executeSQLUpdate(UPDATE_OUTGOING_QUERY,
                        paramOutJobs);
                int JobInfoCount = executeSQLUpdate(UPDATE_JOBINFO_QUERY,
                        paramJobInfo);
                logger.info("Successfully updated the Updatetime for: "
                        + OutJobsCount + "Backup service Outgoing Jobs"
                        + " and the request blob for:" + JobInfoCount
                        + " JobInfo");
            }
            // TODO: Update the blob if multiple JobInfo Records are found
            logger.info("Unable to update blob! " + id.length
                    + " JobInfo record found");
        } catch (HibernateException e) {
            logger.error(
                    "Failed to update the reqest blob for Backup Service Outgoing Job record: ",
                    e);
        } catch (Exception e) {
            logger.error("Error in Backup Service Outgoing Dao: ", e);
        }
    }

    /**
     * Retrieves a single outgoing job for the given id
     *
     * @param jobId
     * @return backup service outgoing job
     */
    public BackupSvcOutgoing getJobForId(long jobId) {
        BackupSvcOutgoing outgoingJob = null;
        Transaction tx = null;
        try (Session s = getSessionFactory().openSession()) {
            tx = s.beginTransaction();
            outgoingJob = s
                    .createQuery(
                            "from BackupSvcOutgoing b where b.bksvcId = :id",
                            BackupSvcOutgoing.class)
                    .setParameter("id", jobId).getSingleResult();
            tx.commit();
        } catch (HibernateException e) {
            logger.error("Failed to retrieve Backup Service Outgoing Job.", e);
            if (tx != null) {
                tx.rollback();
            }
        } catch (Exception e) {
            logger.error("Error in Backup Service Outgoing Dao: ", e);
            if (tx != null) {
                tx.rollback();
            }
        }
        return outgoingJob;
    }

    /**
     * Retrieves a list of outgoing backup service jobs for the given sender
     * site
     *
     * @param sendersite
     *            the site requesting the backup (eg, OAX...)
     * @return a list of outgoing backup service jobs for the given sender site.
     */
    public List<BackupSvcOutgoing> getOutgoingJobsForSite(String sendersite) {
        List<BackupSvcOutgoing> outgoingJobs = null;
        Transaction tx = null;
        try (Session s = getSessionFactory().openSession()) {
            tx = s.beginTransaction();
            outgoingJobs = s.createQuery(
                    "from BackupSvcOutgoing b where b.senderSite = :sendersite",
                    BackupSvcOutgoing.class)
                    .setParameter("sendersite", sendersite).list();
            logger.info("Successfully retrieved: " + outgoingJobs.size()
                    + " Outgoing Backup Service Job's from sender Site: "
                    + sendersite);
            tx.commit();
        } catch (HibernateException e) {
            logger.error(
                    "Failed to retrieve Backup Service Outgoing Jobs from site: "
                            + sendersite,
                    e);
            if (tx != null) {
                tx.rollback();
            }
        } catch (Exception e) {
            logger.error("Error in Backup Service Outgoing Dao: ", e);
            if (tx != null) {
                tx.rollback();
            }
        }
        return outgoingJobs;
    }

    /**
     * Remove the outgoing backup service jobs when they are older than the
     * specified number of days.
     *
     * @param purgeTimePeriod:
     *            default 14 days (in milliseconds)
     *
     */
    public void purgeExpiredJobs(long purgeTimePeriod) {
        long thePurgedTimePoint = System.currentTimeMillis() - purgeTimePeriod;
        String thePurgedCondition = " status in ('" + DbStatus.ACCEPTED + "','"
                + DbStatus.REJECTED + "') and updatetime < "
                + thePurgedTimePoint;

        try {
            // Retrieve all purged BSJs from outgoing table
            List<BackupSvcOutgoing> purgedBackupSvcJobs = getAllOutgoingJobs(
                    thePurgedCondition);
            // Get job and jobInfo IDs from the purged BSJs in outgoing table
            List<Long> jobIds = new ArrayList<>();
            List<Long> jobInfoIds = new ArrayList<>();
            for (BackupSvcOutgoing bsj : purgedBackupSvcJobs) {
                jobIds.add(bsj.getBksvcId());
                jobInfoIds.add(bsj.getJobInfoId());
            }
            // Remove all purged BSJs from outgoing table
            Map<String, Object> paramMap = new HashMap<>();
            paramMap.put("updatetime", (thePurgedTimePoint));
            executeSQLUpdate(PURGE_OLD_JOBS_QUERY, paramMap);

            logger.info(
                    "Successfully removed the purged jobs from backup_svc_outgoing table. "
                            + jobIds);

            // Removed all purged jobinfo records from jobinfo table
            Map<String, Object> jobInfoParamMap = new HashMap<>();
            jobInfoParamMap.put("ids", jobInfoIds);
            executeSQLUpdate(PURGE_OLD_JOBINFO_QUERY, jobInfoParamMap);

            logger.info(
                    "Successfully removed the purged jobs from backup_svc_jobinfo table. "
                            + jobInfoIds);

        } catch (Exception e) {
            logger.error(
                    "Unsuccessfully removed the purged jobs either from backup_svc_outgoing or backup_svc_jobinfo table");
            logger.error(e.toString());
        }
    }
}
