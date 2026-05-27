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

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.hibernate.HibernateException;
import org.hibernate.Session;
import org.hibernate.StatelessSession;
import org.hibernate.Transaction;

import com.raytheon.uf.common.dataplugin.backupsvc.database.BackupSvcIncoming;
import com.raytheon.uf.common.dataplugin.backupsvc.database.BackupSvcJobInfo;
import com.raytheon.uf.common.dataplugin.backupsvc.database.DbStatus;
import com.raytheon.uf.edex.database.dao.CoreDao;
import com.raytheon.uf.edex.database.dao.DaoConfig;

/**
 * Data Access Object for Backup service incoming table which contains the
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
 * ------------ ----------  --------------- --------------------------
 * 04/20/2021   91324       Amanuel Challa  Initial creation
 * May 18, 2021 91044       Robert.Blum     Fixed HQL query.
 * Jun 08, 2021 92508       Robert.Blum     Fixed spelling of success.
 * Jun 22, 2021 92788       Gang Chen       Modify createInBSJ() parameters
 * Jul 01, 2021 93517       Amanuel Challa  Added new parameter to getAllIncomingJobs() and
 *                                          Modified to return filtered jobs
 * Jul 13, 2021 93058       Amanuel Challa  Changed updatetime in updateStatus()
 *                                          from Integer to Long
 * Jul 15, 2021 88368       Gang Chen       Remove the incoming BSJs over 2 weeks
 * Jul 07, 2021 84656       Amanuel Challa  Log messages clean up
 * Aug 13, 2021 92922       Robert.Blum     Cleanup of the job statuses and update DAO for new db fields.
 * Spt 13, 2021 93179       Amanuel Challa  Removed unused recipientHostName parameter in createInBSJ()
 * Sep 17, 2021 96153       Gang Chen       Fix the purging bugs for obsolete Accepted / Rejected BSJs
 * Oct 08, 2021 97253       Robert.Blum     Fix wording of purge log msgs.
 *
 * </pre>
 *
 * @author achalla
 */
public class BackupSvcIncomingDao extends CoreDao {
    private static final String DB_NAME = "metadata";

    private static final String UPDATE_STATUS_QUERY = "UPDATE awips.backup_svc_incoming set status = :status,"
            + "updatetime = :updatetime WHERE bksvc_id in (:ids)";

    private static final String PURGE_OLD_JOBS_QUERY = "delete from awips.backup_svc_incoming"
            + " where status in ('" + DbStatus.ACCEPTED + "','"
            + DbStatus.REJECTED + "') and updatetime < :updatetime";

    private static final String PURGE_OLD_JOBINFO_QUERY = "DELETE from awips.backup_svc_jobinfo WHERE id in (:ids)";

    public static final int SUCCESS = 1;

    public static final int FAIL = 0;

    public BackupSvcIncomingDao() {
        super(DaoConfig.forDatabase(DB_NAME));
    }

    /**
     * Create a Database Data Entry at Recipient Site
     *
     * @param blob
     *            The serialized request blob, limit up to 1GB
     * @param jobFileLocation
     *            The file name and localization path.
     * @param jobFilePath
     *            The file path to the localization path.
     * @param component
     *            Edex component that AWIPS file is linked to
     * @param senderSite
     *            The site that an AWIPS file is received from
     * @param recipientSites
     *            The site where an AWIPS file will be placed to
     * @param senderHostName
     *            Sender host server name or DNS name
     * @param senderHost
     *            Sender port number for HTTP service.
     * @param recipientHostName
     *            Recipient host server name or DNS name
     * @param systemVersion
     *            The AWIPS system version when a backup service job was sent
     * @param senderVersion
     *            The AWIPS system version of the sender site at the time the
     *            job was sent
     * @param outBksvcId
     *            The outgoing backup service job ID, which is received from the
     *            sender site
     *
     */
    /*
     * Note: The ID number for this BSJ info at the sender site is required to
     * be stored into the out_bksvc_id field in backup_svc_incoming table.
     *
     * @param long outBksvcId;
     */
    public int createInBSJ(byte[] blob, String jobFileLocation,
            String jobFilePath, String component, String senderSite,
            String recipientSite, String senderHostName, String senderPort,
            long outBksvcId, String systemVersion, String senderVersion) {
        Transaction tx = null;

        Long now = System.currentTimeMillis();
        try (Session session = getSessionFactory().openSession()) {
            tx = session.beginTransaction();
            BackupSvcJobInfo incomingBSJInfo = new BackupSvcJobInfo();
            incomingBSJInfo.setRequestBlob(blob);
            incomingBSJInfo.setJobFileLocation(jobFileLocation);
            incomingBSJInfo.setJobFilePath(jobFilePath);
            session.persist(incomingBSJInfo);

            BackupSvcIncoming incomingJob = new BackupSvcIncoming();
            incomingJob.setBackupSvcJobInfo(incomingBSJInfo);
            incomingJob.setOutBksvcId(outBksvcId);
            incomingJob.setComponent(component);
            incomingJob.setSenderSite(senderSite);
            incomingJob.setRecipientSite(recipientSite);
            incomingJob.setUpdateTime(now);
            incomingJob.setSenderHostName(senderHostName);
            incomingJob.setSenderPort(senderPort);
            incomingJob.setSystemVersion(systemVersion);
            incomingJob.setSenderVersion(senderVersion);
            incomingJob.setStatus(DbStatus.NEW.toString());
            session.persist(incomingJob);
            logger.info(
                    "Successfully saved Incoming Job content at Recipient Site.");
            tx.commit();
        } catch (HibernateException e) {
            if (tx != null) {
                tx.rollback();
            }
            logger.error("Error saving database data entry at Recipient Site: ",
                    e);
            return FAIL;
        } catch (Exception e) {
            if (tx != null) {
                tx.rollback();
            }
            logger.error("Error in Backup Service Incoming Dao: ", e);
            return FAIL;
        }
        return SUCCESS;
    }

    /**
     * Return all Backup Service Job information found as a list.
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
            logger.error("Error in Backup Service Incoming Dao: ", e);
            if (tx != null) {
                tx.rollback();
            }
        }
        return allJobInfos;
    }

    /**
     * Receive a BackUp Service Incoming Job as a parameter and return the
     * related Job info
     *
     * @param BackupSvcIncoming
     */
    public BackupSvcJobInfo getJobInfo(BackupSvcIncoming incomingJob) {
        BackupSvcJobInfo jobInfo = null;
        try (StatelessSession ss = getSessionFactory().openStatelessSession()) {
            jobInfo = (BackupSvcJobInfo) ss.get(BackupSvcJobInfo.class,
                    incomingJob.getJobInfoId());
            logger.info("Successfully retrieved Backup Service Job Info.");
            return jobInfo;
        } catch (HibernateException e) {
            logger.error("Failed retrieving Backup Service Job Info: ", e);
        } catch (Exception e) {
            logger.error("Error in Backup Service Incoming Dao: ", e);
        }
        return jobInfo;
    }

    /**
     * Return all Backup Service Incoming Jobs found as a List .
     *
     * @return incomingJobs
     *
     *         A list of out Outgoing Backup Service Jobs
     */
    public List<BackupSvcIncoming> getAllIncomingJobs() {
        return getAllIncomingJobs(null);
    }

    /**
     * Return all Backup Service Incoming Jobs found as a List .
     *
     * @param queryStr
     *            SQL query generated in Backup Service Filter Dialog to filter
     *            jobs
     * @return incomingJobs
     *
     *         A list of out Outgoing Backup Service Jobs
     */
    @SuppressWarnings("unchecked")
    public List<BackupSvcIncoming> getAllIncomingJobs(String queryStr) {
        /*
         * sql query example bksvc_id > 3 and component = 'Test Component' and
         * recipientsite = 'dev-01' and systemversion >= '50.1.1' and sendersite
         * = 'OAX'
         */
        List<BackupSvcIncoming> incomingJobs = null;
        Transaction tx = null;

        if (queryStr != null && (!queryStr.isEmpty())) {
            // build the query string from parameter
            StringBuilder sb = new StringBuilder();
            sb.append("from BackupSvcIncoming where ");
            sb.append(queryStr);
            String newQuery = sb.toString();

            try (Session s = getSessionFactory().openSession()) {
                tx = s.beginTransaction();
                incomingJobs = s.createQuery(newQuery).list();
                logger.info("Successfully retrieved: " + incomingJobs.size()
                        + " Incoming Backup Service Job(s) with filtering Query: "
                        + queryStr);

                tx.commit();
            } catch (HibernateException e) {
                logger.error(
                        "Failed to retrieve Backup Service Incoming Job(s): ",
                        e);
                if (tx != null) {
                    tx.rollback();
                }
            } catch (Exception e) {
                logger.error("Error in Backup Service Incoming Dao: ", e);
                if (tx != null) {
                    tx.rollback();
                }
            }
        } else {
            try (Session s = getSessionFactory().openSession()) {
                tx = s.beginTransaction();
                incomingJobs = s.createQuery("from BackupSvcIncoming").list();
                logger.info("Successfully retrieved: " + incomingJobs.size()
                        + " Incoming Backup Service Job(s).");
                tx.commit();
            } catch (HibernateException e) {
                logger.error(
                        "Failed to retrieve Backup Service Incoming Jobs: ", e);
                if (tx != null) {
                    tx.rollback();
                }
            } catch (Exception e) {
                logger.error("Error in Backup Service Incoming Dao: :", e);
                if (tx != null) {
                    tx.rollback();
                }
            }
        }
        return incomingJobs;
    }

    /**
     * Receive a list of Backup Service Incoming Jobs and a status String as a
     * parameter and update all the related Incoming Jobs status field
     *
     * @param BackupSvcIncoming
     *            List of Incoming Jobs that needs their status to be updated
     * @param status
     *            The new status field to replace the old status
     */
    public int updateStatus(List<BackupSvcIncoming> inJobs, DbStatus status) {

        long updatetime = System.currentTimeMillis();

        try (Session s = getSessionFactory().openSession()) {
            s.beginTransaction();
            List<Long> ids = new ArrayList<>();
            for (BackupSvcIncoming jobs : inJobs) {
                ids.add(jobs.getBksvcId());
            }
            Map<String, Object> paramMap = new HashMap<>();
            paramMap.put("status", status.toString());
            paramMap.put("updatetime", updatetime);
            paramMap.put("ids", ids);
            int updateCount = executeSQLUpdate(UPDATE_STATUS_QUERY, paramMap);
            logger.info("Successfully updated" + updateCount
                    + " Incoming Jobs with ID's " + ids + " to " + status
                    + " status.");
        } catch (Exception e) {
            logger.error(
                    "Failed to update Backup Service Incoming Jobs status:", e);
            return FAIL;
        }
        return SUCCESS;
    }

    /**
     * Retrieves a list of incoming backup services jobs for the given sender
     * site
     *
     * @param sendersite
     *            the site requesting the backup (eg, OAX)
     * @return
     */
    public List<BackupSvcIncoming> getReceivingJobsForSite(String sendersite) {
        List<BackupSvcIncoming> incomingJobs = null;
        Transaction tx = null;
        try (Session s = getSessionFactory().openSession()) {
            tx = s.beginTransaction();
            incomingJobs = s.createQuery(
                    "from BackupSvcIncoming d where d.senderSite = :sendersite",
                    BackupSvcIncoming.class)
                    .setParameter("sendersite", sendersite).list();
            logger.info("Successfully retrieved: " + incomingJobs.size()
                    + " Incoming Backup Service Job's from sender Site: "
                    + sendersite);
            tx.commit();
        } catch (HibernateException e) {
            logger.error(
                    "Failed to retrieve Backup Service Incoming Jobs from site: "
                            + sendersite,
                    e);
            if (tx != null) {
                tx.rollback();
            }
        } catch (Exception e) {
            logger.error("Error in Backup Service Incoming Dao: ", e);
            if (tx != null) {
                tx.rollback();
            }
        }
        return incomingJobs;
    }

    /**
     * Retrieves a single backup service incoming job for the given id.
     *
     * @param jobId
     * @return a backup service incoming job
     */
    public BackupSvcIncoming getJobForId(long jobId) {
        BackupSvcIncoming incomingJob = null;
        Transaction tx = null;
        try (Session s = getSessionFactory().openSession()) {
            tx = s.beginTransaction();
            incomingJob = s
                    .createQuery(
                            "from BackupSvcIncoming b where b.bksvcId = :id",
                            BackupSvcIncoming.class)
                    .setParameter("id", jobId).getSingleResult();
            tx.commit();
        } catch (HibernateException e) {
            logger.error("Failed to retrieve Backup Service Incoming Job.", e);
            if (tx != null) {
                tx.rollback();
            }
        } catch (Exception e) {
            logger.error("Error in Backup Service Incoming Dao: ", e);
            if (tx != null) {
                tx.rollback();
            }
        }
        return incomingJob;
    }

    /**
     * Remove the incoming backup service jobs when they are older than the
     * specified number of days.
     *
     * @param purgeTimePeriod
     */
    public void purgeExpiredJobs(long purgeTimePeriod) {
        long thePurgedTimePoint = System.currentTimeMillis() - purgeTimePeriod;
        String thePurgedCondition = " status in ('" + DbStatus.ACCEPTED + "','"
                + DbStatus.REJECTED + "') and updatetime < "
                + thePurgedTimePoint;

        try {
            // Retrieve all purged BSJs from incoming table
            List<BackupSvcIncoming> purgedBackupSvcJobs = getAllIncomingJobs(
                    thePurgedCondition);
            // Get job and jobInfo IDs from the purged BSJs in incoming table
            List<Long> jobIds = new ArrayList<>();
            List<Long> jobInfoIds = new ArrayList<>();
            for (BackupSvcIncoming bsj : purgedBackupSvcJobs) {
                jobIds.add(bsj.getBksvcId());
                jobInfoIds.add(bsj.getJobInfoId());
            }
            // Remove all purged BSJs from incoming table
            Map<String, Object> paramMap = new HashMap<>();
            paramMap.put("updatetime", (thePurgedTimePoint));
            executeSQLUpdate(PURGE_OLD_JOBS_QUERY, paramMap);

            logger.info(
                    "Successfully removed the purged jobs from backup_svc_incoming table. "
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
                    "Unsuccessfully removed the purged jobs either from backup_svc_incoming or backup_svc_jobinfo table");
            logger.error(e.toString());
        }

    }
}
