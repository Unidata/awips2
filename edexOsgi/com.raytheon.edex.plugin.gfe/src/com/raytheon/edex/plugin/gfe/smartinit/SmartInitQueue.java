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
package com.raytheon.edex.plugin.gfe.smartinit;

import java.util.Calendar;
import java.util.Collection;
import java.util.Date;
import java.util.HashMap;
import java.util.Map;

import org.hibernate.HibernateException;
import org.hibernate.LockOptions;
import org.hibernate.Session;
import org.hibernate.Transaction;

import com.raytheon.edex.plugin.gfe.config.IFPServerConfig;
import com.raytheon.edex.plugin.gfe.config.IFPServerConfigManager;
import com.raytheon.edex.plugin.gfe.exception.GfeConfigurationException;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.DatabaseID;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.edex.database.dao.CoreDao;
import com.raytheon.uf.edex.database.dao.DaoConfig;

/**
 * Smart Init Aggregator/Queue for Camel
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 11, 2008            njensen     Initial creation
 * Oct 6, 2009    3172     njensen    Based on GribNotifyMessages
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class SmartInitQueue {
	private int smartInitTimeoutMillis = 60000;

	private Map<SmartInitRecordPK, SmartInitRecord> initSet = new HashMap<SmartInitRecordPK, SmartInitRecord>();

	protected static final transient IUFStatusHandler handler = UFStatus
			.getHandler(SmartInitQueue.class);

	public void addInits(Collection<SmartInitRecord> initsToAdd) {
		// event driven start route etc
		mergeInits(initsToAdd);
	}

	private void mergeInits(Collection<SmartInitRecord> inits) {
		for (SmartInitRecord record : inits) {
			try {
				DatabaseID toAdd = new DatabaseID(record.getDbName());
				IFPServerConfig config = IFPServerConfigManager
						.getServerConfig(toAdd.getSiteId());
				Calendar modelTime = Calendar.getInstance();
				modelTime.setTime(toAdd.getModelTimeAsDate());
				if (config.initSkip(toAdd.getModelName(),
						modelTime.get(Calendar.HOUR_OF_DAY))) {
					continue;
				}
			} catch (GfeConfigurationException e) {
				handler.handle(Priority.ERROR, e.getLocalizedMessage(), e);
				continue;
			}

			synchronized (this) {
				SmartInitRecordPK id = record.getId();
				SmartInitRecord oldRecord = initSet.get(id);
				if (oldRecord == null) {
					initSet.put(id, record);
				} else {
					Date newInsertTime = record.getInsertTime();
					if (newInsertTime.getTime() > oldRecord.getInsertTime()
							.getTime()) {
						oldRecord.setInsertTime(newInsertTime);
					}
					oldRecord.setManual(oldRecord.isManual()
							|| record.isManual());
					oldRecord.setPriority(Math.min(oldRecord.getPriority(),
							record.getPriority()));
				}
			}
		}
	}

	public void addManualInit(String init) {
		Collection<SmartInitRecord> manualInits = InitModules.splitManual(init);
		mergeInits(manualInits);
		// force update the tables
		fireSmartInit();
	}

	public void fireSmartInit() {
		Map<SmartInitRecordPK, SmartInitRecord> initsToStore = null;

		// copy off inits to store, allowing other threads to continue
		// accumulating
		synchronized (this) {
			if (initSet.size() > 0) {
				initsToStore = initSet;
				initSet = new HashMap<SmartInitRecordPK, SmartInitRecord>(
						(int) (initsToStore.size() * 1.25) + 1);
			}
		}

		if (initsToStore != null) {
			CoreDao cd = new CoreDao(DaoConfig.DEFAULT);
			Session s = null;
			Transaction tx = null;
			SmartInitRecord oldRecord = null;

			for (SmartInitRecord record : initsToStore.values()) {
				try {
					s = cd.getHibernateTemplate().getSessionFactory()
							.openSession();
					tx = s.beginTransaction();

					oldRecord = (SmartInitRecord) s.get(SmartInitRecord.class,
							record.getId(), LockOptions.UPGRADE);

					if (oldRecord == null) {
						s.save(record);
					} else {
						Date newInsertTime = record.getInsertTime();
						oldRecord.setPriority(Math.min(oldRecord.getPriority(),
								record.getPriority()));
						if (oldRecord.getInsertTime().getTime() < newInsertTime
								.getTime()) {
							oldRecord.setInsertTime(newInsertTime);
						}
						oldRecord.setManual(oldRecord.isManual()
								|| record.isManual());
						s.update(oldRecord);
					}
					tx.commit();
				} catch (Throwable t) {
					handler.handle(Priority.ERROR, "Error adding smartInit ["
							+ record.getId() + "] to database queue", t);

					if (tx != null) {
						try {
							tx.rollback();
						} catch (HibernateException e) {
							handler.handle(
									Priority.ERROR,
									"Error rolling back smart init lock transaction",
									e);
						}
					}
				} finally {
					if (s != null) {
						try {
							s.close();
						} catch (HibernateException e) {
							handler.handle(Priority.ERROR,
									"Error closing smart init lock session", e);
						}
					}
				}
			}
		}

	}

	public int getSmartInitTimeoutMillis() {
		return smartInitTimeoutMillis;
	}

	public void setSmartInitTimeoutMillis(int smartInitTimeoutMillis) {
		this.smartInitTimeoutMillis = smartInitTimeoutMillis;
	}

}
