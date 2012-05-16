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
package com.raytheon.edex.plugin.grib.notify;

import java.util.ArrayList;
import java.util.List;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.grib.GribRecord;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.serialization.SerializationUtil;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.edex.core.EDEXUtil;
import com.raytheon.uf.edex.core.EdexException;

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

public class GribNotifyQueue {
	protected static final transient IUFStatusHandler handler = UFStatus
			.getHandler(GribNotifyQueue.class);

	private final Object syncObj = new Object();

	private List<GribRecord> queuedRecord = new ArrayList<GribRecord>(35);

	private String destinationUri = null;

	private GribNotifySendThread sendThread = new GribNotifySendThread(
			"gribNotifySend");

	public GribNotifyQueue() {
		sendThread.start();
	}

	public void addRecords(PluginDataObject[] gribs) {
		if (gribs != null && gribs.length > 0) {
			synchronized (syncObj) {
				if (gribs.length > 0) {
					for (PluginDataObject grib : gribs) {
						queuedRecord.add((GribRecord) grib);
					}
				} else {
					queuedRecord.add((GribRecord) gribs[0]);
				}

				syncObj.notifyAll();
			}
		}
	}

	public String getDestinationUri() {
		return destinationUri;
	}

	public void setDestinationUri(String destinationUri) {
		this.destinationUri = destinationUri;
	}

	private class GribNotifySendThread extends Thread {
		public GribNotifySendThread(String name) {
			super(name);
		}

		public void run() {
			while (true) {
				List<GribRecord> recordsToSend = null;

				// small sleep to allow messages to accumulate
				try {
					Thread.sleep(1000);
				} catch (InterruptedException e) {

				}

				try {
					synchronized (syncObj) {
						while (queuedRecord.size() == 0) {
							try {
								syncObj.wait();
							} catch (InterruptedException e) {
							}
						}

						recordsToSend = queuedRecord;
						queuedRecord = new ArrayList<GribRecord>(35);
					}

					GribNotifyContainer msg = GribNotifyTransform
							.transformToMessages(recordsToSend);
					try {
						byte[] data = SerializationUtil.transformToThrift(msg);
						EDEXUtil.getMessageProducer().sendAsyncUri(
								destinationUri, data);
					} catch (SerializationException e) {
						handler.error(
								"Failed to serialize grib notification message. Notification msg for "
										+ recordsToSend.size()
										+ " records won't be sent", e);
					} catch (EdexException e) {
						handler.error(
								"Failed to send grib notification message. Notification msg for "
										+ recordsToSend.size()
										+ " records won't be sent", e);
					}
				} catch (Throwable e) {
					handler.error(
							"Error occurred in GribNotificationSend process. Notification msg for "
									+ (recordsToSend == null ? 0
											: recordsToSend.size())
									+ " records won't be sent", e);
				}
			}
		}
	}
}
