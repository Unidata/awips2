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
package com.raytheon.viz.warngen.comm;

import java.net.UnknownHostException;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.TimeZone;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.jms.BytesMessage;
import javax.jms.Connection;
import javax.jms.JMSException;
import javax.jms.MessageProducer;
import javax.jms.Session;

import com.raytheon.uf.common.dataplugin.text.request.InsertStdTextProductRequest;
import com.raytheon.uf.common.serialization.SerializationUtil;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.comm.JMSConnection;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.localization.LocalizationManager;
import com.raytheon.uf.viz.core.requests.ThriftClient;
import com.raytheon.viz.core.mode.CAVEMode;
import com.raytheon.viz.texteditor.TextWorkstationConstants;
import com.raytheon.viz.texteditor.msgs.IWarngenObserver;
import com.raytheon.viz.texteditor.util.SiteAbbreviationUtil;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 11, 2009            mschenke    Initial creation
 * 01Jun2010    2187       cjeanbap    Added operational mode functionality
 * 02Aug2010    2187       cjeanbap    Update variable/method signature to be consistent.
 * 04Oct2010    7193       cjeanbap    Add time-to-live value to MessageProducer.
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class WarningSender implements IWarngenObserver {
	private static final transient IUFStatusHandler statusHandler = UFStatus
			.getHandler(WarningSender.class);

	private String hostName = null;

	private boolean notifyError;

	private static final long MILLISECONDS_PER_SECOND = 1000;

	private static final long SECONDS_PER_MINUTE = 60;

	private static final long TTL_MINUTES = 5;

	private static Pattern PATTERN = Pattern.compile("(\\d{1,1})");

	private static final SimpleDateFormat sdf;

	static {
		sdf = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss.SSS");
		sdf.setTimeZone(TimeZone.getTimeZone("GMT"));
	}

	/*
	 * (non-Javadoc) Incoming message was not a binary
	 * 
	 * @see
	 * com.raytheon.viz.texteditor.msgs.IWarngenObserver#setTextWarngenDisplay
	 * (java.lang.String)
	 */
	@Override
	public void setTextWarngenDisplay(String warning, boolean ne) {
		this.notifyError = ne;

		String number = "0";
		String host = TextWorkstationConstants.getId();
		long t0 = System.currentTimeMillis();
		String siteNode = SiteAbbreviationUtil.getSiteNode(LocalizationManager
				.getInstance().getCurrentSite());
		System.out.println("Get site node time: "
				+ (System.currentTimeMillis() - t0));
		if (host == null) {
			statusHandler.handle(Priority.ERROR,
					"Text Workstation host not set in preferences.");
		} else {
			Matcher m = PATTERN.matcher(host);
			if (m.find()) {
				number = m.group();
			}
		}

		String id = siteNode + "WRKWG" + number;
		boolean sentToTextDatabase = false;

		try {
			boolean messageNotSent = true;
			int connectCount = 0;
			t0 = System.currentTimeMillis();
			byte[] data = SerializationUtil.transformToThrift(id + ":"
					+ warning);
			while (messageNotSent && connectCount < 4) {
				Session s = null;
				MessageProducer mp = null;
				Connection conn = null;
				try {
					conn = JMSConnection.getInstance().getFactory()
							.createConnection();
					s = conn.createSession(false, Session.CLIENT_ACKNOWLEDGE);
					mp = s.createProducer(s
							.createQueue(TextWorkstationConstants
									.getDestinationTextWorkstationQueueName()));
					mp.setTimeToLive(TTL_MINUTES * SECONDS_PER_MINUTE
							* MILLISECONDS_PER_SECOND);
					BytesMessage m = s.createBytesMessage();
					m.writeBytes(data);
					mp.send(m);
					long t1 = System.currentTimeMillis();
					System.out.println(WarningSender.getCurTimeString() + ": "
							+ id + " sent to text workstation in " + (t1 - t0)
							+ "ms in " + (connectCount + 1)
							+ (connectCount > 0 ? " tries" : " try"));
					messageNotSent = false;
				} catch (JMSException e) {
					if (notifyError) {
						statusHandler
								.handle(Priority.PROBLEM,
										"Error trying to send product ["
												+ id
												+ "] to Text Workstation. Attempting to reconnect. ",
										e);
						notifyError = false;
					}
				} finally {
					if (mp != null) {
						try {
							mp.close();
							mp = null;
						} catch (Exception e) {
							mp = null;
						}
					}
					if (s != null) {
						try {
							s.close();
							s = null;
						} catch (Exception e) {
							s = null;
						}
					}
					if (conn != null) {
						try {
							conn.close();
							conn = null;
						} catch (Exception e) {
							conn = null;
						}
					}
				}
				if (messageNotSent) {
					if (!sentToTextDatabase) {
						try {
							sendToTextDatabase(id, warning);
							sentToTextDatabase = true;
						} catch (Exception e) {
							statusHandler.handle(Priority.PROBLEM,
									"Error trying to save product [" + id
											+ "] to Text Database: ", e);
						}
					}

					connectCount++;
					switch (connectCount) {
					case 1:
						Thread.sleep(1000);
						break;
					case 2:
						Thread.sleep(5 * 1000);
						break;
					case 3:
						Thread.sleep(30 * 1000);
						break;
					case 4:
						statusHandler.handle(Priority.PROBLEM,
								"Could not reconnect (" + id
										+ ") after 3 tries: ");
						break;
					}
				}
			}

			if (!sentToTextDatabase) {
				try {
					sendToTextDatabase(id, warning);
					sentToTextDatabase = true;
				} catch (Exception e) {
					statusHandler.handle(Priority.PROBLEM,
							"Error trying to save product [" + id
									+ "] to Text Database: ", e);
				}
			}
		} catch (UnknownHostException uhe) {
			if (notifyError) {
				statusHandler.handle(Priority.PROBLEM,
						"unable to map hostname, " + hostName
								+ ", to an ip address", uhe);
				notifyError = false;
			}

		} catch (Exception e) {
			statusHandler.handle(Priority.PROBLEM,
					"Error trying to send product [" + id
							+ "] to Text Workstation: ", e);
		}

	}

	/**
	 * Saves a product to the text database.
	 * 
	 * @param id
	 * @param warning
	 * @throws VizException
	 */
	public static void sendToTextDatabase(String id, String warning)
			throws VizException {
		CAVEMode mode = CAVEMode.getMode();
		boolean operationalMode = (CAVEMode.OPERATIONAL.equals(mode)
				|| CAVEMode.TEST.equals(mode) ? true : false);

		// Generate StdTextProduct and insert into db
		long t0 = System.currentTimeMillis();
		ThriftClient.sendRequest(new InsertStdTextProductRequest(id, warning,
				operationalMode));

		System.out.println(WarningSender.getCurTimeString() + ": " + id
				+ " saved to textdb in " + (System.currentTimeMillis() - t0)
				+ "ms");
	}

	public static String getCurTimeString() {
		String rval = null;
		synchronized (sdf) {
			rval = sdf.format(new Date());
		}
		return rval;
	}
}
