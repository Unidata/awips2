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
package com.raytheon.viz.alerts.jobs;

import java.lang.reflect.Array;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.IdentityHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.common.time.SimulatedTime;
import com.raytheon.uf.viz.core.IDisplayPane;
import com.raytheon.uf.viz.core.IDisplayPaneContainer;
import com.raytheon.uf.viz.core.RecordFactory;
import com.raytheon.uf.viz.core.alerts.AbstractAlertMessageParser;
import com.raytheon.uf.viz.core.alerts.AlertMessage;
import com.raytheon.uf.viz.core.comm.Loader;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractRequestableResourceData;
import com.raytheon.uf.viz.core.rsc.AbstractResourceData;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.updater.DataUpdateTree;
import com.raytheon.uf.viz.core.time.TimeMatchingJob;
import com.raytheon.viz.alerts.IAlertObserver;

/**
 * AutoUpdater
 * 
 * Updates the resources as data comes in. RedoTimeMatching is performed to
 * guarantee the screen is fully up-to-date.
 * 
 * 
 * 
 * <pre>
 * 
 *    SOFTWARE HISTORY
 *   
 *    Date         Ticket#     Engineer    Description
 *    ------------ ----------  ----------- --------------------------
 *    Jul 31, 2007             chammack    Initial Creation.
 *    Feb 8, 2008  966         chammack    Converted to use IAlertObserver
 *    Mar 19, 2009             chammack    Modified to better fit new resource architecture, converted to batch-send model instead of one-at-a-time
 * </pre>
 * 
 * @author chammack
 * @version 1
 */
public class AutoUpdater implements IAlertObserver {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(AutoUpdater.class);

    private static final int MAX_ERRORS = 10;

    private static class AlertMessageToPDOParser extends
            AbstractAlertMessageParser {

        @Override
        public Object parseAlertMessage(AlertMessage message,
                AbstractRequestableResourceData reqResourceData)
                throws VizException {
            Object objectToSend = null;
            Map<String, Object> attribs = new HashMap<String, Object>(
                    message.decodedAlert);
            String dataURI = message.dataURI;
            if (reqResourceData.isUpdatingOnMetadataOnly()) {
                PluginDataObject record = RecordFactory.getInstance()
                        .loadRecordFromUri(dataURI);
                objectToSend = record;

            } else {
                attribs.put("dataURI", message.dataURI);
                objectToSend = Loader.loadData(attribs);
            }
            return objectToSend;
        }
    };

    private static AlertMessageToPDOParser defaultParser = new AlertMessageToPDOParser();

    public AutoUpdater() {
    }

	@Override
	public void alertArrived(Collection<AlertMessage> alertMessages) {
		Set<IDescriptor> displayList = new HashSet<IDescriptor>();
		Map<AbstractRequestableResourceData, List<Object>> pdoSendMap = new IdentityHashMap<AbstractRequestableResourceData, List<Object>>();
		int errors = 0;

		for (AlertMessage message : alertMessages) {
			Map<String, Object> attribs = message.decodedAlert;
			DataTime dataTime = (DataTime) attribs.get("dataTime");
			if (dataTime.getRefTime().before(
					SimulatedTime.getSystemTime().getTime())) {
				try {
					// System.out.println("extract took: " + (tZ1 - tZ0));
					java.util.List<AbstractVizResource<?, ?>> rscList = DataUpdateTree
							.getInstance().searchTree(attribs);
					// System.out.println("AutoUpdater found: " + rscList);

					if (rscList != null && rscList.size() > 0) {

						for (AbstractVizResource<?, ?> r1 : rscList) {
							IDescriptor md = r1.getDescriptor();
							AbstractResourceData resourceData = r1
									.getResourceData();
							if (!(resourceData instanceof AbstractRequestableResourceData)
									|| resourceData.isFrozen())
								continue;

							AbstractRequestableResourceData reqResourceData = (AbstractRequestableResourceData) resourceData;
							AbstractAlertMessageParser parserToUse = null;
							if ((parserToUse = reqResourceData.getAlertParser()) == null) {
								parserToUse = defaultParser;
							}
							Object objectToSend = parserToUse
									.parseAlertMessage(message, reqResourceData);

							if (objectToSend != null) {
								if (md.getTimeMatcher() != null) {
									md.getTimeMatcher().redoTimeMatching(r1);
								}
								displayList.add(md);

								List<Object> list = pdoSendMap
										.get(reqResourceData);
								if (list == null) {
									list = new ArrayList<Object>();
									pdoSendMap.put(reqResourceData, list);
								}
								list.add(objectToSend);

								if (list.size() > 100) {
									// update with objects
									Class<?> componentType = list.get(0)
											.getClass();
									reqResourceData.update(list
											.toArray((Object[]) Array
													.newInstance(componentType,
															list.size())));
									list.clear();
								}
							}

						}

					}

				} catch (final Throwable e) {
					if (errors < MAX_ERRORS) {
						statusHandler.handle(Priority.PROBLEM,
								"Error performing autoupdate", e);
					}
					errors++;
				}
			} 

			for (AbstractRequestableResourceData arrd : pdoSendMap.keySet()) {
				List<Object> pdos = pdoSendMap.get(arrd);
				if (pdos == null || pdos.size() < 1) {
					continue;
				}
				Class<?> componentType = pdos.get(0).getClass();
				arrd.update(pdos.toArray((Object[]) Array.newInstance(
						componentType, pdos.size())));
			}

			List<IDescriptor> refreshedDescriptors = new ArrayList<IDescriptor>();
			// Now do redo time matching, and refresh displays
			for (IDescriptor disp : displayList) {
				if (disp != null && disp.getRenderableDisplay() != null) {
					IDisplayPaneContainer container = disp
							.getRenderableDisplay().getContainer();
                if (container == null) {
                    continue;
                }
					for (IDisplayPane pane : container.getDisplayPanes()) {
						IDescriptor desc = pane.getDescriptor();

						if (refreshedDescriptors.contains(desc)) {
							continue;
						}
						TimeMatchingJob.scheduleTimeMatch(desc);
					}
				}
			}
		}
	} 
}
