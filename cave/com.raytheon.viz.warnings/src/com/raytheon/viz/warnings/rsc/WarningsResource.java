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

package com.raytheon.viz.warnings.rsc;

import java.util.Calendar;
import java.util.Map;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.warning.AbstractWarningRecord;
import com.raytheon.uf.common.dataplugin.warning.WarningRecord.WarningAction;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint.ConstraintType;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.catalog.LayerProperty;
import com.raytheon.uf.viz.core.datastructure.DataCubeContainer;
import com.raytheon.uf.viz.core.drawables.IWireframeShape;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.ResourceType;
import com.raytheon.viz.core.rsc.jts.JTSCompiler;
import com.raytheon.viz.texteditor.util.SiteAbbreviationUtil;
import com.vividsolutions.jts.geom.Geometry;

/**
 * Resource for displaying warnings
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 1, 2010            jsanchez     Initial creation
 * Aug 22, 2011  10631   njensen  Major refactor
 * May 3, 2012	DR 14741  porricel	   Stop setting end time of orig.
 *                                     warning to start time of update.
 * Jun 04, 2012 DR14992  mgamazaychikov Fix the problem with plotting expiration time for 
 *										NEW warning when CAN warning is issued 
 * 
 * </pre>
 * 
 * @author jsanchez
 * @version 1.0
 */

public class WarningsResource extends AbstractWarningResource {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(WarningsResource.class);

    /**
     * Constructor
     */
    public WarningsResource(WWAResourceData data, LoadProperties props) {
        super(data, props);
    }

    @Override
    protected synchronized void updateDisplay(IGraphicsTarget target) {
        if (!this.recordsToLoad.isEmpty()) {
            for (AbstractWarningRecord warnrec : recordsToLoad) {
                WarningAction act = WarningAction.valueOf(warnrec.getAct());
                if (act == WarningAction.CON || act == WarningAction.CAN
                        || act == WarningAction.EXP || act == WarningAction.EXT) {
                    AbstractWarningRecord createShape = null;
                    for (String key : entryMap.keySet()) {
                        WarningEntry entry = entryMap.get(key);
                        AbstractWarningRecord rec = entry.record;
                        if (rec.getPhensig().equals(warnrec.getPhensig())
                                && rec.getOfficeid().equals(
                                        warnrec.getOfficeid())
                                && rec.getEtn().equals(warnrec.getEtn())) {

                            if (!entry.altered) {
                                // if it's a con, can, exp, or ext mark the
                            	// original one as altered
                                entry.altered = true;
                                //make note of alteration time without 
                                //changing end time
                                entry.timeAltered = warnrec.getStartTime().getTime();
                                
                                //if cancellation, set end time to start time 
                                //of this action
                                
                                // DR14992: fix the problem with plotting expiration time for 
                                // 			NEW warning when CAN warning is issued
                                if(act == WarningAction.CAN &&
                                   WarningAction.valueOf(entry.record.getAct()) == WarningAction.CAN) {
                                    entry.record.setEndTime((Calendar) warnrec
                                            .getStartTime().clone());
                                }
                                
                                if (!rec.getCountyheader().equals(
                                        warnrec.getCountyheader())
                                        && act == WarningAction.CAN) {
                                    entry.partialCancel = true;
                                }

                                // if it's a con, need to have a new entry for a
                                // new
                                // polygon
                                if (act == WarningAction.CON
                                        || act == WarningAction.EXT) {
                                    createShape = warnrec;
                                }
                            } else if ((entry.altered && entry.partialCancel)) {
                                // if it's a con, need to have a new entry for a
                                // new
                                // polygon
                                // TODO - do we need to create shape when action
                                // is EXT here, is that even possible?
                                if (act == WarningAction.CON) {
                                    createShape = warnrec;
                                }
                            }
                        }
                    }
                    // create the new polygon for the CON outside of the above
                    // for loop
                    if (createShape != null) {
                        initShape(target, warnrec);
                    }
                } else {
                    warnrec.setPil(SiteAbbreviationUtil.getSiteNode(warnrec
                            .getXxxid())
                            + warnrec.getPil()
                            + warnrec.getXxxid());
                    initShape(target, warnrec);
                }
            }

            recordsToLoad.clear();
        }

    }

    @Override
    protected void initShape(IGraphicsTarget target,
            AbstractWarningRecord record) {
        Geometry geo;

        if (record.getGeometry() != null) {
            try {
                WarningEntry entry = entryMap.get(record.getDataURI());
                if (entry == null) {
                    entry = new WarningEntry();
                    entry.record = record;
                    entryMap.put(record.getDataURI(), entry);
                }
                IWireframeShape wfs = entry.wireframeShape;

                if (wfs != null) {
                    wfs.dispose();
                }

                wfs = target.createWireframeShape(false, descriptor);
                geo = (Geometry) record.getGeometry().clone();

                JTSCompiler jtsCompiler = new JTSCompiler(null, wfs, descriptor);
                jtsCompiler.handle(geo);
                wfs.compile();
                entry.wireframeShape = wfs;
            } catch (Exception e) {
                statusHandler.handle(Priority.ERROR,
                        "Error creating wireframe", e);
            }
        }
    }

    @Override
    protected void initInternal(IGraphicsTarget target) throws VizException {
        DataTime earliest = this.descriptor.getFramesInfo().getFrameTimes()[0];
        requestData(earliest);
        synchronized (heartBeatChangeLock) {
            if (heartBeatTask == null) {
                heartBeatTask = new RepaintHeartbeat();
            }
            heartBeatTask.addResource(this);
        }
    }

    @SuppressWarnings("unchecked")
    @Override
    protected void requestData(DataTime earliest) throws VizException {
        System.out.println("requesting data");
        Map<String, RequestConstraint> map = (Map<String, RequestConstraint>) resourceData
                .getMetadataMap().clone();
        if (earliestRequested != null) {
            // don't request data we've already requested
            String[] times = new String[] { earliest.toString(),
                    earliestRequested.toString() };
            RequestConstraint constraint = new RequestConstraint();
            constraint.setConstraintType(ConstraintType.BETWEEN);
            constraint.setBetweenValueList(times);
            map.put("endTime", constraint);
        } else {
            RequestConstraint endConstraint = new RequestConstraint(
                    earliest.toString(), ConstraintType.GREATER_THAN_EQUALS);
            map.put("endTime", endConstraint);
        }

        earliestRequested = earliest;

        LayerProperty property = new LayerProperty();
        property.setDesiredProduct(ResourceType.PLAN_VIEW);
        property.setEntryQueryParameters(map, false);
        property.setNumberOfImages(9999);

        Object[] resp = null;
        resp = DataCubeContainer.getData(property, 60000).toArray(
                new Object[] {});
        PluginDataObject[] arr = new PluginDataObject[resp.length];
        int i = 0;
        for (Object o : resp) {
            arr[i] = (PluginDataObject) o;
            i++;
        }
        addRecord(arr);
    }

}
