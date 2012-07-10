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
package com.raytheon.uf.viz.qpf;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;

import javax.measure.unit.Unit;

import org.geotools.coverage.grid.GeneralGridGeometry;
import org.geotools.coverage.grid.GridGeometry2D;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.qpf.QPFRecord;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.map.MapDescriptor;
import com.raytheon.uf.viz.core.rsc.AbstractRequestableResourceData;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.IResourceDataChanged;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.style.ParamLevelMatchCriteria;
import com.raytheon.uf.viz.core.style.StyleManager;
import com.raytheon.uf.viz.core.style.StyleRule;
import com.raytheon.viz.core.style.contour.ContourPreferences;
import com.raytheon.viz.grid.rsc.AbstractMapVectorResource;
import com.raytheon.viz.grid.rsc.GridLoadProperties;

/**
 * QPFVectorResource
 * 
 * Implements contouring for qpf data, adapted from GridVectorResource
 * 
 * <pre>
 * 
 *    SOFTWARE HISTORY
 *   
 *    Date         Ticket#     Engineer    Description
 *    ------------ ----------  ----------- --------------------------
 *    15Mar2009    2027        dhladky    Initial Creation.
 * 
 * </pre>
 * 
 * @author dhladky
 * @version 1
 */

public class QPFResource extends AbstractMapVectorResource implements
        IResourceDataChanged {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(QPFResource.class);

    protected String icao;

    protected String fieldName;

    protected String fieldUnitString;

    public QPFResource(QPFResourceData data, LoadProperties props) {
        super(data, props);

        data.addChangeListener(this);
        for (QPFRecord rec : data.getRecords()) {
            try {
                this.addRecord(rec);
            } catch (VizException ve) {
                ve.printStackTrace();
            }
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.core.contours.AbstractMapContourResource#getQPFGeometry
     * (com.raytheon.edex.db.objects.PluginDataObject)
     */
    @Override
    protected GeneralGridGeometry getGridGeometry(PluginDataObject obj) {
        QPFRecord qpfRecord = (QPFRecord) obj;

        GridGeometry2D qpfGeometry2D = qpfRecord.getGridGeometry();

        return qpfGeometry2D;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.core.contours.AbstractMapContourResource#validateRecord
     * (com.raytheon.edex.db.objects.PluginDataObject)
     */
    @Override
    protected boolean validateRecord(PluginDataObject obj) {
        return (obj instanceof QPFRecord);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.core.rsc.IVizResource#getName()
     */
    @Override
    public String getName() {
        PluginDataObject pdo = null;
        for (PluginDataObject obj : getDataObjectMap().values()) {
            pdo = obj;
            break;
        }
        if (pdo == null) {
            return "No Data Available";
        }

        QPFRecord record = (QPFRecord) pdo;

        StringBuilder prefix = new StringBuilder();
        prefix.append(record.getIcao());
        prefix.append(" ");
        prefix.append(record.getParameterName());
        prefix.append(" ");
        prefix.append(record.getParameterUnit());

        return prefix.toString();
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.core.contours.rsc.AbstractMapContourResource#getStyleRule
     * (com.raytheon.edex.db.objects.PluginDataObject)
     */
    @Override
    public StyleRule getStyleRule(PluginDataObject obj) throws VizException {
        QPFRecord record = (QPFRecord) obj;

        ParamLevelMatchCriteria match = getMatchCriteria(record);
        ArrayList<String> paramList = new ArrayList<String>();
        paramList.add(record.getParameterName());
        match.setParameterName(paramList);
        StyleRule sr = StyleManager.getInstance().getStyleRule(
                StyleManager.StyleType.CONTOUR, match);
        if (sr != null
                && ((ContourPreferences) sr.getPreferences()).getDisplayUnits() != null) {
            this.fieldUnitString = ((ContourPreferences) sr.getPreferences())
                    .getDisplayUnits().toString();
        }
        return sr;
    }

    /**
     * Get and load the style rule
     * 
     * @return
     */
    public ParamLevelMatchCriteria getMatchCriteria(QPFRecord record) {
        ParamLevelMatchCriteria match = new ParamLevelMatchCriteria();
        ArrayList<String> paramList = new ArrayList<String>();
        paramList.add(record.getPluginName());
        match.setParameterName(paramList);
        return match;
    }

    @SuppressWarnings("unchecked")
    @Override
    public AbstractVizResource<AbstractRequestableResourceData, MapDescriptor> getImageryResource()
            throws VizException {
        Collection<PluginDataObject> pdo = this.getDataObjectMap().values();
        QPFRecord[] recs = new QPFRecord[pdo.size()];
        Iterator<PluginDataObject> pdoIter = pdo.iterator();
        int i = 0;
        while (pdoIter.hasNext()) {
            recs[i] = (QPFRecord) pdoIter.next();
            i++;
        }

        return (AbstractVizResource<AbstractRequestableResourceData, MapDescriptor>) this.resourceData
                .construct(new GridLoadProperties(
                        com.raytheon.uf.viz.core.rsc.DisplayType.IMAGE),
                        descriptor);
    }

    @Override
    public void resourceChanged(ChangeType type, Object object) {
        if (type.equals(ChangeType.DATA_UPDATE)) {
            PluginDataObject[] pdos = (PluginDataObject[]) object;
            for (PluginDataObject pdo : pdos) {
                try {
                    addRecord(pdo);
                } catch (VizException e) {
                    statusHandler.handle(Priority.PROBLEM,
                            "Error updating QPF resource", e);
                }
            }
        }
        issueRefresh();
    }

    @Override
    protected Unit<?> getDataUnits(PluginDataObject obj) {
        // TODO Auto-generated method stub
        return null;
    }

}
