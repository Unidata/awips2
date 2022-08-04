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

import java.nio.FloatBuffer;
import java.text.ParsePosition;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import javax.measure.Unit;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.qpf.QPFRecord;
import com.raytheon.uf.common.datastorage.records.FloatDataRecord;
import com.raytheon.uf.common.datastorage.records.IDataRecord;
import com.raytheon.uf.common.inventory.exception.DataCubeException;
import com.raytheon.uf.common.style.ParamLevelMatchCriteria;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.grid.rsc.AbstractGridResource;
import com.raytheon.uf.viz.core.grid.rsc.data.GeneralGridData;
import com.raytheon.uf.viz.core.grid.rsc.data.ScalarGridData;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.capabilities.DisplayTypeCapability;
import com.raytheon.uf.viz.datacube.DataCubeContainer;

import tec.uom.se.format.SimpleUnitFormat;

/**
 * QPFVectorResource
 *
 * Implements contouring for qpf data, adapted from GridVectorResource
 *
 * <pre>
 *
 *    SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- ------------------------------------
 * Mar 15, 2009  2027     dhladky   Initial Creation.
 * Aug 29, 2019  67962    tjensen   Update for GeneralGridData refactor
 *
 * </pre>
 *
 * @author dhladky
 */

public class QPFResource extends AbstractGridResource<QPFResourceData> {

    public QPFResource(QPFResourceData data, LoadProperties props) {
        super(data, props);
        this.getCapability(DisplayTypeCapability.class)
                .setAlternativeDisplayTypes(null);
    }

    @Override
    protected void initInternal(IGraphicsTarget target) throws VizException {
        for (QPFRecord rec : resourceData.getRecords()) {
            this.addDataObject(rec);
        }
        super.initInternal(target);
    }

    @Override
    public ParamLevelMatchCriteria getMatchCriteria() {
        ParamLevelMatchCriteria match = new ParamLevelMatchCriteria();
        ArrayList<String> paramList = new ArrayList<>();
        paramList.add("qpf");
        match.setParameterName(paramList);
        for (DataTime time : getDataTimes()) {
            List<PluginDataObject> pdos = getPluginDataObjects(time);
            if (pdos != null && !pdos.isEmpty()) {
                paramList.clear();
                paramList.add(((QPFRecord) pdos.get(0)).getParameterName());
                break;
            }
        }
        return match;
    }

    @Override
    public List<GeneralGridData> getData(DataTime time,
            List<PluginDataObject> pdos) throws VizException {
        if (pdos.isEmpty()) {
            return Collections.emptyList();
        }
        QPFRecord rec = (QPFRecord) pdos.get(0);
        IDataRecord[] dataRecs;
        try {
            dataRecs = DataCubeContainer.getDataRecord(rec);
        } catch (DataCubeException e) {
            throw new VizException(e);
        }
        FloatDataRecord fdr = (FloatDataRecord) dataRecs[0];
        FloatBuffer data = FloatBuffer.wrap(fdr.getFloatData());
        Unit<?> unit = SimpleUnitFormat.getInstance()
                .parseObject(rec.getParameterUnit(), new ParsePosition(0));
        GeneralGridData ggd = ScalarGridData
                .createScalarData(rec.getGridGeometry(), data, unit);
        return Arrays.asList(ggd);
    }

    @Override
    public String getName() {
        PluginDataObject pdo = null;
        List<PluginDataObject> pdos = getCurrentPluginDataObjects();
        if (pdos != null && !pdos.isEmpty()) {
            pdo = pdos.get(0);
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

}
