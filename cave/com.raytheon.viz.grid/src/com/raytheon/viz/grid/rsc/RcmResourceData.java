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
package com.raytheon.viz.grid.rsc;

import java.util.Arrays;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.grid.GridRecord;
import com.raytheon.uf.viz.core.alerts.DataCubeAlertMessageParser;
import com.raytheon.uf.viz.core.grid.rsc.AbstractGridResource;
import com.raytheon.uf.viz.core.grid.rsc.GridLoadProperties;
import com.raytheon.uf.viz.core.rsc.LoadProperties;

/**
 * resourceData for constructing RcmResources.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 16, 2009            mnash       Initial creation
 * Dec 20, 2023 2036519    mapeters    Records are now passed into resource constructor
 *                                     instead of being stored in resource data
 * Jul 15, 2024 2037624    mapeters    Make constructResource return type more specific
 *
 * </pre>
 *
 * @author mnash
 */
@XmlAccessorType(XmlAccessType.NONE)
public class RcmResourceData extends GridResourceData {

    public RcmResourceData() {
        setAlertParser(new DataCubeAlertMessageParser());
    }

    @Override
    protected AbstractGridResource<GridResourceData> constructResource(
            LoadProperties loadProperties, PluginDataObject[] objects) {
        GridRecord[] records;
        if (loadProperties instanceof GridLoadProperties) {
            records = Arrays.stream(objects).map(pdo -> (GridRecord) pdo)
                    .toArray(GridRecord[]::new);
        } else {
            records = new GridRecord[0];
        }
        return new RcmResource(this, loadProperties, records);
    }
}
