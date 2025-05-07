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

import java.text.DecimalFormat;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.grid.GridRecord;
import com.raytheon.uf.viz.core.grid.rsc.AbstractGridResource;
import com.raytheon.uf.viz.core.rsc.LoadProperties;

/**
 * Resource data used for grid data that needs complex mapping to colors.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 13, 2010            bsteffen    Initial creation
 * Dec 20, 2023 2036519    mapeters    Records are now passed into resource constructor
 *                                     instead of being stored in resource data
 * Jul 15, 2024 2037624    mapeters    Make constructResource return type more specific
 *
 * </pre>
 *
 * @author bsteffen
 */
@XmlAccessorType(XmlAccessType.NONE)
public class DataMappedGridResourceData extends GridResourceData {

    /** Sampling format string, defaults to 2 decimal places */
    protected DecimalFormat sampleFormat = new DecimalFormat("0.00");

    @Override
    protected AbstractGridResource<GridResourceData> constructResource(
            LoadProperties loadProperties, PluginDataObject[] objects) {
        GridRecord[] records = new GridRecord[objects.length];
        for (int i = 0; i < objects.length; i++) {
            records[i] = (GridRecord) objects[i];
        }
        return new DataMappedGridResource(this, loadProperties, records);
    }

    public DecimalFormat getSampleFormat() {
        return sampleFormat;
    }

    @XmlAttribute(name = "sampleFormat")
    public String getSampleFormatAsString() {
        return sampleFormat.toPattern();
    }

    public void setSampleFormatAsString(String format) {
        sampleFormat = new DecimalFormat(format);
    }
}
