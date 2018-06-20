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
package com.raytheon.uf.viz.pointset.image;

import javax.measure.unit.Unit;

import com.raytheon.uf.common.dataplugin.pointset.PointSetRecord;
import com.raytheon.uf.common.parameter.Parameter;
import com.raytheon.uf.common.style.image.ColorMapParameterFactory;
import com.raytheon.uf.common.style.image.ImagePreferences;

/**
 * Custom version of {@link ImagePreferences} that uses the units from the
 * record as display units. This is only useful if there is no other style rule.
 * This can be used to pass the unit information from the {@link PointSetRecord}
 * to the {@link ColorMapParameterFactory}.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- ---------------------
 * Jan 25, 2016  5208     bsteffen  Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public class PointSetImagePreferences extends ImagePreferences {

    private final Parameter parameter;

    public PointSetImagePreferences(Parameter parameter) {
        super();
        this.parameter = parameter;
    }

    @Override
    public Unit<?> getDisplayUnits() {
        return parameter.getUnit();
    }

    @Override
    public String getDisplayUnitLabel() {
        return parameter.getUnitString();
    }

}
