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
package com.raytheon.viz.pointdata;

import java.util.Set;

import javax.measure.Unit;

import com.raytheon.uf.common.pointdata.PointDataDescription.Type;

/**
 * Interface for the Plot Data Object.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 10/10/2019   71272      mapeters    Initial Creation. Created from the existing PlotData
 *                                       for plot customization work.
 *
 * </pre>
 *
 * @author mapeters
 */

public interface IPlotData {

    public int getInt(String parameter);

    public float getFloat(String parameter);

    public long getLong(String parameter);

    public String getString(String parameter);

    public String[] getStringAllLevels(String parameter);

    public Type getType(String parameter);

    public Number getNumber(String parameter);

    public Unit<?> getUnit(String parameter);

    public int getDimensions(String parameter);

    public Number[] getNumberAllLevels(String parameter);

    public Set<String> getParameters();
}
