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

import java.awt.image.BufferedImage;
import java.util.List;

import org.eclipse.swt.graphics.RGB;

import com.raytheon.uf.viz.core.IGraphicsTarget.LineStyle;
import com.raytheon.viz.pointdata.def.Condition;
import com.raytheon.viz.pointdata.def.PlotParameterDefinition;

/**
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 10/31/2019   71272      ksunil      Initial Creation
 * 12/10/2019   72280      ksunil      Added condition filter
 * 01/13/2020   73084      ksunil      Changed the addElement signature
 *
 * </pre>
 *
 * @author ksunil
 */

public interface IPlotModelFactory {

    String MARKER_SYMBOL_FILE = "MarkerSymbols.svg";

    String PLOT_MODEL_DIR = "plotModels";

    String PLOT_FILTERS_DIR = "ConditionalFilters";

    void clearImageCache();

    RGB getColor();

    void setColor(RGB color);

    void setLineWidth(int width);

    void setLineStyle(LineStyle style);

    void setPlotDimensions(long x, long y);

    int getDefinedPlotModelWidth();

    int getDefinedPlotModelHeight();

    BufferedImage getStationPlot(PlotData stationData, double latitude,
            double longitude);

    /**
     * Returns an image of the "sample" plot, used in the edit plot attribute's
     * palette. This enables the user to visualize the changes before applying
     * and saving the changes to the plot model
     * 
     * @return a BufferedImage representing the sample.
     */

    BufferedImage getSamplePlot();

    String getStationMessage(PlotData stationData, String dataURI);

    List<IPlotModelElement> getPlotFields();

    void setLowerLimit(double lowerLimit);

    void setUpperLimit(double upperLimit);

    void setPlotMissingData(boolean b);

    void setConditionFilter(Condition filter);

    List<IPlotModelElement> getSampleFields();

    /**
     * @return the plugin
     */
    String getPlugin();

    boolean isCachingImages();

    String getPlotModelFilename();

    /**
     * Disposes of the plot model
     */
    void dispose();

    void savePlotModel();

    boolean isSingleColor();

    IPlotModelElement addElement(PlotParameterDefinition paremDef);

}