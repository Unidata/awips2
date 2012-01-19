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
package com.raytheon.viz.aviation.climatology;

import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Image;

/**
 * ICigVisTabComp Interface used by the "By Month", "By Hour", and "By Wind
 * Direction" tabs.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 03 MAR 2008  938        lvenable    Initial creation.
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 * 
 */
public interface ICigVisTabComp {
    /**
     * Set the percent occurrence.
     * 
     * @param percentOccurrence
     *            Percent occurrence.
     */
    void setMaxPercentOccurrence(int percentOccurence);

    /**
     * Redraw the canvas.
     */
    void redrawCanvas();

    /**
     * Set the data.
     * 
     * @param data
     *            Data to set.
     */
    public void setCigVisData(CigVisDistDataManager data);

    public Image getCigVisDistImage();

    public void drawCanvas(GC gc);

    public CigVisDistributionDlg getDialog();

    public float getMaxPercentInData();
}
