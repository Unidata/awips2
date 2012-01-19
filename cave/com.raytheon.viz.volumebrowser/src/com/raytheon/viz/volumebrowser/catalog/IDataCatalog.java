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

package com.raytheon.viz.volumebrowser.catalog;

import java.util.Map;

import org.eclipse.jface.viewers.ListViewer;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Label;

import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.viz.core.rsc.AbstractRequestableResourceData;

/**
 * 
 * An interface to the catalog made available for the various data types. It
 * defines the methods required to properly get data into the volume browser and
 * on to the list of products to be displayed.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 11/29/2006	#7         brockwoo    Initial creation
 * 
 * </pre>
 * 
 * @author brockwoo
 * @version 1.0
 */
public interface IDataCatalog {

    /**
     * An empty string to reset the list boxes to empty.
     */
    public static final String EMPTYLIST = null;

    /**
     * The default height for a selection list box.
     */
    public static final int DEFAULTPANEHEIGHT = 180;

    /**
     * The default width for a single list box.
     */
    public static final int ONEPANEWIDTH = 725;

    /**
     * The default width for dual list boxes.
     */
    public static final int TWOPANEWIDTH = 349;

    /**
     * The default width for three list boxes.
     */
    public static final int THREEPANEWIDTH = 222;

    /**
     * String array to notify the user that the catalog is empty.
     */
    public static final String[] NODATA = { "No Data" };

    /**
     * Will return a string that identifies the data type of the catalog (i.e.
     * Radar, Satellite). This is used to populate the drop down at top of the
     * volume browser.
     * 
     * @return The string of the type of catalog
     */
    public abstract String getDataType();

    /**
     * Will return a map with a string that identifies the editor and a string
     * for the editor input type of the catalog (i.e. Radar, Satellite). This is
     * used to create the correct type of editor for a given product.
     * 
     * @return The string of the type of editor
     */
    public abstract Map<String, String> getEditorConfig();

    /**
     * Initializes the catalog with the proper labels and list boxes for the
     * volume browser. Will then set the controls as needed for the parameters
     * that are available for that data type.
     * 
     * @param label1
     *            The label above the left most list box
     * @param label2
     *            The label above the center list box
     * @param label3
     *            The label above the right most list box
     * @param list1
     *            The list box at the left
     * @param list2
     *            The list box in the center
     * @param list3
     *            The list box at the right
     * @param addButton
     *            The 'Add' button in the volume browser which can be
     *            enabled/disabled from the catalog
     */
    public abstract void initDataCatalog(Label label1, Label label2,
            Label label3, ListViewer list1, ListViewer list2, ListViewer list3,
            Button addButton);

    /**
     * The parameters for the product being added to the list of products to
     * display.
     * 
     * @return A hash map containing all parameters that will allow the layer to
     *         retrieve that specific product
     */
    public abstract Map<String, RequestConstraint> getProductParameters();

    public abstract AbstractRequestableResourceData getResourceData();

    /**
     * A call to dispose will clean up the controls for the present catalog and
     * empty out any data contained within.
     */
    public abstract void dispose();

}
