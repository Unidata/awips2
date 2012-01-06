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
package com.raytheon.uf.viz.app.launcher.dialogs.widgets;

import org.eclipse.swt.widgets.Composite;

/**
 * 
 * This is an abstract base class containing basic functionality for all
 * widget classes utilized by the Application Launcher facility. Two attributes
 * are provided for use by concrete implementations: title and filter. The title
 * provides a displayable name for the widget; the filter provides a string
 * to determine if the widget will accept a data input. 
 * <P>
 * Note that construction of the widget class is separated from creating the 
 * widget itself.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 17, 2009 2081       mfegan     Initial creation
 *
 * </pre>
 *
 * @author mfegan
 * @version 1.0
 */
public abstract class AAppWidget extends Composite implements IClearableWidget{
    
    /** the name/title of the widget */
    protected String title = "";
    /** a filter to determine if the widget will accept a value */
    protected String filter = "";
    
    /**
     * Constructor. The Widget is created with the specified values.
     * 
     * @param parent the container for the widget
     * @param style the style for the widget
     * @param title the title for the widget
     * @param filter the filter for the widget
     */
    public AAppWidget(Composite parent, int style, String title, String filter) {
        super(parent, style);
        this.title = title;
        this.filter = filter;
    }
    /**
     * Called by the client to create the widget.
     */
    public abstract void create();
    
    @Override
    public abstract void clear();
    
    /**
     * Returns true if the string is null or empty.
     * @param string the string to test
     */
    protected static boolean isEmpty(String string) {
        return string == null || "".equals(string);
    }
    /**
     * Returns true if the entry matches the widget's filter.
     * @param entry
     */
    protected boolean isValidEntry(String entry) {
        // valid if ~empty && filter
         return !isEmpty(entry) && matchesFilter(entry);
    }
    /**
     * Returns true if the entry matches the widget's filter.
     * @param entry
     */
    protected boolean matchesFilter(String entry) {
        return entry.matches(filter);
    }
    /**
     * returns the widget's title.
     */
    public String getTitle() {
        return title;
    }
    /**
     * sets the widget's title.
     */
    public void setTitle(String title) {
        this.title = title;
    }
    /**
     * returns the widget's filter.
     */
    public String getFilter() {
        return filter;
    }
    /**
     * sets the widget's filter.
     */
    public void setFilter(String filter) {
        this.filter = filter;
    }
    
}
