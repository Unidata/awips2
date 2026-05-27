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

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;

/**
 * Creates a text entry widget. The widget has two parts; a label and a
 * Text box. The widget's title is displayed in the label. In this widget,
 * the filter is unused.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 17, 2009            mfegan     Initial creation
 *
 * </pre>
 *
 * @author mfegan
 * @version 1.0
 */
public class AppTextWidget extends AAppWidget implements IModifiable {
    /** the text box */
    private Text value = null;
    /** the style for the widget */
    private int style = SWT.NONE;
    /** the desired width of the text box */
    private int textWidth;
    
    /** default widget label */
    private static final String DEF_LABEL = "Value:";
    /** default filter */
    private static final String DEF_FILTER = "^.+$";
    /** default text box width */
    private static final int DEF_TEXT_WIDTH = 400;
    /**
     * Constructor.
     * 
     * @param parent container for the widget
     * @param style style of the widget
     * @param width width of the text box
     * @param title text for the label
     * @param filter text filter (not used)
     */
    public AppTextWidget(Composite parent, int style, int width, String title, String filter) {
        super(parent,
                SWT.NONE,
                isEmpty(title)?DEF_LABEL:title,
                isEmpty(filter)?DEF_FILTER:filter);
        this.style = style;
        this.textWidth = width;
    }
    /**
     * Constructor. Text box is the default width.
     * 
     * @param parent container for the widget
     * @param style style of the widget
     * @param title text for the label
     * @param filter text filter (not used)
     */
    public AppTextWidget(Composite parent, int style, String title, String filter) {
        this(parent, style, DEF_TEXT_WIDTH, title, filter);
    }
    /**
     * Constructor. Text box is the default width and filter accepts all data.
     * 
     * @param parent container for the widget
     * @param style style of the widget
     * @param title text for the label
     */
    public AppTextWidget(Composite parent, int style, String title) {
        this(parent, style, DEF_TEXT_WIDTH, title, null);
    }
    /**
     * Constructor. Text box is the default width and filter accepts all data.
     * Style is set to SWT.BORDER.
     * 
     * @param parent container for the widget
     * @param title text for the label
     */
    public AppTextWidget(Composite parent, String title) {
        this(parent, SWT.BORDER, DEF_TEXT_WIDTH, title,null);
    }
    /**
     * Constructor. The text box is set to the specified width; the filter
     * accepts all data. Style is set to SWT.BORDER.
     * 
     * @param parent container for the widget
     * @param title text for the label
     * @param width width of the text box
     */
    public AppTextWidget(Composite parent, String title, int width) {
        this(parent, SWT.BORDER, width, title, null);
    }
    @Override
    public void create() {
        GridData gd = null;
        Label label = null;
        setLayout(new GridLayout(2,false));

        gd = new GridData(SWT.RIGHT,SWT.DEFAULT,true,true);
        gd.widthHint = 50;
        gd.heightHint = 25;
        
        label = new Label(this,SWT.NONE);
        label.setLayoutData(gd);
        label.setText(this.title);

        gd = new GridData(SWT.FILL,SWT.FILL,true,true);
        gd.widthHint = textWidth;
        gd.heightHint = 25;
        
        value = new Text(this,style);
        value.setLayoutData(gd);

    }
    @Override
    public void clear() {
        value.setText("");
    }
    /**
     * returns the text from the widget.
     */
    public String getText() {
        return value.getText();
    }
    /**
     * sets the text into the widget.
     */
    public void setText(String string) {
        this.value.setText(string);
    }
    @Override
    public void addModifyListener(ModifyListener listener) {
        value.addModifyListener(listener);
    }
}
