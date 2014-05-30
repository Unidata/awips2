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
package com.raytheon.viz.aviation.editor;

import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.StyledText;
import org.eclipse.swt.events.DisposeEvent;
import org.eclipse.swt.events.DisposeListener;
import org.eclipse.swt.events.MouseAdapter;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.MouseTrackAdapter;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.graphics.Cursor;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swt.widgets.ScrollBar;

import com.raytheon.viz.aviation.resource.ResourceConfigMgr;
import com.raytheon.viz.aviation.resource.ResourceConfigMgr.ResourceTag;

/**
 * HeaderTextComp class is a composite that contains a text control header and a
 * text control body that scrolls together horizontally.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 28 FEB 2008  938        lvenable    Initial creation.
 * 6/17/2008    937        grichard    Added text control getters/setters.
 * 9/28/2010    2846       rferrel     Adjust totalControlWidth to mimic 
 *                                     AWIPS I behavior.
 * 12/01/2010   3263       rferrel     Added mouse track listener in order to
 *                                     display tool tip in dataStTxt.
 * 12/09/2010   7380       rferrel     Remove no longer needed constructor and now
 *                                     adjust both height and width of text filed.
 * 12 Aug 2013  #2256      lvenable    Added code to dispose of the cursor.
 * 09Apr2014    #3005      lvenable    Added methods to clear the header and data text controls or
 *                                     mark then as updating.  Removed unused methods.
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 * 
 */
public class HeaderTextComp extends Composite {
    /**
     * Parent composite.
     */
    private Composite parent;

    /**
     * Text cursor.
     */
    private Cursor textCursor;

    /**
     * Header styled text control.
     */
    private StyledText headerStTxt;

    /**
     * Data styled text control.
     */
    private StyledText dataStTxt;

    /**
     * Flag indicating if the header should display 2 lines or 1 line.
     */
    private boolean doubleLine;

    private Menu popupMenu;

    private boolean useResourceConfig;

    /**
     * Constructor.
     * 
     * @param parent
     *            Parent composite.
     * @param doubleLine
     *            Flag indicating if the header text control should be 2 lines
     *            or 1 line.
     * @param useResourceConfig
     */
    public HeaderTextComp(Composite parent, boolean doubleLine,
            boolean useResourceConfig) {
        super(parent, SWT.NONE);

        this.parent = parent;

        this.doubleLine = doubleLine;

        this.useResourceConfig = useResourceConfig;

        init();
    }

    /**
     * Initialize composite.
     */
    private void init() {

        ResourceConfigMgr configMgr = ResourceConfigMgr.getInstance();
        if (configMgr.isResourceLoaded() == false) {
            configMgr.reloadResourceData();
        }

        GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        GridLayout gl = new GridLayout(2, false);
        gl.verticalSpacing = 1;
        gl.marginHeight = 1;
        this.setLayout(gl);
        this.setLayoutData(gd);

        initializeComponents(configMgr);

        this.addDisposeListener(new DisposeListener() {
            @Override
            public void widgetDisposed(DisposeEvent e) {
                if (textCursor != null) {
                    textCursor.dispose();
                }
            }
        });

        this.pack();
    }

    /**
     * Initialize the controls on the composite.
     */
    private void initializeComponents(ResourceConfigMgr configMgr) {
        int textControlWidth = configMgr
                .getDataAsInt(ResourceTag.TextViewerWidth);
        int textControlHeight = configMgr
                .getDataAsInt(ResourceTag.TextViewerHeight);

        int fillerHeight = 0;

        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        gd.widthHint = textControlWidth;

        headerStTxt = new StyledText(this, SWT.BORDER | SWT.MULTI
                | SWT.H_SCROLL);
        headerStTxt.setWordWrap(false);
        headerStTxt.setEditable(false);
        headerStTxt.getHorizontalBar().setVisible(false);

        if (useResourceConfig) {
            configMgr.setTextFontAndColors(headerStTxt);
            String cursorStr = configMgr
                    .getResourceAsString(ResourceTag.TextCursor);
            int cursorInt = configMgr.getCursorAsInt(cursorStr);
            textCursor = new Cursor(parent.getDisplay(), cursorInt);
            headerStTxt.setCursor(textCursor);
        }

        // This leaves a bigger gap then wanted after the last line. However,
        // with larger font sizes the old way would not display all off the last
        // line. Any change to this should be tested with various fonts and
        // sizes.
        GC gc = new GC(headerStTxt);
        int fontHeight = gc.getFontMetrics().getHeight();
        gc.dispose();
        if (doubleLine == true) {
            gd.heightHint = fontHeight * 2;
        } else {
            gd.heightHint = fontHeight;
        }
        headerStTxt.setLayoutData(gd);

        fillerHeight = gd.heightHint;

        gd = new GridData(13, fillerHeight);
        Label filler = new Label(this, SWT.NONE);
        filler.setLayoutData(gd);

        GridLayout gl = new GridLayout(1, false);
        gl.marginHeight = 0;
        gl.marginWidth = 0;
        gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        gd.horizontalSpan = 2;
        Composite dataStComp = new Composite(this, SWT.NONE);
        dataStComp.setLayout(gl);
        dataStComp.setLayoutData(gd);

        gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        gd.widthHint = textControlWidth;
        gd.heightHint = textControlHeight - fillerHeight;
        dataStTxt = new StyledText(dataStComp, SWT.BORDER | SWT.MULTI
                | SWT.V_SCROLL | SWT.H_SCROLL);
        dataStTxt.setWordWrap(false);
        dataStTxt.setEditable(false);
        dataStTxt.setLayoutData(gd);

        if (useResourceConfig) {
            configMgr.setTextFontAndColors(dataStTxt);
        }

        dataStTxt.setCursor(textCursor);

        scrollListsTogether();

        dataStTxt.addMouseListener(new MouseAdapter() {

            @Override
            public void mouseDown(MouseEvent e) {
                if (e.button == 3) {
                    popupMenu.setVisible(true);
                }
            }

        });

        dataStTxt.addMouseTrackListener(new MouseTrackAdapter() {
            public void mouseHover(MouseEvent e) {
                StyledText stText = null;
                if (e.getSource() instanceof StyledText) {
                    stText = (StyledText) e.getSource();
                } else {
                    return;
                }

                try {
                    Point p = new Point(e.x, e.y);
                    int offset = stText.getOffsetAtLocation(p);
                    Object obj = stText.getData();
                    if (obj == null) {
                        stText.setToolTipText(null);
                    } else {
                        HeaderTextToolTip[] tips = (HeaderTextToolTip[]) obj;
                        String tip = HeaderTextToolTip.findTip(tips, offset);
                        stText.setToolTipText(tip);
                    }
                } catch (IllegalArgumentException ex) {
                    stText.setToolTipText(null);
                }
            }
        });

        createEditorPopupMenu(dataStComp);
    }

    /**
     * Create a popup menu to allow the user to copy text.
     * 
     * @param parentComp
     *            Parent composite.
     */
    private void createEditorPopupMenu(Composite parentComp) {
        popupMenu = new Menu(parentComp);

        MenuItem copyMI = new MenuItem(popupMenu, SWT.NONE);
        copyMI.setText("Copy");
        copyMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                dataStTxt.copy();
            }
        });

        parentComp.setMenu(popupMenu);
    }

    /**
     * Setup the text controls so they scroll together horizontally.
     */
    private void scrollListsTogether() {
        final ScrollBar hBar2 = headerStTxt.getHorizontalBar();
        final ScrollBar hBar1 = dataStTxt.getHorizontalBar();

        SelectionListener listener1 = new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                int y = hBar1.getSelection()
                        * (hBar2.getMaximum() - hBar2.getThumb())
                        / Math.max(1, hBar1.getMaximum() - hBar1.getThumb());
                hBar2.setSelection(y);

                headerStTxt.setHorizontalPixel(dataStTxt.getHorizontalPixel());
            }
        };

        SelectionListener listener2 = new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                int y = hBar2.getSelection()
                        * (hBar1.getMaximum() - hBar1.getThumb())
                        / Math.max(1, hBar2.getMaximum() - hBar2.getThumb());
                hBar1.setSelection(y);

                dataStTxt.setHorizontalPixel(headerStTxt.getHorizontalPixel());
            }
        };
        hBar1.addSelectionListener(listener1);
        hBar2.addSelectionListener(listener2);
    }

    /**
     * Method that gets the header styled text edit area.
     * 
     * @return the headerStTxt
     */
    public StyledText getHeaderStTxt() {
        return headerStTxt;
    }

    /**
     * Clear the header text and data text controls.
     */
    public void clearTextControls() {
        headerStTxt.setText("");
        dataStTxt.setText("");
    }

    /**
     * Set the header text and data text controls to display "updating...".
     */
    public void markTextAsUpdating() {
        headerStTxt.setText("updating...");
        dataStTxt.setText("updating...");
    }

    /**
     * Method that gets the data styled text edit area.
     * 
     * @return the dataStTxt
     */
    public StyledText getDataStTxt() {
        return dataStTxt;
    }
}
