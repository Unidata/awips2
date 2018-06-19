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
package com.raytheon.viz.ghg.monitor;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Map.Entry;

import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.StyleRange;
import org.eclipse.swt.custom.StyledText;
import org.eclipse.swt.events.DisposeEvent;
import org.eclipse.swt.events.DisposeListener;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;

import com.raytheon.viz.ghg.monitor.data.GhgConfigData.DataEnum;
import com.raytheon.viz.ghg.monitor.data.GhgData;

/**
 * This class displays the GHG text component.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 25 MAR 2008  N/A        lvenable    Initial creation
 * Dec 16, 2015 #5184      dgilling    Remove viz.gfe dependencies.
 * Feb 05, 2016 #5316      randerso    Moved notification registration into GHGMonitorDlg
 * Aug 02, 2016 #5790      dgilling    Show all segments for a hazard.
 * 
 * </pre>
 * 
 * @author lvenable
 * 
 */
public class GhgTextComp extends Composite {
    private static final int PIL_INDEX = DataEnum.PIL.ordinal();

    private static final int ISSUE_INDEX = DataEnum.ISSUE_TIME.ordinal();

    /**
     * Parent composite.
     */
    private Composite parent;

    /**
     * Editor text control.
     */
    private StyledText styledText;

    /**
     * Font used for editors.
     */
    private Font editorFont;

    /**
     * Normal editor background color.
     */
    private Color editorColor;

    /**
     * Editor background color in header sections
     */
    private Color headerColor;

    /**
     * List of GhgData objects
     */
    private Collection<GhgData> dataList;

    /**
     * Constructor.
     * 
     * @param parent
     *            Parent composite.
     */
    public GhgTextComp(Composite parent) {
        super(parent, SWT.NONE);

        this.parent = parent;

        init();
    }

    /**
     * Initialize the composite.
     */
    private void init() {
        editorFont = new Font(parent.getDisplay(), "Monospace", 14, SWT.BOLD);
        editorColor = new Color(parent.getDisplay(), 230, 230, 230);
        headerColor = new Color(parent.getDisplay(), 30, 30, 230);

        GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        GridLayout gl = new GridLayout(1, false);
        gl.verticalSpacing = 1;
        gl.marginHeight = 1;
        setLayout(gl);
        setLayoutData(gd);

        initializeComponents();

        this.pack();

        addDisposeListener(new DisposeListener() {
            @Override
            public void widgetDisposed(DisposeEvent arg0) {
                disposeItems();
            }
        });
    }

    private void disposeItems() {
        editorFont.dispose();
        editorColor.dispose();
        headerColor.dispose();
    }

    /**
     * Initialize the controls on the composite.
     */
    private void initializeComponents() {
        createTextControl();
    }

    /**
     * Create the text control.
     */
    private void createTextControl() {
        GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        gd.heightHint = 400;
        gd.widthHint = 800;
        styledText = new StyledText(this, SWT.BORDER | SWT.MULTI | SWT.V_SCROLL
                | SWT.H_SCROLL);
        styledText.setWordWrap(false);
        styledText.setFont(editorFont);
        styledText.setEditable(true);
        styledText.setLayoutData(gd);
        styledText.setBackground(editorColor);
        styledText.setForeground(parent.getDisplay().getSystemColor(
                SWT.COLOR_BLACK));
        styledText.setText("No selections made on map or spreadsheet");
    }

    public Display getCurrentDisplay() {
        return styledText.getDisplay();
    }

    /**
     * Refresh the text component
     */
    public void refresh() {
        styledText.setText("");

        if (dataList != null) {
            // combine text where adjacent records are compatible
            List<GhgData> combined = new ArrayList<GhgData>(dataList);

            for (GhgData data : combined) {
                showGhgText(data);
            }
        }
    }

    private void showGhgText(GhgData data) {
        String[] dataCellNames = data.getDataCellNames();
        String overviewText = data.getOverviewText();
        if ((overviewText != null) && (overviewText.length() > 0)) {
            int start = styledText.getCharCount();
            int startLine = styledText.getLineCount() - 1;
            String ovrHdr = String.format(
                    "Overview Text:  Pil: %s    IssueTime: %s\n",
                    dataCellNames[PIL_INDEX], dataCellNames[ISSUE_INDEX]);
            styledText.append(ovrHdr);
            int len = styledText.getCharCount() - start;
            styledText.setLineBackground(startLine, 1, headerColor);
            StyleRange range = new StyleRange(start, len, editorColor,
                    headerColor);
            styledText.setStyleRange(range);
            styledText.append(overviewText);
            styledText.append("\n");

        }

        // Segment text
        for (Entry<Collection<String>, String> segment : data
                .getSegmentTextMap().entrySet()) {
            int start = styledText.getCharCount();
            int startLine = styledText.getLineCount() - 1;
            String segHdr = String.format(
                    "Segment Text:   Pil: %s    IssueTime: %s\n",
                    dataCellNames[PIL_INDEX], dataCellNames[ISSUE_INDEX]);
            styledText.append(segHdr);

            styledText.append("[");
            String sep = "";

            // Get a sorted list of geoIds
            List<String> sortedIds = new ArrayList<>(segment.getKey());
            Collections.sort(sortedIds);

            for (String geoId : sortedIds) {
                styledText.append(String.format("%s'%s'", sep, geoId));
                sep = ",";
            }
            styledText.append("]\n");
            int lineLen = styledText.getLineCount() - startLine - 1;
            int len = styledText.getCharCount() - start;
            styledText.setLineBackground(startLine, lineLen, headerColor);
            StyleRange range = new StyleRange(start, len, editorColor,
                    headerColor);
            styledText.setStyleRange(range);

            String segText = segment.getValue();
            if (segText != null) {
                styledText.append(segText);
            }
            styledText.append("\n");
        }
    }

    public void update(Collection<GhgData> data) {
        dataList = data;
        refresh();
    }
}