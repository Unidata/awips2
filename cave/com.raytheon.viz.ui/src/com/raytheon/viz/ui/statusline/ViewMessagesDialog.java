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
package com.raytheon.viz.ui.statusline;

import java.text.SimpleDateFormat;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.TimeZone;

import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.StyleRange;
import org.eclipse.swt.custom.StyledText;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.FontMetrics;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.viz.ui.dialogs.CaveJFACEDialog;
import com.raytheon.viz.ui.statusline.StatusMessage.Importance;

/**
 * TODO Add Description ViewMessageDialog.java May 19, 2008
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 	May 19, 2008					Eric Babin Initial Creation
 * Oct 22, 2012 1229       rferrel     Converted to CaveJFACEDialog.
 * 
 * </pre>
 * 
 * @author ebabin
 * @version 1.0
 */

public class ViewMessagesDialog extends CaveJFACEDialog {

    private SimpleDateFormat sdf;

    private List<StatusMessage> messageBuffer;

    private Map<Importance, MessageImportance> importanceDict;

    private String title;

    private StyledText styledText;

    private Composite top;

    private Font font;

    /**
     * Track colors used so they can be reused and disposed.
     */
    private final Map<RGB, Color> colorMap = new HashMap<RGB, Color>();

    /**
     * @param parent
     */
    public ViewMessagesDialog(Shell parent, List<StatusMessage> messageBuffer,
            Map<Importance, MessageImportance> importanceDict, String title) {
        super(parent);

        this.setShellStyle(SWT.TITLE | SWT.MODELESS | SWT.RESIZE | SWT.CLOSE);
        this.messageBuffer = messageBuffer;
        this.importanceDict = importanceDict;
        if (title != null) {
            this.title = title;
        }

        sdf = new SimpleDateFormat("yy/MM/dd HH:mm:ss");
        sdf.setTimeZone(TimeZone.getTimeZone("GMT"));
    }

    @Override
    protected Control createDialogArea(Composite parent) {
        top = (Composite) super.createDialogArea(parent);

        GridLayout mainLayout = new GridLayout(1, true);
        mainLayout.marginHeight = 5;
        mainLayout.marginWidth = 5;

        top.setLayout(mainLayout);

        font = new Font(top.getDisplay(), "Monospace", 8, SWT.NORMAL);

        styledText = new StyledText(top, SWT.BORDER | SWT.MULTI | SWT.V_SCROLL
                | SWT.H_SCROLL | SWT.READ_ONLY);
        styledText.setFont(font);
        styledText.setWordWrap(true);

        GC gc = new GC(styledText);
        FontMetrics fm = gc.getFontMetrics();
        fm.getHeight();
        styledText.setLayoutData(new GridData(fm.getAverageCharWidth() * 120,
                fm.getHeight() * 17));
        gc.dispose();

        updateText();

        return top;
    }

    @Override
    protected Control createButtonBar(Composite parent) {
        Composite composite = (Composite) super.createButtonBar(parent);
        GridData data = new GridData(GridData.HORIZONTAL_ALIGN_CENTER
                | GridData.VERTICAL_ALIGN_CENTER);
        composite.setLayoutData(data);
        return composite;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.jface.window.Window#configureShell(org.eclipse.swt.widgets
     * .Shell)
     */
    @Override
    protected void configureShell(Shell shell) {
        super.configureShell(shell);
        shell.setText(title);
    }

    /**
     * @param message
     */
    public void updateText() {
        if (!styledText.isDisposed()) {
            StyleRange[] styleRanges = new StyleRange[messageBuffer.size()];
            int i = 0;
            styledText.setText("");
            for (StatusMessage m : this.messageBuffer) {
                MessageImportance importance = this.importanceDict.get(m
                        .getImportance());

                // get the existing start index...
                int startIndex = styledText.getText().length();
                String s = sdf.format(m.getMessageDate()) + " "
                        + m.getMessageText() + "\n";

                RGB rgb = importance.getForegroundColor();
                Color foregroundColor = colorMap.get(rgb);
                if (foregroundColor == null) {
                    foregroundColor = new Color(top.getDisplay(), rgb);
                    colorMap.put(rgb, foregroundColor);
                }

                rgb = importance.getBackgroundColor();
                Color backgroundColor = colorMap.get(rgb);
                if (backgroundColor == null) {
                    backgroundColor = new Color(top.getDisplay(), rgb);
                    colorMap.put(rgb, backgroundColor);
                }
                styleRanges[i++] = new StyleRange(startIndex, s.length(),
                        foregroundColor, backgroundColor);

                styledText.setText(styledText.getText() + s);

            }
            styledText.setStyleRanges(styleRanges);
        }
    }

    @Override
    public boolean close() {
        for (Color color : colorMap.values()) {
            color.dispose();
        }
        colorMap.clear();
        return super.close();
    }

}
