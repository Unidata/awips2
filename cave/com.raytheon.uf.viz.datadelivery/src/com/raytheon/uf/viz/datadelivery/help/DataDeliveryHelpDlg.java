package com.raytheon.uf.viz.datadelivery.help;

import java.util.ArrayList;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.StyleRange;
import org.eclipse.swt.custom.StyledText;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.uf.common.util.StringUtil;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;

/**
 * Help dialog for Data Delivery.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 06/06/2013     2030     mpduff      Initial creation.
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class DataDeliveryHelpDlg extends CaveSWTDialog {
    /** Styled Text control */
    private StyledText styledText;

    /** List of StyleRange objects */
    private final List<StyleRange> ranges = new ArrayList<StyleRange>();

    /** Pattern for 1 or more spaces */
    private static final Pattern pattern = Pattern.compile(" +");

    /** Help JaxB xml object */
    private final DataDeliveryHelpXML helpXml;

    /**
     * Constructor.
     * 
     * @param parentShell
     *            Parent shell.
     * @param helpXml
     *            The JaxB xml object
     */

    public DataDeliveryHelpDlg(Shell parentShell, DataDeliveryHelpXML helpXml) {
        super(parentShell, SWT.DIALOG_TRIM | SWT.MIN | SWT.RESIZE,
                CAVE.DO_NOT_BLOCK | CAVE.NO_PACK);
        this.helpXml = helpXml;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.ui.dialogs.CaveSWTDialogBase#constructShellLayout()
     */
    @Override
    protected Layout constructShellLayout() {
        return new GridLayout(1, false);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.ui.dialogs.CaveSWTDialogBase#constructShellLayoutData()
     */
    @Override
    protected Object constructShellLayoutData() {
        return new GridData(SWT.FILL, SWT.DEFAULT, true, false);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected void initializeComponents(Shell shell) {
        GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        GridLayout gl = new GridLayout(1, false);
        shell.setLayout(gl);
        shell.setLayoutData(gd);

        gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        gl = new GridLayout(1, false);

        styledText = new StyledText(shell, SWT.WRAP | SWT.BORDER | SWT.V_SCROLL);
        styledText.setLayout(gl);
        styledText.setLayoutData(gd);

        String text = getHelpText();
        styledText.setText(text);
        styledText.setLineAlignment(0, 1, SWT.CENTER);

        for (StyleRange sr : ranges) {
            styledText.setStyleRange(sr);
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected void opened() {
        shell.setMinimumSize(400, 600);
    }

    /**
     * Get the help text from the xml.
     * 
     * @return The help text String
     */
    private String getHelpText() {
        StringBuilder buffer = new StringBuilder();

        this.setText(helpXml.getTitle());

        // indices for style ranges start and end
        int startIdx = 0;
        for (HelpEntryXML entry : helpXml.getEntryList()) {
            startIdx = buffer.length();
            buffer.append(entry.getHeader().trim()).append(StringUtil.NEWLINE);
            StyleRange sr = new StyleRange();
            sr.start = startIdx;
            sr.length = buffer.length() - startIdx;
            sr.fontStyle = SWT.BOLD;
            ranges.add(sr);
            Matcher matcher = pattern.matcher(entry.getText().trim());
            String text = matcher.replaceAll(" ");
            if (text.length() > 0) {
                buffer.append(text).append(StringUtil.NEWLINE)
                        .append(StringUtil.NEWLINE);
            } else {
                buffer.append(StringUtil.NEWLINE);
            }
        }

        return buffer.toString();
    }
}
