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
import java.util.TimeZone;

import org.eclipse.jface.action.ContributionItem;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.MouseAdapter;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Device;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;

import com.raytheon.uf.common.time.SimulatedTime;
import com.raytheon.uf.viz.core.RGBColors;
import com.raytheon.viz.ui.statusline.EdgeLayout.EdgeLayoutData;
import com.raytheon.viz.ui.statusline.EdgeLayout.EdgeLayoutData.EdgeAffinity;
import com.raytheon.viz.ui.statusline.StatusMessage.Importance;
import com.raytheon.viz.ui.statusline.StatusStore.IStatusListener;

/**
 * TODO Add Description
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date			Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * Jul 14, 2008		#1223	randerso	Initial creation
 * Oct 22, 2012 #1229       rferrel     Changes for non-blocking ViewMessageDialog.
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */

public class StatBar extends ContributionItem implements IStatusListener {
    private StatusStore statusStore;

    private Composite comp;

    private Device device;

    private NewMessageIndicator indicator;

    private Label statusLabel;

    private Button pastButton;

    private Label msgText;

    private Color fgColor;

    private Color bgColor;

    private Color gray;

    private Font font;

    public StatBar(StatusStore statusStore) {
        super(statusStore.getLabel());

        this.statusStore = statusStore;
    }

    @Override
    public void fill(Composite parent) {
        createControls(parent);
        parent.layout();
        parent.pack();
    }

    /**
     * @param parent
     */
    private void createControls(Composite parent) {
        // create controls
        device = parent.getDisplay();

        comp = new Composite(parent, SWT.NONE);
        EdgeLayoutData layoutData = new EdgeLayoutData();
        layoutData.edgeAffinity = EdgeAffinity.LEFT;
        layoutData.grabExcessHorizontalSpace = true;
        comp.setLayoutData(layoutData);

        comp.setLayout(new GridLayout(4, false));

        // Default
        setGray(RGBColors.getRGBColor("Gray80"));
        font = new Font(comp.getDisplay(), "Monospace", 8, SWT.NORMAL);

        // Add the new message indicator
        this.indicator = new NewMessageIndicator(comp);
        this.indicator.setLayoutData(new GridData(20, 12));

        // Add the "Status" label
        this.statusLabel = new Label(comp, SWT.NONE);
        this.statusLabel.setFont(font);
        this.statusLabel.setText(this.getId() + ":");

        // Add the button to display past messages
        this.pastButton = new Button(comp, SWT.PUSH);
        this.pastButton.setLayoutData(new GridData(20, 20));
        this.pastButton.setFont(font);
        this.pastButton.setText("^");
        this.pastButton.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected(SelectionEvent e) {
                viewMessages();
            }

        });

        // Now the message text
        this.msgText = new Label(comp, SWT.BORDER);
        this.msgText.setFont(font);
        GridData gridData = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        gridData.minimumWidth = 0;
        this.msgText.setLayoutData(gridData);
        this.msgText.addMouseListener(new MouseAdapter() {

            @Override
            public void mouseUp(MouseEvent e) {
                if (e.button == 1) {
                    turnOffMessage();
                }
            }
        });

        if (statusStore.getMessageBuffer().size() > 0) {
            update(statusStore.getMessageBuffer().getFirst());
        }

        statusStore.addListener(this);
    }

    /**
     * @param color
     */
    private void setGray(RGB color) {
        if (this.gray != null) {
            this.gray.dispose();
        }
        this.gray = new Color(device, color);
    }

    /**
     * @param color
     */
    private void setFgColor(RGB color) {
        if (this.fgColor != null) {
            this.fgColor.dispose();
        }
        this.fgColor = new Color(device, color);
    }

    /**
     * @param color
     */
    private void setBgColor(RGB color) {
        if (this.bgColor != null) {
            this.bgColor.dispose();
        }
        this.bgColor = new Color(device, color);
    }

    @Override
    public void dispose() {
        statusStore.removeListener(this);

        if (indicator != null) {
            indicator.dispose();
        }

        if (gray != null) {
            gray.dispose();
        }
        if (fgColor != null) {
            fgColor.dispose();
        }
        if (bgColor != null) {
            bgColor.dispose();
        }
        if (font != null) {
            font.dispose();
        }
    }

    @Override
    protected void finalize() throws Throwable {
        super.finalize();
        dispose();
    }

    @Override
    public void update(StatusMessage newMessage) {
        // Gather information to create the StatusMessage
        Importance importance = newMessage.getImportance();

        // Get the foreground and background colors corresponding to the
        // importance
        setFgColor(statusStore.getImportanceDict().get(importance)
                .getForegroundColor());
        setBgColor(statusStore.getImportanceDict().get(importance)
                .getBackgroundColor());
        setGray(statusStore.getImportanceDict().get(importance)
                .getDimmedColor());

        // Set NewMessageIndicator according to importance
        long timeout = statusStore.getImportanceDict().get(importance)
                .getFlashingTimeout();

        if (timeout > 0) {
            long remaining = newMessage.getMessageDate().getTime() + timeout
                    - SimulatedTime.getSystemTime().getTime().getTime();
            if (remaining > 0) {
                this.indicator.blink(remaining, fgColor.getRGB());
            }
        }

        // Update StatusBar display
        if (!this.msgText.isDisposed()) {
            this.msgText.setText(this.getText(newMessage));
            this.msgText.setForeground(fgColor);
            this.msgText.setBackground(bgColor);
        }
    }

    private void turnOffMessage() {
        this.msgText.setForeground(this.gray);
        this.indicator.off();
    }

    /**
     * @param message
     * @return
     */
    private String getText(StatusMessage msg) {
        // Create text string for display of message
        // To display in Status Bar, remove line feeds
        String barText = msg.getMessageText().replace('\n', ' ');

        SimpleDateFormat sdf = new SimpleDateFormat("HH:mm:ss");
        sdf.setTimeZone(TimeZone.getTimeZone("GMT"));

        return sdf.format(msg.getMessageDate()) + " "
                + msg.getImportance().toString() + " " + barText;

    }

    private void viewMessages() {
        statusStore.openViewMessagesDialog();
    }
}
