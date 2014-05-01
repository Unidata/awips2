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
package com.raytheon.viz.avnconfig;

import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Set;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.KeyEvent;
import org.eclipse.swt.events.KeyListener;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;

/**
 * Composite class containing a label and text control. The text control turns a
 * specified color when the text is less than 8 characters.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 22 MAY 2008  1119       lvenable    Initial creation
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 * 
 */
public class LabelTextComp extends Composite {
    /**
     * Parent composite.
     */
    private Composite parent;

    /**
     * Map of TAF data.
     */
    private Map<String, String> tafDataMap;

    /**
     * Map of Text controls.
     */
    private Map<String, Text> textControlMap;

    /**
     * Color if a Text control has the incorrect number of characters.
     */
    private Color incorrectColor;

    /**
     * Font used for the Text controls.
     */
    private Font textFont;

    /**
     * Constructor.
     * 
     * @param parent
     *            Parent composite.
     * @param tafDataMap
     *            TAF data map.
     * @param incorrectColor
     *            Incorrect color (use with the Text control).
     * @param textFont
     *            Font for the Text control.
     */
    public LabelTextComp(Composite parent, Map<String, String> tafDataMap,
            Color incorrectColor, Font textFont) {
        super(parent, SWT.NONE);

        this.parent = parent;

        this.tafDataMap = tafDataMap;

        this.incorrectColor = incorrectColor;

        this.textFont = textFont;

        init();
    }

    /**
     * Initialize method.
     */
    private void init() {
        textControlMap = new LinkedHashMap<String, Text>();

        GridLayout gl = new GridLayout(1, false);
        gl.verticalSpacing = 1;
        this.setLayout(gl);

        createLabelTextControls();
    }

    /**
     * Create the label and text controls from the TAF data map.
     */
    private void createLabelTextControls() {
        Composite controlComp = new Composite(this, SWT.NONE);
        GridLayout gl = new GridLayout(2, false);
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        gl.verticalSpacing = 1;
        controlComp.setLayout(gl);

        Set<String> keys = tafDataMap.keySet();
        String key;

        int counter = 0;

        for (Iterator<String> iterator = keys.iterator(); iterator.hasNext();) {
            key = iterator.next();

            gd = new GridData(40, SWT.DEFAULT);
            Label tmpLbl = new Label(controlComp, SWT.RIGHT);
            tmpLbl.setText(key);
            tmpLbl.setFont(textFont);
            tmpLbl.setLayoutData(gd);

            gd = new GridData(100, SWT.DEFAULT);
            Text tmpText = new Text(controlComp, SWT.BORDER);
            tmpText.setText(tafDataMap.get(key));
            tmpText.setLayoutData(gd);
            tmpText.setFont(textFont);
            tmpText.setTextLimit(9);
            addListenerToTextControl(tmpText);

            if (tmpText.getText().length() < 8
                    || tmpText.getText().length() > 9) {
                tmpText.setBackground(incorrectColor);
            }

            textControlMap.put(key, tmpText);

            ++counter;
        }

        // -----------------------------------------------------
        // Create the rest of the text controls (no labels)
        // -----------------------------------------------------
        while (counter < 40) {
            // Filler
            gd = new GridData(40, SWT.DEFAULT);
            Label tmpLbl = new Label(controlComp, SWT.RIGHT);
            tmpLbl.setFont(textFont);
            tmpLbl.setLayoutData(gd);

            gd = new GridData(100, SWT.DEFAULT);
            Text tmpText = new Text(controlComp, SWT.BORDER);
            tmpText.setLayoutData(gd);
            tmpText.setFont(textFont);
            tmpText.setBackground(incorrectColor);
            tmpText.setTextLimit(9);
            addListenerToTextControl(tmpText);

            textControlMap.put(String.valueOf(counter), tmpText);

            ++counter;
        }
    }

    /**
     * Add a listener to the text controls so they will turn a certain color
     * when there are less than 8 characters entered in.
     * 
     * @param tc
     *            Text control.
     */
    private void addListenerToTextControl(Text tc) {
        tc.addKeyListener(new KeyListener() {
            public void keyPressed(KeyEvent ke) {
                // Do nothing...
            }

            public void keyReleased(KeyEvent ke) {
                Text source = (Text) ke.getSource();

                if (source.getText().length() < 8
                        || source.getText().length() > 9) {
                    source.setBackground(incorrectColor);
                } else {
                    source.setBackground(parent.getDisplay().getSystemColor(
                            SWT.COLOR_WHITE));
                }
            }
        });
    }

    /**
     * Replace the text for the control text field associated with key and if
     * applicable change its background color.
     * 
     * @param key
     * @param text
     *            - new text
     */
    public void updateTextControl(String key, String text) {
        Text textControl = textControlMap.get(key);
        textControl.setText(text);
        tafDataMap.put(key, text);

        if (text.length() >= 8 && text.length() <= 9) {
            textControl.setBackground(parent.getDisplay().getSystemColor(
                    SWT.COLOR_WHITE));
        }
    }
}
