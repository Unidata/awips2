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
package com.raytheon.uf.viz.localization.perspective.ui.custom.velocity;

import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.jface.text.source.DefaultCharacterPairMatcher;
import org.eclipse.jface.text.source.ISourceViewer;
import org.eclipse.jface.text.source.IVerticalRuler;
import org.eclipse.swt.custom.StyledText;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.editors.text.TextEditor;
import org.eclipse.ui.texteditor.AnnotationPreference;
import org.eclipse.ui.texteditor.ChainedPreferenceStore;
import org.eclipse.ui.texteditor.SourceViewerDecorationSupport;

import com.raytheon.uf.viz.localization.perspective.Activator;

/**
 * Editor class for velocity template files
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 12, 2011            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class VelocityTemplateEditor extends TextEditor {

    public static final String VELOCITY_PREFS_SCOPE = "com.raytheon.uf.viz.localization.velocity";

    public static final String BRACKET_MATCHING = "BRACKET_MATCHING";

    public static final String BRACKET_MATCHING_COLOR = "BRACKET_MATCHING_COLOR";

    public static final String OCCURANCE_ANNOTATION_TYPE = "com.raytheon.uf.viz.velocity.occurences";

    public static final String OCCURANCE_PREF = "OCCURANCES";

    public static final String OCCURANCE_COLOR_PREF = "OCCURANCES_COLOR";

    static {
        IPreferenceStore store = Activator.getDefault().getPreferenceStore();
        store.setDefault(BRACKET_MATCHING, true);
        store.setDefault(BRACKET_MATCHING_COLOR, "120,120,120");
        store.setDefault(OCCURANCE_PREF, true);
        store.setDefault(OCCURANCE_COLOR_PREF, "210,210,210");
    }

    public class VelocityColorManager {
        protected Map<RGB, Color> fColorTable = new HashMap<RGB, Color>();

        public void dispose() {
            Iterator<Color> e = fColorTable.values().iterator();
            while (e.hasNext())
                e.next().dispose();
        }

        public Color getColor(RGB rgb) {
            Color color = fColorTable.get(rgb);
            if (color == null) {
                color = new Color(Display.getCurrent(), rgb);
                fColorTable.put(rgb, color);
            }
            return color;
        }
    }

    public static interface IVelocityColorConstants {
        // TODO: Move to "theme" based color setting

        public static RGB DEFAULT = new RGB(0, 0, 0);

        public static RGB FUNCTION = new RGB(127, 0, 85);

        public static RGB FUNCTION_START = new RGB(120, 120, 120);

        public static RGB COMMENT = new RGB(63, 127, 95);

        public static RGB STRING = new RGB(55, 0, 255);

        public static RGB OBJECT = new RGB(0, 0, 255);

        public static RGB OBJECT_START = new RGB(120, 120, 120);

    }

    private VelocityColorManager colorManager = new VelocityColorManager();

    public VelocityTemplateEditor() {
        setSourceViewerConfiguration(new VelocityConfiguration(colorManager));
        setDocumentProvider(new VelocityDocumentProvider());
    }

    public void dispose() {
        colorManager.dispose();
        super.dispose();
    }

    @Override
    protected void configureSourceViewerDecorationSupport(
            SourceViewerDecorationSupport support) {
        support.setCharacterPairMatcher(new DefaultCharacterPairMatcher(
                new char[] { '(', ')', '{', '}' }));
        support.setMatchingCharacterPainterPreferenceKeys(BRACKET_MATCHING,
                BRACKET_MATCHING_COLOR);
        AnnotationPreference ap = new AnnotationPreference();
        ap.setAnnotationType(OCCURANCE_ANNOTATION_TYPE);
        ap.setHighlightPreferenceKey(OCCURANCE_PREF);
        ap.setColorPreferenceKey(OCCURANCE_COLOR_PREF);
        support.setAnnotationPreference(ap);
        super.configureSourceViewerDecorationSupport(support);
    }

    @Override
    protected ISourceViewer createSourceViewer(Composite parent,
            IVerticalRuler ruler, int styles) {
        ISourceViewer viewer = super.createSourceViewer(parent, ruler, styles);
        StyledText textWidget = viewer.getTextWidget();

        VelocityEditCursorListener listener = new VelocityEditCursorListener(
                this);
        textWidget.addMouseListener(listener);
        textWidget.addKeyListener(listener);
        return viewer;
    }

    @Override
    protected void initializeEditor() {
        super.initializeEditor();
        setPreferenceStore(new ChainedPreferenceStore(new IPreferenceStore[] {
                getPreferenceStore(),
                Activator.getDefault().getPreferenceStore() }));
    }

}
