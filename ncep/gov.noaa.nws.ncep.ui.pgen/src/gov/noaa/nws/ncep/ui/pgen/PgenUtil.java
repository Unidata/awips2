/*
 * gov.noaa.nws.ncep.ui.pgen.PgenUtil
 * 
 * 15 December 2008
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */

package gov.noaa.nws.ncep.ui.pgen;

import gov.noaa.nws.ncep.ui.pgen.contours.Contours;
import gov.noaa.nws.ncep.ui.pgen.elements.AbstractDrawableComponent;
import gov.noaa.nws.ncep.ui.pgen.elements.DrawableElement;
import gov.noaa.nws.ncep.ui.pgen.elements.Layer;
import gov.noaa.nws.ncep.ui.pgen.elements.Line;
import gov.noaa.nws.ncep.ui.pgen.elements.Outlook;
import gov.noaa.nws.ncep.ui.pgen.elements.Product;
import gov.noaa.nws.ncep.ui.pgen.elements.ProductInfo;
import gov.noaa.nws.ncep.ui.pgen.elements.ProductTime;
import gov.noaa.nws.ncep.ui.pgen.elements.Symbol;
import gov.noaa.nws.ncep.ui.pgen.elements.WatchBox;
import gov.noaa.nws.ncep.ui.pgen.elements.labeledlines.Label;
import gov.noaa.nws.ncep.ui.pgen.elements.labeledlines.LabeledLine;
import gov.noaa.nws.ncep.ui.pgen.file.FileTools;
import gov.noaa.nws.ncep.ui.pgen.file.ProductConverter;
import gov.noaa.nws.ncep.ui.pgen.file.Products;
import gov.noaa.nws.ncep.ui.pgen.gfa.Gfa;
import gov.noaa.nws.ncep.ui.pgen.graphtogrid.CoordinateTransform;
import gov.noaa.nws.ncep.ui.pgen.productmanage.ProductConfigureDialog;
import gov.noaa.nws.ncep.ui.pgen.producttypes.ProductType;
import gov.noaa.nws.ncep.ui.pgen.rsc.PgenResource;
import gov.noaa.nws.ncep.ui.pgen.rsc.PgenResourceData;
import gov.noaa.nws.ncep.ui.pgen.sigmet.Sigmet;
import gov.noaa.nws.ncep.ui.pgen.sigmet.Volcano;
import gov.noaa.nws.ncep.ui.pgen.tca.TCAElement;
import gov.noaa.nws.ncep.viz.common.display.INatlCntrsPaneManager;
import gov.noaa.nws.ncep.viz.common.display.NcDisplayName;
import gov.noaa.nws.ncep.viz.common.display.NcDisplayType;

import java.awt.Color;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.text.DecimalFormat;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.TimeZone;

import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;
import javax.xml.transform.stream.StreamSource;

import org.eclipse.core.commands.Command;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.VerifyEvent;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.commands.ICommandService;
import org.geotools.geometry.jts.JTS;
import org.opengis.referencing.operation.MathTransform;

import com.raytheon.uf.common.geospatial.MapUtil;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.IDisplayPane;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.drawables.IRenderableDisplay;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.localization.LocalizationManager;
import com.raytheon.uf.viz.core.map.IMapDescriptor;
import com.raytheon.uf.viz.core.map.MapDescriptor;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.ResourceList;
import com.raytheon.viz.ui.EditorUtil;
import com.raytheon.viz.ui.editor.AbstractEditor;
import com.raytheon.viz.ui.editor.EditorInput;
import com.raytheon.viz.ui.editor.ISelectedPanesChangedListener;
import com.raytheon.viz.ui.panes.PaneManager;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.CoordinateList;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.LineString;
import com.vividsolutions.jts.geom.MultiPolygon;
import com.vividsolutions.jts.geom.Polygon;
import com.vividsolutions.jts.linearref.LinearLocation;
import com.vividsolutions.jts.linearref.LocationIndexedLine;

/**
 * Utilities for PGEN
 * 
 * <pre>
 * 
 *    SOFTWARE HISTORY
 *   
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 
 * 04/22/09       #99       Greg Hull     Moved some methods to NmapUiUtils
 * 05/15/09       #116      B. Yin        Added validateLatLonTextField
 * 05/15/09       #116      B. Yin        Added text/symbol drawing mode methods 
 * 08/03/09       #116      B. Yin        changed validateLatLonTextField 
 *                                          to  validateNumberTextField
 * 09/30/09       #169      Greg Hull     NCMapEditor
 * 10/15/09       #160      G. Zhang      INTL Sigmet LatLonPre/Post pend
 * 12/10/09       #167      J. Wu         Added contours drawing method
 * 05/04/10       #267      B.  Yin       Added a method to load 'SetCont' tool for outlook
 * 03/10          #223      M.Laryukhin   Added Gfa
 * 04/10          #165      G.Zhang       Modified LatLonPre/Post pend formatting
 * 07/10          #215      J. Wu         Added calendarToGempakDattim()
 * 09/10          #63       J. Wu         Added CURRENT_WORKING_DIRCTORY
 * 09/10          #304      B. Yin        Added a method to load a tool editing LabeledLine
 * 10/10          #310      S. Gilbert    Added PgenMode
 * 02/11          #405      J. Wu         Set CURRENT_WORKING_DIRCTORY to user-defined directory
 *                                          in the PGEN preference page
 * 03/11                    J. Wu         Added getSphPolyArea() && getPolyArea()
 * 05/11                    J. Wu         Added methods to setup/covert between lat/lon
 *                                          and a custom coordinate.
 * 08/11                    J. Wu         Added getPgenOprDirectory()
 * 03/12         #611       S. Gurung     Added computePoint()
 * 03/12         #704       B. Yin        Move applyStylesheet() here from ProdType
 * 05/12         #708       J. Wu         Add methods to retrieve current data frame time.
 * 05/12         #769       B. Yin        Moved the creation of UTC time from TCA dialog to here.
 * 03/13         #972       G. Hull       add isNatlCntrsEditor()
 * 03/13         #927       B. Yin        Moved isUnmovable from the PgenSelectTool class.
 * 06/13         #1000      J. Wu         Added utility function writePgenFile().
 * 07/26         TTR        J. Wu         Extract "DEL_PART" in DeletePartCommand into deleteLinePart().
 * 12/13         #1089      B. Yin        Removed the UTC time functions to a new class.
 * 12/13         #1091      J. Wu         Added getLayerMergeOption()
 * 
 * </pre>
 * 
 * @author
 * @version 1
 */
public class PgenUtil {

    /**
     * SINGLE mode is used to display and interact with the same PGEN data on
     * every Editor, while MULTIPLE mode allows users to have a separate PGEN
     * instance in each Editor.
     * 
     * @author sgilbert
     * 
     */
    public static enum PgenMode {
        SINGLE, MULTIPLE
    };

    private static PgenResourceData rscData = null;

    /*
     * A computational coordinate system allowing conversion between lat/lon and
     * a customized 800x600 grid coordinate via PgenPreference (projection,
     * garea).
     */
    private static CoordinateTransform coordTrans = null;

    /*
     * Pgen Palette view ID is also defined in plugin.xml.
     */
    public static final String VIEW_ID = "gov.noaa.nws.ncep.ui.PGEN";

    /*
     * Statute miles to meters
     */
    public static final float SM2M = 1609.34f;

    /*
     * Nautical miles to meters
     */
    public static final float NM2M = 1852.0f;

    /*
     * Prefix and postfix for PGEN temporary recovery files.
     */
    public static final String RECOVERY_PREFIX = "pgen_session.";

    public static final String RECOVERY_POSTFIX = ".tmp";

    public static final String PGEN_PROD_DIR = "prod";

    public static final String PGEN_XML_DIR = "xml";

    public static final String PGEN_TEXT_PROD_DIR = "text";

    /*
     * Format string patterns for Lat/Lon Pre/Postpend
     */
    public static final String FOUR_ZERO = "0000";

    public static final String FIVE_ZERO = "00000";

    // Default CAVE window title string
    // public static String caveTitle = "CAVE";
    public static String caveTitle = null;

    /**
     * Check the given editor for a PgenResource. If editor is null then the
     * current Nmap Editor is used. If found, return it.
     * 
     * @param editor
     * @return reference to a PgenResource
     */
    public static final PgenResource findPgenResource(AbstractEditor editor) {
        // return (PgenResource)NmapUiUtils.findResource( PgenResource.class,
        // editor );
        return (PgenResource) findResource(PgenResource.class, editor);
    }

    /**
     * Check the given display pane for a PgenResource. If found, return it.
     * 
     * @param pane
     * @return reference to a PgenResource
     */
    public static final PgenResource findPgenResourceInPane(IDisplayPane pane) {

        if (pane == null)
            return null;

        ResourceList rscList = pane.getDescriptor().getResourceList();

        for (ResourcePair rp : rscList) {
            AbstractVizResource<?, ?> rsc = rp.getResource();

            if (rsc.getClass() == PgenResource.class) {
                return (PgenResource) rsc;
            }
        }

        return null;
    }

    /**
     * set current ModalTool to Selecting mode
     */
    public static final void setSelectingMode() {

        setCommandMode("gov.noaa.nws.ncep.ui.pgen.rsc.PgenSelect");

    }

    /**
     * set current ModalTool to "MultiSelect" mode
     */
    public static final void setMultiSelectMode() {

        setCommandMode("gov.noaa.nws.ncep.ui.pgen.rsc.PgenMultiSelect");

    }

    /**
     * set current ModalTool to "DelectObj" mode
     */
    public static final void setDelObjMode() {
        setCommandMode("gov.noaa.nws.ncep.ui.pgen.rsc.PgenDeleteObj");
    }

    private static final void setCommandMode(String command) {
        IEditorPart part = EditorUtil.getActiveEditor();

        if (part == null)
            return;

        ICommandService service = (ICommandService) part.getSite().getService(
                ICommandService.class);
        Command cmd = service.getCommand(command);

        if (cmd != null) {

            try {
                HashMap<String, Object> params = new HashMap<String, Object>();
                params.put("editor", part);
                ExecutionEvent exec = new ExecutionEvent(cmd, params, null,
                        null);

                cmd.executeWithChecks(exec);

            } catch (Exception e) {

                e.printStackTrace();
            }
        }
    }

    /**
     * Set the drawing mode to symbol. When drawing a symbol with a text, this
     * method is called after the text is finished to return to the
     * symbol-drawing mode.
     * 
     * @param symbolType
     */
    public static final void setDrawingSymbolMode(String symbolCat,
            String symbolType, boolean usePrevColor,
            AbstractDrawableComponent adc) {
        IEditorPart part = EditorUtil.getActiveEditor();
        ICommandService service = (ICommandService) part.getSite().getService(
                ICommandService.class);
        Command cmd = service
                .getCommand("gov.noaa.nws.ncep.ui.pgen.rsc.PgenSingleDraw");

        if (cmd != null) {

            try {
                HashMap<String, Object> params = new HashMap<String, Object>();
                params.put("editor", part);
                params.put("name", symbolType);
                params.put("className", symbolCat);
                params.put("usePrevColor", new Boolean(usePrevColor).toString());

                ExecutionEvent exec = new ExecutionEvent(cmd, params, adc, null);

                cmd.executeWithChecks(exec);

            } catch (Exception e) {

                e.printStackTrace();
            }
        }
    }

    /**
     * Set the drawing mode to general text.
     * 
     * @param addLabel
     *            - whether to add the text to a symbol
     * @param symbolType
     *            - used to return to the symbol-drawing mode
     * @param useSymbolColor
     *            - whether to use symbol's color
     */
    public static final void setDrawingTextMode(boolean addLabel,
            boolean usePrevColor, String defaultTxt,
            AbstractDrawableComponent adc) {
        IEditorPart part = EditorUtil.getActiveEditor();
        ICommandService service = (ICommandService) part.getSite().getService(
                ICommandService.class);
        Command cmd = service
                .getCommand("gov.noaa.nws.ncep.ui.pgen.rsc.PgenTextDraw");

        if (cmd != null) {

            try {
                HashMap<String, Object> params = new HashMap<String, Object>();
                params.put("editor", part);
                params.put("name", "General Text");
                params.put("className", "Text");
                params.put("addLabel", new Boolean(addLabel).toString());
                params.put("usePrevColor", new Boolean(usePrevColor).toString());
                params.put("defaultTxt", defaultTxt);

                ExecutionEvent exec = new ExecutionEvent(cmd, params, adc, null);

                cmd.executeWithChecks(exec);

            } catch (Exception e) {

                e.printStackTrace();
            }
        }
    }

    /**
     * Set the drawing mode to drawing the Gfa text.
     * 
     * @param lastUsedGfa
     */
    public static final void setDrawingGfaTextMode(Gfa lastUsedGfa) {
        IEditorPart part = EditorUtil.getActiveEditor();
        ICommandService service = (ICommandService) part.getSite().getService(
                ICommandService.class);
        Command cmd = service
                .getCommand("gov.noaa.nws.ncep.ui.pgen.rsc.PgenGfaDraw");

        if (cmd != null) {

            try {
                HashMap<String, Object> params = new HashMap<String, Object>();
                params.put("editor", part);
                params.put("name", "GFA");
                params.put("className", "MET");
                StringBuilder sb = new StringBuilder("");

                for (String s : lastUsedGfa.getString()) {
                    sb.append(s).append(",,"); // ,, delimited
                }
                if (lastUsedGfa.getString() != null
                        && lastUsedGfa.getString().length > 0) {
                    sb.setLength(sb.length() - 2);
                }
                params.put("startGfaText", "true");
                params.put("lastUsedGfa", lastUsedGfa);

                ExecutionEvent exec = new ExecutionEvent(cmd, params, null,
                        null);

                cmd.executeWithChecks(exec);

            } catch (Exception e) {

                e.printStackTrace();
            }
        }
    }

    /**
     * Set the drawing mode to watch status line.
     */
    public static final void setDrawingStatusLineMode(WatchBox wb) {
        IEditorPart part = EditorUtil.getActiveEditor();
        ICommandService service = (ICommandService) part.getSite().getService(
                ICommandService.class);
        Command cmd = service
                .getCommand("gov.noaa.nws.ncep.ui.pgen.rsc.PgenWatchStatusLineDraw");

        if (cmd != null) {

            try {
                HashMap<String, Object> params = new HashMap<String, Object>();
                params.put("editor", part);
                params.put("name", "STATUS_LINE");
                params.put("className", "Watch");

                ExecutionEvent exec = new ExecutionEvent(cmd, params, wb, null);

                cmd.executeWithChecks(exec);

            } catch (Exception e) {

                e.printStackTrace();
            }
        }
    }

    /**
     * Set the drawing mode to watch status line.
     */
    public static final void setDrawingFrontMode(Line front) {
        IEditorPart part = EditorUtil.getActiveEditor();
        ICommandService service = (ICommandService) part.getSite().getService(
                ICommandService.class);
        Command cmd = service
                .getCommand("gov.noaa.nws.ncep.ui.pgen.rsc.PgenMultiDraw");

        if (cmd != null) {

            try {
                HashMap<String, Object> params = new HashMap<String, Object>();
                params.put("editor", part);
                params.put("name", front.getPgenType());
                params.put("className", front.getPgenCategory());

                ExecutionEvent exec = new ExecutionEvent(cmd, params, front,
                        null);

                cmd.executeWithChecks(exec);

            } catch (Exception e) {

                e.printStackTrace();
            }
        }
    }

    /**
     * Load the 'Set continue Line' tool for outlooks.
     */
    public static final void loadOutlookSetContTool(Outlook otlk) {
        IEditorPart part = EditorUtil.getActiveEditor();
        ICommandService service = (ICommandService) part.getSite().getService(
                ICommandService.class);
        Command cmd = service
                .getCommand("gov.noaa.nws.ncep.ui.pgen.rsc.PgenOutlookSetCont");

        if (cmd != null) {

            try {
                HashMap<String, Object> params = new HashMap<String, Object>();
                params.put("editor", part);
                params.put("name", "Outlook");
                params.put("className", "");

                ExecutionEvent exec = new ExecutionEvent(cmd, params, otlk,
                        null);

                cmd.executeWithChecks(exec);

            } catch (Exception e) {

                e.printStackTrace();
            }
        }
    }

    /**
     * Load the TCA drawing tool.
     * 
     * @param de
     *            TCAElement to load into the TcaAttrDlg when the tool is loaded
     */
    public static final void loadTCATool(DrawableElement de) {
        IEditorPart part = EditorUtil.getActiveEditor();
        ICommandService service = (ICommandService) part.getSite().getService(
                ICommandService.class);
        Command cmd = service.getCommand("gov.noaa.nws.ncep.ui.pgen.tca");

        if (cmd != null && de instanceof TCAElement) {

            try {
                HashMap<String, Object> params = new HashMap<String, Object>();
                params.put("editor", part);
                params.put("name", de.getPgenType());
                params.put("className", de.getPgenCategory());

                ExecutionEvent exec = new ExecutionEvent(cmd, params, de, null);

                cmd.executeWithChecks(exec);

            } catch (Exception e) {

                e.printStackTrace();
            }
        }
    }

    /**
     * Load the watch box modifying tool.
     * 
     * @param de
     *            WatchBox to load when the tool is loaded
     */
    public static final void loadWatchBoxModifyTool(DrawableElement de) {
        IEditorPart part = EditorUtil.getActiveEditor();
        ICommandService service = (ICommandService) part.getSite().getService(
                ICommandService.class);
        Command cmd = service
                .getCommand("gov.noaa.nws.ncep.ui.pgen.rsc.PgenWatchBoxModify");

        if (cmd != null && de instanceof WatchBox) {

            try {
                HashMap<String, Object> params = new HashMap<String, Object>();
                params.put("editor", part);
                params.put("name", de.getPgenType());
                params.put("className", de.getPgenCategory());

                ExecutionEvent exec = new ExecutionEvent(cmd, params, de, null);

                cmd.executeWithChecks(exec);

            } catch (Exception e) {

                e.printStackTrace();
            }
        }
    }

    /**
     * Load the LabeledLine modifying tool.
     * 
     * @param ll
     *            - LabeledLine to edit when the tool is loaded
     */
    public static final void loadLabeledLineModifyTool(LabeledLine ll) {
        IEditorPart part = EditorUtil.getActiveEditor();
        ICommandService service = (ICommandService) part.getSite().getService(
                ICommandService.class);
        Command cmd = service
                .getCommand("gov.noaa.nws.ncep.ui.pgen.rsc.PgenLabeledLineModify");

        if (cmd != null) {

            try {
                HashMap<String, Object> params = new HashMap<String, Object>();
                params.put("editor", part);
                params.put("name", ll.getPgenType());
                params.put("className", ll.getPgenCategory());

                if ("CCFP_SIGMET".equals(ll.getPgenType())) {
                    params.put("type",
                            ((gov.noaa.nws.ncep.ui.pgen.sigmet.Ccfp) ll)
                                    .getSigmet().getType());
                }

                ExecutionEvent exec = new ExecutionEvent(cmd, params, ll, null);

                cmd.executeWithChecks(exec);

            } catch (Exception e) {

                e.printStackTrace();
            }
        }
    }

    /**
     * Load the LabeledLine modifying tool.
     * 
     * @param ll
     *            - LabeledLine to edit when the tool is loaded
     */
    public static final void loadTcmTool(DrawableElement elem) {
        IEditorPart part = EditorUtil.getActiveEditor();
        ICommandService service = (ICommandService) part.getSite().getService(
                ICommandService.class);
        Command cmd = service
                .getCommand("gov.noaa.nws.ncep.ui.pgen.rsc.PgenTCMtool");

        if (cmd != null) {

            try {
                HashMap<String, Object> params = new HashMap<String, Object>();
                params.put("editor", part);
                params.put("name", elem.getPgenType());
                params.put("className", elem.getPgenCategory());

                ExecutionEvent exec = new ExecutionEvent(cmd, params, elem,
                        null);

                cmd.executeWithChecks(exec);

            } catch (Exception e) {

                e.printStackTrace();
            }
        }
    }

    /**
     * Load the outlook drawing tool.
     */
    public static final void loadOutlookDrawingTool() {
        IEditorPart part = EditorUtil.getActiveEditor();
        ICommandService service = (ICommandService) part.getSite().getService(
                ICommandService.class);
        Command cmd = service
                .getCommand("gov.noaa.nws.ncep.ui.pgen.outlookDraw");

        if (cmd != null) {

            try {
                HashMap<String, Object> params = new HashMap<String, Object>();
                params.put("editor", part);
                params.put("name", "Outlook");
                params.put("className", "");

                ExecutionEvent exec = new ExecutionEvent(cmd, params, null,
                        null);

                cmd.executeWithChecks(exec);

            } catch (Exception e) {

                e.printStackTrace();
            }
        }
    }

    /**
     * Create a new PgenResource and add it to the current editor.
     * 
     * @return the PgenResource
     */
    public static final PgenResource createNewResource() {

        PgenResource drawingLayer = null;
        // NCMapEditor editor = NmapUiUtils.getActiveNatlCntrsEditor();
        AbstractEditor editor = getActiveEditor();
        if (editor != null) {
            try {
                switch (getPgenMode()) {
                case SINGLE:
                    /*
                     * Use existing (or new) PgenResourceData to construct new
                     * Resources to add to each Pane's ResourceList
                     */
                    if (rscData == null) {
                        rscData = new PgenResourceData();
                    }
                    for (IDisplayPane pane : editor.getDisplayPanes()) {
                        IDescriptor idesc = pane.getDescriptor();
                        if (idesc.getResourceList().size() > 0) {
                            drawingLayer = rscData.construct(
                                    new LoadProperties(), idesc);
                            // System.out.println("NEW pgen resource: "+drawingLayer);
                            idesc.getResourceList().add(drawingLayer);
                            idesc.getResourceList().addPreRemoveListener(
                                    drawingLayer);
                            drawingLayer.init(pane.getTarget());
                        }
                    }
                    break;
                case MULTIPLE:
                    /*
                     * Add a new PgenResourceData and Resource to active Pane's
                     * ResourceList
                     */
                    IMapDescriptor desc = (IMapDescriptor) editor
                            .getActiveDisplayPane().getRenderableDisplay()
                            .getDescriptor();
                    drawingLayer = new PgenResourceData().construct(
                            new LoadProperties(), desc);
                    desc.getResourceList().add(drawingLayer);
                    desc.getResourceList().addPreRemoveListener(drawingLayer);
                    drawingLayer
                            .init(editor.getActiveDisplayPane().getTarget());
                    break;
                }

            } catch (Exception e) {
                e.printStackTrace();
            }
        }
        return drawingLayer;
    }

    /*
     * Refresh the PGEN drawing editor.
     */
    public static final void refresh() {
        // if( NmapUiUtils.getActiveNatlCntrsEditor() != null )
        // NmapUiUtils.getActiveNatlCntrsEditor().refresh();
        if (getActiveEditor() != null)
            getActiveEditor().refresh();
    }

    /**
     * This method checks if the content in a text filed is a valid double. It
     * should be called in a verify-listener of the text field.
     * 
     * @param event
     *            - Event that activates the listener.
     * @return - true if the text is a valid double.
     */
    public static final boolean validateNumberTextField(Event event) {

        boolean valid = false;

        /*
         * Only validate text field
         */
        if (event.widget instanceof Text) {

            Text latLonText = (Text) event.widget;
            StringBuffer str = new StringBuffer(latLonText.getText());

            // if the event is from key press, insert the text,
            // otherwise, replace the text
            if (event.keyCode != 0) {
                str.insert(event.start, event.text);
            }

            try {
                Double.valueOf(new String(str));
                valid = true;
            } catch (NumberFormatException e) {
                valid = false;
            }

            /*
             * The first character can be '-' or '.'
             */
            if (!valid
                    && (((event.start == 0) && (event.character == '-' || event.character == '.')) || ((str
                            .length() == 1) && (str.charAt(0) == '-' || str
                            .charAt(0) == '.')))) {
                valid = true;
            } else if (valid
                    && (event.character == 'd' || event.character == 'D'
                            || event.character == 'f' || event.character == 'F'
                            || event.character == 'e' || event.character == 'E')) {
                /*
                 * 'D/d', 'F/f', and 'E/e' can be in or at the end of a Java
                 * number, but we do not want them in lat/lon
                 */
                valid = false;
            }
        }

        return valid;

    }

    public static boolean validatePositiveInteger(VerifyEvent ve) {

        boolean stat = false;

        if (ve.widget instanceof Text) {
            Text advnum = (Text) ve.widget;
            StringBuffer str = new StringBuffer(advnum.getText());
            str.replace(ve.start, ve.end, ve.text);

            if (str.toString().isEmpty())
                return true;

            try {
                if (Integer.parseInt(str.toString()) > 0)
                    return true;
            } catch (NumberFormatException nfe) {
                return false;
            }

            return false;
        }

        return stat;
    }

    /**
     * Converts an array of lat/lons to pixel coordinates
     * 
     * @param pts
     *            An array of points in lat/lon coordinates
     * @param mapDescriptor
     *            Descriptoer to use for world to pixel transform
     * @return The array of points in pixel coordinates
     */
    public static final double[][] latlonToPixel(Coordinate[] pts,
            IMapDescriptor mapDescriptor) {
        double[] point = new double[3];
        double[][] pixels = new double[pts.length][3];

        for (int i = 0; i < pts.length; i++) {
            point[0] = pts[i].x;
            point[1] = pts[i].y;
            point[2] = 0.0;
            pixels[i] = mapDescriptor.worldToPixel(point);
        }

        return pixels;
    }

    /**
     * Gets the current site id from the localization Manager
     * 
     * @return
     */
    public static String getCurrentOffice() {

        String wfo = LocalizationManager.getInstance().getCurrentSite();
        if (wfo.equalsIgnoreCase("none") || wfo.isEmpty())
            wfo = new String("KNHC");

        return wfo;
    }

    /**
     * Converts an array of lat/lons to text; the format is "N","S","E", and/or
     * "W" prepended
     * 
     * @param coors
     *            An array of points in lat/lon coordinates
     * @return The String showing lat/lons in text string
     */
    public static final String getLatLonStringPrepend(Coordinate[] coors,
            boolean isLineTypeArea) {
        String twoSpace = gov.noaa.nws.ncep.ui.pgen.sigmet.SigmetInfo.LINE_SEPERATER;
        StringBuilder result = new StringBuilder();
        for (Coordinate coor : coors) {

            result.append(coor.y >= 0 ? 'N' : 'S');
            int y = (int) Math.abs(coor.y * 100);
            result.append(new DecimalFormat(FOUR_ZERO).format(y));

            result.append(coor.x >= 0 ? 'E' : 'W');
            int x = (int) Math.abs(coor.x * 100);
            result.append(new DecimalFormat(FIVE_ZERO).format(x));

            result.append(twoSpace);
        }
        if (isLineTypeArea)
            result.append(result.toString().split(twoSpace)[0]);
        return result.toString();
    }

    /**
     * Converts an array of lat/lons to text; the format is "N","S","E", and/or
     * "W" postpended
     * 
     * @param coors
     *            An array of points in lat/lon coordinates
     * @return The String showing lat/lons in text string
     */
    public static final String getLatLonStringPostpend(Coordinate[] coors,
            boolean isLineTypeArea) {
        String twoSpace = gov.noaa.nws.ncep.ui.pgen.sigmet.SigmetInfo.LINE_SEPERATER;
        StringBuilder result = new StringBuilder();
        for (Coordinate coor : coors) {

            int y = (int) Math.abs(coor.y * 100);
            result.append(new DecimalFormat(FOUR_ZERO).format(y));
            result.append(coor.y >= 0 ? 'N' : 'S');

            int x = (int) Math.abs(coor.x * 100);
            result.append(new DecimalFormat(FIVE_ZERO).format(x));
            result.append(coor.x >= 0 ? 'E' : 'W');

            result.append(twoSpace);
        }
        if (isLineTypeArea)
            result.append(result.toString().split(twoSpace)[0]);
        return result.toString();
    }

    /**
     * Load the Contours drawing tool.
     * 
     * @param de
     *            Contours to load into the ContoursAttrDlg when the tool is
     *            loaded
     */
    public static final void loadContoursTool(Contours de) {
        IEditorPart part = EditorUtil.getActiveEditor();
        ICommandService service = (ICommandService) part.getSite().getService(
                ICommandService.class);
        Command cmd = service.getCommand("gov.noaa.nws.ncep.ui.pgen.contours");

        if (cmd != null) {

            try {
                HashMap<String, Object> params = new HashMap<String, Object>();
                params.put("editor", part);

                if (de != null) {
                    params.put("name", de.getPgenType());
                    params.put("className", de.getPgenCategory());
                } else {
                    params.put("name", "Contours");
                    params.put("className", "MET");
                }

                ExecutionEvent exec = new ExecutionEvent(cmd, params, de, null);

                cmd.executeWithChecks(exec);

            } catch (Exception e) {

                e.printStackTrace();
            }
        }
    }

    /**
     * Trigger the selecting tool with a selected element.
     */
    public static final void setSelectingMode(AbstractDrawableComponent de) {
        IEditorPart part = EditorUtil.getActiveEditor();
        ICommandService service = (ICommandService) part.getSite().getService(
                ICommandService.class);
        Command cmd = service
                .getCommand("gov.noaa.nws.ncep.ui.pgen.rsc.PgenSelect");

        if (cmd != null) {

            try {
                HashMap<String, Object> params = new HashMap<String, Object>();
                params.put("editor", part);
                ExecutionEvent exec = new ExecutionEvent(cmd, params, de, null);

                cmd.executeWithChecks(exec);

            } catch (Exception e) {

                e.printStackTrace();
            }
        }
    }

    /**
     * Returns the name of the directory that holds PGEN recovery files
     * 
     * @return
     */
    public static String getTempWorkDir() {
        IPreferenceStore prefs = Activator.getDefault().getPreferenceStore();
        return prefs.getString(PgenPreferences.P_RECOVERY_DIR);
    }

    /**
     * 
     * @return
     */
    public static PgenMode getPgenMode() {
        IPreferenceStore prefs = Activator.getDefault().getPreferenceStore();
        String name = prefs.getString(PgenPreferences.P_PGEN_MODE);
        PgenMode mode = PgenMode.valueOf(name);
        return mode;
    }

    /**
     * Indicates whether PGEN layers should be linked with the editor
     */
    public static boolean doesLayerLink() {
        IPreferenceStore prefs = Activator.getDefault().getPreferenceStore();
        return prefs.getBoolean(PgenPreferences.P_LAYER_LINK);
    }

    /*
     * Checks to see if a file exists. If so, pop up a dialog asking for
     * permission to overwrite the file.
     */
    public static boolean checkFileStatus(String filename) {

        boolean canWrite = false;
        File f = new File(filename);

        if (f.exists()) {
            // display confirmation dialog
            String msg = "File " + filename + " already exists. Overwrite?";
            MessageDialog confirmDlg = new MessageDialog(PlatformUI
                    .getWorkbench().getActiveWorkbenchWindow().getShell(),
                    "Confirm", null, msg, MessageDialog.QUESTION, new String[] {
                            "OK", "Cancel" }, 0);
            confirmDlg.open();

            if (confirmDlg.getReturnCode() == MessageDialog.OK) {
                canWrite = true;
            }
        } else {
            canWrite = true;
        }

        return canWrite;
    }

    /**
     * Convert a java.util.Calendar to a GEMPAK date/time string
     * 
     * @return
     */
    public static String calendarToGempakDattim(Calendar jdattim) {

        String gDattim = null;

        if (jdattim != null) {

            String gstr = "";

            int year = jdattim.get(Calendar.YEAR);
            gstr += year - year / 100 * 100;

            int month = jdattim.get(Calendar.MONTH) + 1;
            if (month < 10) {
                gstr += "0";
            }
            gstr += month;

            int day = jdattim.get(Calendar.DAY_OF_MONTH);
            if (day < 10) {
                gstr += "0";
            }
            gstr += day;

            gstr += "/";

            int hour = jdattim.get(Calendar.HOUR_OF_DAY);
            if (hour < 10) {
                gstr += "0";
            }
            gstr += hour;

            int minute = jdattim.get(Calendar.MINUTE);
            if (minute < 10) {
                gstr += "0";
            }
            gstr += minute;

            gDattim = new String(gstr);
        }

        return gDattim;

    }

    /**
     * The current working directory may be changed in eclipse.sh or cave.sh, so
     * it may not be the directory where you are running them. To get around, We
     * save and export it as "CURRENT_WORKING_DIRECTORY".
     * 
     * Set to a user-defined directory in PGEN preference.
     */
    // public static String CURRENT_WORKING_DIRECTORY = System.getenv(
    // "CURRENT_WORKING_DIRECTORY" );
    public static String CURRENT_WORKING_DIRECTORY = System
            .getProperty("user.home");

    public static String getWorkingDirectory() {
        return Activator.getDefault().getPreferenceStore()
                .getString(PgenPreferences.P_WORKING_DIR);
    }

    /**
     * This function merges two labels of the input LabeledLine(ll) that are at
     * the same location(loc) into one label(one text with several arrowed
     * lines)/
     * 
     * @param ll
     *            - LabeledLine
     * @param loc
     *            - Location
     * @param me
     *            - map editor
     */
    // static public void mergeLabels(LabeledLine ll, Coordinate loc,
    // NCMapEditor mapEditor ){
    static public LabeledLine mergeLabels(LabeledLine ll, Label testLbl,
            Coordinate loc, AbstractEditor mapEditor, PgenResource rsc) {

        // label at location loc.
        // Label testLbl = null;

        // label close to testLbl
        Label mergeLbl = null;

        // translate lat/lon to screen coordinate
        double scnLoc[] = mapEditor.translateInverseClick(loc);

        // search for the closest two labels at loc
        Layer activeLayer = rsc.getActiveLayer();
        Iterator<AbstractDrawableComponent> it = activeLayer
                .getComponentIterator();
        LabeledLine nearestLine = null;
        while (it.hasNext()) {
            AbstractDrawableComponent adc = it.next();
            if (adc instanceof LabeledLine
                    && ((LabeledLine) adc).getPgenType().equalsIgnoreCase(
                            ll.getPgenType())) {
                LabeledLine lline = (LabeledLine) adc;
                for (Label lbl : lline.getLabels()) {
                    if (Math.abs(lbl.getSpe().getLocation().x - loc.x) < 0.0001
                            && Math.abs(lbl.getSpe().getLocation().y - loc.y) < 0.0001) {

                        // get the label at loc
                        // testLbl = lbl;
                    } else {

                        // calculate distance from lbl to scnLoc
                        double scnPt[] = mapEditor.translateInverseClick(lbl
                                .getSpe().getLocation());
                        double dist = Math.sqrt((scnLoc[0] - scnPt[0])
                                * (scnLoc[0] - scnPt[0])
                                + (scnLoc[1] - scnPt[1])
                                * (scnLoc[1] - scnPt[1]));

                        if (dist < 20) { // 20 is the screen distance.
                            // a label in this range(<20) is considered as being
                            // at the same location
                            mergeLbl = lbl;
                            nearestLine = (LabeledLine) adc;
                            break;
                        }
                    }
                }
            }
            if (mergeLbl != null)
                break;
        }

        // add all arrow lines of one label to the other label and remove the
        // second label
        if (testLbl != null && mergeLbl != null) {
            for (Line ln : testLbl.getArrows()) {
                mergeLbl.addArrow(ln);
                ln.removePoint(0);
                ln.addPoint(0, mergeLbl.getSpe().getLocation());
            }
            // ll.add(mergeLbl);
            ll.remove(testLbl);
        }

        if (nearestLine != null) {
            Iterator<AbstractDrawableComponent> iterator = nearestLine
                    .getComponentIterator();
            while (iterator.hasNext()) {
                ll.add(iterator.next());
            }
        }

        return nearestLine;
    }

    /**
     * Removes the current PgenResourceData object
     */
    public static void resetResourceData() {
        rscData = null;
    }

    /**
     * Retrieve the last used tool public static AbstractModalTool
     * getLastUsedTool() {
     * 
     * AbstractModalTool lastTool = null;
     * 
     * AbstractVizPerspectiveManager mgr = VizPerspectiveListener
     * .getCurrentPerspectiveManager(); if ( mgr != null) { lastTool =
     * mgr.getToolManager().getLastModalTool(); }
     * 
     * return lastTool; }
     */

    /**
     * This function computes the area of a spherical polygon on the earth
     * 
     * Note: The ANSI C code is adapted and modified from the article
     * "Computing the Area of a Spherical Polygon" by Robert D. Miller, in
     * "Graphics Gems IV", Academic Press, 1994.
     * 
     * @param pts
     *            [] polygon points in map coordinates
     * @return area area in unit of square nautical miles.
     */
    public static double getSphPolyArea(Coordinate ptsin[]) {

        final double HalfPi = 1.5707963267948966192313, Degree = 57.295779513082320876798, // degrees
                                                                                           // per
                                                                                           // radian
        M2NM = 5.4e-4F, // meter to nautical mile
        RADIUS = 6371200.0F, // earth radius
        GDIFFD = 0.000001;

        int jj, kk;
        double Lam1, Lam2, Beta1, Beta2, CosB1, CosB2, HavA;
        double T, A, B, C, S, sum, excess;

        /*---------------------------------------------------------------------*/

        int npts = ptsin.length;
        double[] tmplat = new double[npts];
        double[] tmplon = new double[npts];

        /*
         * Convert from degrees to radians and save into local arrays
         */
        for (jj = 0; jj < npts; jj++) {
            tmplat[jj] = ptsin[jj].y / Degree;
            tmplon[jj] = ptsin[jj].x / Degree;
        }

        /*
         * Calculate the area in spherical degrees.
         */
        sum = 0;
        Lam2 = 0.0;
        Beta2 = 0.0;
        CosB2 = 0.0;
        for (jj = 0; jj < npts; jj++) {
            kk = jj + 1;
            if (jj == 0) {
                Lam1 = (double) tmplon[jj];
                Beta1 = (double) tmplat[jj];
                Lam2 = (double) tmplon[jj + 1];
                Beta2 = (double) tmplat[jj + 1];
                CosB1 = Math.cos(Beta1);
                CosB2 = Math.cos(Beta2);
            } else {
                kk = (jj + 1) % (npts);
                Lam1 = Lam2;
                Beta1 = Beta2;
                Lam2 = (double) tmplon[kk];
                Beta2 = (double) tmplat[kk];
                CosB1 = CosB2;
                CosB2 = Math.cos(Beta2);
            }

            if (!(Math.abs(Lam1 - Lam2) < GDIFFD)) {

                double a = (1.0 - Math.cos(Beta2 - Beta1)) / 2.0;
                double b = (1.0 - Math.cos(Lam2 - Lam1)) / 2.0;
                HavA = a + CosB1 * CosB2 * b;

                A = 2 * Math.asin(Math.sqrt(HavA));
                B = HalfPi - Beta2;
                C = HalfPi - Beta1;
                S = 0.5 * (A + B + C);
                T = Math.tan(S / 2) * Math.tan((S - A) / 2)
                        * Math.tan((S - B) / 2) * Math.tan((S - C) / 2);

                excess = Math.abs(4 * Math.atan(Math.sqrt(Math.abs(T))))
                        * Degree;
                if (Lam2 < Lam1)
                    excess = -excess;

                sum = sum + excess;
            }
        }

        double area = Math.abs(sum);

        /*
         * Convert into "square nautical miles".
         */
        double radius = RADIUS * M2NM; /* The Earth's radius in nautical miles */

        area *= ((radius) * (radius) / Degree);

        return area;
    }

    /**
     * This function computes the area of polygon on the earth.
     * 
     * @param poly
     *            polygon in map coordinates
     * @return area area in unit of square nautical miles.
     */
    public static double getSphPolyArea(Polygon poly) {
        double extnl_area = getSphPolyArea(poly.getExteriorRing()
                .getCoordinates());

        double intnl_area = 0.0;
        for (int nn = 0; nn < poly.getNumInteriorRing(); nn++) {
            intnl_area += getSphPolyArea(poly.getInteriorRingN(nn)
                    .getCoordinates());
            System.out.println("internal??");
        }

        return (extnl_area - intnl_area);
    }

    /**
     * This function computes the area of a geometry on the earth.
     * 
     * @param geom
     *            geometry in map coordinate.
     * @return area area in unit of square nautical miles.
     */
    public static double getSphPolyArea(Geometry geom) {
        double area = 0.0;
        if (geom instanceof Polygon) {
            area = getSphPolyArea((Polygon) geom);
        } else if (geom instanceof MultiPolygon) {
            MultiPolygon mp = (MultiPolygon) geom;
            for (int nn = 0; nn < mp.getNumGeometries(); nn++) {
                area += getSphPolyArea((Polygon) mp.getGeometryN(nn));
            }
        }

        return area;
    }

    /**
     * This function computes the area of polygon in grid coordinate.
     * 
     * Note: the input is assumed to be in grid coordinate.
     * 
     * @param poly
     *            polygon in grid coordinates
     * @return area area in unit of square nautical miles.
     */
    public static double getSphPolyAreaInGrid(Polygon poly) {

        Coordinate[] ring = poly.getExteriorRing().getCoordinates();
        Coordinate[] ringInMap = gridToLatlon(ring);
        double extnl_area = getSphPolyArea(ringInMap);

        double intnl_area = 0.0;
        for (int nn = 0; nn < poly.getNumInteriorRing(); nn++) {
            ring = poly.getInteriorRingN(nn).getCoordinates();
            ringInMap = gridToLatlon(ring);
            intnl_area += getSphPolyArea(ring);
        }

        return (extnl_area - intnl_area);
    }

    /**
     * This function computes the area of a geometry on the earth.
     * 
     * @param geom
     *            geometry in GRID coordinate.
     * @return area area in unit of square nautical miles.
     */
    public static double getSphPolyAreaInGrid(Geometry geom) {
        double area = 0.0;
        if (geom instanceof Polygon) {
            area = getSphPolyAreaInGrid((Polygon) geom);
        } else if (geom instanceof MultiPolygon) {
            MultiPolygon mp = (MultiPolygon) geom;
            for (int nn = 0; nn < mp.getNumGeometries(); nn++) {
                area += getSphPolyAreaInGrid((Polygon) mp.getGeometryN(nn));
            }
        }

        return area;
    }

    /**
     * This function computes the area of polygon on the earth but requires to
     * build a local projection and math transform Note: it may throw exception
     * for invalid polygon.
     * 
     * @param poly
     *            polygon in map coordinates
     * @return area area in unit of square nautical miles.
     */
    public static double getPolyArea(Polygon poly) {
        double area = 0.0;
        try {
            org.opengis.referencing.crs.ProjectedCRS localProjectionCRS = MapUtil
                    .constructStereographic(MapUtil.AWIPS_EARTH_RADIUS,
                            MapUtil.AWIPS_EARTH_RADIUS, poly.getCentroid()
                                    .getY(), poly.getCentroid().getX());
            MathTransform mt;
            mt = MapUtil.getTransformFromLatLon(localProjectionCRS);
            Geometry newIntersectInLocalProj = JTS.transform(poly, mt);
            double areaInMeters = newIntersectInLocalProj.getArea();

            area = areaInMeters / PgenUtil.NM2M / PgenUtil.NM2M;

        } catch (Exception e) {
            e.printStackTrace();
        }

        return area;
    }

    /**
     * Returns projection for PGEN computational coordinate
     * 
     * @return
     */
    public static String getPgenCompCoordProj() {
        IPreferenceStore prefs = Activator.getDefault().getPreferenceStore();
        String coordStr = prefs.getString(PgenPreferences.P_COMP_COORD);
        String[] s = coordStr.split("\\|");
        return s[0];
    }

    /**
     * Returns GRAEA for PGEN computational coordinate
     * 
     * @return
     */
    public static String getPgenCompCoordGarea() {
        IPreferenceStore prefs = Activator.getDefault().getPreferenceStore();
        String coordStr = prefs.getString(PgenPreferences.P_COMP_COORD);
        String[] s = coordStr.split("\\|");
        return s[1];
    }

    /**
     * Returns a custom CoordinateTransform for use.
     * 
     * @return
     */
    public static CoordinateTransform getCompCoord() {

        if (coordTrans == null
                || !coordTrans.getProjection().equals(getPgenCompCoordProj())
                || !coordTrans.getGarea().equals(getPgenCompCoordGarea())) {

            coordTrans = new CoordinateTransform(getPgenCompCoordProj(),
                    getPgenCompCoordGarea(), 800, 600);
        }

        return coordTrans;

    }

    /**
     * Convert a set of lat/lon points into a custom grid coordinate.
     * 
     * @param lonlat
     *            points to be converted
     * @return
     */
    public static Coordinate[] latlonToGrid(Coordinate[] lonlat) {

        return getCompCoord().worldToGrid(lonlat);
    }

    /**
     * Convert a set of points in a custom grid coordinate to lat/lon points.
     * 
     * @param lonlat
     *            points to be converted
     * @return
     */
    public static Coordinate[] gridToLatlon(Coordinate[] gridpts) {

        return getCompCoord().gridToWorld(gridpts);
    }

    /**
     * Convert a list of lat/lon points into a custom grid coordinate.
     * 
     * @param lonlat
     *            points to be converted
     * @return
     */
    public static ArrayList<Coordinate> latlonToGrid(
            ArrayList<Coordinate> lonlat) {

        Coordinate[] aa = new Coordinate[lonlat.size()];

        aa = PgenUtil.latlonToGrid(lonlat.toArray(aa));

        return new ArrayList<Coordinate>(Arrays.asList(aa));
    }

    /**
     * Convert a list of points in a custom grid coordinate to lat/lon points.
     * 
     * @param lonlat
     *            points to be converted
     * @return
     */
    public static ArrayList<Coordinate> gridToLatlon(
            ArrayList<Coordinate> gridpts) {

        Coordinate[] aa = new Coordinate[gridpts.size()];

        aa = PgenUtil.gridToLatlon(gridpts.toArray(aa));

        return new ArrayList<Coordinate>(Arrays.asList(aa));

    }

    /**
     * Returns the base directory for storing/access PGEN operation product
     * file.
     * 
     * @return
     */
    public static String getPgenOprDirectory() {
        return Activator.getDefault().getPreferenceStore()
                .getString(PgenPreferences.P_OPR_DIR);
    }

    /**
     * Returns the file path of the current PGEN activity
     * 
     * @return
     */
    public static String getPgenActivityPath() {
        String pdName = PgenSession.getInstance().getPgenResource()
                .getActiveProduct().getType();
        ProductType pt = ProductConfigureDialog.getProductTypes().get(pdName);
        if (pt != null)
            pdName = pt.getType();

        // String pd1 = pdName.replaceAll(" ", "_");

        return PgenUtil.getPgenOprDirectory() + File.separator + pdName;
    }

    /**
     * Returns the file path of the current PGEN activity products
     * 
     * @return
     */
    public static String getPgenActivityProdPath() {

        return PgenUtil.getPgenActivityPath() + File.separator + PGEN_PROD_DIR;
    }

    /**
     * Returns the file path of the current PGEN activity products
     * 
     * @return
     */
    public static String getPgenActivityTextProdPath() {

        return PgenUtil.getPgenActivityProdPath() + File.separator
                + PGEN_TEXT_PROD_DIR;
    }

    /**
     * Returns the file path of the current PGEN activity xml files
     * 
     * @return
     */
    public static String getPgenActivityXmlPath() {

        return PgenUtil.getPgenActivityPath() + File.separator + PGEN_XML_DIR;
    }

    /**
     * Format a Calendar date into a string of "DDMMYYYY"
     */
    public static String formatDate(Calendar cal) {

        StringBuilder dstr = new StringBuilder();

        int day = cal.get(Calendar.DAY_OF_MONTH);
        if (day < 10) {
            dstr.append("0" + day);
        } else {
            dstr.append(day);
        }

        int mon = cal.get(Calendar.MONTH) + 1;
        if (mon < 10) {
            dstr.append("0" + mon);
        } else {
            dstr.append(mon);
        }

        dstr.append(cal.get(Calendar.YEAR));

        return dstr.toString();
    }

    /**
     * Parse the environmental variables to get a full path file name. // *
     * Default path will be the user's PGEN_OPR directory.
     * 
     * @param filename
     * @return a full path file name or unchanged if no "/" in the input file
     *         name
     */
    public static String parsePgenFileName(String fileName) {

        String parsedFile = fileName;

        if (fileName != null) {
            if (fileName.contains(File.separator)) {

                String[] items = fileName.split(File.separator);
                StringBuilder stb = new StringBuilder();
                if (fileName.startsWith(File.separator)) {
                    stb.append(File.separator);
                }

                for (String str : items) {
                    if (str.equals(".")) {
                        stb.append(System.getProperty("user.home"));
                    } else if (str.startsWith("$")
                            && System.getenv(str.substring(1)) != null) {
                        stb.append(System.getenv(str.substring(1)));
                    } else {
                        stb.append(str);
                    }

                    stb.append(File.separator);
                }

                parsedFile = stb.toString();

                if (!fileName.endsWith(File.separator)) {
                    parsedFile = parsedFile.substring(0,
                            parsedFile.length() - 1);
                }
            }
            // else {
            // parsedFile = new String( PgenUtil.getPgenOprDirectory() +
            // File.separator + fileName );
            // }
        }

        return parsedFile;
    }

    /**
     * This function computes the resulting coordinate (latitude/longitude
     * point) based on the coordinate (latitude/longitude point) passed, a
     * distance and a direction.
     * 
     * @param coor
     *            coordinate of the point (lat/lon)
     * @param dist
     *            distance (nautical mile)
     * @param dir
     *            direction (degrees from N)
     * @return newCoor coordinate of the new point created
     */
    public static Coordinate computePoint(Coordinate coor, float dist, float dir) {
        final double PI = 3.14159265, HALFPI = PI / 2.0, TWOPI = 2.0 * PI, DTR = PI / 180.0, // Degrees
                                                                                             // to
                                                                                             // Radians
        RTD = 180.0 / PI, // Radians to Degrees
        RADIUS = 6371200.0F, // Earth radius
        NM2M = 1852.0F;

        /*
         * Convert the input values to radians.
         */

        double direction = (double) dir * DTR;
        double lat = (double) coor.y * DTR;
        double lon = (double) coor.x * DTR;
        double distance = (double) dist * NM2M / RADIUS;

        double dLat = Math.asin(Math.sin(lat) * Math.cos(distance)
                + Math.cos(lat) * Math.sin(distance) * Math.cos(direction));
        double dLon, dLt, dLn;

        /*
         * Make sure the longitude is between -180 and +180 degrees.
         */
        lon = lon - (double) ((double) ((int) (lon / TWOPI)) * TWOPI);
        if (lon < -PI)
            lon = lon + TWOPI;
        if (lon > PI)
            lon = lon - TWOPI;

        /*
         * Compute the delta longitude. If the initial latitude is either pole,
         * then use the original longitude, otherwise, compute the new
         * longitude.
         */
        if ((Math.abs(lat - 90.0F) < 0.000001)
                || (Math.abs(-lat - 90.0F) < 0.000001)) {
            dLon = lon;
        } else {
            dLon = Math.atan2(
                    Math.sin(direction) * Math.sin(distance) * Math.cos(lat),
                    Math.cos(distance) - Math.sin(lat) * Math.sin(dLat));
            dLon = (lon + dLon + PI % TWOPI) - PI;
        }

        /*
         * Make sure that latitude is between -90 and +90 degrees. Adjust the
         * longitude, if necessary.
         */
        dLt = dLat - (double) ((double) ((int) (dLat / PI)) * PI);

        if (dLt > HALFPI) {
            dLt = PI - dLt;
            dLon = -dLon;
        }
        if (dLt < -HALFPI) {
            dLt = -PI - dLt;
            dLon = -dLon;
        }

        /*
         * Make sure the longitude is between -180 and +180 degrees.
         */
        dLn = dLon - (double) ((double) ((int) (dLon / TWOPI)) * TWOPI);
        if (dLn < -PI)
            dLn = dLn + TWOPI;
        if (dLn > PI)
            dLn = dLn - TWOPI;

        // Convert the new position to degrees and create coordinate based on
        // new lat/lon
        Coordinate newCoor = new Coordinate((float) (dLn * RTD),
                (float) (dLt * RTD));
        return newCoor;
    }

    /**
     * Apply the style sheet on the DOMSource and generate text
     * 
     * @param dSource
     * @param xsltName
     * @return
     */
    static public String applyStyleSheet(DOMSource dSource, String xsltName) {

        String ret = "";
        if (xsltName != null && !xsltName.isEmpty()) {
            File xslt = new File(xsltName);
            if (xslt.canRead()) {

                ByteArrayOutputStream baos = new ByteArrayOutputStream();

                try {
                    TransformerFactory tf = TransformerFactory.newInstance();
                    StreamSource myStylesheetSrc = new StreamSource(xslt);

                    Transformer t = tf.newTransformer(myStylesheetSrc);

                    t.transform(dSource, new StreamResult(baos));

                    ret = new String(baos.toByteArray());
                } catch (Exception e) {
                    System.out.println(e.getMessage());
                }
            }
        }

        return ret;
    }

    /**
     * Based on apache's WordUtils to wrap the long lines of OBS/Fcst info
     * 
     * @param String
     *            str1: the line to be wrapped
     * @param int wrapLength: the line's desired length
     * @param String
     *            newLineStr:the new line string
     * @param boolean wrapLongWords: if to wrapped long words
     * @return String: the wrapped String
     */

    public static String wrap(String str1, int wrapLength, String newLineStr,
            boolean wrapLongWords) {
        if (str1 == null) {
            return "";
        }
        if (newLineStr == null) {
            newLineStr = System.getProperty("line.separator");

        }
        if (wrapLength < 1) {
            wrapLength = 1;
        }
        int inputLineLength1 = str1.length();
        int offset = 0;
        StringBuffer wrappedLine = new StringBuffer(inputLineLength1 + 32);

        String[] lines = str1.split(newLineStr);

        for (int i = 0; i < lines.length; i++) {
            offset = 0;
            int inputLineLength = lines[i].length();

            if (lines[i].length() < wrapLength) {

                wrappedLine.append(lines[i]).append(newLineStr);

            } else {
                String str = lines[i];
                while ((inputLineLength - offset) > wrapLength) {
                    if (str.charAt(offset) == ' ') {
                        offset++;
                        continue;
                    }
                    int spaceToWrapAt = str.lastIndexOf(' ', wrapLength
                            + offset);

                    if (spaceToWrapAt >= offset) {
                        // normal case
                        wrappedLine
                                .append(str.substring(offset, spaceToWrapAt));
                        wrappedLine.append(newLineStr);
                        offset = spaceToWrapAt + 1;

                    } else {
                        // really long word or URL
                        if (wrapLongWords) {
                            // wrap really long word one line at a time
                            wrappedLine.append(str.substring(offset, wrapLength
                                    + offset));
                            wrappedLine.append(newLineStr);
                            offset += wrapLength;
                        } else {
                            // do not wrap really long word, just extend beyond
                            // limit
                            spaceToWrapAt = str.indexOf(' ', wrapLength
                                    + offset);
                            if (spaceToWrapAt >= 0) {
                                wrappedLine.append(str.substring(offset,
                                        spaceToWrapAt));
                                wrappedLine.append(newLineStr);
                                offset = spaceToWrapAt + 1;
                            } else {
                                wrappedLine.append(str.substring(offset));
                                offset = inputLineLength;
                            }
                        }
                    }
                }

                // Whatever is left in line is short enough to just pass through
                wrappedLine.append(str.substring(offset)).append(newLineStr);
            }
        }

        return wrappedLine.toString();
    }

    /**
     * Get a reference to the current editor, if it is a AbstractEditor.
     * 
     * Copy from NmapUiUtils.getActiveNatlCntrsEditor() - to remove dependency
     * on ui.display.
     */
    public static final AbstractEditor getActiveEditor() {
        // bsteffen change to EditorUtils
        // if (VizApp.getCurrentEditor() instanceof NCMapEditor) {
        // return (NCMapEditor) VizApp.getCurrentEditor();
        if (EditorUtil.getActiveEditor() instanceof AbstractEditor) {
            return (AbstractEditor) EditorUtil.getActiveEditor();
        } else {
            return null;
        }
    }

    /*
     * Check if a workbench part is NCMapEditor
     */
    public static boolean isNatlCntrsEditor(IWorkbenchPart part) {
        if (part instanceof AbstractEditor) {

            IEditorInput edInput = ((AbstractEditor) part).getEditorInput();

            if (edInput instanceof EditorInput) {

                PaneManager pmngr = ((EditorInput) edInput).getPaneManager();

                if (pmngr instanceof INatlCntrsPaneManager) {

                    NcDisplayType dispType = ((INatlCntrsPaneManager) pmngr)
                            .getDisplayType();

                    // if other display types are supported then add them here.
                    //
                    if (dispType.equals(NcDisplayType.NMAP_DISPLAY)) {
                        return true;
                    }
                }
            }
        }
        return false;
    }

    // TODO: Do we need to look in all the panes or just the active (or the
    // selected panes)
    //
    public static final AbstractVizResource findResource(
            Class<? extends AbstractVizResource> rscClass, AbstractEditor aEdit) {
        AbstractEditor editor = (aEdit != null ? aEdit : getActiveEditor());
        if (editor == null)
            return null;

        IRenderableDisplay disp = editor.getActiveDisplayPane()
                .getRenderableDisplay();

        if (disp == null)
            return null;

        ResourceList rscList = disp.getDescriptor().getResourceList();

        for (ResourcePair rp : rscList) {
            AbstractVizResource rsc = rp.getResource();

            if (rsc.getClass() == rscClass) {
                return rsc;
            }
        }

        return null;
    }

    /**
     * Append title to the current CAVE title - save the original title for
     * restoring when exiting PGEN.
     */
    public static final void setCaveTitle(String title) {

        if (PlatformUI.getWorkbench().getActiveWorkbenchWindow() != null
                && PlatformUI.getWorkbench().getActiveWorkbenchWindow()
                        .getShell() != null) {
            // Save the existing title to reset.
            if (caveTitle == null) {
                caveTitle = PlatformUI.getWorkbench()
                        .getActiveWorkbenchWindow().getShell().getText();
            }

            PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell()
                    .setText(caveTitle + "  (" + title + ")");
        }

    }

    /**
     * Reset the title of the CAVE to "CAVE".
     */
    public static final void resetCaveTitle() {
        if (caveTitle != null) {
            PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell()
                    .setText(caveTitle);
        }
    }

    /*
     * Get an NCP editor's name/id.
     * 
     * This must be called with an ncp editor.
     */
    public static NcDisplayName getDisplayName(AbstractEditor ed) {
        if (!isNatlCntrsEditor(ed)) {
            return null;
        }

        return ((INatlCntrsPaneManager) ((EditorInput) ed.getEditorInput())
                .getPaneManager()).getDisplayName();
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.ui.editor.IMultiPaneEditor#getNumberofPanes()
     */
    public static int getNumberofPanes(AbstractEditor editor) {
        return ((EditorInput) editor.getEditorInput()).getPaneManager()
                .getNumberofPanes();
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.ui.editor.IMultiPaneEditor#addSelectedPaneChangedListener
     * (com.raytheon.viz.ui.editor.ISelectedPanesChangedListener)
     */
    public static void addSelectedPaneChangedListener(AbstractEditor editor,
            ISelectedPanesChangedListener listener) {
        ((EditorInput) editor.getEditorInput()).getPaneManager()
                .addSelectedPaneChangedListener(listener);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.ui.editor.IMultiPaneEditor#removeSelectedPaneChangedListener
     * (com.raytheon.viz.ui.editor.ISelectedPanesChangedListener)
     */
    public static void removeSelectedPaneChangedListener(AbstractEditor editor,
            ISelectedPanesChangedListener listener) {
        ((EditorInput) editor.getEditorInput()).getPaneManager()
                .removeSelectedPaneChangedListener(listener);
    }

    /**
     * Retrieve the current data frame time in format of "yyMMdd/HHmm".
     * 
     * Note - this method is modified from
     * FramDataDisply.updateFrameDataDisplay().
     * 
     * @param
     * @return a time string in format of "yyMMdd/HHmm"
     */
    public static String getCurrentFrameTime() {
        return getFrameTime(getCurrentFrameCalendar());
    }

    /**
     * Retrieve the current data frame's Calendar time.
     * 
     * @param
     * @return a Calendar
     */
    public static Calendar getCurrentFrameCalendar() {

        Calendar cal = null;

        if (EditorUtil.getActiveEditor() != null) {

            AbstractEditor absEditor = (AbstractEditor) EditorUtil
                    .getActiveEditor();

            MapDescriptor mapDescriptor = (MapDescriptor) absEditor
                    .getActiveDisplayPane().getRenderableDisplay()
                    .getDescriptor();

            if (mapDescriptor != null) {

                int currentFrame = mapDescriptor.getFramesInfo()
                        .getFrameIndex();

                DataTime[] frameTimes = mapDescriptor.getFramesInfo()
                        .getFrameTimes();

                if (frameTimes != null && currentFrame >= 0) {
                    cal = frameTimes[currentFrame].getValidTime();
                }
            }
        }

        return cal;

    }

    /**
     * Build a frame time in format of "yyMMdd/HHmm" from a Calendar .
     * 
     * @param cal
     *            a Calendar
     * @return a time string in format of "yyMMdd/HHmm"
     */
    public static String getFrameTime(Calendar cal) {

        String frameTimeString = "";

        if (cal != null) {

            SimpleDateFormat FRAME_DATE_FORMAT = new SimpleDateFormat(
                    "EEE yyMMdd/HHmm");
            FRAME_DATE_FORMAT.setTimeZone(TimeZone.getTimeZone("GMT"));

            String fTimeString = new String(FRAME_DATE_FORMAT.format(cal
                    .getTime()));
            if (fTimeString != null && fTimeString.trim().length() > 0) {
                String[] ftArrays = fTimeString.split(" ");
                if (ftArrays.length > 1) {
                    frameTimeString = ftArrays[1];
                }
            }
        }

        return frameTimeString;

    }

    /**
     * Get a Calendar by incrementing "HH:MM" to a given Calendar. By default,
     * one hour is added to the give Calendar.
     * 
     * @param calIn
     *            Calendar
     * @param interval
     *            a timeString as "HH:MM"
     * @return a Calendar
     */
    public static Calendar getNextCalendar(Calendar calIn, String interval) {

        Calendar nCal = null;

        if (calIn != null) {
            long ftime = calIn.getTimeInMillis();
            long ntime = 60 * 60 * 1000;
            if (interval != null & interval.trim().length() > 0) {
                String[] intTimes = interval.split(":");
                if (intTimes.length > 1) {
                    ntime = Integer.parseInt(intTimes[0]) * 60 * 60 * 1000
                            + Integer.parseInt(intTimes[1]) * 60 * 1000;
                } else {
                    ntime = Integer.parseInt(intTimes[0]) * 60 * 1000;
                }
            }

            nCal = Calendar.getInstance();
            nCal.setTimeInMillis(ftime + ntime);
        }

        return nCal;

    }

    /**
     * Simulate a mouse down event.
     * 
     * @param x
     * @param y
     * @param button
     * @param mapEditor
     */
    public static void simulateMouseDown(int x, int y, int button,
            AbstractEditor mapEditor) {
        Event me = new Event();
        me.display = mapEditor.getActiveDisplayPane().getDisplay();
        me.button = 1;
        me.type = SWT.MouseDown;
        me.x = x;
        me.y = y;
        mapEditor.getMouseManager().handleEvent(me);
    }

    /**
     * telling if the DE is NOT movable.
     * 
     * @param DrawableElement
     *            : the DE to be judged.
     * @return boolean: true not movable.
     */
    public static boolean isUnmovable(DrawableElement tmpEl) {
        if (tmpEl instanceof Volcano)
            return true;

        if (tmpEl instanceof Sigmet) {
            Sigmet vaCloud = (Sigmet) tmpEl;
            String type = vaCloud.getType();

            if (type != null
                    && (type.contains("WINDS") || (type
                            .contains(Sigmet.ISOLATED))))
                return true;
        }

        return false;
    }

    /*
     * Write a set of points into a PGEN file - each point represents the
     * location of a Symbol.
     */
    public static void writePgenFile(String path, String fname, String symName,
            Color symClr, double size, List<Coordinate> pts) {

        // Default product/layer to hold symbols
        Product activeProduct = new Product("Default", "Default", "Default",
                new ProductInfo(), new ProductTime(), new ArrayList<Layer>());

        Layer activeLayer = new Layer();
        activeProduct.addLayer(activeLayer);

        List<Product> productList = new ArrayList<Product>();
        productList.add(activeProduct);

        for (Coordinate c : pts) {
            Symbol cmm = new Symbol(null, new Color[] { symClr }, 1.0F, size,
                    false, c, "Symbol", symName);
            activeLayer.add(cmm);
        }

        Products filePrds1 = ProductConverter.convert(productList);
        String aa = path + "/" + fname;
        FileTools.write(aa, filePrds1);
    }

    /**
     * Delete part of a line.
     * 
     * @param List
     *            <Coordinate>: line to be partially deleted.
     * @param boolean: flag to indicate if a line is closed or open.
     * @param Coordinate
     *            : first clicked point on line.
     * @param Coordinate
     *            : second clicked point on line.
     * @return List<ArrayList<Coordinate>> a list of new lines.
     */
    public static List<ArrayList<Coordinate>> deleteLinePart(
            List<Coordinate> linePoints, boolean closed, Coordinate point1,
            Coordinate point2) {

        ArrayList<ArrayList<Coordinate>> listOfNewLines = new ArrayList<ArrayList<Coordinate>>();

        Coordinate firstPt;
        Coordinate secondPt;
        LocationIndexedLine lil;
        LinearLocation firstLoc, secondLoc;

        GeometryFactory gf = new GeometryFactory();

        /*
         * For each given point, find the location of its closest point on the
         * line. Save order of points along line.
         */
        Coordinate lps[] = new Coordinate[linePoints.size()];
        linePoints.toArray(lps);
        CoordinateList clist = new CoordinateList(lps);
        if (closed)
            clist.closeRing();
        LineString ls = gf.createLineString(clist.toCoordinateArray());
        lil = new LocationIndexedLine(ls);
        LinearLocation loc1 = lil.project(point1);
        LinearLocation loc2 = lil.project(point2);
        if (loc1.compareTo(loc2) <= 0) {
            firstLoc = loc1;
            secondLoc = loc2;
            firstPt = point1;
            secondPt = point2;
        } else {
            firstLoc = loc2;
            secondLoc = loc1;
            firstPt = point2;
            secondPt = point1;
        }

        /*
         * Delete open/closed line differently.
         */
        if (!closed) {
            if (lil.getStartIndex().compareTo(firstLoc) == 0
                    && lil.getEndIndex().getSegmentIndex() == secondLoc
                            .getSegmentIndex()) {
                // Both points selected were endpoints, remove whole element, no
                // new lines.
            } else if (lil.getStartIndex().compareTo(firstLoc) == 0
                    || lil.getEndIndex().getSegmentIndex() == secondLoc
                            .getSegmentIndex()) {

                // One point selected was an endpoint, remove part from one end
                // to get a new line.
                ArrayList<Coordinate> newLine = new ArrayList<Coordinate>();
                if (lil.getStartIndex().compareTo(firstLoc) == 0) {
                    newLine.add(secondPt);
                    newLine.addAll(linePoints.subList(
                            secondLoc.getSegmentIndex() + 1, linePoints.size()));
                } else if (lil.getEndIndex().getSegmentIndex() == secondLoc
                        .getSegmentIndex()) {
                    newLine.addAll(linePoints.subList(0,
                            firstLoc.getSegmentIndex() + 1));
                    newLine.add(firstPt);
                }

                if (newLine.size() >= 2)
                    listOfNewLines.add(newLine);
            } else {
                // remove part in the middle of line, create two new lines.
                ArrayList<Coordinate> newLine1 = new ArrayList<Coordinate>(
                        linePoints.subList(0, firstLoc.getSegmentIndex() + 1));
                newLine1.add(firstPt);

                ArrayList<Coordinate> newLine2 = new ArrayList<Coordinate>();
                newLine2.add(secondPt);
                newLine2.addAll(linePoints.subList(
                        secondLoc.getSegmentIndex() + 1, linePoints.size()));

                if (newLine1.size() >= 2)
                    listOfNewLines.add(newLine1);
                if (newLine2.size() >= 2)
                    listOfNewLines.add(newLine2);
            }
        } else { // closed line

            ArrayList<Coordinate> newLine = new ArrayList<Coordinate>();

            int pointsBetween = secondLoc.getSegmentIndex()
                    - firstLoc.getSegmentIndex();

            if (pointsBetween > (linePoints.size() - pointsBetween)) {
                // if there are more points between pt1 and pt2, remove the
                // other part.
                newLine.add(firstPt);
                newLine.addAll(linePoints.subList(
                        firstLoc.getSegmentIndex() + 1,
                        secondLoc.getSegmentIndex() + 1));
                newLine.add(secondPt);
            } else {
                newLine.add(secondPt);
                newLine.addAll(linePoints.subList(
                        secondLoc.getSegmentIndex() + 1, linePoints.size()));
                newLine.addAll(linePoints.subList(0,
                        firstLoc.getSegmentIndex() + 1));
                newLine.add(firstPt);
            }

            listOfNewLines.add(newLine);
        }

        return listOfNewLines;
    }

    /**
     * Returns text auto placement flag
     * 
     * @return
     */
    public static boolean getTextAutoPlacement() {
        IPreferenceStore prefs = Activator.getDefault().getPreferenceStore();
        return prefs.getBoolean(PgenPreferences.P_AUTOPLACE_TEXT);
    }

    /**
     * Returns the default PGEN layer merge option.
     * 
     * @return
     */
    public static int getLayerMergeOption() {
        IPreferenceStore prefs = Activator.getDefault().getPreferenceStore();
        return prefs.getInt(PgenPreferences.P_LAYER_MERGE);
    }

}
