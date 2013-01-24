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

package com.raytheon.viz.texteditor.dialogs;

import static com.raytheon.viz.texteditor.TextWarningConstants.BEGIN_ELEMENT_TAG;
import static com.raytheon.viz.texteditor.TextWarningConstants.END_ELEMENT_TAG;

import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStream;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.List;
import java.util.Scanner;
import java.util.TimeZone;
import java.util.Timer;
import java.util.TimerTask;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.xml.bind.JAXB;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.MenuManager;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.ExtendedModifyEvent;
import org.eclipse.swt.custom.ExtendedModifyListener;
import org.eclipse.swt.custom.ST;
import org.eclipse.swt.custom.StyleRange;
import org.eclipse.swt.custom.StyledText;
import org.eclipse.swt.custom.VerifyKeyListener;
import org.eclipse.swt.dnd.Clipboard;
import org.eclipse.swt.dnd.TextTransfer;
import org.eclipse.swt.dnd.Transfer;
import org.eclipse.swt.events.DisposeEvent;
import org.eclipse.swt.events.DisposeListener;
import org.eclipse.swt.events.FocusAdapter;
import org.eclipse.swt.events.FocusEvent;
import org.eclipse.swt.events.KeyAdapter;
import org.eclipse.swt.events.KeyEvent;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.MouseListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.events.ShellAdapter;
import org.eclipse.swt.events.ShellEvent;
import org.eclipse.swt.events.VerifyEvent;
import org.eclipse.swt.events.VerifyListener;
import org.eclipse.swt.graphics.Cursor;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.FontData;
import org.eclipse.swt.graphics.FontMetrics;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.layout.RowData;
import org.eclipse.swt.layout.RowLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.FileDialog;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.menus.IMenuService;

import com.raytheon.edex.textdb.dbapi.impl.AFOSParser;
import com.raytheon.uf.common.activetable.SendPracticeProductRequest;
import com.raytheon.uf.common.dataplugin.text.RemoteRetrievalResponse;
import com.raytheon.uf.common.dataplugin.text.alarms.AlarmAlertProduct;
import com.raytheon.uf.common.dataplugin.text.db.StdTextProduct;
import com.raytheon.uf.common.dataplugin.text.db.StdTextProductId;
import com.raytheon.uf.common.dataplugin.text.request.RemoteRetrievalRequest;
import com.raytheon.uf.common.dataplugin.text.request.StdTextProductServerRequest;
import com.raytheon.uf.common.dataplugin.text.request.TextProductInfoCreateRequest;
import com.raytheon.uf.common.dissemination.OUPRequest;
import com.raytheon.uf.common.dissemination.OUPResponse;
import com.raytheon.uf.common.dissemination.OfficialUserProduct;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.serialization.SerializationUtil;
import com.raytheon.uf.common.serialization.comm.IServerRequest;
import com.raytheon.uf.common.site.SiteMap;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.SimulatedTime;
import com.raytheon.uf.edex.decodertools.time.TimeTools;
import com.raytheon.uf.edex.services.textdbsrv.IQueryTransport;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.notification.INotificationObserver;
import com.raytheon.uf.viz.core.notification.NotificationException;
import com.raytheon.uf.viz.core.notification.NotificationMessage;
import com.raytheon.uf.viz.core.notification.jobs.NotificationManagerJob;
import com.raytheon.uf.viz.core.requests.ThriftClient;
import com.raytheon.uf.viz.core.status.StatusConstants;
import com.raytheon.uf.viz.spellchecker.dialogs.SpellCheckDlg;
import com.raytheon.uf.viz.ui.menus.DiscoverMenuContributions;
import com.raytheon.viz.core.mode.CAVEMode;
import com.raytheon.viz.texteditor.Activator;
import com.raytheon.viz.texteditor.AfosBrowserModel;
import com.raytheon.viz.texteditor.TextDisplayModel;
import com.raytheon.viz.texteditor.TextWarningConstants;
import com.raytheon.viz.texteditor.command.AFOSCommand;
import com.raytheon.viz.texteditor.command.CommandFactory;
import com.raytheon.viz.texteditor.command.CommandHistory;
import com.raytheon.viz.texteditor.command.CommandType;
import com.raytheon.viz.texteditor.command.ICommand;
import com.raytheon.viz.texteditor.command.IProductQueryCallback;
import com.raytheon.viz.texteditor.command.ProductQueryJob;
import com.raytheon.viz.texteditor.fax.dialogs.FaxMessageDlg;
import com.raytheon.viz.texteditor.fax.dialogs.LdadFaxSitesDlg;
import com.raytheon.viz.texteditor.msgs.IAfosBrowserCallback;
import com.raytheon.viz.texteditor.msgs.IRecoverEditSessionCallback;
import com.raytheon.viz.texteditor.msgs.IScriptRunnerObserver;
import com.raytheon.viz.texteditor.msgs.ITextCharWrapCallback;
import com.raytheon.viz.texteditor.msgs.ITextEditorCallback;
import com.raytheon.viz.texteditor.msgs.IWmoBrowserCallback;
import com.raytheon.viz.texteditor.notify.NotifyExpiration;
import com.raytheon.viz.texteditor.print.PrintDisplay;
import com.raytheon.viz.texteditor.qc.QualityControl;
import com.raytheon.viz.texteditor.scripting.dialogs.IScriptEditor;
import com.raytheon.viz.texteditor.scripting.dialogs.IScriptEditorObserver;
import com.raytheon.viz.texteditor.scripting.dialogs.ScriptEditorDialog;
import com.raytheon.viz.texteditor.scripting.dialogs.ScriptOutputDlg;
import com.raytheon.viz.texteditor.scripting.runner.TextWsScriptRunner;
import com.raytheon.viz.texteditor.util.SiteAbbreviationUtil;
import com.raytheon.viz.texteditor.util.TextEditorUtil;
import com.raytheon.viz.texteditor.util.VtecObject;
import com.raytheon.viz.texteditor.util.VtecUtil;
import com.raytheon.viz.ui.dialogs.CaveJFACEDialog;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;
import com.raytheon.viz.ui.dialogs.ICloseCallback;
import com.raytheon.viz.ui.dialogs.SWTMessageBox;

/**
 * Main Text Editor dialog.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------  --------------------------
 * 9/13/07      368         lvenable    Initial creation.
 * 9/28/07      368         grichard    Added script building.
 * 10/10/2007   459         grichard    Added `token' for display model.
 * 10/11/2007   482         grichard    Implemented build 9 features.
 * 10/25/2007   501         grichard    Implemented build 10 features.
 * 12/07/2007   582         grichard    Implemented build 12 features.
 * 12/17/2007   639         grichard    Added &quot;fxa&quot; parm to scripts.
 * 1/3/2008     637         grichard    Implemented build 13 features.
 * 1/7/2008     722         grichard    Implemented build 14 features.
 * 1/8/2007     681         grichard    Resize text window for large font.
 * 1/10/2008    722         grichard    Implemented localization.
 * 1/10/2008    722         grichard    Saved text product info.
 * 1/11/2008    749         grichard    Guarded auto-save / window close interaction.
 * 12/2/2008    1588        grichard    Made determineSelectionRange a static method.
 * 12/2/2008    1588        grichard    Highlight specific syntax errors in TAF editor.
 * 12/9/2008    1735        grichard    Format bulleted sections in warnings upon saving. 
 * 12/10/2008   1735        grichard    Implement lockable text for warngen products.
 * 12/17/2008   1538        jkorman     TextDB plugin/Cave Text Workstation mods.
 * 12/21/2008   1803        grichard    Restored changes from December 2008.
 * 1/5/2009     1688        grichard    Update text model based on database dip.
 * 1/7/2009     1688        grichard    Backout executeAFOSCommand.
 * 3/23/2009    1956        bwoodle     Change SiteNode and WMO ID retrieval for hazard prods.
 * 7/10/2009    2374        MW Fegan    Hooking in script running capabilities.
 * 7/28/2009    2610        rjpeter     Changed error handling to AlertViz.
 * 10/02/2009   2646        rjpeter     Added required field validation.
 * 12/30/2009   3778        mhuang      Added no acknowledgment pop-up notice
 * 16Feb2010    4276        MW Fegan    Allow import/export of files w/o extensions.
 * 23Feb2010    3425        MW Fegan    Correct problems with sending products.
 * 02Mar2010    4765        MW Fegan    Hook in spell checker.
 * 15Mar2010    3317        MW Fegan    Implemented 'Update Obs' functionality 
 * 29Mar2010    3331        MW Fegan    Corrected Cancel Editor functionality to match A-I.
 * 24May2010    2187        cjeanbap    Added StdTextProduct and ThriftClient functionality.
 * 07Jun2010    5004        cjeanbap    Handle threading blocking issue
 * 09Jun2010    4523        cjeanbap    Handle restoring of character Word Wrap.
 * 15Jun2010    0766        cjeanbap    Handle cut/copy/paste in editor window.
 * 22Jun2010    5377        cjeanbap    Added Print Dialog confirmation.
 * 22Jun2010    5378        cjeanbap    Print Selection cleared after printing.
 * 24Jun2010    4001        cjeanbap    buttons on keyboard are non-functioning in the text workstation
 * 29Jun2010    5378        cjeanbap    Replaced JOptionPane with MessageBox.
 * 29Jun2010    4639        cjeanbap    Modified editHeader() place &quot;-&quot; when 
 *                                      wmoId and/or siteid is empty
 * 06Jul2010    4773        cjeanbap    Add Go Ahead dialog when send button push when product comes 
 *                                      from WarnGen.
 * 06Jul2010    4523        cjeanbap    Set Auto Wrap to 69 chars.
 * 09Jul2010    5508        cjeanbap    Return focus to text editor when forcaster
 *                                      must modify the selected region.
 * 30Jul2010    4773        cjeanbap    Added WarnGen Product confirmation dialog.
 * 23Aug2010    2187        cjeanbap    Changed from Dialog to CaveSWTDialog.
 * 09Sep2010    5468        rferrel     determineCurrentWord now works when word at end of the line
 * 08Nov2010    7433        cjeanbap    Remove initially added private variable and method; used 
 *                                      existing inEditMode.
 * 16Nov2010    7433        cjeanbap    Fixed bug of inEditMode flag.
 * 30Nov2010    7433        cjeanbap    Call confirmation dialog if user cancels edit of TextEditor.
 * 17DEC2010    7221        Qinglu Lin  Added code to prevent locked lines or segments from sticking  
 *                          D. Friedman together after being moved.
 * 08Jan2011    7643        L. Venable  Fix the closing and opening of the dialogs.
 * 11Jan2011    7375        cjeanbap    Fix multiple enter dialogs and disposed widget.
 * 20JAN2011    7826        Qinglu Lin  Sync Ins key on keypad with INS/OVR button and overStrikeItem 
 *                                      on Text Editor.
 * 08Feb2011    7433        cjeanbap    Warngen generated Text Editor Windows being overwritten
 *                                      by new warnings.
 * 30Mar2011    8561        jdortiz     Added call to enterEditor() if flag in AFOS command is set.
 * 05Apr2011    8378        jdortiz     Fixed Document private class and printDocument method
 *                                      to print entire document.
 * 05Mar2011	8420		cjeanbap	Added nearest Airport display for Metars and Saos.
 * 07Apr2011	8394		cjeanbap	Added a shell addControlListener to handle the width resize.
 * 13Apr2011	5097		cjeanbap	Added file Attachment functionality.
 * 16May2011    7545        cjeanbap    Added enableVersionMenuItems().
 * 17May2011    8394        cjeanbap    Handle the width resize below the minimum size.
 * 18May2011    8420        cjeanbap    Enhanced nearest Airport functionality to mimick A1.
 * 03Jun2011    9742        cjeanbap    Change awips id length to 5 chars.
 * 05Jun2011    9741        cjeanbap    Enabled Version menu items; removing #7545 changes.
 * 14Jun2011    8394        cjeanbap    Handle the height resize below the minimum size.
 * 19Jul2011   10165        rferrel     rewrap now done after cut/paste.
 * 31Aug2011   10793        rferrel     setCurrentHeaderAndBody now removes DRAFT_PIL header.
 * 15Sep2011   10651        rferrel     Implemented clearUpdateFlags and have updateDispalyedProduct
 *                                      highlighted updated METARs.
 * 15Sep2011   10557        rferrel     Use class PrintDisplay to handle printing.
 * 28Sep2011   10800        rferrel     Fixed airport tooltip to work from Cave and stand alone.
 * 03Oct2011   10998        rferrel     No longer accumulate obs when update and accum checked.
 * 02Nov2011   11450        rferrel     Catch errors when parsing for lock tags, perform popup 
 *                                      for queued WarnGen and fixed autosave.
 * 11Nov2011   11552        rferrel     Product no longer needs to be a RESEND in order to be sent.
 * 14Nov2011   11203        rferrel     Header included when exporting a file while in edit mode.
 * 08Mar2012   14553        mhuang      Add blank line between product header and body.
 * 23Apr2012   14783        rferrel     Allow line wrap at white space or hyphen.
 * 24Apr2012   14548        rferrel     Merging lines for wrap places a space beween words when needed.
 * 27Apr2012   14902        rferrel     No longer have blank line between AWIPS ID and UGC line.
 * 06/19/2012  14975        D.Friedman  Prevent zeroed-out WMO header times.
 * 18JUL2012   14457        rferrel     Add mouse listener to clear site's update obs when clicked on.
 * 25JUL2012   14459        rferrel     Strip WMH headers when getting all METARs.
 * 13AUG2012   14613        M.Gamazaychikov	Ensured the WMO and MND header times are the same.
 * 20AUG2012   15340        D.Friedman  Use callbacks for stop sign dialog.  Prevent NOR in header.
 * 10SEP2012   15334        rferrel     No longer wrap text pasted to an empty text field.
 * 10Sep2012   15103	    M.Gamazaychikov	DR15103 -do not clear AFOS command from the text box 
 * 						when obs are updated and refactored executeCommand
 * 10SEP2012   15401        D.Friedman  Fix QC problem caused by DR 15340.
 * 20SEP2012   1196         rferrel     Refactor dialogs to prevent blocking.
 * 25SEP2012   1196         lvenable    Refactor dialogs to prevent blocking.
 * 26SEP2012   1196         lvenable    Refactor dialogs to prevent blocking.
 * 27SEP2012   1196         rferrel     Changes for non-blocking ScriptOutputDlg.
 * 27SEP2012   15424        S.Naples    Set focus on AFOS command text field after executing retrieval of product.
 * 09Oct2012   14889	    M.Gamazaychikov	Add call to checkAndWrapPreviousLine
 * 26SEP2012   1196         lvenable    Refactor dialogs to prevent blocking.
 * 27SEP2012   1196         rferrel     Changes for non-blocking ScriptOutputDlg.
 * 01OCT2012   1229         rferrel     Change WmoBrowserDlg to non-blocking
 * 10OCT2012   1229         rferrel     Changed AwipsBrowserDlg to non-blocking.
 * 12OCT2012   15418        D.Friedman  Do not store product when sending in operational mode.
 *                                      Do not use changed BBB from OUPResponse.
 * 17OCT2012   1229         rferrel     Changes for non-blocking SWTMessageBox.
 * 05Nov2012   15560        S. Naples   Added check to see if we are in edit mode before capturing keys.
 * 28Nov2012   14842	    M.Gamazaychikov	Re-wrote processPopup method
 * 13Dec2012   1353         rferrel     Change to make edit cancel message not
 *                                       dispaly the red had kill job message.
 * 31Dec2012   15651	    M.Gamazaychikov	Added an argument to re-factored PrintDisplay.print
 * 10JAN2012   15704		M.Gamazaychikov Added setting userKeyPressed to false in verifyText method.
 * 22JAN2013   1496         rferrel     Query for loading products no longer on the UI thread.
 * </pre>
 * 
 * @author lvenable
 */
public class TextEditorDialog extends CaveSWTDialog implements VerifyListener,
        IAfosBrowserCallback, IWmoBrowserCallback, IRecoverEditSessionCallback,
        ITextCharWrapCallback, IScriptRunnerObserver, IScriptEditorObserver,
        INotificationObserver, IProductQueryCallback {

    /**
     * Handler used for messges.
     */
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(TextEditorDialog.class);

    /**
     * List of CCCs that can be handled even if the PIL is on the gfe pil list.
     */
    private static List<String> exceptionCCCs = Arrays.asList("SJU", "MFL",
            "MLB", "BRO", "EPZ", "GUM", "HFO");

    /**
     * List of forbidden GFE pils.
     */
    private static List<String> gfePils = Arrays.asList("WSW", "NPW", "HLS",
            "CFW", "WCN", "FFA", "MWW", "RFW");

    /**
     * Auto wrap start range column..
     */
    private static final int DEFAULT_START = 69;

    /**
     * Auto wrap end range column.
     */
    private static final int DEFAULT_END = 80;

    /**
     * System color to use for background color when an obs is updated.
     */
    private static final int UPDATE_BG = SWT.COLOR_BLACK;

    /**
     * System colro to use for foreground color when an obs is updated.
     */
    private static final int UPDATE_FG = SWT.COLOR_WHITE;

    /**
     * The length of BEGIN_ELEMENT_TAG.
     */
    private static final int BEGIN_ELEMENT_TAG_LEN = BEGIN_ELEMENT_TAG.length();

    /**
     * The length of the END_ELEMENT_TAG.
     */
    private static final int END_ELEMENT_TAG_LEN = END_ELEMENT_TAG.length();

    /**
     * The expression for find the start of an obs.
     */
    private static final String METAR_LINE = "^(METAR |SPECI |\\s+\\S)";

    /**
     * The pattern to use to find the start of an obs.
     */
    private static final Pattern METAR_PATTERN = Pattern.compile(METAR_LINE);

    /**
     * List of pils in that are being displayed.
     */
    private final List<String> displayedPils = new ArrayList<String>();

    /**
     * A dynamic pattern for find the current obs in the display.
     */
    private Pattern obsRegex = null;

    /**
     * Pattern to find start of locations. Must be dynamic to include column
     * wrap.
     */
    private Pattern locationsFirstRegex = null;

    /**
     * Pattern to find body of locatin. Must be dynamic to include column wrap.
     */
    private Pattern locationsBodyRegex = null;

    /**
     * Pattern for wrapping normal lines of text. Must be dynamic to include
     * column wrap
     */
    private Pattern standardWrapRegex = null;

    /**
     * Flag to indicate if user wants wrapping enabled.
     */
    private boolean wordWrapEnabled = true;

    /**
     * Either a WmoBrowserDlg or AwipsBrowserDlg. Use to select from many
     * products.
     */
    private CaveJFACEDialog browser = null;

    /**
     * When auto wrapping the last line that needs to be wrapped.
     */
    private int endWrapLine = -1;

    /**
     * Last line was wrapped backwards
     */
    private static final String PARAGRAPH_DELIMITERS = "*$.-/^#";

    /**
     * String to delimit padded paragraphs.
     */
    private static final String PADDED_PARAGRAPH_DELIMITERS = "*";

    /**
     * Expression for start of an obs.
     */
    private static final String METAR_PARAGRAPH = "(METAR|SPECI)\\b.*";

    // Pattern no long used but keeping it here for just in case not using
    // it breaks being compatialbe with A1.

    // private static final Pattern cPattern = Pattern
    // .compile(String
    // .format("%s|%s",
    // "\\/([OTEX])\\.([A-Z]{3})\\.([A-Za-z0-9]{4})\\.([A-Z]{2})\\.([WAYSFON])\\.(\\d{4})\\.(\\d{6}T\\d{4}Z)-(\\d{6}T\\d{4}Z)\\/",
    // "\\/([A-Za-z0-9]{5})\\.([N0123U])\\.([A-Z]{2})\\.(\\d{6}T\\d{4}Z)\\.(\\d{6}T\\d{4}Z)\\.(\\d{6}T\\d{4}Z)\\.([A-Z]{2})\\/"));

    /**
     * Pattern for finding a date.
     */
    private static final Pattern datePtrn = Pattern
            .compile("((\\d{1,2}\\d{2})(\\s(AM|PM)\\s(\\w{3})\\s\\w{3}\\s\\w{3}\\s{1,}\\d{1,2}\\s\\d{4}))");

    /**
     * Pattern used when mergeing line to determine if white space is needed.
     */
    private static final Pattern noSeparatorPattern = Pattern
            .compile("[^-\\s]");

    /**
     * Pattern to determine if the body of the product is a UGC line.
     */
    private static final Pattern UGC_FIRST_LINE_PATTERN = Pattern
            .compile("^[A-Z][A-Z0-9][CZ]\\d{3}[->].*-\\s*$");

    /**
     * The directory to place saved sessions.
     */
    public static final String SAVED_SESSION_DIR = Activator.PLUGIN_ID
            + "/savedSession/";

    /**
     * Format string use to build the store message sent.
     */
    public static final String STORED_SENT_MSG = "  The \nproduct will %s be stored in the text database\nand %s be sent on the WAN.";

    /**
     * Format of date to use to create auto save file adjusted to the proper
     * time zone..
     */
    private static final SimpleDateFormat AUTOSAVE_DATE_FORMAT = new SimpleDateFormat(
            "yyMMdd_HHmm");
    static {
        AUTOSAVE_DATE_FORMAT.setTimeZone(TimeZone
                .getTimeZone(TimeTools.ZULU_TIMEZONE));
    }

    /*
     * The direction to go to find selection.
     */
    private enum SelectDirection {
        FORWARD, BACKWARD
    };

    /**
     * Number of lines scroll up or down based on select direction.
     */
    private static final int PAGE_SCROLL_LINES = 20;

    /**
     * Use to determine the width of the text field.
     */
    private static final int EDITOR_WIDTH = 80;

    /**
     * Filters for file import/export and attach functions.
     */
    private static final String[] FILTER_NAMES = { "All Files" };

    /**
     * Allowable extensions for import/export and attach functions.
     */
    private static final String[] FILTER_EXTS = { "*" };

    /* the alarm/alert topic */
    private static final String ALARM_ALERT_TOPIC = "edex.alarms.msg";

    /**
     * AFOS browser menu item.
     */
    private MenuItem AFOSBrowserItem;

    /**
     * Print All menu item.
     */
    private MenuItem printAllItem;

    /**
     * Print Selection menu item.
     */
    private MenuItem printSelectionItem;

    /**
     * Fax All products menu item.
     */
    private MenuItem faxAllItem;

    /**
     * Fax selected aread menu item.
     */
    private MenuItem faxSelectionItem;

    /**
     * Configuration Auto fax menu item.
     */
    private MenuItem configAutoFaxItem;

    /**
     * Enter editor mode menu item.
     */
    private MenuItem enterEditorItem;

    /**
     * Save menu item.
     */
    private MenuItem saveItem;

    /**
     * Send and Exit menu item.
     */
    private MenuItem sendExitEditorItem;

    /**
     * Cancel editor item.
     */
    private MenuItem cancelEditorItem;

    /**
     * Import from file editor menu item.
     */
    private MenuItem importFromFileItem;

    /**
     * Export to file menu item.
     */
    private MenuItem exportFromFileItem;

    /**
     * Request from remote site menu item.
     */
    private MenuItem requestFromRemoteSiteItem;

    /**
     * Recover edit session menu item.
     */
    private MenuItem recoverEditSessionItem;

    /**
     * Re-send warning product menu item.
     */
    private MenuItem resendWarningProductnItem;

    /**
     * Close menu item.
     */
    private MenuItem closeItem;

    /**
     * Cut text menu item.
     */
    private MenuItem cutItem;

    /**
     * Copy text menu item.
     */
    private MenuItem copyItem;

    /**
     * Paste text menu item.
     */
    private MenuItem pasteItem;

    /**
     * Fill (remove hard returns) menu item.
     */
    private MenuItem fillItem;

    /**
     * Select sub menu item.
     */
    private MenuItem selectSubMenuItem;

    /**
     * Select to previous word item.
     */
    private MenuItem toPreviousWordItem;

    /**
     * Select to next word menu item.
     */
    private MenuItem toNextWordItem;

    /**
     * Select to beginning of line menu item.
     */
    private MenuItem toBeginningOfLineItem;

    /**
     * Select to end of line menu item.
     */
    private MenuItem toEndOfLineItem;

    /**
     * Select to previous page menu item.
     */
    private MenuItem toPreviousPageItem;

    /**
     * Select to next page item menu item.
     */
    private MenuItem toNextPageItem;

    /**
     * Select to beginning of text menu item.
     */
    private MenuItem toTopOfProductItem;

    /**
     * Select to end of text menu item.
     */
    private MenuItem toEndOfProductItem;

    /**
     * Select all text menu item.
     */
    private MenuItem selectAllItem;

    /**
     * Delete sub-menu.
     */
    private MenuItem deleteSubMenuItem;

    /**
     * Delete a single character menu item.
     */
    private MenuItem deleteCharacterItem;

    /**
     * Delete a word menu item.
     */
    private MenuItem deleteWordItem;

    /**
     * Delete rest of the line menu item.
     */
    private MenuItem deleteLineItem;

    /**
     * Undelete sub-menu.
     */
    private MenuItem undeleteSubMenuItem;

    /**
     * Undelete character menu item.
     */
    private MenuItem undeleteCharacterItem;

    /**
     * Undelete word menu item.
     */
    private MenuItem undeleteWordItem;

    /**
     * Undelete line menu item.
     */
    private MenuItem undeleteLineItem;

    /**
     * Clear the text fields menu item.
     */
    private MenuItem clearWindowItem;

    /**
     * Clear editor text menu item.
     */
    private MenuItem clearSelectionItem;

    /**
     * Edit header menu item.
     */
    private MenuItem editHeaderItem;

    /**
     * Search text menu item.
     */
    private MenuItem searchItem;

    /**
     * Spell checker menu item.
     */
    private MenuItem spellCheckerItem;

    /**
     * Clear update flags menu item.
     */
    private MenuItem clearUpdateFlagsItem;

    /**
     * Tools menu.
     */
    private MenuItem toolsMenuItem;

    /**
     * Autowrap sub-menu.
     */
    private MenuItem autoWrapMenuItem;

    /**
     * Column number to use to wrap characters.
     */
    private int charWrapCol = -1;

    /**
     * User defined column wrap.
     */
    private Integer otherCharWrapCol;

    /**
     * Font size sub-menu.
     */
    private MenuItem fontSizeMenuItem;

    /**
     * Small font menu item.
     */
    private MenuItem smallFontItem;

    /**
     * Medium font menu item.
     */
    private MenuItem mediumFontItem;

    /**
     * Large font menu item.
     */
    private MenuItem largeFontItem;

    /**
     * Overstrike (overwrite) menu item.
     */
    private MenuItem overStrikeItem;

    /**
     * Version menu.
     */
    private MenuItem versionMenuItem;

    /**
     * Previous version menu item.
     */
    private MenuItem versionPreviousItem;

    /**
     * Next version menu item.
     */
    private MenuItem versionNextItem;

    /**
     * Latest version menu item.
     */
    private MenuItem versionLatestItem;

    /**
     * All versions menu item.
     */
    private MenuItem versionAllItem;

    /**
     * Products menu.
     */
    private MenuItem productsMenuItem;

    /**
     * Help on a window item menu item.
     */
    private MenuItem onTextWindowItem;

    /**
     * About menu item.
     */
    private MenuItem aboutItem;

    /**
     * Composite for the script runner controls
     */
    private Composite scriptRunnerComp;

    /**
     * Scripts->Edit menu item
     */
    private MenuItem scriptsEditItem = null;

    /**
     * Script->Run menu item
     */
    private MenuItem scriptsRunItem = null;

    /**
     * Script->Show Output menu item
     */
    private MenuItem scriptsShowOutputItem = null;

    /**
     * Script->Continue menu item
     */
    private MenuItem scriptsContinueItem = null;

    /**
     * the continue button
     */
    private Button scriptContinueBtn = null;

    /**
     * Script->Skip Wait menu item
     */
    private MenuItem scriptsSkipWaitItem = null;

    /**
     * the skip wait button
     */
    private Button scriptSkipWaitBtn = null;

    /**
     * Script->Cancel menu item
     */
    private MenuItem scriptsCancelItem = null;

    /**
     * the cancel script button
     */
    private Button scriptCancelBtn = null;

    /**
     * Button height.
     */
    private final int BUTTON_HEIGHT = 27;

    /**
     * Button width.
     */
    private final int BUTTON_WIDTH = 110;

    /**
     * Small button width.
     */
    private final int EDITOR_BTN_WIDTH_SML = 60;

    /**
     * Medium button width.
     */
    private final int EDITOR_BTN_WIDTH_MED = 68;

    /**
     * Large button width.
     */
    private final int EDITOR_BTN_WIDTH_LRG = 110;

    /**
     * Insert text combo index;
     */
    private final int INSERT_TEXT = 0;

    /**
     * Overwrite text combo index;
     */
    private final int OVERWRITE_TEXT = 1;

    /**
     * AFOS browser button to launch the AFOS browser.
     */
    private Button afosBrowserBtn;

    /**
     * Load history button.
     */
    private Button loadHistoryBtn;

    /**
     * WMO search button.
     */
    private Button wmoSearchBtn;

    /**
     * Enter editor button.
     */
    private Button enterEditorBtn;

    /**
     * Current contents of the text editor.
     */
    private String originalText;

    /**
     * Current date in the form DDHHMM as part of the header block.
     */
    private final String currentDateId = "DDHHMM";

    /**
     * Accumulate text check box.
     */
    private Button accumChkBtn;

    /**
     * Update OBS check box.
     */
    private Button updateObsChkBtn;

    /**
     * Clear button to clear the text fields.
     */
    private Button clearBtn;

    /**
     * AFOS command text field.
     */
    private Text afosCmdTF;

    /**
     * WMO data type and area indicator text field (aka WMO ID).
     */
    private Text wmoTtaaiiTF;

    /**
     * International location indicator text field (aka Site ID).
     */
    private Text ccccTF;

    /**
     * AWIPS ID text field.
     */
    private Text awipsIdTF;

    /**
     * Header text field composite.
     */
    private Composite headerTFComp;

    /**
     * Header text field.
     */
    private Text headerTF;

    /**
     * Text editor composite.
     */
    private Composite textEditorComp;

    /**
     * Styled text editor.
     */
    private StyledText textEditor;

    /**
     * Small font.
     */
    private Font smlFont;

    /**
     * Medium font.
     */
    private Font medFont;

    /**
     * Large font.
     */
    private Font lrgFont;

    /**
     * Composite containing the editor buttons.
     */
    private Composite editorBtnRowComp;

    /**
     * Editor save button.
     */
    private Button editorSaveBtn;

    /**
     * Editor cut button.
     */
    private Button editorCutBtn;

    /**
     * Editor copy button.
     */
    private Button editorCopyBtn;

    /**
     * Editor paste button.
     */
    private Button editorPasteBtn;

    /**
     * Editor fill button.
     */
    private Button editorFillBtn;

    /**
     * Editor edit header button.
     */
    private Button editorEditHeaderBtn;

    /**
     * Editor send button.
     */
    private Button editorSendBtn;

    /**
     * Editor cancel button.
     */
    private Button editorCancelBtn;

    /**
     * Editor attach button.
     */
    private Button editorAttachBtn;

    /**
     * Editor insert/overwrite combo box.
     */
    private Combo editorInsertCmb;

    /**
     * Flag indicating if the editor is in edit mode.
     */
    private boolean inEditMode = false;

    /**
     * Search and replace dialog.
     */
    private SearchReplaceDlg searchReplaceDlg;

    /**
     * Flag to indicate if the document being edited has been modified.
     */
    private boolean dirty = false;

    /**
     * Flag to indicate if the document being edited has been saved.
     */
    private boolean saved = false;

    /**
     * Flag indicating if the editor is in overwrite mode.
     */
    private boolean overwriteMode = false;

    /**
     * flag to indicate it a product request is from the GUI or an updated ob.
     */
    private AtomicInteger updateCount = new AtomicInteger(0);

    /**
     * The expire notification when editing a warn gen product.
     */
    private NotifyExpiration notify;

    /**
     * who to notify once queue product is retrived.
     */
    private NotifyExpiration queuedNotify = null;

    /**
     * Queued produc to retrive.
     */
    private String queuedProduct = null;

    /**
     * The Warngen work product id for draft PILs.
     */
    private String workProductId;

    /**
     * Deleted character stored as a string.
     */
    private String deletedChar;

    /**
     * Deleted word stored as a string.
     */
    private String deletedWord;

    /**
     * Deleted line stored as a string.
     */
    private String deletedLine;

    /**
     * The windows title or what is in the shell's text field.
     */
    private String winTitle;

    /**
     * Indictes the dialog should be really be disposed on close instead of just
     * hidden.
     */
    private boolean disposeOnExit = true;

    /**
     * When true a dialog that is normally just hidden must now be diposed of
     * when closed.
     */
    private boolean disposeDialog = false;

    /**
     * Allows text workstation to update it display based on what this instance
     * of the editor is working on.
     */
    private ITextEditorCallback callbackClient = null;

    /**
     * The AFOS browser associated with this dialog.
     */
    private AfosBrowserDlg afosBrowser;

    /**
     * Flag to indicate the AFOS brower is opened.
     */
    private boolean displayAfosBrowser = false;

    /**
     * Flag set to true when user try close editor whith unsaved changes.
     */
    private boolean cancelDoClose = false;

    /**
     * Where the last modified text event started.
     */
    private int eventStart = -1;

    /**
     * Token that identifies this text editor display instance uniquely.
     */
    private final String token;

    /**
     * Flag that determines if this text editor supports scripting
     */
    private final boolean hasScripting;

    /**
     * Transport usded to send query to server and get results.
     */
    private IQueryTransport queryTransport = null;

    /**
     * Job to handle query for products off the UI thread.
     */
    private ProductQueryJob productQueryJob = new ProductQueryJob(this);

    /**
     * Flag to indicate if the dialog is in wait mode.
     */
    private boolean busy = false;

    /**
     * Determines if marked text is uneditable -- equates to the text coming
     * from warngen
     */
    private boolean markedTextUndeditable = false;

    /**
     * the script output window
     */
    private ScriptOutputDlg scriptOutput = null;

    /**
     * the Script Editor window
     */
    private ScriptEditorDialog scriptEditor = null;

    /**
     * The script runner.
     */
    private TextWsScriptRunner scriptRunner = null;

    /**
     * Error in script flag
     */
    private boolean scriptHasError = false;

    private StringBuffer scriptErrorBfr = null;

    /**
     * the continue flag
     */
    private boolean scriptContinue = true;

    /**
     * the cancel script flag
     */
    private boolean cancelScript = false;

    /**
     * the skip wait flag
     */
    private boolean skipWait = false;

    /**
     * auto save task
     */
    private AutoSaveTask autoSave = null;

    /**
     * the command history
     */
    private CommandHistory commandHistory = null;

    /**
     * handle for pop up menu
     */
    private MenuManager menuMgr = null;

    /**
     * Label updated with current status.
     */
    private Label statusBarLabel = null;

    /**
     * Who gets a warn get when it is sent.
     */
    private String addressee = "ALL";

    /**
     * Name of any attachment file
     */
    private String attachedFilename = null;

    /**
     * Contents of an attachment file.
     */
    private byte[] attachedFile = null;

    /**
     * Edit options for a popup menu.
     */
    private static final String[] popupItems = { "Select All", "Cut", "Copy",
            "Paste" };

    /**
     * Currently active popupItems.
     */
    private static final boolean[] isPopItemDefault = { true, false, true,
            false };

    /**
     * Indictes this instance of dialog if for a warnGen.
     */
    private boolean warnGenFlag = false;

    /**
     * Indicates the instance of the dialog is for text work station.
     */
    private boolean textWorkstationFlag = false;

    /**
     * When true this is a warn gen dialog.
     */
    private boolean isWarnGenDlg = false;

    /**
     * set to true when user key pressed triggers an event. Used to determine if
     * line wrapping needs to be done.
     */
    private boolean userKeyPressed = false;

    /**
     * Flag to indicte if the current PIL is for SAO or METAR.
     */
    private boolean isSaoMetarFlag = false;

    /**
     * String to indicate there is an attachment file.
     */
    private static final String ATTACHMENT_STR = "\nAttachment File:\n";

    /**
     * The last remote retrieval request. Use to populate the remote retrieval
     * request dialog.
     */
    private RemoteRetrievalRequest lastRemoteRetrievalRequest;

    /**
     * Allows text to be copy/paste to system.
     */
    private Clipboard clipboard;

    /**
     * Listner to update the status of obs.
     */
    private MouseListener updateObsListener = null;

    /** Text character wrap dialog */
    private TextCharWrapDlg textCharWrapDlg;

    /** LDAD fax sites dialog */
    private LdadFaxSitesDlg ldadFaxSitesDlg;

    /** Fax all message dialog */
    private FaxMessageDlg faxAllMsgDlg;

    /** Fax message dialog */
    private FaxMessageDlg faxMsgDlg;

    /*
     * enum to detemine if editor session can be closed.
     */
    private enum HeaderEditSession {
        CLOSE_ON_EXIT, IN_EDITOR
    }

    /**
     * Current allowable close state for header editor session.
     */
    private HeaderEditSession headerEditSession;

    /**
     * Indicates if previous line was wrapped. Use to determine if more wrapping
     * needs to be done.
     */
    private boolean isPreviousLineWrapped;

    /**
     * Constructor with additional cave style rules
     * 
     * @param parent
     *            parent shell
     * @param additionalCaveStyle
     *            additional cave dialog attributes to apply
     */
    public TextEditorDialog(Shell parent, int additionalCaveStyle) {
        this(parent, "Text Display", false, null, "0", false, false,
                additionalCaveStyle);
    }

    /**
     * Constructor
     * 
     * @param parent
     *            Parent shell.
     * @param title
     *            Dialog title.
     * @param disposeOnExit
     *            Dispose on exit flag.
     * @param token
     *            Token.
     * @param isWarnGenDlg
     *            Is WarnGen dialog flag.
     */
    public TextEditorDialog(Shell parent, String title, boolean disposeOnExit,
            String token, boolean isWarnGenDlg) {
        this(parent, title, disposeOnExit, null, token, true, false, 0);

        this.isWarnGenDlg = isWarnGenDlg;
    }

    /**
     * Constructor.
     * 
     * @param parent
     *            Parent shell.
     * @param title
     *            Dialog title.
     * @param disposeOnExit
     *            Dispose on exit flag.
     * @param cbClient
     *            Callback client.
     * @param token
     *            Token.
     * @param hasScripting
     *            Scripting flag.
     * @param textWorkstationFlag
     *            Text workstation flag.
     */
    public TextEditorDialog(Shell parent, String title, boolean disposeOnExit,
            ITextEditorCallback cbClient, String token, boolean hasScripting,
            boolean textWorkstationFlag, int additionalCaveStyle) {
        super(parent, SWT.DIALOG_TRIM | SWT.MIN | SWT.RESIZE, CAVE.NO_PACK
                | CAVE.DO_NOT_BLOCK | CAVE.INDEPENDENT_SHELL
                | additionalCaveStyle);

        winTitle = title;
        setText(winTitle);

        this.token = token;
        this.hasScripting = hasScripting;

        this.disposeOnExit = disposeOnExit;

        callbackClient = cbClient;
        this.textWorkstationFlag = textWorkstationFlag;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.ui.dialogs.CaveSWTDialogBase#constructShellLayout()
     */
    @Override
    protected Layout constructShellLayout() {
        // Create the main layout for the shell.
        GridLayout mainLayout = new GridLayout(1, true);
        mainLayout.marginHeight = 0;
        mainLayout.marginWidth = 0;
        return mainLayout;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.ui.dialogs.CaveSWTDialogBase#initializeComponents(org
     * .eclipse.swt.widgets.Shell)
     */
    @Override
    protected void initializeComponents(final Shell shell) {
        Display display = getDisplay();
        clipboard = new Clipboard(getDisplay());

        // Set the fonts.
        smlFont = new Font(display, "Courier", 9, SWT.NORMAL);
        medFont = new Font(display, "Courier", 11, SWT.NORMAL);
        lrgFont = new Font(display, "Courier", 13, SWT.NORMAL);

        if (textWorkstationFlag || isWarnGenDlg) {
            shell.addShellListener(new ShellAdapter() {
                @Override
                public void shellClosed(ShellEvent event) {
                    if (inEditMode) {
                        event.doit = false;
                        cancelDoClose = true;
                        bringToTop();
                        cancelEditor(true);
                        return;
                    }

                    // If the disposeDialog flag is true then return so this
                    // dialog will get disposed.
                    if (disposeDialog == true) {
                        return;
                    }

                    // If the disposeOnExit is true then return since this
                    // dialog
                    // will be modal and we can't prevent the dialog from
                    // disposing.
                    if (disposeOnExit == true) {
                        return;
                    }

                    if (afosBrowser != null
                            && afosBrowser.isAfosBrowserActive()) {
                        afosBrowser.hide();
                        displayAfosBrowser = true;
                    } else {
                        displayAfosBrowser = false;
                    }

                    if (browser != null) {
                        browser.hide();
                    }

                    // Block the disposal of this dialog.
                    hide();
                    event.doit = false;
                }
            });

            getParent().addDisposeListener(new DisposeListener() {
                @Override
                public void widgetDisposed(DisposeEvent e) {
                    inEditMode = false;
                    close();
                }
            });
        }

        if (isWarnGenDlg) {
            queryTransport = TextEditorUtil.getTextDbsrvTransport();
        }

        commandHistory = new CommandHistory();

        // Create the menus
        createMenus();

        // Initialize all of the controls and layouts
        initializeComponents();

        // initialize scripting controls
        setScriptControls(false);
    }

    /**
     * Use this to open the dialog in case there is a need to do more then just
     * the open.
     */
    private void openDialog() {
        this.open();
    }

    /**
     * Create the menu bar and all of the menus.
     */
    private void createMenus() {
        Menu menuBar = new Menu(shell, SWT.BAR);

        createFileMenus(menuBar);
        createEditMenus(menuBar);
        createOptionsMenus(menuBar);
        createVersionMenus(menuBar);
        createToolsMenus(menuBar);
        createScriptsMenus(menuBar);
        createProductsMenus(menuBar);
        createHelpMenus(menuBar);

        shell.setMenuBar(menuBar);
    }

    /**
     * Create the 'File' menu and all of the menu items.
     * 
     * @param menuBar
     *            Menu bar that contains all of the menus.
     */
    private void createFileMenus(Menu menuBar) {
        // -------------------------------------
        // Create all the items in the file menu
        // -------------------------------------
        MenuItem fileMenuItem = new MenuItem(menuBar, SWT.CASCADE);
        fileMenuItem.setText("File");

        // Create the File menu item with a File "dropdown" menu
        Menu fileMenu = new Menu(menuBar);
        fileMenuItem.setMenu(fileMenu);

        // Create all the items in the File dropdown menu
        AFOSBrowserItem = new MenuItem(fileMenu, SWT.NONE);
        AFOSBrowserItem.setText("AFOS Browser...");
        AFOSBrowserItem.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                displayAfosBrowser();
            }
        });

        new MenuItem(fileMenu, SWT.SEPARATOR);

        printAllItem = new MenuItem(fileMenu, SWT.NONE);
        printAllItem.setText("Print All\tCtrl+P");
        printAllItem.setAccelerator(SWT.CTRL + 'P');
        printAllItem.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                printAllText();
            }
        });

        printSelectionItem = new MenuItem(fileMenu, SWT.NONE);
        printSelectionItem.setText("Print Selection");
        printSelectionItem.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                printSelectedText();
            }
        });

        new MenuItem(fileMenu, SWT.SEPARATOR);

        faxAllItem = new MenuItem(fileMenu, SWT.NONE);
        faxAllItem.setText("Fax All...");
        faxAllItem.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {

                if (faxAllMsgDlg == null || faxAllMsgDlg.isDisposed()) {
                    faxAllMsgDlg = new FaxMessageDlg(shell);
                    faxAllMsgDlg.setInitialText(textEditor.getText());
                    faxAllMsgDlg.open();
                } else {
                    faxAllMsgDlg.bringToTop();
                }
            }
        });

        faxSelectionItem = new MenuItem(fileMenu, SWT.NONE);
        faxSelectionItem.setText("Fax Selection...");
        faxSelectionItem.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                if (faxMsgDlg == null || faxMsgDlg.isDisposed()) {
                    faxMsgDlg = new FaxMessageDlg(shell);
                    faxMsgDlg.setInitialText(textEditor.getSelectionText());
                    faxMsgDlg.open();
                } else {
                    faxMsgDlg.bringToTop();
                }
            }
        });

        configAutoFaxItem = new MenuItem(fileMenu, SWT.NONE);
        configAutoFaxItem.setText("Configure Auto Fax...");
        configAutoFaxItem.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                if (ldadFaxSitesDlg == null || ldadFaxSitesDlg.isDisposed()) {
                    ldadFaxSitesDlg = new LdadFaxSitesDlg(shell);
                    ldadFaxSitesDlg.open();
                } else {
                    ldadFaxSitesDlg.bringToTop();
                }
            }
        });

        new MenuItem(fileMenu, SWT.SEPARATOR);

        enterEditorItem = new MenuItem(fileMenu, SWT.NONE);
        enterEditorItem.setText("Enter Editor");
        enterEditorItem.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                inEditMode = true;
                enterEditor();
            }
        });

        saveItem = new MenuItem(fileMenu, SWT.NONE);
        saveItem.setText("Save");
        saveItem.setEnabled(false);
        saveItem.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                saveProduct();
            }
        });

        sendExitEditorItem = new MenuItem(fileMenu, SWT.NONE);
        sendExitEditorItem.setText("Send && Exit Editor");
        sendExitEditorItem.setEnabled(false);
        sendExitEditorItem.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                sendProduct(false);
            }
        });

        cancelEditorItem = new MenuItem(fileMenu, SWT.NONE);
        cancelEditorItem.setText("Cancel Editor");
        cancelEditorItem.setEnabled(false);
        cancelEditorItem.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                cancelEditor(true);
            }
        });

        new MenuItem(fileMenu, SWT.SEPARATOR);

        importFromFileItem = new MenuItem(fileMenu, SWT.NONE);
        importFromFileItem.setText("Import From File...");
        importFromFileItem.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                importFile();
            }
        });

        exportFromFileItem = new MenuItem(fileMenu, SWT.NONE);
        exportFromFileItem.setText("Export To File...");
        exportFromFileItem.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                exportFile();
            }
        });

        new MenuItem(fileMenu, SWT.SEPARATOR);

        requestFromRemoteSiteItem = new MenuItem(fileMenu, SWT.NONE);
        requestFromRemoteSiteItem.setText("Request From Remote Site...");
        requestFromRemoteSiteItem.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                RemoteSiteRequestDlg requestDlg = new RemoteSiteRequestDlg(
                        shell);
                if (lastRemoteRetrievalRequest != null) {
                    requestDlg.setRequest(lastRemoteRetrievalRequest);
                }
                requestDlg.setCloseCallback(new ICloseCallback() {

                    @Override
                    public void dialogClosed(Object returnValue) {
                        RemoteRetrievalRequest req = (RemoteRetrievalRequest) returnValue;
                        if (req != null) {
                            lastRemoteRetrievalRequest = req;
                            sendRemoteRetrievalRequest(req);
                        }
                    }
                });
                requestDlg.open();
            }
        });

        recoverEditSessionItem = new MenuItem(fileMenu, SWT.NONE);
        recoverEditSessionItem.setText("Recover Edit Sesssion...");
        recoverEditSessionItem.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                EditSessionRecoveryDialog recoveryDlg = new EditSessionRecoveryDialog(
                        TextEditorDialog.this.getParent(),
                        TextEditorDialog.this);
                recoveryDlg.setBlockOnOpen(false);
                recoveryDlg.open();
            }
        });

        new MenuItem(fileMenu, SWT.SEPARATOR);

        resendWarningProductnItem = new MenuItem(fileMenu, SWT.NONE);
        resendWarningProductnItem.setText("Resend Warning Product...");
        resendWarningProductnItem.setEnabled(false);
        resendWarningProductnItem.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                sendProduct(true);
            }
        });

        new MenuItem(fileMenu, SWT.SEPARATOR);

        closeItem = new MenuItem(fileMenu, SWT.NONE);
        closeItem.setText("Close\tAlt+F4");
        closeItem.setAccelerator(SWT.ALT | SWT.F4);
        closeItem.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                if (disposeOnExit == true) {
                    close();
                } else {
                    hide();
                }
                inEditMode = false;
            }
        });
    }

    /**
     * Create the Edit menu (top menu bar) and all of the menu items under the
     * Edit menu.
     * 
     * @param menuBar
     *            The main menu bar.
     */
    private void createEditMenus(Menu menuBar) {
        // -------------------------------------
        // Create all the items in the Edit menu
        // -------------------------------------
        MenuItem editMenuItem = new MenuItem(menuBar, SWT.CASCADE);
        editMenuItem.setText("Edit");

        // Create the Edit menu item with a Edit "dropdown" menu
        Menu editMenu = new Menu(menuBar);
        editMenuItem.setMenu(editMenu);

        cutItem = new MenuItem(editMenu, SWT.NONE);
        cutItem.setText("Cut\tCtrl+X");
        cutItem.setAccelerator(SWT.CTRL + 'X');
        cutItem.setEnabled(false);
        cutItem.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                cutText();
            }
        });

        copyItem = new MenuItem(editMenu, SWT.NONE);
        copyItem.setText("Copy\tCtrl+C");
        copyItem.setAccelerator(SWT.CTRL + 'C');
        copyItem.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                copyText();
            }
        });

        pasteItem = new MenuItem(editMenu, SWT.NONE);
        pasteItem.setText("Paste\tCtrl+V");
        pasteItem.setAccelerator(SWT.CTRL + 'V');
        pasteItem.setEnabled(false);
        pasteItem.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                pasteText();
            }
        });

        fillItem = new MenuItem(editMenu, SWT.NONE);
        fillItem.setText("Fill");
        fillItem.setEnabled(false);
        fillItem.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                fillText();
            }
        });

        // Add a menu separator.
        new MenuItem(editMenu, SWT.SEPARATOR);

        // ---------------------------
        // Create the Select sub menu
        // ---------------------------
        selectSubMenuItem = new MenuItem(editMenu, SWT.CASCADE);
        selectSubMenuItem.setText("Select");

        Menu selectSubMenu = new Menu(shell, SWT.DROP_DOWN);
        selectSubMenuItem.setMenu(selectSubMenu);

        createSelectSubMenu(selectSubMenu);

        // ---------------------------
        // Create the Delete sub menu
        // ---------------------------
        deleteSubMenuItem = new MenuItem(editMenu, SWT.CASCADE);
        deleteSubMenuItem.setText("Delete");
        deleteSubMenuItem.setEnabled(false);

        Menu deleteSubMenu = new Menu(shell, SWT.DROP_DOWN);
        deleteSubMenuItem.setMenu(deleteSubMenu);

        createDeleteSubMenu(deleteSubMenu);

        // -----------------------------
        // Create the Undelete sub menu
        // -----------------------------
        undeleteSubMenuItem = new MenuItem(editMenu, SWT.CASCADE);
        undeleteSubMenuItem.setText("Undelete");
        undeleteSubMenuItem.setEnabled(false);

        Menu undeleteSubMenu = new Menu(shell, SWT.DROP_DOWN);
        undeleteSubMenuItem.setMenu(undeleteSubMenu);

        createUndeleteSubMenu(undeleteSubMenu);

        // Add a menu separator.
        new MenuItem(editMenu, SWT.SEPARATOR);

        clearWindowItem = new MenuItem(editMenu, SWT.NONE);
        clearWindowItem.setText("Clear Window\tCtrl+Shift+F4");
        clearWindowItem.setAccelerator(SWT.CTRL | SWT.SHIFT | SWT.F4);
        clearWindowItem.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                clearTextEditor();
            }
        });

        // Add a menu separator.
        new MenuItem(editMenu, SWT.SEPARATOR);

        clearSelectionItem = new MenuItem(editMenu, SWT.NONE);
        clearSelectionItem.setText("Clear Selection");
        clearSelectionItem.setEnabled(false);
        clearSelectionItem.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                if (textEditor.getSelectionCount() > 0) {
                    Point p = textEditor.getSelectionRange();
                    textEditor.replaceTextRange(p.x, p.y, "");
                } else {
                    userInformation("You must first select a region to\n"
                            + "clear (by dragging, for example).");
                }
            }
        });

        editHeaderItem = new MenuItem(editMenu, SWT.NONE);
        editHeaderItem.setText("Edit Header...");
        editHeaderItem.setEnabled(false);
        editHeaderItem.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                // Edit the header block of the text product.
                editHeader("", false);
            }
        });

        searchItem = new MenuItem(editMenu, SWT.NONE);
        searchItem.setText("Search...\tF2");
        searchItem.setAccelerator(SWT.F2);
        searchItem.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                if (searchReplaceDlg == null || searchReplaceDlg.isDisposed()) {
                    searchReplaceDlg = new SearchReplaceDlg(shell, textEditor,
                            inEditMode);
                    searchReplaceDlg.open();
                } else {
                    searchReplaceDlg.bringToTop();
                }
            }
        });

        spellCheckerItem = new MenuItem(editMenu, SWT.NONE);
        spellCheckerItem.setText("Spell Checker...\tCtrl+F1");
        spellCheckerItem.setAccelerator(SWT.CTRL | SWT.F1);
        spellCheckerItem.setEnabled(false);
        spellCheckerItem.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                checkSpelling();
            }
        });

        // Add a menu separator.
        new MenuItem(editMenu, SWT.SEPARATOR);

        clearUpdateFlagsItem = new MenuItem(editMenu, SWT.NONE);
        clearUpdateFlagsItem.setText("Clear Update Flags");
        clearUpdateFlagsItem.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                clearUpdateFlags();
            }
        });
    }

    /**
     * Remove style ranges from the text editor.
     */
    private void clearUpdateFlags() {
        textEditor.setStyleRange(null);
    }

    /**
     * Implements the clear command on the text editor.
     */
    private void clearTextEditor() {
        clearUpdateObs();
        clearTextFields();
        clearButtonology();
        TextDisplayModel.getInstance().clearStdTextProduct(token);
        clearHeaderTextField();
        saved = false;
        isSaoMetarFlag = false;
        displayedPils.clear();
        obsRegex = null;
        originalText = null;
    }

    /**
     * Create the Options menu (top menu bar) and all of the menu items under
     * the Options menu.
     * 
     * @param menuBar
     *            The main menu bar.
     */
    private void createOptionsMenus(Menu menuBar) {
        // ----------------------------------------
        // Create all the items in the Options menu
        // ----------------------------------------
        MenuItem optionsMenuItem = new MenuItem(menuBar, SWT.CASCADE);
        optionsMenuItem.setText("Options");

        // Create the Options menu item with a Options "dropdown" menu
        Menu optionsMenu = new Menu(menuBar);
        optionsMenuItem.setMenu(optionsMenu);

        // -----------------------------
        // Create the Autowrap sub menu
        // -----------------------------
        autoWrapMenuItem = new MenuItem(optionsMenu, SWT.CASCADE);
        autoWrapMenuItem.setText("Autowrap");

        Menu autoWrapSubMenu = new Menu(shell, SWT.DROP_DOWN);
        autoWrapMenuItem.setMenu(autoWrapSubMenu);

        createAutoWrapSubMenu(autoWrapSubMenu);

        // -------------------------------
        // Create the Font Size sub menu
        // -------------------------------
        fontSizeMenuItem = new MenuItem(optionsMenu, SWT.CASCADE);
        fontSizeMenuItem.setText("Font Size");

        Menu fontSizeSubMenu = new Menu(shell, SWT.DROP_DOWN);
        fontSizeMenuItem.setMenu(fontSizeSubMenu);

        createFontSizeSubMenu(fontSizeSubMenu);

        // ------------------------------
        // Create overstrike menu item
        // ------------------------------
        overStrikeItem = new MenuItem(optionsMenu, SWT.CHECK);
        overStrikeItem.setText("Overstrike Mode\tIns");
        overStrikeItem.setAccelerator(SWT.INSERT);
        overStrikeItem.setEnabled(false);
        overStrikeItem.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                if (overwriteMode == true) {
                    overwriteMode = false;
                    editorInsertCmb.select(INSERT_TEXT);
                } else {
                    overwriteMode = true;
                    editorInsertCmb.select(OVERWRITE_TEXT);
                }
                textEditor.invokeAction(ST.TOGGLE_OVERWRITE);
            }
        });
    }

    /**
     * Create the Version menu (top menu bar) and all of the menu items under
     * the Version menu.
     * 
     * @param menuBar
     *            The main menu bar.
     */
    private void createVersionMenus(Menu menuBar) {
        // ----------------------------------------
        // Create all the items in the Version menu
        // ----------------------------------------
        versionMenuItem = new MenuItem(menuBar, SWT.CASCADE);
        versionMenuItem.setText("Version");

        // Create the Version menu item with a Version "dropdown" menu
        Menu versionMenu = new Menu(menuBar);
        versionMenuItem.setMenu(versionMenu);

        versionPreviousItem = new MenuItem(versionMenu, SWT.NONE);
        versionPreviousItem.setText("Previous\tAlt+Left Arrow");
        versionPreviousItem.setAccelerator(SWT.ALT | SWT.ARROW_LEFT);
        versionPreviousItem.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                commandHistory.resetIndex(CommandType.AFOS);
                ICommand command = commandHistory
                        .getPreviousCommand(CommandType.AFOS);
                if (command != null) {
                    ICommand cmd = CommandFactory
                            .getPreviousForAfosCommand(command);
                    if (cmd.isValid()) {
                        executeCommand(cmd);
                    }
                }
            }
        });

        versionNextItem = new MenuItem(versionMenu, SWT.NONE);
        versionNextItem.setText("Next\tAlt+Right Arrow");
        versionNextItem.setAccelerator(SWT.ALT | SWT.ARROW_RIGHT);
        versionNextItem.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                commandHistory.resetIndex(CommandType.AFOS);
                ICommand command = commandHistory
                        .getPreviousCommand(CommandType.AFOS);
                if (command != null) {
                    ICommand cmd = CommandFactory
                            .getNextForAfosCommand(command);
                    if (cmd.isValid()) {
                        executeCommand(cmd);
                    }
                }
            }
        });

        versionLatestItem = new MenuItem(versionMenu, SWT.NONE);
        versionLatestItem.setText("Latest");
        versionLatestItem.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                commandHistory.resetIndex(CommandType.AFOS);
                ICommand command = commandHistory
                        .getPreviousCommand(CommandType.AFOS);
                if (command != null) {
                    ICommand cmd = CommandFactory
                            .getLatestForAfosCommand(command);
                    if (cmd.isValid()) {
                        executeCommand(cmd);
                    }
                }
            }
        });

        versionAllItem = new MenuItem(versionMenu, SWT.NONE);
        versionAllItem.setText("All");
        versionAllItem.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                commandHistory.resetIndex(CommandType.AFOS);
                ICommand command = commandHistory
                        .getPreviousCommand(CommandType.AFOS);
                if (command != null) {
                    ICommand cmd = CommandFactory.getAllForAfosCommand(command);
                    if (cmd.isValid()) {
                        executeCommand(cmd);
                    }
                }
            }
        });
    }

    /**
     * Create the Tools menu (top menu bar) and all of the menu items under the
     * Tools menu.
     * 
     * @param menuBar
     *            The main menu bar.
     */
    private void createToolsMenus(Menu menuBar) {
        // ----------------------------------------
        // Create all the items in the Tools menu
        // ----------------------------------------
        MenuManager toolsMenuMgr = new MenuManager("Tools");
        IMenuService ms = (IMenuService) PlatformUI.getWorkbench().getService(
                IMenuService.class);
        ms.populateContributionManager(toolsMenuMgr,
                "menu:#texteditor.tools.menu");
        toolsMenuMgr.fill(menuBar, -1);
        Menu toolsMenu = toolsMenuMgr.getMenu();
        toolsMenu.setData("Dialog", this);
        toolsMenuItem = toolsMenu.getParentItem();
    }

    /**
     * Create the Scripts menu (top menu bar) and all of the menu items under
     * the Scripts menu.
     * 
     * @param menuBar
     *            The main menu bar.
     */
    private void createScriptsMenus(Menu menuBar) {
        // ----------------------------------------
        // Create all the items in the Scripts menu
        // ----------------------------------------
        MenuItem scriptsMenuItem = new MenuItem(menuBar, SWT.CASCADE);
        scriptsMenuItem.setText("Scripts");

        // Create the Scripts menu item with a Scripts "drop-down" menu
        Menu scriptsMenu = new Menu(menuBar);
        scriptsMenuItem.setMenu(scriptsMenu);
        /* disable scripts menu when launched from D-2D */
        scriptsMenuItem.setEnabled(this.hasScripting);

        // create the menu items
        scriptsEditItem = new MenuItem(scriptsMenu, SWT.NONE);
        scriptsEditItem.setText("Edit...");
        scriptsEditItem.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                onDisplayScriptEditor();
            }

        });

        scriptsRunItem = new MenuItem(scriptsMenu, SWT.NONE);
        scriptsRunItem.setText("Run...");
        scriptsRunItem.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                loadAndRunScript();

            }
        });

        scriptsShowOutputItem = new MenuItem(scriptsMenu, SWT.CHECK);
        scriptsShowOutputItem.setText("Show Output");
        scriptsShowOutputItem.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                Object object = e.getSource();
                if (!(object instanceof MenuItem)) {
                    // should never happen...
                    return;
                }
                MenuItem source = (MenuItem) object;
                manageScriptOutputWindow(source.getSelection());
            }
        });

        new MenuItem(scriptsMenu, SWT.SEPARATOR);

        scriptsContinueItem = new MenuItem(scriptsMenu, SWT.NONE);
        scriptsContinueItem.setText("Continue");
        scriptsContinueItem.setEnabled(false);
        scriptsContinueItem.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                //
                onContinueScript();
            }

        });

        scriptsSkipWaitItem = new MenuItem(scriptsMenu, SWT.NONE);
        scriptsSkipWaitItem.setText("Skip Wait");
        scriptsSkipWaitItem.setEnabled(false);
        scriptsSkipWaitItem.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                onSkipWait();
            }
        });

        scriptsCancelItem = new MenuItem(scriptsMenu, SWT.NONE);
        scriptsCancelItem.setText("Cancel");
        scriptsCancelItem.setEnabled(false);
        scriptsCancelItem.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                onCancelScript();
            }
        });

    }

    /**
     * Create the Products menu (top menu bar) and all of the menu items under
     * the Products menu.
     * 
     * @param menuBar
     *            The main menu bar.
     */
    private void createProductsMenus(Menu menuBar) {
        DiscoverMenuContributions
                .discoverContributions(new String[] { "menus/textws" });
        // -----------------------------------------
        // Create all the items in the Products menu
        // -----------------------------------------
        MenuManager productsMenuMgr = new MenuManager("Products");
        IMenuService ms = (IMenuService) PlatformUI.getWorkbench().getService(
                IMenuService.class);
        ms.populateContributionManager(productsMenuMgr,
                "menu:#texteditor.products.menu");
        productsMenuMgr.fill(menuBar, -1);
        Menu productsMenu = productsMenuMgr.getMenu();
        productsMenu.setData("Dialog", this);
        productsMenuItem = productsMenu.getParentItem();
    }

    /**
     * Create the Help menu (top menu bar) and all of the menu items under the
     * Help menu.
     * 
     * @param menuBar
     *            The main menu bar.
     */
    private void createHelpMenus(Menu menuBar) {
        // -----------------------------------------
        // Create all the items in the Help menu
        // -----------------------------------------
        MenuItem helpMenuItem = new MenuItem(menuBar, SWT.CASCADE);
        helpMenuItem.setText("Help");

        // Create the Products menu item with a Products "dropdown" menu
        Menu helpMenu = new Menu(menuBar);
        helpMenuItem.setMenu(helpMenu);

        onTextWindowItem = new MenuItem(helpMenu, SWT.NONE);
        onTextWindowItem.setText("On Text Window...\tF1");
        onTextWindowItem.setAccelerator(SWT.F1);
        onTextWindowItem.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                notImplementedYet("On text window");
            }
        });

        // Add a menu separator.
        new MenuItem(helpMenu, SWT.SEPARATOR);

        aboutItem = new MenuItem(helpMenu, SWT.NONE);
        aboutItem.setText("About...");
        aboutItem.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                notImplementedYet("About");
            }
        });
    }

    /**
     * Method to determine the target position for page up or page down.
     * 
     * @param st
     *            -- the styled text widget containing the text
     * @param isPageUp
     *            -- true if paging up, false if paging down
     * @return targetPos -- the target position for paging up or down
     */
    private static int determineTargetOffsetInPage(StyledText st,
            SelectDirection direction) {
        int targetPos = 0;
        int sourcePos = st.getCaretOffset();
        int targetLine = st.getLineAtOffset(sourcePos);

        // Determine what offset to use
        switch (direction) {
        case BACKWARD: {
            targetLine -= PAGE_SCROLL_LINES;
            break;
        }
        case FORWARD: {
            targetLine += PAGE_SCROLL_LINES;
            break;
        }
        }

        if (targetLine < 0) {
            targetPos = 0;
        } else if (targetLine > st.getLineCount()) {
            targetPos = st.getCharCount();
        } else {
            targetPos = st.getOffsetAtLine(targetLine);
        }

        return targetPos;
    }

    /**
     * Method to determine the start and finish positions of the text selection
     * within the "next word" or the "previous word" edit selections.
     * 
     * @param st
     *            -- the styled text widget containing the selection of text
     * @return result -- the start and finish positions of the selected range
     */
    public static final int[] determineCurrentWord(StyledText st) {
        int[] result = { 0, 0 };
        int caretOffset = st.getCaretOffset();
        int lineIndex = st.getLineAtOffset(caretOffset);
        String lineText = st.getLine(lineIndex);
        int lineOffset = st.getOffsetAtLine(lineIndex);

        // caret location within text string
        int lineCaretOffset = caretOffset - st.getOffsetAtLine(lineIndex);

        // find the first space
        int wordLineStart = lineText.lastIndexOf(' ', lineCaretOffset);

        // skip space if not start of line, makes no space found also start of
        // line
        if (wordLineStart != 0) {
            wordLineStart++;
        }

        int wordLineEnd = lineText.indexOf(' ', lineCaretOffset + 1);

        if (wordLineEnd == -1) {
            wordLineEnd = lineText.length();
        }

        result[0] = wordLineStart + lineOffset;
        result[1] = wordLineEnd + lineOffset;
        return result;
    }

    /**
     * Method to preserve the spacing in a bulleted section of text. Only
     * applies to VTEC warning (vtecAfosProductEnum) type products.
     */
    private void PreserveBulletedTextSpacing(StyledText st) {
        // Capture the input from the styled text widget.
        String in = st.getText();
        // New up a Scanner to use to parse the text in the editor.
        Scanner sc = new Scanner(in).useDelimiter("\\n");
        // Declare variables for processing the editor's contents.
        int thisLine = -1;
        String whatMatched;

        // Process the editor's contents.
        while (sc.hasNext()) {
            whatMatched = sc.next();
            thisLine++;
            if ((whatMatched != null) && (sc.hasNext())
                    && (whatMatched.startsWith("*"))) {
                while (sc.hasNext()) {
                    whatMatched = sc.next();
                    thisLine++;
                    if ((whatMatched != null)
                            && (!(whatMatched.startsWith(" ")))
                            && (!(whatMatched.equals("")))) {
                        // Indent the bullet list entry by one space.
                        // If operator enables word wrap they are responsible
                        // for reformatting bullet list items that span lines.
                        st.setCaretOffset(st.getOffsetAtLine(thisLine));
                        st.insert(" ");
                    } else if ((whatMatched != null)
                            && (whatMatched.equals(""))) {
                        break; // The end of bullet list is detected.
                    }
                }
            }
        }

    }

    /**
     * Method to create selection sub-menu.
     * 
     * @param selectSubMenu
     */
    private void createSelectSubMenu(Menu selectSubMenu) {
        toPreviousWordItem = new MenuItem(selectSubMenu, SWT.NONE);
        toPreviousWordItem.setText("To previous word\tCtrl+Shift+Left");
        toPreviousWordItem
                .setAccelerator(SWT.CTRL | SWT.SHIFT | SWT.ARROW_LEFT);
        toPreviousWordItem.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                String text = textEditor.getText();
                int caretOffset = textEditor.getCaretOffset();

                int searchOffset = caretOffset;
                // don't include current character but handle start of text
                if (searchOffset > 0) {
                    searchOffset--;
                }

                // skip any spaces at cursor
                while (text.charAt(searchOffset) == ' ' && searchOffset > 0) {
                    searchOffset--;
                }

                int index = text.lastIndexOf(' ', searchOffset);

                // don't include the space, also handles the -1 of none found to
                // select all
                index++;

                int start = TextEditorUtil.determineSelectionStart(textEditor);
                textEditor.setSelection(start, index);
            }
        });

        toNextWordItem = new MenuItem(selectSubMenu, SWT.NONE);
        toNextWordItem.setText("To next word\tCtrl+Shift+Right");
        toNextWordItem.setAccelerator(SWT.CTRL | SWT.SHIFT | SWT.ARROW_RIGHT);
        toNextWordItem.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                String text = textEditor.getText();
                int caretOffset = textEditor.getCaretOffset();

                int searchOffset = caretOffset;
                // don't include current character but handle end of text
                if (searchOffset < text.length()) {
                    searchOffset++;
                }

                // skip any spaces at cursor
                while (text.charAt(searchOffset) == ' '
                        && searchOffset < text.length()) {
                    searchOffset++;
                }

                int index = textEditor.getText().indexOf(' ', searchOffset);

                // set missing to end of text
                if (index == -1) {
                    index = text.length() - 1;
                } else if (index < text.length() - 1) {
                    // skip the space
                    index++;
                }

                int start = TextEditorUtil.determineSelectionStart(textEditor);
                textEditor.setSelection(start, index);
            }
        });

        toBeginningOfLineItem = new MenuItem(selectSubMenu, SWT.NONE);
        toBeginningOfLineItem.setText("To beginning of line\tShift+Home");
        toBeginningOfLineItem.setAccelerator(SWT.SHIFT | SWT.HOME);
        toBeginningOfLineItem.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                // Select to the beginning of the current line
                int start = TextEditorUtil.determineSelectionStart(textEditor);
                int caretOffset = textEditor.getCaretOffset();
                int currentLine = textEditor.getLineAtOffset(caretOffset);
                int finish = textEditor.getOffsetAtLine(currentLine);
                textEditor.setSelection(start, finish);
            }
        });

        toEndOfLineItem = new MenuItem(selectSubMenu, SWT.NONE);
        toEndOfLineItem.setText("To end of line\tShift+End");
        toEndOfLineItem.setAccelerator(SWT.SHIFT | SWT.END);
        toEndOfLineItem.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                // Select to the end of the current line
                int start = TextEditorUtil.determineSelectionStart(textEditor);
                int caretOffset = textEditor.getCaretOffset();
                int targetLine = textEditor.getLineAtOffset(caretOffset) + 1;
                int finish = 0;

                if (targetLine < textEditor.getLineCount()) {
                    finish = textEditor.getOffsetAtLine(targetLine) - 1;
                } else {
                    finish = textEditor.getCharCount();
                }

                textEditor.setSelection(start, finish);
            }
        });

        toPreviousPageItem = new MenuItem(selectSubMenu, SWT.NONE);
        toPreviousPageItem.setText("To previous page\tShift+Page Up");
        toPreviousPageItem.setAccelerator(SWT.SHIFT | SWT.PAGE_UP);
        toPreviousPageItem.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                int start = TextEditorUtil.determineSelectionStart(textEditor);
                int finish = determineTargetOffsetInPage(textEditor,
                        SelectDirection.BACKWARD);
                textEditor.setSelection(start, finish);
            }
        });

        toNextPageItem = new MenuItem(selectSubMenu, SWT.NONE);
        toNextPageItem.setText("To next page\tShift+Page Down");
        toNextPageItem.setAccelerator(SWT.SHIFT | SWT.PAGE_DOWN);
        toNextPageItem.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                int start = TextEditorUtil.determineSelectionStart(textEditor);
                int finish = determineTargetOffsetInPage(textEditor,
                        SelectDirection.FORWARD);
                textEditor.setSelection(start, finish);
            }
        });

        toTopOfProductItem = new MenuItem(selectSubMenu, SWT.NONE);
        toTopOfProductItem.setText("To top of Product\tCtrl+Shift+Home");
        toTopOfProductItem.setAccelerator(SWT.CTRL | SWT.SHIFT | SWT.HOME);
        toTopOfProductItem.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                int start = TextEditorUtil.determineSelectionStart(textEditor);
                int finish = 0;
                textEditor.setSelection(start, finish);
            }
        });

        toEndOfProductItem = new MenuItem(selectSubMenu, SWT.NONE);
        toEndOfProductItem.setText("To end of Product\tCtrl+Shift+End");
        toEndOfProductItem.setAccelerator(SWT.CTRL | SWT.SHIFT | SWT.END);
        toEndOfProductItem.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                int start = TextEditorUtil.determineSelectionStart(textEditor);
                int finish = textEditor.getCharCount();
                textEditor.setSelection(start, finish);
            }
        });

        selectAllItem = new MenuItem(selectSubMenu, SWT.NONE);
        selectAllItem.setText("All");
        selectAllItem.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                textEditor.selectAll();
            }
        });
    }

    /**
     * Create the Delete sub menu items. The Delete menu item is located under
     * the Edit menu on the top menu bar.
     * 
     * @param deleteSubMenu
     *            The Delete sub menu.
     */
    private void createDeleteSubMenu(Menu deleteSubMenu) {
        deleteCharacterItem = new MenuItem(deleteSubMenu, SWT.NONE);
        deleteCharacterItem.setText("Character\tF6");
        deleteCharacterItem.setAccelerator(SWT.F6);
        deleteCharacterItem.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                if (textEditor.getCaretOffset() < textEditor.getCharCount()) {
                    undeleteCharacterItem.setEnabled(true);
                    deletedChar = textEditor.getText(
                            textEditor.getCaretOffset(),
                            textEditor.getCaretOffset());
                    textEditor.replaceTextRange(textEditor.getCaretOffset(), 1,
                            "");
                }
            }
        });

        deleteWordItem = new MenuItem(deleteSubMenu, SWT.NONE);
        deleteWordItem.setText("Word\tF7");
        deleteWordItem.setAccelerator(SWT.F7);
        deleteWordItem.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                undeleteWordItem.setEnabled(true);
                int[] result = determineCurrentWord(textEditor);
                // Guard against over-indexing in getText()
                if (result[1] == textEditor.getCharCount()) {
                    result[1]--;
                }
                deletedWord = textEditor.getText(result[0], result[1]);
                textEditor.replaceTextRange(result[0], deletedWord.length(), "");
            }
        });

        deleteLineItem = new MenuItem(deleteSubMenu, SWT.NONE);
        deleteLineItem.setText("Line\tF8");
        deleteLineItem.setAccelerator(SWT.F8);
        deleteLineItem.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                int caretOffset = textEditor.getCaretOffset();
                if (caretOffset < textEditor.getCharCount()) {
                    undeleteLineItem.setEnabled(true);
                    int line = textEditor.getLineAtOffset(caretOffset);
                    int finish;

                    if (line + 1 < textEditor.getLineCount()) {
                        finish = textEditor.getOffsetAtLine(line + 1) - 1;
                    } else {
                        // Guard against over-indexing in getText()
                        finish = textEditor.getCharCount() - 1;
                    }

                    deletedLine = textEditor.getText(caretOffset, finish);
                    textEditor.replaceTextRange(caretOffset,
                            deletedLine.length(), "");
                }
            }
        });
    }

    /**
     * Create the Undelete sub menu items. The Undelete menu item is located
     * under the Edit menu on the top menu bar.
     * 
     * @param deleteSubMenu
     *            The Delete sub menu.
     */
    private void createUndeleteSubMenu(Menu undeleteSubMenu) {
        undeleteCharacterItem = new MenuItem(undeleteSubMenu, SWT.NONE);
        undeleteCharacterItem.setText("Character\tShift+F6");
        undeleteCharacterItem.setAccelerator(SWT.SHIFT | SWT.F6);
        undeleteCharacterItem.setEnabled(false);
        undeleteCharacterItem.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                undeleteCharacterItem.setEnabled(false);
                if (overwriteMode) {
                    textEditor.replaceTextRange(textEditor.getCaretOffset(), 1,
                            deletedChar);
                } else {
                    textEditor.insert(deletedChar);
                }
            }
        });

        undeleteWordItem = new MenuItem(undeleteSubMenu, SWT.NONE);
        undeleteWordItem.setText("Word\tShift+F7");
        undeleteWordItem.setAccelerator(SWT.SHIFT | SWT.F7);
        undeleteWordItem.setEnabled(false);
        undeleteWordItem.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                undeleteWordItem.setEnabled(false);
                if (overwriteMode) {
                    textEditor.replaceTextRange(textEditor.getCaretOffset(),
                            deletedWord.length(), deletedWord);
                } else {
                    textEditor.insert(deletedWord);
                }
            }
        });

        undeleteLineItem = new MenuItem(undeleteSubMenu, SWT.NONE);
        undeleteLineItem.setText("Line\tShift+F8");
        undeleteLineItem.setAccelerator(SWT.SHIFT | SWT.F8);
        undeleteLineItem.setEnabled(false);
        undeleteLineItem.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                undeleteLineItem.setEnabled(false);
                if (overwriteMode) {
                    textEditor.replaceTextRange(textEditor.getCaretOffset(),
                            deletedLine.length(), deletedLine);
                } else {
                    textEditor.insert(deletedLine);
                }
            }
        });
    }

    /**
     * Get the auto wrap menu's configuration and perform sanity check.
     * 
     * @return autoWrapCfg
     */
    private AutoWrapCfg getAutoWrapCfg() {
        AutoWrapCfg autoWrapCfg = null;
        try {
            IPathManager pm = PathManagerFactory.getPathManager();
            File path = pm.getStaticFile("textws/gui/AutoWrapCfg.xml");

            autoWrapCfg = JAXB.unmarshal(path, AutoWrapCfg.class);
        } catch (Exception e) {
            IUFStatusHandler statusHandler = UFStatus
                    .getHandler(TextEditorDialog.class);
            statusHandler.handle(Priority.ERROR,
                    "Unable to parse Autowrap menu configuration.", e);
            autoWrapCfg = new AutoWrapCfg();
            autoWrapCfg.setRangeStart(DEFAULT_START);
            autoWrapCfg.setRangeEnd(DEFAULT_END);
        }

        // Perform Sanity Checks on configuration.
        StringBuilder message = new StringBuilder();
        if (autoWrapCfg.getRangeStart() <= 0) {
            message.append("Bad value for RangeStart (")
                    .append(autoWrapCfg.getRangeStart())
                    .append(") using default: ").append(DEFAULT_START)
                    .append("\n");
            autoWrapCfg.setRangeStart(DEFAULT_START);
        }

        if (autoWrapCfg.getRangeEnd() <= 0) {
            message.append("Bad value for RangeEnd (")
                    .append(autoWrapCfg.getRangeEnd())
                    .append(") using default: ").append(DEFAULT_END)
                    .append("\n");
            autoWrapCfg.setRangeEnd(DEFAULT_END);
        }

        if (autoWrapCfg.getRangeEnd() < autoWrapCfg.getRangeStart()) {
            message.append("RangeEnd (").append(autoWrapCfg.getRangeEnd())
                    .append(") less then RangeStart (")
                    .append(autoWrapCfg.getRangeStart())
                    .append(") swapping values\n");
            int tmp = autoWrapCfg.getRangeEnd();
            autoWrapCfg.setRangeEnd(autoWrapCfg.getRangeStart());
            autoWrapCfg.setRangeStart(tmp);
        }

        if (autoWrapCfg.getButtons() == null) {
            autoWrapCfg.setButtons(new ArrayList<WrapButtonCfg>());
            wordWrapEnabled = false;
        }

        // Check buttonCfg values.
        int selectionCnt = 0;
        String selectionLabel = null;
        for (WrapButtonCfg buttonCfg : autoWrapCfg.getButtons()) {
            if (buttonCfg.isSelected()) {
                ++selectionCnt;
                if (selectionCnt == 1) {
                    selectionLabel = buttonCfg.getLabelName();
                } else {
                    buttonCfg.setSelected(false);
                }
            }

            if (buttonCfg.isWrapEnabled()) {
                int wrapCol = buttonCfg.getWrapCol();
                if (wrapCol <= 0) {
                    message.append("Item \"").append(buttonCfg.getLabelName())
                            .append("\" bad WrapCol value (")
                            .append(buttonCfg.getWrapCol())
                            .append(") changing to ").append(DEFAULT_START)
                            .append("\n");
                    buttonCfg.setWrapCol(DEFAULT_START);
                }
            }
        }

        if (selectionCnt == 0 && autoWrapCfg.getButtons().size() > 0) {
            WrapButtonCfg buttonCfg = autoWrapCfg.getButtons().get(0);
            message.append("No button selected. Selecting top item \"")
                    .append(buttonCfg.getLabelName()).append("\"\n");
            buttonCfg.setSelected(true);
        } else if (selectionCnt > 1) {
            message.append(selectionCnt)
                    .append(" items selected; will select item \"")
                    .append(selectionLabel).append("\"\n");
        }

        if (message.length() > 0) {
            message.insert(0, "Autowrap problem(s): ");
            IUFStatusHandler statusHandler = UFStatus
                    .getHandler(TextEditorDialog.class);
            statusHandler.handle(Priority.PROBLEM, message.toString());
        }
        return autoWrapCfg;
    }

    /**
     * Create the Autowrap sub menu items. The Autowrap menu item is located
     * under the Options menu on the top menu bar.
     * 
     * @param autoWrapSubMenu
     *            The Autowrap sub menu.
     */
    private void createAutoWrapSubMenu(Menu autoWrapSubMenu) {
        AutoWrapCfg autoWrapcfg = getAutoWrapCfg();
        for (WrapButtonCfg buttonCfg : autoWrapcfg.getButtons()) {
            MenuItem item = new MenuItem(autoWrapSubMenu, SWT.RADIO);
            item.setText(buttonCfg.getLabelName());
            item.setSelection(buttonCfg.isSelected());
            item.setData(buttonCfg);
            if (buttonCfg.isWrapEnabled()) {
                if (buttonCfg.isSelected()) {
                    charWrapCol = buttonCfg.getWrapCol();
                    wordWrapEnabled = true;
                }
                item.addSelectionListener(new SelectionAdapter() {
                    @Override
                    public void widgetSelected(SelectionEvent event) {
                        MenuItem item = (MenuItem) event.getSource();
                        if (item.getSelection()) {
                            charWrapCol = ((WrapButtonCfg) item.getData())
                                    .getWrapCol();
                            recompileRegex();
                            wordWrapEnabled = true;
                        }
                    }
                });
            } else {
                if (buttonCfg.isSelected()) {
                    wordWrapEnabled = false;
                }
                item.addSelectionListener(new SelectionAdapter() {
                    @Override
                    public void widgetSelected(SelectionEvent event) {
                        MenuItem item = (MenuItem) event.getSource();
                        if (item.getSelection()) {
                            wordWrapEnabled = false;
                        }
                    }
                });
            }
        }

        MenuItem autoWrapOtherItem = new MenuItem(autoWrapSubMenu, SWT.NONE);
        autoWrapOtherItem.setText("Other...");
        final int rangeStart = autoWrapcfg.getRangeStart();
        final int rangeEnd = autoWrapcfg.getRangeEnd();
        autoWrapOtherItem.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                // Create the text character wrap dialog.
                createTextCharWrapDialog(rangeStart, rangeEnd);
            }
        });
    }

    /**
     * Dialog to allow user to create non-standard word wrap column.
     * 
     * @param rangeStart
     * @param rangeEnd
     */
    private void createTextCharWrapDialog(final int rangeStart,
            final int rangeEnd) {
        // Create the text character wrap dialog.
        if (textCharWrapDlg == null || textCharWrapDlg.isDisposed()) {
            textCharWrapDlg = new TextCharWrapDlg(shell, this,
                    otherCharWrapCol, rangeStart, rangeEnd);

            textCharWrapDlg.setCloseCallback(new ICloseCallback() {

                @Override
                public void dialogClosed(Object returnValue) {
                    if ((Boolean) returnValue == true) {
                        recompileRegex();
                        wordWrapEnabled = true;
                    }
                }
            });

            textCharWrapDlg.open();
        } else {
            textCharWrapDlg.bringToTop();
        }
    }

    /**
     * Create the font size sub menu items. The font size menu item is located
     * under the Options menu on the top menu bar.
     * 
     * @param fontSizeSubMenu
     *            The font size sub menu.
     */
    private void createFontSizeSubMenu(Menu fontSizeSubMenu) {
        smallFontItem = new MenuItem(fontSizeSubMenu, SWT.RADIO);
        smallFontItem.setText("Small");
        smallFontItem.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                if (smallFontItem.getSelection()) {
                    textEditor.setFont(smlFont);
                    headerTF.setFont(smlFont);
                }
            }
        });

        mediumFontItem = new MenuItem(fontSizeSubMenu, SWT.RADIO);
        mediumFontItem.setText("Medium");
        mediumFontItem.setSelection(true);
        mediumFontItem.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                if (mediumFontItem.getSelection()) {
                    textEditor.setFont(medFont);
                    headerTF.setFont(medFont);
                }
            }
        });

        largeFontItem = new MenuItem(fontSizeSubMenu, SWT.RADIO);
        largeFontItem.setText("Large");
        largeFontItem.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                if (largeFontItem.getSelection()) {
                    textEditor.setFont(lrgFont);
                    headerTF.setFont(lrgFont);
                }
            }
        });
    }

    /**
     * Initialize the components and put them on the display.
     */
    private void initializeComponents() {
        createTopButtonRow();

        Label sepLbl = new Label(shell, SWT.SEPARATOR | SWT.HORIZONTAL);
        sepLbl.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));

        createTextRow();
        createHeaderTextField();
        createEditorControlButtons();
        createTextAreaEditor();
        createScriptRunnerControlBar();
        createStatusBar();
    }

    /**
     * Create the top row of buttons on the display.
     */
    private void createTopButtonRow() {
        // Create the composite to contain the row of buttons.
        Composite topBtnRowComp = new Composite(shell, SWT.NONE);
        RowLayout rowLayout = new RowLayout();
        rowLayout.marginLeft = 1;
        rowLayout.spacing = 1;
        rowLayout.marginTop = 1;
        topBtnRowComp.setLayout(rowLayout);

        // Add the AFOS Browser button.
        RowData rd = new RowData(110, BUTTON_HEIGHT);
        afosBrowserBtn = new Button(topBtnRowComp, SWT.PUSH);
        afosBrowserBtn.setText("AFOS Browser");
        afosBrowserBtn.setLayoutData(rd);
        afosBrowserBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                displayAfosBrowser();
            }
        });

        // Add the Load History button.
        rd = new RowData(BUTTON_WIDTH, BUTTON_HEIGHT);
        loadHistoryBtn = new Button(topBtnRowComp, SWT.PUSH);
        loadHistoryBtn.setText("Load History");
        loadHistoryBtn.setLayoutData(rd);
        loadHistoryBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                // dispose of current menu
                if (menuMgr != null) {
                    menuMgr.dispose();
                    menuMgr = null;
                }

                List<ICommand> commands = commandHistory.getRecentHistory();
                if (commands.size() > 0) {
                    menuMgr = new MenuManager("#PopupMenu");

                    for (int i = commands.size() - 1; i >= 0; i--) {
                        final ICommand cmd = commands.get(i);
                        menuMgr.add(new Action(TextEditorUtil
                                .getCommandText(cmd)) {
                            @Override
                            public void run() {
                                executeCommand(cmd);
                            }
                        });
                    }

                    Menu menu = menuMgr.createContextMenu(loadHistoryBtn);
                    menu.setVisible(true);
                    loadHistoryBtn.setMenu(menu);
                }
            }
        });

        // Add the WMO Search button.
        rd = new RowData(BUTTON_WIDTH, BUTTON_HEIGHT);
        wmoSearchBtn = new Button(topBtnRowComp, SWT.PUSH);
        wmoSearchBtn.setText("WMO Search");
        wmoSearchBtn.setLayoutData(rd);
        wmoSearchBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                wmoSearch();
            }
        });

        // Add the Enter Editor button.
        rd = new RowData(BUTTON_WIDTH, BUTTON_HEIGHT);
        enterEditorBtn = new Button(topBtnRowComp, SWT.PUSH);
        enterEditorBtn.setText("Enter Editor");
        enterEditorBtn.setLayoutData(rd);
        enterEditorBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                enterEditor();
            }
        });

        // Add the Accumulate text check button.
        rd = new RowData(80, BUTTON_HEIGHT);
        accumChkBtn = new Button(topBtnRowComp, SWT.CHECK);
        accumChkBtn.setText("Accum");
        accumChkBtn.setLayoutData(rd);
        accumChkBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                productQueryJob.setAccumulate(accumChkBtn.getSelection());
            }
        });

        // Add the Update Observation check button.
        rd = new RowData(BUTTON_WIDTH, BUTTON_HEIGHT);
        updateObsChkBtn = new Button(topBtnRowComp, SWT.CHECK);
        updateObsChkBtn.setText("Update Obs");
        updateObsChkBtn.setLayoutData(rd);
        updateObsChkBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                handleUpdateObsChkBtn(updateObsChkBtn.getSelection());
            }
        });

        // Add the Clear button.
        rd = new RowData(SWT.DEFAULT, BUTTON_HEIGHT);
        clearBtn = new Button(topBtnRowComp, SWT.PUSH);
        clearBtn.setText("Clear");
        clearBtn.setLayoutData(rd);
        clearBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                clearTextEditor();
                if (updateObsListener != null) {
                    textEditor.removeMouseListener(updateObsListener);
                    updateObsListener = null;
                }
            }
        });
    }

    /**
     * Create the row of text fields and add them to the display.
     */
    private void createTextRow() {
        // Create the container to hold the labels and text fields.
        Composite textRowComp = new Composite(shell, SWT.NONE);
        RowLayout rowLayout = new RowLayout();
        rowLayout.marginLeft = 1;
        rowLayout.spacing = 1;
        rowLayout.marginTop = 1;
        textRowComp.setLayout(rowLayout);

        // Create a AFOS command label that is vertically aligned.
        TextEditorUtil.createVerticallyCenteredLabel(textRowComp, "AFOS Cmd: ");

        // Add the AFOS command text field.
        RowData rd = new RowData(120, SWT.DEFAULT);
        afosCmdTF = new Text(textRowComp, SWT.BORDER);
        afosCmdTF.setTextLimit(14);
        afosCmdTF.setLayoutData(rd);
        afosCmdTF.addFocusListener(new FocusAdapter() {
            @Override
            public void focusGained(FocusEvent event) {
                clearWmoTF();
                clearAwipsIdTF();
                versionMenuItem.setEnabled(true);
            }

            @Override
            public void focusLost(FocusEvent event) {
                String tmp = afosCmdTF.getText();
                tmp = tmp.trim();
                tmp = tmp.toUpperCase();
                afosCmdTF.setText(tmp);
            }
        });

        afosCmdTF.addVerifyListener(new VerifyListener() {

            @Override
            public void verifyText(VerifyEvent e) {
                e.text = e.text.toUpperCase();
            }
        });

        afosCmdTF.addSelectionListener(new SelectionListener() {
            public void widgetSelected(SelectionEvent event) {

            }

            public void widgetDefaultSelected(SelectionEvent event) {
                String tmp = afosCmdTF.getText();
                tmp = tmp.trim();
                afosCmdTF.setText(tmp);

                AFOSParser parser = new AFOSParser(afosCmdTF.getText(),
                        AfosBrowserModel.getInstance().getLocalSite());
                if (!parser.isValidCommand()) {
                    userInformation("AFOSCMD is invalid");
                    afosCmdTF.setFocus();
                    return;
                }

                TextDisplayModel.getInstance().setAfosCommand(token,
                        parser.getAfosCommand());

                // Perform the query of the product identified by the Afos
                // Command.
                executeCommand(CommandFactory.getAfosCommand(afosCmdTF
                        .getText()));

                // Place cursor back in the Afos Command Field.
                afosCmdTF.setFocus();
            }
        });

        afosCmdTF.addKeyListener(new KeyAdapter() {
            @Override
            public void keyPressed(KeyEvent e) {
                if (e.keyCode == SWT.ARROW_UP) {
                    TextEditorUtil.setCommandField(
                            commandHistory.getPreviousCommand(CommandType.AFOS),
                            afosCmdTF);
                    afosCmdTF.setSelection(afosCmdTF.getText().length());
                } else if (e.keyCode == SWT.ARROW_DOWN) {
                    TextEditorUtil.setCommandField(
                            commandHistory.getNextCommand(CommandType.AFOS),
                            afosCmdTF);
                    afosCmdTF.setSelection(afosCmdTF.getText().length());
                }
            }
        });

        // Create a WMO label that is vertically aligned.
        TextEditorUtil.createVerticallyCenteredLabel(textRowComp,
                "WMO TTAAii CCCC: ");

        // Add the WMO data type and area indicator text field.
        rd = new RowData(55, SWT.DEFAULT);
        wmoTtaaiiTF = new Text(textRowComp, SWT.BORDER);
        wmoTtaaiiTF.setTextLimit(6);
        wmoTtaaiiTF.setLayoutData(rd);
        wmoTtaaiiTF.addFocusListener(new FocusAdapter() {
            @Override
            public void focusGained(FocusEvent event) {
                versionMenuItem.setEnabled(false);
                clearAfosCmdTF();
                clearAwipsIdTF();
            }

            @Override
            public void focusLost(FocusEvent event) {
            }
        });

        wmoTtaaiiTF.addVerifyListener(new VerifyListener() {

            @Override
            public void verifyText(VerifyEvent e) {
                e.text = e.text.toUpperCase();
            }
        });

        wmoTtaaiiTF.addModifyListener(new ModifyListener() {
            public void modifyText(ModifyEvent event) {
                if (wmoTtaaiiTF.getCaretPosition() == wmoTtaaiiTF
                        .getTextLimit()) {
                    ccccTF.setFocus();
                }
            }
        });

        wmoTtaaiiTF.addSelectionListener(new SelectionListener() {
            public void widgetSelected(SelectionEvent event) {

            }

            public void widgetDefaultSelected(SelectionEvent event) {
                wmoTtaaiiTF.setText(wmoTtaaiiTF.getText().toUpperCase());
                ccccTF.setText(ccccTF.getText().toUpperCase());
                wmoSearch();
            }

        });

        wmoTtaaiiTF.addKeyListener(new KeyAdapter() {
            @Override
            public void keyPressed(KeyEvent e) {
                if (e.keyCode == SWT.ARROW_UP) {
                    TextEditorUtil.setCommandField(
                            commandHistory.getPreviousCommand(CommandType.WMO),
                            wmoTtaaiiTF, ccccTF);
                    ccccTF.setSelection(ccccTF.getText().length());
                    wmoTtaaiiTF.setSelection(wmoTtaaiiTF.getText().length());
                } else if (e.keyCode == SWT.ARROW_DOWN) {
                    TextEditorUtil.setCommandField(
                            commandHistory.getNextCommand(CommandType.WMO),
                            wmoTtaaiiTF, ccccTF);
                    ccccTF.setSelection(ccccTF.getText().length());
                    wmoTtaaiiTF.setSelection(wmoTtaaiiTF.getText().length());
                }
            }
        });

        // Add the International location indicator text field.
        rd = new RowData(45, SWT.DEFAULT);
        ccccTF = new Text(textRowComp, SWT.BORDER);
        ccccTF.setTextLimit(4);
        ccccTF.setLayoutData(rd);
        ccccTF.addFocusListener(new FocusAdapter() {
            @Override
            public void focusGained(FocusEvent event) {
                versionMenuItem.setEnabled(false);
                clearAfosCmdTF();
                clearAwipsIdTF();
            }

            @Override
            public void focusLost(FocusEvent event) {
            }
        });

        ccccTF.addVerifyListener(new VerifyListener() {

            @Override
            public void verifyText(VerifyEvent e) {
                e.text = e.text.toUpperCase();
            }
        });

        ccccTF.addSelectionListener(new SelectionListener() {
            public void widgetSelected(SelectionEvent event) {

            }

            public void widgetDefaultSelected(SelectionEvent event) {
                wmoTtaaiiTF.setText(wmoTtaaiiTF.getText().toUpperCase());
                ccccTF.setText(ccccTF.getText().toUpperCase());
                wmoSearch();
            }

        });

        ccccTF.addKeyListener(new KeyAdapter() {
            @Override
            public void keyPressed(KeyEvent e) {
                if (e.keyCode == SWT.ARROW_UP) {
                    TextEditorUtil.setCommandField(
                            commandHistory.getPreviousCommand(CommandType.WMO),
                            wmoTtaaiiTF, ccccTF);
                    wmoTtaaiiTF.setSelection(wmoTtaaiiTF.getText().length());
                    ccccTF.setSelection(ccccTF.getText().length());
                } else if (e.keyCode == SWT.ARROW_DOWN) {
                    TextEditorUtil.setCommandField(
                            commandHistory.getNextCommand(CommandType.WMO),
                            wmoTtaaiiTF, ccccTF);
                    wmoTtaaiiTF.setSelection(wmoTtaaiiTF.getText().length());
                    ccccTF.setSelection(ccccTF.getText().length());
                }
            }
        });

        // Create an AWIPS ID label that is vertically aligned.
        TextEditorUtil.createVerticallyCenteredLabel(textRowComp, "AWIPS ID: ");

        // Add the AWIPS ID text field.
        rd = new RowData(65, SWT.DEFAULT);
        awipsIdTF = new Text(textRowComp, SWT.BORDER);
        awipsIdTF.setTextLimit(6);
        awipsIdTF.setLayoutData(rd);
        awipsIdTF.addFocusListener(new FocusAdapter() {
            @Override
            public void focusGained(FocusEvent event) {
                versionMenuItem.setEnabled(false);
                clearAfosCmdTF();
                clearWmoTF();
            }

            @Override
            public void focusLost(FocusEvent event) {
            }
        });

        awipsIdTF.addVerifyListener(new VerifyListener() {

            @Override
            public void verifyText(VerifyEvent e) {
                e.text = e.text.toUpperCase();
            }
        });

        awipsIdTF.addSelectionListener(new SelectionListener() {
            public void widgetSelected(SelectionEvent event) {

            }

            public void widgetDefaultSelected(SelectionEvent event) {
                awipsIdTF.setText(awipsIdTF.getText().trim().toUpperCase());
                int charCount = awipsIdTF.getCharCount();
                if (charCount < 4 || charCount > 6) {
                    userInformation("Must enter a 4 to 6 character AWIPS ID");
                    awipsIdTF.setFocus();
                    return;
                } else {
                    TextDisplayModel.getInstance().setProductCategory(token,
                            awipsIdTF.getText(0, 2));
                    TextDisplayModel.getInstance().setProductDesignator(token,
                            awipsIdTF.getText(3, charCount - 1));
                }
                // Highlight the text contained in the Awips ID Field.
                awipsIdTF.selectAll();

                // Perform the query of the product identified by the Awips ID.
                // TODO Generate and AWIPSCommand
                ICommand command = CommandFactory.getAwipsCommand(awipsIdTF
                        .getText());
                executeCommand(command);
            }

        });

        awipsIdTF.addKeyListener(new KeyAdapter() {
            @Override
            public void keyPressed(KeyEvent e) {
                if (e.keyCode == SWT.ARROW_UP) {
                    TextEditorUtil.setCommandField(commandHistory
                            .getPreviousCommand(CommandType.AWIPS), awipsIdTF);
                    awipsIdTF.setSelection(awipsIdTF.getText().length());
                } else if (e.keyCode == SWT.ARROW_DOWN) {
                    TextEditorUtil.setCommandField(
                            commandHistory.getNextCommand(CommandType.AWIPS),
                            awipsIdTF);
                    awipsIdTF.setSelection(awipsIdTF.getText().length());
                }
            }
        });

    }

    /**
     * Handles clearing of the 'Update Obs' button. Sets the state of the button
     * to "unchecked" and cancels notifications.
     */
    private void clearUpdateObs() {
        boolean checked = updateObsChkBtn.getSelection();
        updateObsChkBtn.setSelection(false);
        if (checked) {
            handleUpdateObsChkBtn(false);
        }
    }

    /**
     * Handles the actual click on the 'Update Obs' check box. When the 'Update
     * Obs' box is checked and an ob is displayed, new obs will be displayed as
     * they are received. If the 'Accum' box is checked, the new ob will be
     * appended.
     * 
     * @param checked
     *            state of the check box
     */
    private void handleUpdateObsChkBtn(boolean checked) {
        if (checked) {
            if (updateObsListener == null) {
                updateObsListener = new MouseListener() {

                    @Override
                    public void mouseUp(MouseEvent e) {
                        try {
                            int offset = textEditor
                                    .getOffsetAtLocation(new Point(e.x, e.y));
                            clearUpdateFlag(offset);
                        } catch (IllegalArgumentException ex) {
                            // bad mouse location ignore
                        }
                    }

                    @Override
                    public void mouseDown(MouseEvent e) {
                        // Ignore
                    }

                    @Override
                    public void mouseDoubleClick(MouseEvent e) {
                        // Ignore
                    }
                };
                textEditor.addMouseListener(updateObsListener);
            }
            NotificationManagerJob.addObserver(ALARM_ALERT_TOPIC, this);
        } else {
            NotificationManagerJob.removeObserver(ALARM_ALERT_TOPIC, this);
        }

    }

    /**
     * Clear the colors associated with any style range at the given offset.
     * 
     * @param offset
     */
    private void clearUpdateFlag(int offset) {
        for (StyleRange range : textEditor.getStyleRanges()) {
            if (range.start <= offset && offset < (range.start + range.length)) {
                StyleRange lock = (StyleRange) range.clone();
                lock.background = null;
                lock.foreground = null;
                textEditor.setStyleRange(lock);
                break;
            }
        }
    }

    /**
     * Clear the AFOS Cmd text and adjust history.
     */
    private void clearAfosCmdTF() {
        if (!afosCmdTF.isDisposed()) {
            afosCmdTF.setText("");
            commandHistory.resetIndex(CommandType.AFOS);
        }
    }

    /**
     * Clear the WMO TTAAii CCCC fields and ajust history.
     */
    private void clearWmoTF() {
        if (!wmoTtaaiiTF.isDisposed()) {
            wmoTtaaiiTF.setText("");
            ccccTF.setText("");
            commandHistory.resetIndex(CommandType.WMO);
        }
    }

    /**
     * Clear AWIPS ID: field and adjust history.
     */
    private void clearAwipsIdTF() {
        if (!awipsIdTF.isDisposed()) {
            awipsIdTF.setText("");
            commandHistory.resetIndex(CommandType.AWIPS);
        }
    }

    /**
     * Create the header text field.
     */
    private void createHeaderTextField() {
        GridData gd = new GridData(GridData.FILL_HORIZONTAL);
        headerTFComp = new Composite(shell, SWT.NONE);
        GridLayout gridLayout = new GridLayout(1, false);
        headerTFComp.setLayout(gridLayout);
        gd.exclude = true;
        headerTFComp.setLayoutData(gd);
        headerTFComp.setVisible(false);

        gd = new GridData(GridData.FILL_HORIZONTAL);
        gd.heightHint = 40;
        headerTF = new Text(headerTFComp, SWT.BORDER | SWT.MULTI
                | SWT.READ_ONLY);
        headerTF.setLayoutData(gd);
        headerTF.setFont(medFont);
        headerTF.setEditable(false);

        headerTFComp.layout();
    }

    /**
     * Create editor control buttons.
     */
    private void createEditorControlButtons() {
        // Create the container for the editor buttons.
        editorBtnRowComp = new Composite(shell, SWT.NONE);
        GridLayout gridLayout = new GridLayout(10, false);
        gridLayout.marginLeft = 1;
        gridLayout.marginTop = 1;
        editorBtnRowComp.setLayout(gridLayout);
        GridData gd = new GridData();
        gd.exclude = true;
        editorBtnRowComp.setLayoutData(gd);
        editorBtnRowComp.setVisible(false);

        // Add the Save button.
        gd = new GridData(EDITOR_BTN_WIDTH_SML, BUTTON_HEIGHT);
        editorSaveBtn = new Button(editorBtnRowComp, SWT.PUSH);
        editorSaveBtn.setText("Save");
        editorSaveBtn.setLayoutData(gd);
        editorSaveBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                saveProduct();
            }
        });

        // Add the Cut button.
        gd = new GridData(EDITOR_BTN_WIDTH_SML, BUTTON_HEIGHT);
        editorCutBtn = new Button(editorBtnRowComp, SWT.PUSH);
        editorCutBtn.setText("Cut");
        editorCutBtn.setLayoutData(gd);
        editorCutBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                cutText();
            }
        });

        // Add the Copy button.
        gd = new GridData(EDITOR_BTN_WIDTH_SML, BUTTON_HEIGHT);
        editorCopyBtn = new Button(editorBtnRowComp, SWT.PUSH);
        editorCopyBtn.setText("Copy");
        editorCopyBtn.setLayoutData(gd);
        editorCopyBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                copyText();
            }
        });

        // Add the Paste button.
        gd = new GridData(EDITOR_BTN_WIDTH_SML, BUTTON_HEIGHT);
        editorPasteBtn = new Button(editorBtnRowComp, SWT.PUSH);
        editorPasteBtn.setText("Paste");
        editorPasteBtn.setLayoutData(gd);
        editorPasteBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                pasteText();
            }
        });

        // Add the Fill button.
        gd = new GridData(EDITOR_BTN_WIDTH_SML, BUTTON_HEIGHT);
        editorFillBtn = new Button(editorBtnRowComp, SWT.PUSH);
        editorFillBtn.setText("Fill");
        editorFillBtn.setLayoutData(gd);
        editorFillBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                fillText();
            }
        });

        // Add the Edit Header button.
        gd = new GridData(EDITOR_BTN_WIDTH_LRG, BUTTON_HEIGHT);
        editorEditHeaderBtn = new Button(editorBtnRowComp, SWT.PUSH);
        editorEditHeaderBtn.setText("Edit Header");
        editorEditHeaderBtn.setLayoutData(gd);
        editorEditHeaderBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                // Edit the header block of the text product.
                editHeader("", false);
            }
        });

        // Add the Send button.
        gd = new GridData(EDITOR_BTN_WIDTH_SML, BUTTON_HEIGHT);
        editorSendBtn = new Button(editorBtnRowComp, SWT.PUSH);
        editorSendBtn.setText("Send");
        editorSendBtn.setLayoutData(gd);
        editorSendBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                sendProduct(false);
            }
        });

        // Add the Cancel button.
        gd = new GridData(EDITOR_BTN_WIDTH_SML, BUTTON_HEIGHT);
        editorCancelBtn = new Button(editorBtnRowComp, SWT.PUSH);
        editorCancelBtn.setText("Cancel");
        editorCancelBtn.setLayoutData(gd);
        editorCancelBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                cancelEditor(true);
            }
        });

        // Add the Attach button.
        gd = new GridData(EDITOR_BTN_WIDTH_SML, BUTTON_HEIGHT);
        editorAttachBtn = new Button(editorBtnRowComp, SWT.PUSH);
        editorAttachBtn.setText("Attach");
        editorAttachBtn.setLayoutData(gd);
        editorAttachBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                attachFile();
            }
        });

        // Add the Insert/Overwrite combo box.
        gd = new GridData(EDITOR_BTN_WIDTH_MED, SWT.DEFAULT);
        editorInsertCmb = new Combo(editorBtnRowComp, SWT.DROP_DOWN
                | SWT.READ_ONLY);
        editorInsertCmb.add("INS");
        editorInsertCmb.add("OVR");
        editorInsertCmb.select(INSERT_TEXT);
        editorInsertCmb.setLayoutData(gd);
        editorInsertCmb.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                if (editorInsertCmb.getSelectionIndex() == INSERT_TEXT
                        && overwriteMode == true) {
                    textEditor.invokeAction(ST.TOGGLE_OVERWRITE);
                    overwriteMode = false;
                    overStrikeItem.setSelection(false);
                } else if (editorInsertCmb.getSelectionIndex() == OVERWRITE_TEXT
                        && overwriteMode == false) {
                    textEditor.invokeAction(ST.TOGGLE_OVERWRITE);
                    overwriteMode = true;
                    overStrikeItem.setSelection(true);
                }
                textEditor.setFocus();
            }
        });

        editorBtnRowComp.layout();
    }

    /**
     * Create the text editor (styled text) control.
     */
    private void createTextAreaEditor() {
        GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        textEditorComp = new Composite(shell, SWT.NONE);
        GridLayout gridLayout = new GridLayout(1, false);
        textEditorComp.setLayout(gridLayout);
        textEditorComp.setLayoutData(gd);

        textEditor = new StyledText(textEditorComp, SWT.BORDER | SWT.MULTI
                | SWT.V_SCROLL | SWT.H_SCROLL);
        gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        textEditor.setFont(medFont);
        GC gc = new GC(textEditor);
        FontMetrics fm = gc.getFontMetrics();
        gc.dispose();
        int width = EDITOR_WIDTH * fm.getAverageCharWidth();
        gd.widthHint = width;

        textEditor.setLayoutData(gd);
        textEditor.setWordWrap(false);
        textEditor.setEditable(false);
        textEditor.setKeyBinding(SWT.INSERT, SWT.NULL); // DR 7826

        textEditor.addKeyListener(new KeyAdapter() {
            @Override
            public void keyPressed(KeyEvent e) {
                if (e.keyCode == SWT.ARROW_LEFT && !textEditor.getEditable()) {
                    commandHistory.resetIndex(CommandType.AFOS);
                    ICommand command = commandHistory
                            .getPreviousCommand(CommandType.AFOS);
                    if (command != null) {
                        ICommand cmd = CommandFactory
                                .getPreviousForAfosCommand(command);
                        if (cmd.isValid()) {
                            executeCommand(cmd);
                        }
                    }
                } else if (e.keyCode == SWT.ARROW_RIGHT
                        && !textEditor.getEditable()) {
                    commandHistory.resetIndex(CommandType.AFOS);
                    ICommand command = commandHistory
                            .getPreviousCommand(CommandType.AFOS);
                    if (command != null) {
                        ICommand cmd = CommandFactory
                                .getNextForAfosCommand(command);
                        if (cmd.isValid()) {
                            executeCommand(cmd);
                        }
                    }
                }
            }
        });

        textEditor.addExtendedModifyListener(new ExtendedModifyListener() {

            @Override
            public void modifyText(ExtendedModifyEvent event) {
                eventStart = event.start;
                if (userKeyPressed) {
                    userKeyPressed = false;
                    rewrap(eventStart, eventStart);
                }
            }

        });

        // TODO - Use this to convert tabs to spaces and extra spaces in the
        // middle of a line to a single space?
        // textEditor.addVerifyListener(new VerifyListener() {
        //
        // @Override
        // public void verifyText(VerifyEvent e) {
        // // TODO Auto-generated method stub
        // if (performingWrap) {
        // return;
        // }
        // System.out.println("start: " + e.start + ", end: " + e.end
        // + " text:'" + e.text + "'");
        //
        // }
        // });

        textEditor.addVerifyKeyListener(new VerifyKeyListener() {
            public void verifyKey(VerifyEvent event) {
                // Ignore edit keys when not in edit mode.
                if (textEditor.getEditable() == false) {
                    return;
                }
                if (event.keyCode == SWT.DEL || event.keyCode == SWT.BS
                        || event.keyCode == SWT.SHIFT) {
                    // Do nothing...
                    // We need to capture the Delete, Backspace and Shift
                    // keystrokes...
                } else if (event.keyCode == SWT.HOME
                        || event.keyCode == SWT.END) {
                    if (!textEditor.getEditable()) {
                        int offset = 0;

                        if (event.keyCode == SWT.END) {
                            offset = textEditor.getCharCount();
                        }

                        textEditor.setCaretOffset(offset);
                        textEditor.showSelection();
                        event.doit = false;
                    }
                } else if (event.keyCode == SWT.PAGE_UP
                        && event.stateMask == SWT.CTRL) {
                    event.doit = false; // Ingnore Ctrl + PageUp
                } else if (event.keyCode == SWT.PAGE_DOWN
                        && event.stateMask == SWT.CTRL) {
                    event.doit = false; // Ignore Ctrl + PageDown
                } else if (event.keyCode == SWT.PAGE_UP
                        && event.stateMask == (SWT.CTRL | SWT.SHIFT)) {
                    event.doit = false; // Ignore Ctrl+Shift+PageUp
                } else if (event.keyCode == SWT.PAGE_DOWN
                        && event.stateMask == (SWT.CTRL | SWT.SHIFT)) {
                    event.doit = false; // Ignore Ctrl+Shift+PageDown
                } else if (event.keyCode == SWT.INSERT) {
                    // Ins key on the keypad
                    if (overwriteMode == true) {
                        overwriteMode = false;
                        overStrikeItem.setSelection(false);
                        editorInsertCmb.select(INSERT_TEXT);
                    } else {
                        overwriteMode = true;
                        overStrikeItem.setSelection(true);
                        editorInsertCmb.select(OVERWRITE_TEXT);
                    }
                    textEditor.invokeAction(ST.TOGGLE_OVERWRITE);
                } else if (event.keyCode > 500) {
                    // Do nothing...
                    // We need to capture the non-alphanumeric editing-related
                    // keystrokes...
                }

                // if some event is going to happen and the key was not enter
                // then set userKeyPressed to true
                if (event.doit && event.character != 0
                        && event.character != '\r' && event.character != '\n') {
                    userKeyPressed = true;
                }
            }
        });
        textEditor.addModifyListener(new ModifyListener() {

            @Override
            public void modifyText(ModifyEvent e) {
                // when we modify the text, we want to set the 'dirty' flag.
                dirty = true;
            }
        });

        textEditor.addMouseListener(new MouseListener() {
            @Override
            public void mouseDoubleClick(MouseEvent e) {
            }

            @Override
            public void mouseDown(org.eclipse.swt.events.MouseEvent e) {
                if (e.button == 2) {
                    int cCaretOffset = textEditor.getCaretOffset();
                    boolean valid = true;
                    try {
                        // intentionally convert the current mouse location
                        // relative to textEditor component. if an exception is
                        // thrown then the current mouse current is at an
                        // invalid location; outside the scope the text within
                        // the textEditor component.
                        textEditor.getOffsetAtLocation(new Point(e.x, e.y));
                    } catch (IllegalArgumentException iae) {
                        textEditor.setCaretOffset(cCaretOffset);
                        valid = false;
                    }

                    if (valid) {
                        if (isSaoMetarFlag) {
                            Point p = textEditor.getLocation();
                            textEditor.setLocation(p);
                            displayAirportTooltip(textEditor);
                        }
                        if (inEditMode) {
                            rewrap(eventStart, eventStart);
                        }
                    }
                }

                if (e.button == 3) {
                    processPopup();
                }
            }

            @Override
            public void mouseUp(org.eclipse.swt.events.MouseEvent e) {
                textEditor.setToolTipText(null);
            }
        });
    }

    /**
     * Process the user choice from the popup list. DR14842 - re-written
     */
    private void processPopup() {
        Menu menu = new Menu(shell, SWT.POP_UP);
        List<String> items = Arrays.asList(popupItems);
        for (String pi : popupItems) {
            MenuItem mi = new MenuItem(menu, SWT.PUSH);
            mi.setText(pi);
            if (isEditMode()) {
                mi.setEnabled(true);
            } else {
                mi.setEnabled(isPopItemDefault[items.indexOf(pi)]);
            }
            mi.addListener(SWT.Selection, new Listener() {
                public void handleEvent(Event event) {
                    handleSelection(event);
                }
            });
        }
        menu.setVisible(true);
    }

    /**
     * Handle the selection from the popup menu
     * 
     * @param event
     */
    protected void handleSelection(Event event) {
        MenuItem item = (MenuItem) event.widget;
        String choice = item.getText();
        if (choice != null) {
            if (popupItems[0].equals(choice)) {
                textEditor.selectAll();
            } else if (popupItems[1].equals(choice)) {
                cutText();
            } else if (popupItems[2].equals(choice)) {
                copyText();
            } else if (popupItems[3].equals(choice)) {
                pasteText();
            }
            textEditor.update();
        }
    }

    /**
     * creates the bar containing the script runner controls.
     */
    private void createScriptRunnerControlBar() {
        scriptRunnerComp = new Composite(shell, SWT.NONE);
        GridLayout layout = new GridLayout(4, false);
        scriptRunnerComp.setLayout(layout);
        scriptRunnerComp.setLayoutData(new GridData(
                GridData.HORIZONTAL_ALIGN_END));

        GridData gd = new GridData();
        gd.widthHint = 100;
        gd.horizontalAlignment = SWT.RIGHT;
        scriptContinueBtn = new Button(scriptRunnerComp, SWT.PUSH);
        scriptContinueBtn.setText("Continue");
        scriptContinueBtn.setLayoutData(gd);
        scriptContinueBtn.addSelectionListener(new SelectionListener() {
            @Override
            public void widgetDefaultSelected(SelectionEvent e) {
                // intentionally empty
            }

            @Override
            public void widgetSelected(SelectionEvent e) {
                onContinueScript();
            }
        });

        scriptSkipWaitBtn = new Button(scriptRunnerComp, SWT.PUSH);
        scriptSkipWaitBtn.setText("Skip Wait");
        scriptSkipWaitBtn.setLayoutData(gd);
        scriptSkipWaitBtn.addSelectionListener(new SelectionListener() {
            @Override
            public void widgetDefaultSelected(SelectionEvent e) {
                // intentionally empty
            }

            @Override
            public void widgetSelected(SelectionEvent e) {
                onSkipWait();
            }
        });

        scriptCancelBtn = new Button(scriptRunnerComp, SWT.PUSH);
        scriptCancelBtn.setText("Cancel");
        scriptCancelBtn.setLayoutData(gd);
        scriptCancelBtn.addSelectionListener(new SelectionListener() {
            @Override
            public void widgetDefaultSelected(SelectionEvent e) {
                // intentionally empty
            }

            @Override
            public void widgetSelected(SelectionEvent e) {
                onCancelScript();
            }
        });
    }

    /**
     * creates composite with status label.
     */
    private void createStatusBar() {
        Composite comp = new Composite(shell, SWT.NONE);
        GridLayout layout = new GridLayout(1, false);
        comp.setLayout(layout);
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        comp.setLayoutData(gd);

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        statusBarLabel = new Label(comp, SWT.BORDER);
        statusBarLabel.setLayoutData(gd);
    }

    /**
     * Enter the text editor mode.
     */
    private void enterEditor() {
        StdTextProduct product = TextDisplayModel.getInstance()
                .getStdTextProduct(token);
        if (product != null
                && gfeForbidden(product.getCccid(), product.getNnnid())) {
            // Pop up forbidden window.
            inEditMode = false;
            userInformation("This product MUST be edited in GFE! \n Please exit and return to GFE. \n Action Aborted!");
            return;
        }
        // Set the edit mode flag to true.
        inEditMode = true;
        int ranges[] = textEditor.getRanges();
        if (ranges == null || ranges.length == 0) {
            originalText = removeSoftReturns(textEditor.getText());
        } else {
            textEditor.setText(originalText);
        }
        // Capture the contents of the current header section and the body
        // section
        setCurrentHeaderAndBody();

        // Mark the uneditable warning text
        if (markUneditableText(textEditor)) {
            // Add listener to monitor attempt to edit locked text
            textEditor.addVerifyListener(TextEditorDialog.this);
        }

        // Set the menu buttons to reflect the edit mode.
        editorButtonMenuStates(inEditMode);

        // Show the header text field.
        headerTFComp.setVisible(true);
        ((GridData) headerTFComp.getLayoutData()).exclude = false;

        // Show the editor control buttons.
        editorBtnRowComp.setVisible(true);
        ((GridData) editorBtnRowComp.getLayoutData()).exclude = false;

        headerTFComp.getParent().layout();
        editorBtnRowComp.layout();

        // Set the search and replace dialog to reflect the edit state.
        if (searchReplaceDlg != null) {
            searchReplaceDlg.setEditMode(inEditMode);
        }

        if (shell.getShells().length > 0) {
            shell.getShells()[0].close();
        }

        // Edit the header block of the text product.
        editHeader("warning", true);
    }

    /**
     * Cancel the editor mode.
     * 
     * @param prompt
     *            true if prompting desired
     */
    private boolean cancelEditor(boolean prompt) {
        if (isDisposed()) {
            // If the shell has been disposed due to closing of a parent,
            // do not follow the normal cancel procedure.
            return false;
        }
        if (prompt && inEditMode) {
            // Notify the user that any unsaved changes will be lost.
            SWTMessageBox mb = new SWTMessageBox(shell, "Cancel Editor",
                    "Any unsaved changes will be lost. Cancel anyway?",
                    SWT.ICON_QUESTION | SWT.YES | SWT.NO | SWT.ICON_WARNING);
            mb.setCloseCallback(new ICloseCallback() {

                @Override
                public void dialogClosed(Object returnValue) {
                    if (returnValue instanceof Integer) {
                        int rval = (Integer) returnValue;
                        if (rval == SWT.YES) {
                            doCancelEditor();
                            if (cancelDoClose) {
                                hide();
                            }
                        }
                        cancelDoClose = false;
                    }
                }
            });

            mb.open();
            return false;
        }
        return doCancelEditor();
    }

    /**
     * Attempt to cancel the current editing session.
     * 
     * @return true if cancel performed otherwise false.
     */
    private boolean doCancelEditor() {

        stopAutoSave();

        if (warnGenFlag && queuedProduct != null) {
            // Display the WarnGen in the queue, perform the popup and stop the
            // cancel.
            showWarngenProduct(queuedProduct, queuedNotify);
            queuedNotify = null;
            queuedProduct = null;
            return false;
        }
        // Set the edit mode flag
        inEditMode = false;

        // Update the "now editing" title of the text editor window.
        updateNowEditingTitle();

        // Clear buttonology if there was no product loaded or saved.
        if (!TextDisplayModel.getInstance().hasStdTextProduct(token)) {
            clearButtonology();
            // Otherwise update buttonology per the product loaded or saved.
        } else {
            updateButtonology(TextDisplayModel.getInstance().getAfosPil(token));
            setAfosCmdField("");
        }

        // Set the menu buttons to reflect the edit mode.
        editorButtonMenuStates(inEditMode);

        // Hide the header text field.
        headerTFComp.setVisible(false);
        ((GridData) headerTFComp.getLayoutData()).exclude = true;

        // Hide the editor control buttons.
        editorBtnRowComp.setVisible(false);
        ((GridData) editorBtnRowComp.getLayoutData()).exclude = true;

        headerTFComp.getParent().layout();
        editorBtnRowComp.layout();

        // Enable the Enter Editor button.
        enterEditorBtn.setEnabled(true);

        // Set the search and replace dialog to reflect the edit state.
        if (searchReplaceDlg != null) {
            searchReplaceDlg.setEditMode(inEditMode);
        }

        // Restore the contents of the text editor if needed
        // Note: this block conpensates for the unusual behavior of the AWIPS-I
        // cancel editor functionality. Basically, once a product has been saved
        // cancelling DOES NOT roll back changes in AWIPS-I!
        if (saved) {
            replaceWorkProductId();
            originalText = combineOriginalMessage();
        }

        // update editor status flags
        dirty = false;

        if (originalText != null) {
            textEditor.setText(originalText);
        }
        // Mark the uneditable warning text
        if (markUneditableText(textEditor)) {
            // Add listener to monitor attempt to edit locked text
            textEditor.addVerifyListener(TextEditorDialog.this);
        }

        // Eliminate the lockable text listener since the application is no
        // longer in edit mode for the warning product that was being edited.
        if (markedTextUndeditable) {
            textEditor.removeVerifyListener(TextEditorDialog.this);
            markedTextUndeditable = false;
        }

        // Setting focus back to a button, which was the last window control bar
        // button pressed, seems to allow SWT to then allow listeners on text
        // fields to function properly. If focus is not set to a button, then
        // the text fields' listeners do not activate properly.
        enterEditorBtn.setFocus();

        statusBarLabel.setText("");
        attachedFile = null;
        attachedFilename = null;

        return true;
    }

    /**
     * Edit the header block of the text product.
     * 
     * @param warning
     *            The warning
     * @param closeEditorOnCancel
     *            True if the text editor should be closed on a cancel action,
     *            false otherwise.
     */
    private void editHeader(String warning, boolean closeEditorOnCancel) {
        if (headerEditSession != null)
            return;

        // Create and display the AWIPS header block dialog.
        AWIPSHeaderBlockDlg awipsHeaderBlockDlg = new AWIPSHeaderBlockDlg(
                shell, this);

        headerEditSession = closeEditorOnCancel ? HeaderEditSession.CLOSE_ON_EXIT
                : HeaderEditSession.IN_EDITOR;

        awipsHeaderBlockDlg.open();
        // headerBlockDlgDismissed() is called when the dialog is dismissed.
    }

    /**
     * Called by AWIPSHeaderBlockDlg when it is dismissed.
     * 
     * @param dialogResult
     *            True if header block editor was dismissed by clicking the
     *            "Enter" button, false otherwise.
     */
    public void headerBlockDlgDismissed(boolean dialogResult) {
        HeaderEditSession lastSession = headerEditSession;
        headerEditSession = null;

        // If the user cancels the AWIPS header block dialog then
        // get out of edit mode.
        // Otherwise use the node, product category, and product designator.

        boolean editing = false;

        if (dialogResult == true) {

            TextDisplayModel tdm = TextDisplayModel.getInstance();

            // Update the buttonology.
            updateButtonology(tdm.getAfosPil(token));
            String bbbid = tdm.getBbbId(token);

            String nnnxxx = workProductId != null ? workProductId : tdm
                    .getProductCategory(token)
                    + tdm.getProductDesignator(token);
            // Set the header text field.
            if (bbbid.equals("NOR")) {
                String wmoId = tdm.getWmoId(token);
                wmoId = (wmoId.length() > 0 ? wmoId : "-");
                String siteId = tdm.getSiteId(token);
                siteId = (siteId.length() > 0 ? siteId : "-");
                setHeaderTextField(wmoId, siteId, currentDateId, "\n", nnnxxx);
            } else {
                setHeaderTextField(tdm.getWmoId(token), tdm.getSiteId(token),
                        bbbid.length() > 0 ? currentDateId + " " + bbbid
                                : currentDateId, "\n", nnnxxx);
            }

            // Update the "now editing" title of the text editor window.
            updateNowEditingTitle();

            editing = true;
        } else {
            if (lastSession == HeaderEditSession.CLOSE_ON_EXIT)
                editing = !cancelEditor(false);
        }

        if (lastSession == HeaderEditSession.CLOSE_ON_EXIT)
            if (editing) {
                StdTextProduct product = TextDisplayModel.getInstance()
                        .getStdTextProduct(token);
                if (product == null)
                    return;
                if (autoSave == null) {
                    // user can cancel the edit immediately when the header is
                    // displayed, verify it was not cancelled before starting
                    // the
                    // autoSave task.
                    autoSave = new AutoSaveTask(product.getWmoid(),
                            product.getSite());
                }
            } else {
                stopAutoSave();
            }
    }

    /**
     * Enable/Disable menus, menu items, text fields, and buttons when entering
     * or exiting editor mode.
     * 
     * @param inEditMode
     *            Flag indicating if the text editor is in edit mode.
     */
    private void editorButtonMenuStates(boolean inEditMode) {
        closeItem.setEnabled(!inEditMode);

        // -----------------------------------
        // Top buttons on the display
        // Disabled when in editor mode
        // -----------------------------------
        afosBrowserBtn.setEnabled(!inEditMode);
        loadHistoryBtn.setEnabled(!inEditMode);
        wmoSearchBtn.setEnabled(!inEditMode);
        enterEditorBtn.setEnabled(!inEditMode);
        accumChkBtn.setEnabled(!inEditMode);
        updateObsChkBtn.setEnabled(!inEditMode);
        clearBtn.setEnabled(!inEditMode);

        // ---------------------------------
        // File Menu menu items
        // Disabled when in editor mode
        // ---------------------------------
        AFOSBrowserItem.setEnabled(!inEditMode);
        enterEditorItem.setEnabled(!inEditMode);
        importFromFileItem.setEnabled(!inEditMode);
        recoverEditSessionItem.setEnabled(!inEditMode);

        // ---------------------------------
        // File Menu menu items
        // Disabled when in editor mode
        // ---------------------------------
        resendWarningProductnItem.setEnabled(!inEditMode
                && textEditor.getText() != null
                && textEditor.getText().length() > 0);

        // ---------------------------------
        // File Menu menu items
        // Enabled when in editor mode
        // ---------------------------------
        saveItem.setEnabled(inEditMode);
        sendExitEditorItem.setEnabled(inEditMode);
        cancelEditorItem.setEnabled(inEditMode);

        // ---------------------------------
        // Edit Menu menu items
        // Disabled when in editor mode
        // ---------------------------------
        clearWindowItem.setEnabled(!inEditMode);
        clearUpdateFlagsItem.setEnabled(!inEditMode);

        // ---------------------------------
        // Edit Menu menu items
        // Enabled when in editor mode
        // ---------------------------------
        cutItem.setEnabled(inEditMode);
        pasteItem.setEnabled(inEditMode);
        fillItem.setEnabled(inEditMode);
        deleteSubMenuItem.setEnabled(inEditMode);
        undeleteSubMenuItem.setEnabled(inEditMode);
        clearSelectionItem.setEnabled(inEditMode);
        editHeaderItem.setEnabled(inEditMode);
        spellCheckerItem.setEnabled(inEditMode);

        // ---------------------------------
        // Options Menu menu items
        // Enabled when in editor mode
        // ---------------------------------
        overStrikeItem.setEnabled(inEditMode);

        // ---------------------------------
        // Menu bar menus
        // Disable menu bar menus
        // ---------------------------------
        versionMenuItem.setEnabled(!inEditMode);
        toolsMenuItem.setEnabled(!inEditMode);
        productsMenuItem.setEnabled(!inEditMode);

        // ------------------------------------
        // Text fields
        // Disable the text fields
        // ------------------------------------
        afosCmdTF.setEnabled(!inEditMode);
        wmoTtaaiiTF.setEnabled(!inEditMode);
        ccccTF.setEnabled(!inEditMode);
        awipsIdTF.setEnabled(!inEditMode);

        // ------------------------------------
        // Enable the text editor when in
        // edit mode.
        // ------------------------------------
        textEditor.setEditable(inEditMode);
    }

    /**
     * Cut selected text from the text editor and put it in the clipboard.
     */
    private void cutText() {
        if (textEditor.getSelectionCount() == 0) {
            MessageBox mb = new MessageBox(shell, SWT.ICON_ERROR | SWT.OK);
            mb.setText("Cut Error");
            mb.setMessage("You must first select text to\n"
                    + "cut (by dragging, for example).");
            mb.open();
            return;
        }

        Point p = textEditor.getSelectionRange();
        copyText();
        textEditor.replaceTextRange(p.x, p.y, "");
        rewrap(p.x, p.x);
    }

    /**
     * Copy Selected text from the editor and put it in the clipboard.
     */
    private void copyText() {
        if (textEditor.getSelectionCount() == 0) {
            MessageBox mb = new MessageBox(shell, SWT.ICON_ERROR | SWT.OK);
            mb.setText("Copy Error");
            mb.setMessage("You must first select text to\n"
                    + "copy (by dragging, for example).");
            mb.open();
            return;
        }

        // Remove soft line wraps before placeing in clipboard.
        String selection = textEditor.getSelectionText().replace("\r\n", "");
        TextTransfer textTransfer = TextTransfer.getInstance();
        clipboard.setContents(new Object[] { selection },
                new Transfer[] { textTransfer });
    }

    /**
     * Print all text from the text editor to the default printer.
     */
    private void printAllText() {
        FontData fontData = textEditor.getFont().getFontData()[0];
        PrintDisplay.print(textEditor.getText(), fontData, charWrapCol,
                statusHandler);
    }

    /**
     * Print selected text from the text editor to the default printer.
     */
    private void printSelectedText() {
        if (textEditor.getSelectionCount() == 0) {
            MessageBox mb = new MessageBox(shell, SWT.ICON_ERROR | SWT.OK);
            mb.setText("Print Selection Error");
            mb.setMessage("You must first select text to\n"
                    + "print (by dragging, for example).");
            mb.open();
            return;
        }

        MessageBox mb = new MessageBox(shell, SWT.ICON_QUESTION | SWT.YES
                | SWT.NO);
        mb.setMessage("Do you want to print the current selection?");
        if (mb.open() == SWT.YES) {
            String tmpText = textEditor.getText();
            Point point = textEditor.getSelection();
            FontData fontData = textEditor.getFont().getFontData()[0];
            PrintDisplay.print(textEditor.getSelectionText(), fontData,
                    charWrapCol, statusHandler);
            textEditor.setText(tmpText);
            textEditor.setSelection(point);
        }
    }

    /**
     * Paste text from the clipboard into the text editor.
     */
    private void pasteText() {
        // AWIPS I just does the pasted in both overwrite and insert mode.
        try {
            // When pasting to empty editor assume text is properly formatted
            // and does not need wrapping.
            boolean doWrap = true;
            if (textEditor.getText().trim().length() == 0) {
                doWrap = false;
                textEditor.setText("");
            }
            int start = -1;
            if (textEditor.getSelectionCount() == 0) {
                start = textEditor.getCaretOffset();
            } else {
                start = textEditor.getSelectionRange().x;
            }
            textEditor.paste();
            if (doWrap) {
                rewrap(start, textEditor.getCaretOffset());
            }
        } catch (IllegalArgumentException ex) {
            // Ignore
        }
    }

    /**
     * Fill selected text, that is, remove line separators from selected text.
     */
    private void fillText() {
        String selectedText = textEditor.getSelectionText();
        String replacementText;
        int cursorPos = textEditor.getSelection().x;
        int selectionLength = textEditor.getSelectionCount();
        if (selectedText != null) {
            replacementText = selectedText.replaceAll("[\r\n]", "");
            textEditor.replaceTextRange(cursorPos, selectionLength,
                    replacementText);
        }
    }

    /**
     * Import data from a file.
     * 
     */
    private void importFile() {
        FileDialog dlg = new FileDialog(shell, SWT.OPEN);
        dlg.setText("Import From File");
        dlg.setFilterNames(FILTER_NAMES);
        dlg.setFilterExtensions(FILTER_EXTS);
        String fn = dlg.open();
        if (fn != null) {
            try {
                BufferedReader in = new BufferedReader(new FileReader(fn));
                String s;
                try {
                    textEditor.setText("");
                    if ((s = in.readLine()) != null) {
                        textEditor.append(s);
                    }
                    while ((s = in.readLine()) != null) {
                        textEditor.append("\n" + s);
                    }
                    TextDisplayModel.getInstance().createStdTextProduct(token,
                            textEditor.getText(), "");
                    textEditor.setFocus();
                } catch (IOException e1) {
                    statusHandler.handle(Priority.PROBLEM,
                            "Error retrieving metatdata", e1);
                }
            } catch (FileNotFoundException e1) {
                statusHandler.handle(Priority.PROBLEM,
                        "Error retrieving metatdata", e1);
            }
        }
    }

    /**
     * Attaches the contents of a file to a product.
     */
    private void attachFile() {
        FileDialog dlg = new FileDialog(shell, SWT.OPEN);
        dlg.setText("Attach File");
        dlg.setFilterNames(FILTER_NAMES);
        dlg.setFilterExtensions(FILTER_EXTS);
        if (attachedFilename != null && attachedFilename.trim().length() > 0) {
            int startIndex = statusBarLabel.getText().indexOf(":") + 1;
            int endIndex = statusBarLabel.getText().lastIndexOf(File.separator) + 1;
            String filterPath = statusBarLabel.getText().substring(startIndex,
                    endIndex);
            dlg.setFilterPath(filterPath);
            dlg.setFileName(statusBarLabel.getText().substring(startIndex));
        }
        String fn = dlg.open();
        if (fn != null) {
            try {
                File file = new File(fn);
                if (file.exists() && (file.length() <= 50000)
                        && isTextFile(file)) {
                    FileInputStream in = new FileInputStream(file);
                    byte[] bytes = new byte[(int) file.length()];
                    int offset = 0;
                    int numRead = 0;
                    while (offset < bytes.length
                            && (numRead = in.read(bytes, offset, bytes.length
                                    - offset)) >= 0) {
                        offset += numRead;
                    }
                    in.close();
                    attachedFile = bytes;
                    attachedFilename = fn.substring(fn
                            .lastIndexOf(File.separator) + 1);
                    statusBarLabel.setText("Attachment: " + fn);
                } else {
                    MessageBox mb = new MessageBox(shell, SWT.ICON_ERROR
                            | SWT.OK);
                    StringBuilder sb = new StringBuilder();
                    if (!file.exists()) {
                        sb.append("File does NOT exist!\n\n");
                    } else if (file.length() > 50000) {
                        sb.append("File size is too large!\n\n");
                    } else {
                        sb.append("File is NOT a text file!\n\n");
                    }
                    mb.setText("Notice");
                    mb.setMessage(sb.toString() + fn);
                    mb.open();
                }
            } catch (Exception e) {
                statusHandler.handle(Priority.PROBLEM, "Error attaching file",
                        e);
            }
        } else {
            if (statusBarLabel.getText().startsWith("Attachment: ")) {
                statusBarLabel.setText("");
                attachedFile = null;
                attachedFilename = null;
            }
        }
    }

    /**
     * Export data to a file.
     */
    private void exportFile() {
        FileDialog dlg = new FileDialog(shell, SWT.OPEN);
        dlg.setText("Export To File");
        dlg.setFilterNames(FILTER_NAMES);
        dlg.setFilterExtensions(FILTER_EXTS);
        String fn = dlg.open();
        if (fn != null) {
            try {
                BufferedWriter out = new BufferedWriter(new FileWriter(fn));
                StringBuilder s = new StringBuilder();
                if (inEditMode) {
                    s.append(removeSoftReturns(headerTF.getText()));
                    s.append("\n\n");
                }
                s.append(removeSoftReturns(textEditor.getText()));
                int eolIndex = s.indexOf("\n");
                int ddhhmmIndex = s.indexOf("DDHHMM");
                if (ddhhmmIndex > 0 && ddhhmmIndex < eolIndex) {
                    s.replace(ddhhmmIndex, ddhhmmIndex + 6, "000000");
                }
                out.append(s);
                out.close();
            } catch (IOException e1) {
                statusHandler.handle(Priority.PROBLEM,
                        "Error retrieving metatdata", e1);
            }
        }
    }

    /**
     * Clear all of the text field.
     */
    private void clearTextFields() {
        textEditor.setText("");
        afosCmdTF.setText("");
        wmoTtaaiiTF.setText("");
        ccccTF.setText("");
        awipsIdTF.setText("");
        afosCmdTF.setFocus();
    }

    /**
     * Display the AFOS Browser dialog.
     */
    private void displayAfosBrowser() {
        if (mustCreate(afosBrowser)) {
            afosBrowser = new AfosBrowserDlg(shell, shell.getText(), this,
                    token);
            afosBrowser.open();
        } else {
            afosBrowser.bringToTop();
        }
    }

    /**
     * Determine if dialog needs to be open or just bring to top.
     */
    public void showDialog() {
        if (disposeOnExit == true) {
            return;
        }

        if (shell == null) {
            open();
        }

        bringToTop();
        if (displayAfosBrowser) {
            afosBrowser.showDialog();
        }

        if (browser != null) {
            browser.bringToTop();
        }
    }

    /**
     * Determine if dialog should be hidden or closed for proper disposal.
     */
    public void hideDialog() {
        if (disposeOnExit == false) {
            hide();
            return;
        }

        // Allow the shell listener to clean up other dialogs and then hide the
        // dialog.
        close();
        inEditMode = false;
    }

    public void disposeDialog() {
        disposeDialog = true;
        inEditMode = false;
    }

    // TODO - remove this when needed...
    // this is a convenience method to show a dialog
    // when functionality has not been implemented...
    //
    private void notImplementedYet(String information) {
        MessageBox mb = new MessageBox(shell, SWT.ICON_ERROR | SWT.OK);
        mb.setText("Notice");
        mb.setMessage("Functionality not implemented yet:\n\n" + information);
        mb.open();
    }

    /**
     * Bring up an error dialog to display a problem to the user.
     * 
     * @param title
     * @param information
     */
    private void userInformation(String title, String information) {
        MessageBox mb = new MessageBox(shell, SWT.ICON_ERROR | SWT.OK);
        mb.setText(title);
        mb.setMessage(information);
        mb.open();
    }

    /**
     * Display a notice to the user.
     * 
     * @param information
     */
    private void userInformation(String information) {
        userInformation("Notice", information);
    }

    /**
     * Disseminate the product.
     * 
     * @param resend
     *            true if product is to be resent
     */
    synchronized private void sendProduct(final boolean resend) {
        final CAVEMode mode = CAVEMode.getMode();
        StdTextProduct prod = getStdTextProduct();
        String afosId = prod.getCccid() + prod.getNnnid() + prod.getXxxid();
        final String title = QualityControl.getProductWarningType(prod
                .getNnnid());
        final StringBuilder productMessage = new StringBuilder();

        final StringBuilder modeMessage = new StringBuilder();
        modeMessage.append("The workstation is in ").append(mode)
                .append(" mode.");

        if (resend) {
            productMessage.append("You are about to RESEND a " + afosId + "\n");
            productMessage.append(title).append(".\n");
            modeMessage.append("\nThere is no QC check for resend product.");
        } else if (warnGenFlag) {
            productMessage.append("You are about to SEND a " + afosId + "\n");
            productMessage.append(title).append(".\n");

            QualityControl qcCheck = new QualityControl();
            if (qcCheck.checkWarningInfo(headerTF.getText().toUpperCase(),
                    textEditor.getText().toUpperCase(), prod.getNnnid()) == false) {
                WarnGenConfirmationDlg wgcd = new WarnGenConfirmationDlg(shell,
                        "Problem Detected by QC", qcCheck.getErrorMessage(),
                        "Do you really want to Send?\n", mode);
                wgcd.setCloseCallback(new ICloseCallback() {

                    @Override
                    public void dialogClosed(Object returnValue) {
                        if (Boolean.TRUE.equals(returnValue))
                            finishSendProduct(resend, title, mode,
                                    productMessage, modeMessage);

                    }
                });
                wgcd.open();

                return;
            }
        }
        finishSendProduct(resend, title, mode, productMessage, modeMessage);
    }

    /**
     * This finishes preparing to send a product as part of normal compleation
     * of sendProduct or as part of the call back when there is a problem with
     * the WarnGen being sent.
     * 
     * @param resend
     * @param title
     * @param mode
     * @param productMessage
     * @param modeMessage
     */
    private void finishSendProduct(final boolean resend, String title,
            CAVEMode mode, StringBuilder productMessage,
            StringBuilder modeMessage) {
        Pattern p = Pattern.compile(".\\%[s].");
        Matcher m = p.matcher(STORED_SENT_MSG);

        final boolean result = (CAVEMode.OPERATIONAL.equals(mode) || CAVEMode.TEST
                .equals(mode));
        modeMessage.append(result ? m.replaceAll(" ") : m.replaceAll(" not "));

        if (statusBarLabel.getText().startsWith("Attachment:")) {
            StringBuilder sb = new StringBuilder("An Attachment file (");
            int startIndex = "Attachment:".length() + 1;
            sb.append(statusBarLabel.getText().substring(startIndex));
            sb.append(") will be transmitted with this message.");
            MessageBox mb = new MessageBox(shell, SWT.OK | SWT.CANCEL);
            mb.setText("Notice");
            mb.setMessage(sb.toString());
            if (SWT.OK != mb.open()) {
                return;
            }
        }

        // verify if product has already been resent
        if (!verifyResendProduct()) {
            return;
        }

        // verify required fields
        if (!verifyRequiredFields()) {
            return;
        }

        WarnGenConfirmationDlg wgcd = new WarnGenConfirmationDlg(shell, title,
                productMessage.toString(), modeMessage.toString(), mode);
        wgcd.setCloseCallback(new ICloseCallback() {

            @Override
            public void dialogClosed(Object returnValue) {
                if (Boolean.TRUE.equals(returnValue)) {
                    warngenCloseCallback(resend, result);
                }
            }
        });
        wgcd.open();
    }

    /**
     * This is used by finishedSendProduct as the call back to the warnGen
     * confirmaiton Dialog.
     * 
     * @param resend
     * @param result
     */
    private void warngenCloseCallback(boolean resend, boolean isOperational) {

        // DR14553 (make upper case in product)
        String body = textEditor.getText().toUpperCase();
        if (isOperational) {
            removeOptionalFields();

            try {
                /* update the vtec string in the message */
                // DR14553 (make upper case in product)
                if (!resend) {
                    body = VtecUtil.getVtec(removeSoftReturns(textEditor
                            .getText().toUpperCase()), true);
                }
                updateTextEditor(body);
                if ((inEditMode || resend)
                        && saveEditedProduct(false, resend, true)) {
                    inEditMode = false;
                }

                String product = TextDisplayModel.getInstance().getProduct(
                        token);
                OUPRequest req = new OUPRequest();
                OfficialUserProduct oup = new OfficialUserProduct();
                StdTextProduct prod = getStdTextProduct(); // TODO: makes me
                                                           // nervous...
                String awipsWanPil = prod.getSite() + prod.getNnnid()
                        + prod.getXxxid();
                String awipsID = prod.getNnnid() + prod.getXxxid();

                if (notify != null) {
                    notify.add(product);
                }
                oup.setAwipsWanPil(awipsWanPil);
                oup.setNeedsWmoHeader(false);
                oup.setProductText(product);
                oup.setSource("TextWS");
                oup.setWmoType(fixNOR(prod.getBbbid()));
                oup.setUserDateTimeStamp(prod.getHdrtime());
                oup.setFilename(awipsID + ".wan"
                        + (System.currentTimeMillis() / 1000));
                oup.setAddress(addressee);
                if (attachedFile != null && attachedFilename != null) {
                    oup.setAttachedFile(attachedFile);
                    oup.setAttachedFilename(attachedFilename);
                }
                req.setCheckBBB(true);
                req.setProduct(oup);

                // Code in Run statement goes here!
                new Thread(new ThriftClientRunnable(req)).start();
            } catch (VizException e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Error transmitting text product", e);
            }
        } else {
            try {
                if (!resend) {
                    body = VtecUtil.getVtec(removeSoftReturns(textEditor
                            .getText()));
                }
                updateTextEditor(body);
                if ((inEditMode || resend)
                        && saveEditedProduct(false, resend, false)) {
                    inEditMode = false;
                }
                SendPracticeProductRequest req = new SendPracticeProductRequest();
                req.setProductText(TextDisplayModel.getInstance().getProduct(
                        token));

                ThriftClient.sendRequest(req);
            } catch (VizException e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Error transmitting text product", e);
            }
        }

        if (inEditMode == false && resend == false) {
            saved = true;
            StdTextProductId finalProduct = this.getStdTextProduct()
                    .getProdId();
            String header = null;
            if (finalProduct.getNnnid().equals("WRK")
                    && !finalProduct.getXxxid().startsWith("WG")) {
                header = "ZCZC " + finalProduct.getCccid()
                        + finalProduct.getNnnid() + finalProduct.getXxxid()
                        + " " + getAddressee() + "\nTTAA00 "
                        + finalProduct.getSite() + " "
                        + finalProduct.getHdrtime();
            } else {
                header = finalProduct.getWmoid() + " " + finalProduct.getSite()
                        + " " + finalProduct.getHdrtime() + "\n"
                        + finalProduct.getNnnid() + finalProduct.getXxxid();
            }
            headerTF.setText(header);
            cancelEditor(false);
        }
    }

    /**
     * Recreates the original message by combining the header and the body from
     * the edit windows.
     * 
     * @return the combined message
     */
    private String combineOriginalMessage() {
        if (headerTF.getCharCount() == 0) {
            return textEditor.getText();
        } else if (textEditor.getCharCount() == 0) {
            return headerTF.getText();
        }
        StringBuilder body = new StringBuilder();
        body.append(headerTF.getText()).append("\n");
        String line = null;
        for (int i = 0; i < textEditor.getLineCount(); ++i) {
            line = textEditor.getLine(i).trim();
            if (line.length() > 0) {
                break;
            }
        }
        if (!UGC_FIRST_LINE_PATTERN.matcher(line).matches()) {
            // DR_14902 Only add blank line when not a UGC line.
            body.append("\n");
        }
        body.append(textEditor.getText().trim());

        return body.toString();
    }

    /**
     * Callback method called when the user requests a save of the currently
     * editing product. If the save is successful, the restore text following a
     * cancel is updated to the saved text.
     */
    synchronized private void saveProduct() {
        StdTextProduct product = TextDisplayModel.getInstance()
                .getStdTextProduct(token);
        if (product != null
                && gfeForbidden(product.getCccid(), product.getNnnid())) {
            // Pop up forbidden window.
            inEditMode = false;
            userInformation("This product MUST be edited in GFE! \n Please exit and return to GFE. \n Action Aborted!");
            return;
        }
        boolean successful = saveEditedProduct(false, false, false);
        if (successful) {
            // reset the editor status flags
            dirty = false;
            saved = true;
            replaceWorkProductId();
            originalText = combineOriginalMessage();
            if (warnGenFlag == true) {
                originalText = removeSoftReturns(originalText);
            }
        }
    }

    /**
     * Saves the edited product.
     * 
     * @param isAutoSave
     *            true if auto save operation
     * @param resend
     *            true if product is to be resent
     * 
     * @return true is the save was successful
     */
    synchronized private boolean saveEditedProduct(boolean isAutoSave,
            boolean resend, boolean isOperationalSend) {
        StdTextProduct product = TextDisplayModel.getInstance()
                .getStdTextProduct(token);
        if (product != null
                && gfeForbidden(product.getCccid(), product.getNnnid())) {
            // Pop up forbidden window.
            inEditMode = false;
            userInformation("This product MUST be edited in GFE! \n Please exit and return to GFE. \n Action Aborted!");
            return false;
        }
        boolean successful = false;

        /*
         * DR14613 - string currectDate is derived from Date now ensuring the
         * same time in WMO heading and in the MND heading.
         */
        Date now = SimulatedTime.getSystemTime().getTime();
        String currentDate = getCurrentDate(now);
        TextDisplayModel tdmInst = TextDisplayModel.getInstance();

        // Convert the text in the text editor to uppercase
        if (!isAutoSave) {
            if (!verifyRequiredFields()) {
                return false;
            }
            replaceWorkProductId();

            String header = headerTF.getText().toUpperCase();
            String body = resend ? resendMessage()
                    : removeSoftReturns(textEditor.getText().toUpperCase());
            // verify text
            headerTF.setText(header);
            updateTextEditor(body);
            textEditor.setFocus();
        }

        // New up request constraint for table response using the
        // tmpStr editor content after marshalling this string to
        // XML format via the Util class.
        // New up a StdTextProduct, then set the product component
        // to the tmpStr that represents the new content.
        StdTextProduct storedProduct = tdmInst.getStdTextProduct(token);
        String productText = resend ? resendMessage()
                : combineOriginalMessage();

        if (warnGenFlag == true && resend == false) {
            productText = removeSoftReturns(productText);
        }

        if (!isAutoSave) {
            if (!resend) {
                // If not a resend, set the DDHHMM field to the current time
                productText = replaceDDHHMM(productText, currentDate);

                VtecObject vtecObj = VtecUtil.parseMessage(productText);
                if (warnGenFlag) {
                    /*
                     * DR14613 - string currectDate is derived from Date now
                     * ensuring the same time in WMO heading and in the MND
                     * heading.
                     */
                    productText = updateVtecTimes(productText, vtecObj, now);
                    productText = updateHeaderTimes(productText, now);
                    // Update editor so the proper send times are displayed.
                    String[] b = productText.split("\n");
                    StringBuilder body = new StringBuilder();
                    for (int i = 2; i < b.length; ++i) {
                        body.append(b[i]).append("\n");
                    }
                    updateTextEditor(body.toString());
                }

                storedProduct.setHdrtime(currentDate);
            }
            storedProduct.setRefTime(System.currentTimeMillis());
            if (productText.contains(AFOSParser.DRAFT_PIL)) {
                int start = productText.indexOf(AFOSParser.DRAFT_PIL);
                productText = productText.replace(productText.substring(start,
                        start + 6), storedProduct.getProdId().getNnnid()
                        + storedProduct.getProdId().getXxxid());
            }
        }

        if (statusBarLabel.getText().startsWith("Attachment:")) {
            int startIndex = statusBarLabel.getText().indexOf(":") + 2;
            productText += ATTACHMENT_STR
                    + statusBarLabel.getText().substring(startIndex);
        }

        storedProduct.setProduct(productText.trim());

        /*
         * if (storedProduct == null) { tdmInst.setStdTextProduct(token,
         * currentWmoId, currentSiteId, currentWsfoId, currentProdCategory,
         * currentProdDesignator, currentDate, currentBbbId,
         * System.currentTimeMillis(), "HELLO AWIPS!"); storedProduct =
         * tdmInst.getStdTextProduct(token); } else {
         * tdmInst.setStdTextProduct(token, currentWmoId, currentSiteId,
         * currentWsfoId, currentProdCategory, currentProdDesignator,
         * currentDate, currentBbbId, storedProduct.getCreatetime(),
         * storedProduct .getProduct()); storedProduct =
         * tdmInst.getStdTextProduct(token); }
         * 
         * tmpProd = new StdTextProduct(storedProduct);
         * tmpProd.setProduct(currentText); } else { if (storedProduct == null)
         * { tmpProd = new StdTextProduct(currentWmoId, currentSiteId,
         * currentWsfoId, "TEX", "T0" + token, currentDate, currentBbbId,
         * System.currentTimeMillis(), currentHeader + "\n" + tmpStr + "@@TEXT0"
         * + token + "@@" + currentProdCategory + currentProdDesignator); } else
         * { tmpProd = new StdTextProduct(currentWmoId, currentSiteId,
         * currentWsfoId, "TEX", "T0" + token, currentDate, currentBbbId,
         * storedProduct.getCreatetime(), currentHeader + "\n" + tmpStr +
         * "@@TEXT0" + token + "@@" + currentProdCategory +
         * currentProdDesignator); } }
         */
        if (isAutoSave) {
            autoSave.saveProduct(storedProduct);
        } else if (isOperationalSend || resend) {
            // OUPRequest will update the StdTextProduct table.
            successful = true;
        } else {
            if (!saveStoredTextProduct(storedProduct)) {
                return false;
            }

            // Update the TextProductInfo table within the Text Database when a
            // successful database update has occurred to the StdTextProduct
            // table of the Text Database unless the update happened as a result
            // of an auto-save operation. The TextProductInfo table is used by
            // the AFOS Browser to generate the list of products that are
            // retrievable.
            successful = saveTextProductInfo();
        }

        return successful;
    }

    /**
     * Replaces the WMO heading DDHHMM field with the given text.
     * 
     * @param productText
     *            Product text which includes the WMO heading
     * @param ddhhmm
     *            Replacement text
     * @return The modified product text
     */
    private static String replaceDDHHMM(String productText, String ddhhmm) {
        String[] parts = productText.split("\n", 2);
        if (parts.length > 0) {
            String[] headerParts = parts[0].split("\\s+", 0);
            if (headerParts.length >= 3)
                headerParts[2] = ddhhmm;
            // TODO: else raise error?
            StringBuilder sb = new StringBuilder(productText.length());
            boolean first = true;
            for (String s : headerParts) {
                if (first)
                    first = false;
                else
                    sb.append(' ');
                sb.append(s);
            }
            if (parts.length > 1) {
                sb.append('\n').append(parts[1]);
            }

            productText = sb.toString();
        }

        return productText;
    }

    /**
     * Replaces the contents of the Text Editor widget while preserving the
     * existing Style Ranges. If the new text doesn't match the existing text in
     * length, the method does not update the text. Note this allows updating of
     * locked text.
     * 
     * @param body
     *            the replacement text for the Text Editor
     */
    private void updateTextEditor(String body) {
        if (body == null) {
            return;
        }
        // Note: setting the text into the StyledText widget will
        // reset any StyleRanges. This approach used here is to
        // get clones of the existing StyleRanges and then reapply
        // them to the StyledText widget.
        List<StyleRange> locks = new ArrayList<StyleRange>();
        for (StyleRange range : textEditor.getStyleRanges()) {
            StyleRange lock = (StyleRange) range.clone();
            locks.add(lock);
        }

        // remove verify listener to stop lock text checking
        textEditor.removeVerifyListener(TextEditorDialog.this);
        textEditor.setText(body);
        textEditor.addVerifyListener(TextEditorDialog.this);

        for (StyleRange lock : locks) {
            if (0 <= lock.start
                    && lock.start + lock.length <= textEditor.getCharCount()) {
                textEditor.setStyleRange(lock);
            }
        }
    }

    /**
     * When allowed to save a product send request to server.
     * 
     * @return true request saved to server otherwise false.
     */
    private boolean saveTextProductInfo() {
        StdTextProduct product = TextDisplayModel.getInstance()
                .getStdTextProduct(token);
        if (gfeForbidden(product.getCccid(), product.getNnnid())) {
            // Pop up forbidden window.
            inEditMode = false;
            userInformation("This product MUST be edited in GFE! \n Please exit and return to GFE. \n Action Aborted!");
            return false;
        }
        try {
            TextDisplayModel tdm = TextDisplayModel.getInstance();
            ThriftClient.sendRequest(createTextProductInfoServerRequest(
                    tdm.getProductNode(token), tdm.getProductCategory(token),
                    tdm.getProductDesignator(token)));
        } catch (VizException e1) {
            statusHandler.handle(Priority.PROBLEM, "Error retrieving metadata",
                    e1);
            return false;
        }

        return true;
    }

    /**
     * Generate a request to obtain product(s) from the server.
     * 
     * @param cccid
     * @param nnnid
     * @param xxxid
     * @return
     */
    private IServerRequest createTextProductInfoServerRequest(String cccid,
            String nnnid, String xxxid) {
        TextProductInfoCreateRequest request = new TextProductInfoCreateRequest();
        request.setCccid(cccid);
        request.setNnnid(nnnid);
        request.setXxxid(xxxid);
        CAVEMode mode = CAVEMode.getMode();
        boolean operationalMode = (CAVEMode.OPERATIONAL.equals(mode)
                || CAVEMode.TEST.equals(mode) ? true : false);
        request.setOperationalMode(operationalMode);

        return request;
    }

    /**
     * When allowd attempt to save stored product.
     * 
     * @param storedProduct
     * @return true product saved to server otherwise false
     */
    private boolean saveStoredTextProduct(StdTextProduct storedProduct) {
        StdTextProduct product = TextDisplayModel.getInstance()
                .getStdTextProduct(token);
        if (product != null
                && gfeForbidden(product.getCccid(), product.getNnnid())) {
            // Pop up forbidden window.
            inEditMode = false;
            userInformation("This product MUST be edited in GFE! \n Please exit and return to GFE. \n Action Aborted!");
            return false;
        }
        try {
            ThriftClient
                    .sendRequest(createStdTextProductServerRequest(storedProduct));
        } catch (VizException e1) {
            statusHandler.handle(Priority.PROBLEM, "Error retrieving metadata",
                    e1);
            return false;
        }

        return true;
    }

    /**
     * Create a Standard Text Product Server Request Object to be used in the
     * thrift service.
     * 
     * @param product
     * @return
     */
    private IServerRequest createStdTextProductServerRequest(
            StdTextProduct product) {

        StdTextProductServerRequest request = new StdTextProductServerRequest();

        request.setBbbid(fixNOR(product.getBbbid()));
        request.setCccid(product.getCccid());
        request.setCreatetime(product.getRefTime());
        request.setDataCrc(product.getDataCrc());
        request.setHdrtime(product.getHdrtime());
        request.setNnnid(product.getNnnid());
        request.setProduct(product.getProduct());
        request.setWmoid(product.getWmoid());
        request.setSite(product.getSite());
        request.setXxxid(product.getXxxid());
        CAVEMode mode = CAVEMode.getMode();
        boolean result = (CAVEMode.OPERATIONAL.equals(mode)
                || CAVEMode.TEST.equals(mode) ? true : false);
        request.setOpertionalFlag(result);

        return request;
    }

    /**
     * Method to mark uneditable warning text. Only applies to VTEC warning
     * (vtecAfosProductEnum) type products.
     */
    private boolean markUneditableText(StyledText st) {
        // Capture the input from the styled text widget.
        StringBuffer sb = new StringBuffer(st.getText());
        String errMsg = null;
        int currentIndex = 0;
        int startIndex = 0;
        int endIndex = 0;
        try {
            while (sb.indexOf(BEGIN_ELEMENT_TAG, 0) >= 0) {
                currentIndex = 0;
                startIndex = 0;
                endIndex = 0;

                // Looks for the most inner <L></L> tags
                do {
                    startIndex = sb.indexOf(BEGIN_ELEMENT_TAG, currentIndex);
                    endIndex = sb.indexOf(END_ELEMENT_TAG, currentIndex);
                } while (startIndex > 0
                        && endIndex > 0
                        && (currentIndex = sb.indexOf(BEGIN_ELEMENT_TAG,
                                startIndex + BEGIN_ELEMENT_TAG_LEN)) > 0
                        && currentIndex < endIndex);

                if (currentIndex > 0 && currentIndex < endIndex) {
                    startIndex = currentIndex;
                }

                st.replaceTextRange(endIndex, END_ELEMENT_TAG_LEN, "");
                // Likewise in the string buffer
                sb.replace(endIndex, endIndex + END_ELEMENT_TAG_LEN, "");
                // Replace the begin tag in the styled text with an empty string
                st.replaceTextRange(startIndex, BEGIN_ELEMENT_TAG_LEN, "");
                // Likewise in the string buffer
                sb.replace(startIndex, startIndex + BEGIN_ELEMENT_TAG_LEN, "");
                // Mark the text between the begin and end tags as uneditable
                st.setStyleRange(new StyleRange(startIndex, endIndex
                        - startIndex - BEGIN_ELEMENT_TAG_LEN, getDisplay()
                        .getSystemColor(SWT.COLOR_BLUE), null, SWT.BOLD));
                markedTextUndeditable = true;
            }
        } catch (IllegalArgumentException ex) {
            errMsg = ex.toString();
        } catch (StringIndexOutOfBoundsException ex) {
            errMsg = ex.toString();
        }

        if (errMsg != null) {
            Status status = new Status(
                    IStatus.ERROR,
                    "com.raytheon.viz.texteditor",
                    String.format(
                            "Bad Lock Tag pairing:\ncurrentIndex %d\nstartIndex %d\nendIndex %d",
                            currentIndex, startIndex, endIndex));
            ErrorDialog
                    .openError(
                            shell,
                            "Product Problems",
                            "Problem parsing product to determine uneditable text\nPlease capture this dialog informaion and the contents of the Wargen screen prior to any changes.",
                            status);
        }
        return markedTextUndeditable;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.swt.events.VerifyListener#verifyText(org.eclipse.swt.events
     * .VerifyEvent)
     */
    @Override
    public void verifyText(VerifyEvent event) {
        // Enforces uneditability of lockable text in warning products
        int length = event.end - event.start;
        try {
            if (length == 0) {
                if (event.start != 0
                        && event.start != textEditor.getCharCount()) {
                    int ranges[] = textEditor.getRanges(event.start - 1,
                            length + 2);
                    for (int i = 0; i < ranges.length; i += 2) {
                        int rangeStart = ranges[i];
                        int rangeEnd = rangeStart + ranges[i + 1];
                        if (event.start > rangeStart && event.start < rangeEnd) {
                            event.doit = false;
                            /*
                             * DR15704 - this needs to be set so the rewrap is
                             * not called when locked text gets editted.
                             */
                            userKeyPressed = false;
                            break;
                        }
                    }
                }
            } else {
                int ranges[] = textEditor.getRanges(event.start, length);
                if (inEditMode && ranges != null && ranges.length != 0) {
                    event.doit = false;
                    /*
                     * DR15704 - this needs to be set so the rewrap is not
                     * called when locked text gets editted.
                     */
                    userKeyPressed = false;
                }
            }
        } catch (IllegalArgumentException e) {
            // TODO Auto-generated catch block
            // e.printStackTrace();
        }
    }

    /**
     * Perform a search of product(s) based on the WMO TTAAII CCCC field's
     * values.
     */
    private void wmoSearch() {
        if (wmoTtaaiiTF.getText().isEmpty() && ccccTF.getText().isEmpty()) {
            // warn and no search
            Status status = new Status(IStatus.ERROR,
                    "com.raytheon.viz.texteditor",
                    "Cannot perform query with no parameters");
            ErrorDialog.openError(shell, "No query parameters",
                    "Please fill in TTAAii and/or CCCC fields", status);
        } else {
            ICommand command = CommandFactory.getWmoCommand(
                    wmoTtaaiiTF.getText(), ccccTF.getText());
            executeCommand(command);
        }
    }

    /**
     * This method separates the header from the body when entering 'edit' mode.
     * The basic idea is to avoid manual editing of the header. Other editing
     * may be blocked by the use of {@literal <LOCK>} tags in the warning
     * template.
     * 
     */
    private void setCurrentHeaderAndBody() {
        // Set the current Header and the current Body of the text
        // product.
        int finish = 0;
        int start = 0;
        int thisLine = textEditor.getLineAtOffset(start);
        int numberOfLinesOfHeaderText = 2;
        int afosNnnLimit = 2; // first three characters is AFOS NNN
        int afosXxxLimit = 5; // second three characters is AFOS XXX

        // TODO FIX PARSING

        // First, set the current header by assuming that it usually
        // consists of the first two lines of text in the text product,
        // though there will be exceptions to that "rule" as handled below.
        // So, obtain the AFOS NNNxxx. If it's where it is supposed to be
        // in the new format, then the existing header is already an AWIPS
        // text product identifier. Otherwise it is a legacy AFOS identifier.
        if (TextDisplayModel.getInstance().hasStdTextProduct(token)) {
            StdTextProduct textProd = TextDisplayModel.getInstance()
                    .getStdTextProduct(token);
            StdTextProductId prodId = textProd.getProdId();
            try {
                // start of second line of text
                start = textEditor.getOffsetAtLine(thisLine + 1);
                if ((textEditor.getText(start, start + afosNnnLimit)
                        .equals(prodId.getNnnid()))
                        && (textEditor.getText(start + afosNnnLimit + 1, start
                                + afosXxxLimit).equals(prodId.getXxxid()))) {
                    // Text matches the products nnnid and xxxid
                    numberOfLinesOfHeaderText = 2;
                } else if (textEditor.getText(start, start + afosNnnLimit + 2)
                        .equals(AFOSParser.DRAFT_PIL)
                        || textEditor.getText(start, start + afosNnnLimit + 2)
                                .equals("TTAA0")) {
                    // Text matches temporary WRKWG#
                    numberOfLinesOfHeaderText = 2;
                } else {
                    // Assume this header block is a legacy AFOS identifier.
                    numberOfLinesOfHeaderText = 1;
                }
            } catch (IllegalArgumentException e) {
                // Assume this header block is a legacy AFOS identifier.
                numberOfLinesOfHeaderText = 1;
            }
        }

        try {
            start = 0;
            finish = textEditor.getOffsetAtLine(thisLine
                    + numberOfLinesOfHeaderText) - 1;
        } catch (IllegalArgumentException e) {
            // The text does not span enough lines so use the full extent
            // of the product.
            finish = textEditor.getCharCount() - 1;
        }

        // Set the content of the header block to consist of just the header of
        // the text product... it will get reunited with the body when it is
        // saved.
        if (finish > start) {
            headerTF.setText(textEditor.getText(start, finish));
        } else {
            headerTF.setText("");
        }

        // Next, set the current body by assuming that it always
        // consists of the rest of the text product beyond the line(s)
        // of text in the header.
        try {
            int numberOfBlankLines = -1;
            String line = null;
            do {
                numberOfBlankLines++;
                line = textEditor.getLine(thisLine + numberOfLinesOfHeaderText
                        + numberOfBlankLines);
            } while (line.length() == 0 || line.equals(""));
            // Note: 'st' is a reference to 'textEditor'...
            // delelete the header from the text in 'textEditor'
            finish = textEditor.getOffsetAtLine(thisLine
                    + numberOfLinesOfHeaderText + numberOfBlankLines);
            textEditor.setSelection(start, finish);
            textEditor.setEditable(true);
            textEditor.invokeAction(SWT.DEL);
            textEditor.setEditable(false);
        } catch (IllegalArgumentException e) {
            // There is no text product body, so set it to the empty string.
            textEditor.setText("");
        }
        // set editor status flags
        dirty = false;
    }

    /**
     * Update the editor's header text field.
     * 
     * @param wmoId
     * @param siteId
     * @param dateId
     * @param separator
     * @param nnnxxx
     */
    private void setHeaderTextField(String wmoId, String siteId, String dateId,
            String separator, String nnnxxx) {
        headerTF.setText(wmoId + " " + siteId + " " + dateId + separator
                + nnnxxx);
    }

    /**
     * Remove text from the header text field.
     */
    private void clearHeaderTextField() {
        headerTF.setText("");
    }

    /**
     * Consistent format for the String derivated from date.
     * 
     * @param now
     * @return
     */
    private String getCurrentDate(Date now) {
        /*
         * DR14613 - pass the Date now as an argument
         */
        SimpleDateFormat formatter = new SimpleDateFormat("ddHHmm");
        formatter.setTimeZone(TimeZone.getTimeZone("GMT"));
        return (formatter.format(now));
    }

    /**
     * Update the VTEC time using the Date now.
     * 
     * @param product
     * @param vtecObj
     * @param now
     * @return
     */
    private String updateVtecTimes(String product, VtecObject vtecObj, Date now) {

        if (vtecObj == null || vtecObj.getAction().equals("COR")) {
            return product;
        }
        // Update the vtec start time
        if (vtecObj.getAction().equals("NEW")) {
            SimpleDateFormat vtecFormatter = new SimpleDateFormat(
                    "yyMMdd'T'HHmm'Z'");
            vtecFormatter.setTimeZone(TimeZone.getTimeZone("GMT"));
            product = product.replace(
                    vtecFormatter.format(vtecObj.getStartTime().getTime()),
                    vtecFormatter.format(now));
        }

        return product;
    }

    /**
     * Update the MND header time using the Date now.
     * 
     * @param product
     * @param now
     * @return
     */
    private String updateHeaderTimes(String product, Date now) {
        // Update the header time
        Matcher m = datePtrn.matcher(product);
        if (m.find()) {
            SimpleDateFormat headerFormat = new SimpleDateFormat(
                    "hmm a z EEE MMM d yyyy");
            headerFormat
                    .setTimeZone(TextWarningConstants.timeZoneAbbreviationMap
                            .get(m.group(5).substring(0, 1)));
            product = product.replace(m.group(1), headerFormat.format(now)
                    .toUpperCase());
        }
        return product;
    }

    /**
     * Set the dispaly model's wmoID value for this text editor.
     * 
     * @param wmoId
     */
    public void setCurrentWmoId(String wmoId) {
        TextDisplayModel.getInstance().setWmoId(token, wmoId);
    }

    /**
     * Set the dispaly model's SiteId for this text editor.
     * 
     * @param siteId
     */
    public void setCurrentSiteId(String siteId) {
        TextDisplayModel.getInstance().setSiteId(token, siteId);
    }

    /**
     * Set the dispaly model's bbbID for this text editor.
     * 
     * @param bbbId
     */
    public void setCurrentBbbId(String bbbId) {
        TextDisplayModel.getInstance().setBbbId(token, fixNOR(bbbId));
    }

    /**
     * Set the dispaly model's Product node for ths text editor.
     * 
     * @param wsfoId
     */
    public void setCurrentWsfoId(String wsfoId) {
        TextDisplayModel.getInstance().setProductNode(token, wsfoId);
    }

    /**
     * Set the dispaly model's product category for this text editor.
     * 
     * @param prodCategory
     */
    public void setCurrentProdCategory(String prodCategory) {
        TextDisplayModel.getInstance().setProductCategory(token, prodCategory);
    }

    /**
     * Set the dispaly model's product designator for this text editor.
     * 
     * @param prodDesignator
     */
    public void setCurrentProdDesignator(String prodDesignator) {
        TextDisplayModel.getInstance().setProductDesignator(token,
                prodDesignator);
    }

    /**
     * Get the dispaly model's StdTextProduct for this editor.
     * 
     * @return
     */
    public StdTextProduct getStdTextProduct() {
        return TextDisplayModel.getInstance().getStdTextProduct(token);
    }

    /**
     * Set the dispaly model's AFOS command for this editor.
     */
    public void setAfosCmdField(String cmd) {
        afosCmdTF.setText(cmd);
        TextDisplayModel.getInstance().setAfosCommand(token, cmd);
    }

    /**
     * Setter method for addressee.
     * 
     * @param addressee
     */
    public void setAddressee(String addressee) {
        this.addressee = addressee;
    }

    /**
     * Getter method for addressee.
     * 
     * @return addressee
     */
    public String getAddressee() {
        return addressee;
    }

    /**
     * Convience method to execuete comand without updating ObsUpdated.
     */
    public void executeCommand(ICommand command) {
        executeCommand(command, false);
    }

    /**
     * Add command to the Product Query Job queue.
     * 
     * @param command
     * @param isObsUpdated
     */
    public void executeCommand(ICommand command, final boolean isObsUpdated) {
        if (isDisposed()) {
            return;
        }

        if (browser != null) {
            browser.close();
            browser = null;
        }

        commandHistory.addCommand(command);
        statusBarLabel.setText("Loading "
                + TextEditorUtil.getCommandText(command));
        statusBarLabel.update();
        setBusy(true);

        if (queryTransport == null) {
            queryTransport = TextEditorUtil.getTextDbsrvTransport();
        }
        productQueryJob.addRequest(command, isObsUpdated);
    }

    /**
     * Request for product(s) is finish now update the display with the
     * information.
     */
    public void requestDone(ICommand command,
            final List<StdTextProduct> prodList, final boolean isObsUpdated) {
        boolean enterEditor = false;
        boolean hasAttachment = false;
        String attachedFilename = new String();
        boolean validExecuteCommand = command != null;

        if (validExecuteCommand) {
            if (prodList != null && prodList.size() > 0) {
                if (prodList.size() > 1) {
                    if (CommandType.WMO.equals(command.getType())) {
                        final boolean hasAtt = hasAttachment;
                        final boolean enterEd = enterEditor;
                        final boolean validExecuteCmd = validExecuteCommand;
                        final String attachedFN = attachedFilename;
                        browser = new WmoBrowserDlg(getShell(), this, prodList);
                        browser.setCloseCallback(new ICloseCallback() {

                            @Override
                            public void dialogClosed(Object returnValue) {
                                postProductCheck(isObsUpdated, prodList);
                                postExecute(hasAtt, enterEd, validExecuteCmd,
                                        attachedFN);
                                browser = null;
                            }
                        });
                        browser.setBlockOnOpen(false);
                        browser.open();
                        return;
                    } else if (CommandType.AWIPS.equals(command.getType())) {
                        final boolean hasAtt = hasAttachment;
                        final boolean enterEd = enterEditor;
                        final boolean validExecuteCmd = validExecuteCommand;
                        final String attachedFN = attachedFilename;
                        browser = new AwipsBrowserDlg(getShell(), this,
                                prodList);
                        browser.setCloseCallback(new ICloseCallback() {

                            @Override
                            public void dialogClosed(Object returnValue) {
                                postProductCheck(isObsUpdated, prodList);
                                postExecute(hasAtt, enterEd, validExecuteCmd,
                                        attachedFN);
                                browser = null;
                            }
                        });
                        browser.setBlockOnOpen(false);
                        browser.open();
                        return;
                    }
                } else {
                    StdTextProduct prod = prodList.get(0);
                    if (prod.getProduct().contains(ATTACHMENT_STR)) {
                        int endIndex = prod.getProduct()
                                .indexOf(ATTACHMENT_STR);
                        String product = prod.getProduct().substring(0,
                                endIndex);
                        attachedFilename = prod.getProduct().substring(
                                endIndex + ATTACHMENT_STR.length());
                        prod.setProduct(product);
                        hasAttachment = true;
                    }

                    String commandText = command.getCommandTextFields()[0];
                    StdTextProductId stdProdId = prod.getProdId();

                    if ("MTR".equals(stdProdId.getNnnid())
                            && (commandText.startsWith("ALL:")
                                    || commandText.startsWith("A:") || commandText
                                    .endsWith("000"))) {
                        stripWMOHeaders(prod);
                    }

                    if (updateCount.get() > 0) {
                        updateDisplayedProduct(prod);
                    } else {
                        setDisplayedProduct(prod);
                    }

                    // Update the buttonology of the text workstation if this
                    // text editor dialog corresponds to one that belongs to the
                    // text workstation (TextWS) and not the D-2D.
                    updateButtonology(TextEditorUtil.getCommandText(command));
                }

                // If this is an AFOS command, check to see if the flag was set
                // to automatically enter the editor window.
                if (CommandType.AFOS.equals(command.getType())
                        && ((AFOSCommand) command).isEnterEditor()) {
                    enterEditor = true;
                }

                postProductCheck(isObsUpdated, prodList);

            } else {
                userInformation("No product in the database matches your request.");

                if (!accumChkBtn.getSelection()) {
                    textEditor.setText("");
                }
                validExecuteCommand = false;
            }
        }

        postExecute(hasAttachment, enterEditor, validExecuteCommand,
                attachedFilename);
    }

    /**
     * This is work to perform after selecting products from the prodList. This
     * allows the dialogs to select entries to be non-blocking.
     * 
     * @param isObsUpdated
     * @param prodList
     */
    private void postProductCheck(boolean isObsUpdated,
            List<StdTextProduct> prodList) {
        // Once we've got products and done all our processing, we need
        // to tell the system that we've got a new set of standard
        // text product parameters to work with here.
        String warning = prodList.get(0).getProduct();
        String[] nnnxxx = TextDisplayModel.getNnnXxx(warning);
        if (nnnxxx[0].equals("WRK")) {
            String siteNode = SiteAbbreviationUtil.getSiteNode(nnnxxx[1]);
            if (siteNode.isEmpty()) {
                // look up failed use current siteNode.
                siteNode = prodList.get(0).getCccid();
            }
            String ttaaii = SiteAbbreviationUtil.getTtaaii(siteNode + nnnxxx[0]
                    + nnnxxx[1]);
            final String w = warning.replace(TextWarningConstants.TTAAII,
                    ttaaii);
            TextDisplayModel.getInstance().createStdTextProduct(token, w,
                    siteNode);
        } else if (!nnnxxx[1].equals("xxx")) {
            String siteNode = SiteAbbreviationUtil.getSiteNode(nnnxxx[1]);
            String ttaaii = SiteAbbreviationUtil.getTtaaii(siteNode + nnnxxx[0]
                    + nnnxxx[1]);
            final String w = warning.replace(TextWarningConstants.TTAAII,
                    ttaaii);
            TextDisplayModel.getInstance().createStdTextProduct(token, w,
                    siteNode);
        }
        /*
         * DR15103 - do not clear AFOS command from the text box when obs are
         * updated
         */
        if (!isObsUpdated) {
            clearAfosCmdTF();
        }
        clearWmoTF();
        clearAwipsIdTF();

    }

    /**
     * The final part of the executeCommand after the command is executed and
     * products seleted. This allows product selection dialogs to be
     * non-blocking.
     * 
     * @param hasAttachment
     * @param enterEditor
     * @param validExecuteCommand
     * @param attachedFilename
     */
    private void postExecute(boolean hasAttachment, boolean enterEditor,
            boolean validExecuteCommand, String attachedFilename) {
        if (!this.isDisposed()) {
            if (hasAttachment) {
                statusBarLabel.setText("Attachment: " + attachedFilename);
            } else {
                statusBarLabel.setText("");
            }
            statusBarLabel.update();
            setBusy(false);
            // Automatically open the editor window with returned data.
            if (enterEditor) {
                enterEditor();
            } else {
                resendWarningProductnItem.setEnabled(true);
            }

            // Always give focus to textEditor after populating it.
            if (validExecuteCommand) {
                textEditor.setFocus();
            }
        }
    }

    /**
     * Sets the dialog busy state. When it changes the cursor the dialog is
     * changed and the AFOS browser for this dialog is notified of the state
     * change.
     * 
     * @param busy
     */
    private void setBusy(boolean busy) {
        if (this.busy != busy) {
            this.busy = busy;
            if (afosBrowser != null) {
                afosBrowser.setLoading(busy);
            }
            Cursor cursor = null;
            if (busy) {
                cursor = shell.getDisplay().getSystemCursor(SWT.CURSOR_WAIT);
            }
            shell.setCursor(cursor);
            textEditor.setCursor(cursor);
        }
    }

    /**
     * This strips off the WMO headers from a list of METAR products.
     * 
     * @param prod
     */
    private void stripWMOHeaders(StdTextProduct prod) {
        String[] lines = prod.getProduct().split("\n");
        StringBuilder sb = new StringBuilder();
        for (String line : lines) {
            Matcher m = METAR_PATTERN.matcher(line);
            if (m.find()) {
                sb.append(line).append("\n");
            }
        }
        prod.setProduct(sb.toString());
    }

    /**
     * Set up to edit a Warngen Product prior to issuing or saving it.
     * 
     * @param product
     * @param notify
     */
    public void showWarngenProduct(String product, NotifyExpiration notify) {
        inEditMode = true;
        this.notify = notify;
        String[] tokens = product.split(":", 2);

        if (tokens.length == 2) {
            String afosId = tokens[0];
            String warning = tokens[1];

            String[] nnnxxx = TextDisplayModel.getNnnXxx(warning);
            String siteNode = SiteAbbreviationUtil.getSiteNode(nnnxxx[1]);
            String ttaaii = SiteAbbreviationUtil.getTtaaii(siteNode + nnnxxx[0]
                    + nnnxxx[1]);
            final String w = warning.replace(TextWarningConstants.TTAAII,
                    ttaaii);

            TextDisplayModel.getInstance().createStdTextProduct(token, w,
                    siteNode);

            workProductId = afosId.substring(3);
            warnGenFlag = true;
            VizApp.runAsync(new Runnable() {
                @Override
                public void run() {
                    long t0 = System.currentTimeMillis();
                    // For VTEC related warning messages, turn off wordwrap by
                    // default.
                    if (textEditor == null) {
                        openDialog();
                    }

                    if (textEditor.isDisposed()) {
                        return;
                    }

                    // textEditor.setWordWrap(false);

                    // Set the text editor's contents to the warning message.
                    textEditor.removeVerifyListener(TextEditorDialog.this);
                    textEditor.setText(w);
                    //
                    // // Mark the uneditable warning text
                    // if (markUneditableText(textEditor)) {
                    // // Add listener to monitor attempt to edit locked text
                    // textEditor.addVerifyListener(TextEditorDialog.this);
                    // }
                    showDialog();
                    long t1 = System.currentTimeMillis();
                    SimpleDateFormat sdf = new SimpleDateFormat(
                            "yyyy-MM-dd HH:mm:ss.SSS");
                    sdf.setTimeZone(TimeZone.getTimeZone("GMT"));
                    System.out.println(sdf.format(new Date())
                            + ": Text Workstation took " + (t1 - t0)
                            + "ms to show dialog");
                    enterEditor();

                    if (autoWrapMenuItem != null) {
                        Menu menu = autoWrapMenuItem.getMenu();
                        for (MenuItem item : menu.getItems()) {
                            if (item.getSelection()) {
                                Object obj = item.getData();
                                if (obj instanceof WrapButtonCfg) {
                                    WrapButtonCfg buttonCfg = (WrapButtonCfg) obj;
                                    if (buttonCfg.isWrapEnabled()) {
                                        charWrapCol = buttonCfg.getWrapCol();
                                        wordWrapEnabled = true;
                                        recompileRegex();
                                    } else {
                                        wordWrapEnabled = false;
                                    }
                                }
                            }
                        }
                    }
                    saved = false;
                }
            });
        }
    }

    /**
     * Update the header text field based on the Text Display Model's
     * information for this editor.
     */
    private void replaceWorkProductId() {
        String headerText = headerTF.getText();

        if (headerText.contains(AFOSParser.DRAFT_PIL)
                && TextDisplayModel.getInstance().hasStdTextProduct(token)) {
            StdTextProductId textProdId = TextDisplayModel.getInstance()
                    .getStdTextProduct(token).getProdId();
            String operationPIL = textProdId.getNnnid() + textProdId.getXxxid();

            int iOpPIL = headerText.indexOf(operationPIL);
            int iDraftPIL = headerText.indexOf(AFOSParser.DRAFT_PIL);

            String draftPIL = headerText.substring(iDraftPIL, iDraftPIL + 6);
            if (iOpPIL < 0) {
                headerTF.setText(headerText.replace(draftPIL, operationPIL));
            } else {
                headerTF.setText(headerText.replace(draftPIL, ""));
            }
        } else if (headerText.startsWith("- -") && headerText.contains("WRK")) {
            StdTextProduct textProd = TextDisplayModel.getInstance()
                    .getStdTextProduct(token);

            // cccid + nnnid + xxxid
            String nnn = textProd.getNnnid();
            String xxx = textProd.getXxxid();
            String nnnXxx = nnn + xxx;
            String site = SiteMap.getInstance().getSite4LetterId(
                    textProd.getCccid());
            String wmoWRKId = textProd.getCccid() + nnnXxx + " "
                    + getAddressee() + "\nTTAA00 " + site;
            String header = headerText;
            header = header.replaceFirst("\n" + nnnXxx, "");
            header = header.replaceFirst("-", "ZCZC");
            header = header.replaceFirst("-", wmoWRKId);
            headerTF.setText(header);
            this.getStdTextProduct().setSite(site);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.texteditor.msgs.ITextCharWrapCallback#setCharWrapCol
     * (int)
     */
    @Override
    public void setCharWrapCol(int charWrapCol) {
        clearButtons();
        Menu menu = autoWrapMenuItem.getMenu();
        menu.getItem(menu.getItemCount() - 1).setSelection(true);
        this.charWrapCol = charWrapCol;
        otherCharWrapCol = new Integer(charWrapCol);
        recompileRegex();
    }

    /**
     * Update the buttonology of the text workstation if this text editor dialog
     * corresponds to one that belongs to the text workstation (TextWS) and not
     * the D-2D.
     * 
     * @param buttonology
     */
    private void updateButtonology(String buttonology) {
        if ((!(token.equals("0"))) && (!(token.equals("9")))) {
            // Update the title of this TextWS text window
            winTitle = "Text " + token + ": " + buttonology;
            shell.setText(winTitle);
            callbackClient.updateText(Integer.parseInt(token) - 1, winTitle);
        } else if (token.equals("0")) {
            winTitle = "Text Display" + ": " + buttonology;
            shell.setText(winTitle);
        } else {
            winTitle = "Text Warngen" + ": " + buttonology;
            shell.setText(winTitle);
        }
    }

    /**
     * Clear the information on the text workstaion button for this text editor
     * dialog.
     */
    public void clearButtonology() {
        if ((!(token.equals("0"))) && (!(token.equals("9")))) {
            // Update the title of this TextWS text window
            winTitle = "Text " + token;
            shell.setText(winTitle);
            callbackClient.restoreText(Integer.parseInt(token) - 1);
        } else if (token.equals("0")) {
            winTitle = "Text Display";
            shell.setText(winTitle);
        } else {
            winTitle = "Text Warngen";
            shell.setText(winTitle);
        }

        if (statusBarLabel.getText().contains("Attachment: ")) {
            statusBarLabel.setText("");
            attachedFile = null;
            attachedFilename = null;
        }
    }

    private void updateNowEditingTitle() {
        if (inEditMode) {
            winTitle += " (Now Editing)";
            shell.setText(winTitle);
            inEditMode = true;
        } else {
            if (winTitle.indexOf("(") != -1) {
                winTitle = winTitle.substring(0, winTitle.indexOf("("));
                shell.setText(winTitle);
            }
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.texteditor.msgs.IRecoverEditSessionCallback#recoverSession
     * (java.lang.String)
     */
    @Override
    public void recoverSession(String sessionToRecover) {
        // read the auto save file, create first
        autoSave = new AutoSaveTask(sessionToRecover);

        // recover StdTextProduct
        StdTextProduct product = autoSave.retrieveProduct();

        textEditor.setText(product.getProduct());

        // Update text display model with the product that was
        // retrieved for display in this text editor dialog
        // instance.
        TextDisplayModel.getInstance().setStdTextProduct(token, product);

        // TODO Need to set the title fields, etc based on saved product,
        // determined by parsing filename?
        // Update the buttonology of the text workstation if this
        // text editor dialog corresponds to one that belongs to the
        // text workstation (TextWS) and not the D-2D.
        // updateButtonology(afosCmdTF.getText());

        // enter editor
        enterEditor();
        saved = true;
    }

    /**
     * Displays the spell checker dialog to initiate spell checking.
     */
    private void checkSpelling() {
        SpellCheckDlg spellCheckDlg = new SpellCheckDlg(getParent(),
                textEditor, StatusConstants.CATEGORY_WORKSTATION,
                StatusConstants.SUBCATEGORY_CONNECTIVITY);
        spellCheckDlg.open();

    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.texteditor.msgs.IScriptRunnerObserver#clearTextDisplay()
     */
    @Override
    public void clearTextDisplay() {
        clearTextEditor();
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.texteditor.msgs.IScriptRunnerObserver#
     * manageScriptOutputWindow(boolean)
     */
    @Override
    public void manageScriptOutputWindow(boolean visible) {
        if (visible) {
            // need to set state of menu item to true
            if (scriptsShowOutputItem != null
                    && !scriptsShowOutputItem.isDisposed()) {
                scriptsShowOutputItem.setSelection(true);
            }

            // update the script editor window
            if (scriptEditor != null) {
                scriptEditor.setScriptOutputState(true);
            }

            // create the script output window
            if (scriptOutput == null || !scriptOutput.isDisposed()) {
                scriptOutput = new ScriptOutputDlg(shell, token);
                // open the script output window
                scriptOutput.setCloseCallback(new ICloseCallback() {

                    @Override
                    public void dialogClosed(Object returnValue) {
                        // update the menu following close
                        if (scriptsShowOutputItem != null
                                && !scriptsShowOutputItem.isDisposed()) {
                            scriptsShowOutputItem.setSelection(false);
                        }
                        // update script editor window
                        if (scriptEditor != null) {
                            scriptEditor.setScriptOutputState(false);
                        }
                        scriptOutput = null;
                    }
                });
                scriptOutput.open();
            } else {
                scriptOutput.bringToTop();
            }
        } else {
            if (scriptOutput != null) {
                scriptOutput.close();
            } else {
                if (scriptsShowOutputItem != null
                        && !scriptsShowOutputItem.isDisposed()) {
                    scriptsShowOutputItem.setSelection(false);
                }
                if (scriptEditor != null) {
                    scriptEditor.setScriptOutputState(false);
                }
            }
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.texteditor.msgs.IScriptRunnerObserver#setAccumulation
     * (boolean)
     */
    @Override
    public void setAccumulation(boolean flag) {
        this.accumChkBtn.setSelection(flag);
        productQueryJob.setAccumulate(flag);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.texteditor.msgs.IScriptRunnerObserver#showErrorMessage
     * (java.lang.String, java.lang.Throwable)
     */
    @Override
    public void showErrorMessage(String errorMsg, Throwable cause) {
        statusHandler.handle(Priority.PROBLEM, errorMsg, cause);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.texteditor.msgs.IScriptRunnerObserver#showScriptStatus
     * (java.lang.String)
     */
    @Override
    public void showScriptStatus(String statusMsg) {
        statusBarLabel.setText(statusMsg);
        statusHandler.handle(Priority.EVENTA, statusMsg);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.texteditor.msgs.IScriptRunnerObserver#writeText(java
     * .lang.String)
     */
    @Override
    public void writeText(String text) {
        if (scriptOutput != null) {
            scriptOutput.addMessage(text);
        }
    }

    /**
     * Sets the state of the script running controls at the start and end of
     * running a script.
     * 
     * @param flag
     *            controls enabled status of controls
     */
    private void setScriptControls(boolean flag) {
        scriptRunnerComp.setVisible(flag);
        ((GridData) scriptRunnerComp.getLayoutData()).exclude = !flag;

        scriptsContinueItem.setEnabled(false);
        scriptContinueBtn.setVisible(flag);
        scriptContinueBtn.setEnabled(false);
        scriptContinue = false;

        scriptsCancelItem.setEnabled(flag);
        scriptCancelBtn.setVisible(flag);
        scriptCancelBtn.setEnabled(flag);
        cancelScript = false;

        scriptsSkipWaitItem.setEnabled(false);
        scriptSkipWaitBtn.setVisible(flag);
        scriptSkipWaitBtn.setEnabled(false);
        skipWait = false;

        shell.layout();
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.texteditor.msgs.IScriptRunnerObserver#executeTextScript
     * (java.lang.String)
     */
    @Override
    public void executeTextScript(String script) {
        // This need to implement execution of script runner -- basically, the
        // script runner needs to live in this class
        if (scriptRunner == null) {
            scriptRunner = new TextWsScriptRunner(this, this.token);
        }
        try {
            setScriptMenuControls(false);
            setScriptControls(true);
            scriptRunner.executeScript(script);
        } catch (Exception e) {
            setScriptMenuControls(true);
            statusHandler.handle(Priority.PROBLEM,
                    "Unable to execute script (SCRP)", e);
        } finally {
            // intentionally empty
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.texteditor.msgs.IScriptRunnerObserver#postProductToEditor
     * (java.lang.String[], java.lang.String[])
     */
    @Override
    public void postProductToEditor(final String[] products, final String[] pils) {
        versionMenuItem.setEnabled(false);
        StringBuffer text = new StringBuffer();
        if (accumChkBtn.getSelection() && !textEditor.getText().isEmpty()) {
            textEditor.append("\n");
            for (String pil : pils) {
                displayedPils.add(pil);
            }
            makeObsRegex();
        } else {
            textEditor.setText("");
            displayedPils.clear();
            isSaoMetarFlag = false;
            for (String pil : pils) {
                displayedPils.add(pil);
            }
            makeObsRegex();
        }
        String first = "";
        for (String product : products) {
            text.append(first).append(product);
            first = "\n";
        }
        textEditor.append(text.toString());
    }

    /**
     * Provides actions needed by the 'Script -> Run...' menu item. It uses a
     * file open dialog to request a script from the user and then executes the
     * script.
     */
    private void loadAndRunScript() {
        FileDialog fd = new FileDialog(shell, SWT.OPEN);
        fd.setText("Select script to run");
        fd.setFilterExtensions(IScriptEditor.SCRIPT_EXTNS);
        fd.setFilterNames(IScriptEditor.SCRIPT_NAMES);
        String result = fd.open();
        if (null == result) {
            return;
        }
        executeTextScript(result);
    }

    /**
     * Open up the script editor with the desired values.
     */
    private void onDisplayScriptEditor() {
        scriptsEditItem.setEnabled(false);
        scriptsRunItem.setEnabled(false);
        getDisplay().asyncExec(new Runnable() {
            @Override
            public void run() {
                if (scriptEditor == null) {
                    scriptEditor = new ScriptEditorDialog(shell,
                            TextEditorDialog.this, token, scriptsShowOutputItem
                                    .getSelection());
                }
                scriptEditor.open();
            }
        });
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.texteditor.scripting.dialogs.IScriptEditorObserver#
     * onContinueScript()
     */
    @Override
    public void onContinueScript() {
        scriptContinue = true;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.texteditor.scripting.dialogs.IScriptEditorObserver#
     * onCancelScript()
     */
    @Override
    public void onCancelScript() {
        cancelScript = true;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.texteditor.scripting.dialogs.IScriptEditorObserver#
     * onSkipWait()
     */
    @Override
    public void onSkipWait() {
        skipWait = true;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.texteditor.msgs.IScriptRunnerObserver#scriptComplete()
     */
    @Override
    public void scriptComplete() {
        if (scriptRunner != null) {
            scriptRunner.dispose();
        }
        scriptRunner = null;
        setScriptControls(false);
        if (scriptEditor != null) {
            scriptEditor.scriptComplete();
        } else {
            setScriptMenuControls(true);
        }
        statusBarLabel.setText("");
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.texteditor.msgs.IScriptRunnerObserver#cancelScript()
     */
    @Override
    public boolean cancelScript() {
        return cancelScript;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.texteditor.msgs.IScriptRunnerObserver#skipWait()
     */
    @Override
    public boolean skipWait() {
        return skipWait;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.texteditor.msgs.IScriptRunnerObserver#continueScript()
     */
    @Override
    public boolean continueScript() {
        return scriptContinue;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.texteditor.scripting.dialogs.IScriptEditorObserver#
     * windowClosing()
     */
    @Override
    public void windowClosing() {
        if (scriptRunner == null) {
            setScriptMenuControls(true);
        }
        scriptEditor = null;
    }

    /**
     * Enables/disables the Script=>Edit and script=>Run menu items.
     * 
     * @param state
     *            true to enable, false to disable
     */
    private void setScriptMenuControls(boolean state) {
        scriptsEditItem.setEnabled(state);
        scriptsRunItem.setEnabled(state);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.texteditor.msgs.IScriptRunnerObserver#activateControls
     * (boolean, boolean)
     */
    @Override
    public void activateControls(boolean skipWait, boolean canContinue) {
        scriptsContinueItem.setEnabled(canContinue);
        scriptContinueBtn.setEnabled(canContinue);
        scriptContinue = false;

        scriptsSkipWaitItem.setEnabled(skipWait);
        scriptSkipWaitBtn.setEnabled(skipWait);
        this.skipWait = false;
        if (scriptEditor != null) {
            scriptEditor.setScriptControls(canContinue, skipWait);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.texteditor.msgs.IScriptRunnerObserver#isEditMode()
     */
    @Override
    public boolean isEditMode() {
        boolean result = false;
        if (inEditMode && isOpen()) {
            result = true;
        } else {
            inEditMode = result;
        }

        return result;
    }

    @Override
    public void setDisplayedProduct(StdTextProduct product) {
        // Implement the Accum button in the Window Control Bar
        if (accumChkBtn.getSelection()) {
            displayedPils.add(product.getCccid() + product.getNnnid()
                    + product.getXxxid());
            if (textEditor.getText().length() > 0) {
                textEditor.append("\n");
            }
            makeObsRegex();
        } else {
            textEditor.setText("");
            displayedPils.clear();
            displayedPils.add(product.getCccid() + product.getNnnid()
                    + product.getXxxid());
            makeObsRegex();
        }
        String textProduct = product.getASCIIProduct();
        if ((product.getNnnid() + product.getXxxid())
                .startsWith(AFOSParser.DRAFT_PIL)) {
            String[] nnnxxx = TextDisplayModel.getNnnXxx(textProduct);
            String operationalPil = nnnxxx[0] + nnnxxx[1];
            String siteNode = SiteAbbreviationUtil.getSiteNode(nnnxxx[1]);
            String ttaaii = SiteAbbreviationUtil.getTtaaii(siteNode
                    + operationalPil);
            textProduct = textProduct.replace(TextWarningConstants.TTAAII,
                    ttaaii).replace(operationalPil,
                    product.getNnnid() + product.getXxxid());
            originalText = textProduct;
        }

        textEditor.append(textProduct);
        markUneditableText(textEditor);

        // Update text display model with the product that was
        // retrieved for display in this text editor dialog
        // instance.
        TextDisplayModel.getInstance().setStdTextProduct(token, product);
    }

    /**
     * Handles the update option of product retrieval.
     * 
     * @param product
     *            the product to update
     */
    public void updateDisplayedProduct(StdTextProduct product) {
        // need to find the product we are replacing -- assuming there are more
        // than 1
        int lineIndex = -1;
        int noOfLines = textEditor.getLineCount();
        String regex = null;
        // If needed set up to use a pattern
        if ("SAO".equals(product.getNnnid())) {
            regex = "(METAR |SPECI )" + product.getXxxid();
        } else if ("MTR".equals(product.getNnnid())) {
            regex = "(METAR |SPECI )K" + product.getXxxid();
        }

        boolean haveWMOHeader = false;

        if (regex != null) {
            // Find METAR to replace.
            // This assumes the product's METAR is the latest METAR and not a
            // duplicate or older in the display. If not the case a check of
            // the timestamp can be performed adjusting for possible roll over
            // to the next month.
            Pattern pattern = Pattern.compile(regex);
            for (int i = 0; i < noOfLines; ++i) {
                if (pattern.matcher(textEditor.getLine(i)).find()) {
                    lineIndex = i;
                    if (i > 0) {
                        Matcher m = METAR_PATTERN.matcher(textEditor
                                .getLine(i - 1));
                        if (!m.find()) {
                            // Adjust to the METAR's Wmo Header line.
                            --lineIndex;
                            haveWMOHeader = true;
                        }
                    }
                    break;
                }
            }

            if (lineIndex == -1) {
                // METAR for site not being observed
                return;
            }
        } else {
            String prefix = product.getWmoid() + " " + product.getSite();
            for (int i = 0; i < noOfLines; i++) {
                if (textEditor.getLine(i).startsWith(prefix)) {
                    // found replacement point
                    lineIndex = i;
                    break;
                }
            }
        }
        if (lineIndex == -1) {
            lineIndex = noOfLines;
        }

        String productText = product.getProduct();

        if (lineIndex == noOfLines) {
            textEditor.append("\n");
            textEditor.append(productText);
        } else {
            // need to find end of current product, then replace.
            int nextProductLoc = -1;
            if (haveWMOHeader) {
                // Assume next product also has WMO header
                for (int i = lineIndex + 2; i < noOfLines; i++) {
                    String temp = textEditor.getLine(i);
                    if (!temp.startsWith(" ")) {
                        if (temp.startsWith("METAR")
                                || temp.startsWith("SPECI")) {
                            continue;
                        } else {
                            // found next product
                            nextProductLoc = i;
                            break;
                        }
                    }
                }
            } else {
                // Remove WMO header
                productText = productText
                        .substring(productText.indexOf('\n') + 1);
                // Assume next product does not have a WMO header
                for (int i = lineIndex + 1; i < noOfLines; ++i) {
                    String temp = textEditor.getLine(i);
                    if (temp.startsWith("METAR") || temp.startsWith("SPECI")) {
                        nextProductLoc = i;
                        break;
                    }
                }
            }

            int start = textEditor.getOffsetAtLine(lineIndex);
            int end = textEditor.getCharCount();
            if (nextProductLoc != -1) {
                end = textEditor.getOffsetAtLine(nextProductLoc);
                textEditor.replaceTextRange(start, end - start, productText
                        + "\n");
            } else {
                textEditor.replaceTextRange(start, end - start, productText);
            }
        }

        // if updating, we need to highlight the site name. If the first word of
        // the line is "METAR" or "SPECI", we need to highlight the second word
        if (haveWMOHeader) {
            lineIndex++; // need to skip the WMO header
        }
        String line = textEditor.getLine(lineIndex);
        int startIndex = textEditor.getOffsetAtLine(lineIndex);
        if (line.startsWith("METAR") || line.startsWith("SPECI")) {
            startIndex += 6; // skip first word plus a space
        }
        int endIndex = textEditor.getText().indexOf(" ", startIndex);
        textEditor.setStyleRange(new StyleRange(startIndex, endIndex
                - startIndex, getDisplay().getSystemColor(UPDATE_FG),
                getDisplay().getSystemColor(UPDATE_BG), SWT.NORMAL));
        textEditor.setSelection(startIndex, endIndex);

        // Update text display model with the product that was
        // retrieved for display in this text editor dialog
        // instance.
        TextDisplayModel.getInstance().setStdTextProduct(token, product);
        updateCount.addAndGet(-1);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.texteditor.msgs.IWmoBrowserCallback#setCCCCField(java
     * .lang.String)
     */
    @Override
    public void setCCCCField(String cccc) {
        ccccTF.setText(cccc);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.texteditor.msgs.IWmoBrowserCallback#setTTAAiiField(java
     * .lang.String)
     */
    @Override
    public void setTTAAiiField(String ttaaii) {
        wmoTtaaiiTF.setText(ttaaii);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.texteditor.msgs.IWmoBrowserCallback#setCommandText(java
     * .lang.String)
     */
    @Override
    public void setCommandText(String commandText) {
        updateButtonology(commandText);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.texteditor.msgs.IWmoBrowserCallback#getQueryTransport()
     */
    @Override
    public IQueryTransport getQueryTransport() {
        return queryTransport;
    }

    /**
     * Checks product to verify all required fields have been filled in.
     * 
     * @return
     */
    private boolean verifyRequiredFields() {
        boolean rval = true;

        String text = textEditor.getText();
        int startIndex = text.indexOf("!**");
        if (startIndex >= 0) {
            int endIndex = text.indexOf("**!", startIndex);

            if (endIndex >= startIndex) {
                rval = false;
                textEditor.setSelection(startIndex, endIndex + 3);
                userInformation("You must modify the selected region before sending or saving the product.");
                textEditor.setFocus();
            }
        }

        return rval;
    }

    /**
     * Checks to see if a product can be resent.
     * 
     * @return true when product can be resent
     */
    private boolean verifyResendProduct() {
        boolean resend = true;

        String body = textEditor.getText();
        StdTextProduct stdTextProduct = TextDisplayModel.getInstance()
                .getStdTextProduct(token);
        if (body == null || body.length() == 0) {
            userInformation("Resend Warning Product Error",
                    "There is no product to send. \n Action aborted!");
            resend = false;
        } else if (gfeForbidden(stdTextProduct.getCccid(),
                stdTextProduct.getNnnid())) {
            userInformation(
                    "Resend Warning Product Error",
                    "This product MUST be edited in GFE! \n Please exit and return to GFE. \n Action Aborted!");
            resend = false;
        }

        return resend;
    }

    /**
     * Add RESENT to the end of the MND line
     */
    private String resendMessage() {
        boolean updatedMND = false;
        StringBuffer sb = new StringBuffer();

        for (String line : textEditor.getText().split("\n")) {
            if (!updatedMND
                    && (line.endsWith("WARNING")
                            || line.endsWith("WARNING...TEST")
                            || line.endsWith("WARNING...CORRECTED")
                            || line.endsWith("WARNING...CORRECTED...TEST")
                            || line.endsWith("STATEMENT")
                            || line.endsWith("STATEMENT...TEST")
                            || line.endsWith("STATEMENT...CORRECTED")
                            || line.endsWith("STATEMENT...CORRECTED...TEST")
                            || line.endsWith("FORECAST")
                            || line.endsWith("FORECAST...TEST")
                            || line.endsWith("ADVISORY")
                            || line.endsWith("ADVISORY...TEST")
                            || line.endsWith("ADVISORY...CORRECTED") || line
                            .endsWith("ADVISORY...CORRECTED...TEST"))) {
                line += "...RESENT";
                updatedMND = true;
            }
            sb.append(line + "\n");
        }

        return sb.toString();
    }

    private void removeOptionalFields() {
        String text = textEditor.getText();
        int startIndex = text.indexOf("!--");
        int endIndex = text.indexOf("--!", startIndex);
        while (startIndex >= 0 && endIndex >= startIndex) {
            String part1 = text.substring(0, startIndex).trim();
            String part2 = text.substring(endIndex + 3).trim();
            text = part1 + "\n\n" + part2;
            startIndex = text.indexOf("!--");
            endIndex = text.indexOf("--!", startIndex);
        }
        textEditor.setText(text);
    }

    /*
     * This class handles a timer to auto save a product to a file.
     */
    private class AutoSaveTask {

        private String filenameIdentifier = null;

        private Timer timer = null;

        private final IPathManager pathManager = PathManagerFactory
                .getPathManager();

        private final LocalizationContext lc = pathManager.getContext(
                LocalizationType.CAVE_STATIC, LocalizationLevel.USER);

        private File file = null;

        public AutoSaveTask(String ttaaii, String cccc) {
            this.filenameIdentifier = ttaaii + "_" + cccc;
            setupTimer();
        }

        public AutoSaveTask(String filename) {
            if (filename != null) {
                this.file = getFile(filename);
            }

            StdTextProduct prod = retrieveProduct();
            this.filenameIdentifier = prod.getWmoid() + "_" + prod.getSite();

            setupTimer();
        }

        private File getFile(String filename) {
            LocalizationFile lFile = pathManager.getLocalizationFile(lc,
                    SAVED_SESSION_DIR + filename);
            return lFile.getFile();
        }

        public void saveProduct(StdTextProduct stdTextProduct) {
            String filename = "window_"
                    + token
                    + "_"
                    + filenameIdentifier
                    + "_"
                    + AUTOSAVE_DATE_FORMAT.format(TimeTools.getSystemCalendar()
                            .getTime()) + ".txt";
            BufferedOutputStream bufStream = null;

            try {
                // delete and write new file, rename didn't always work
                // rename would end up writing a new file every time and
                // kept the original in sync
                if (file != null && file.exists()) {
                    file.delete();
                }

                file = getFile(filename);

                synchronized (this) {
                    bufStream = new BufferedOutputStream(new FileOutputStream(
                            file));
                    bufStream.write(SerializationUtil.marshalToXml(
                            stdTextProduct).getBytes());
                }

                // TODO Should the edit session be backed up to the server?
                // lFile.save();
            } catch (Exception e) {
                statusHandler.handle(Priority.PROBLEM, "Auto save failed", e);
            } finally {
                if (bufStream != null) {
                    try {
                        bufStream.close();
                    } catch (IOException e) {
                        statusHandler.handle(Priority.VERBOSE,
                                "Failed to close file stream", e);
                    }
                }
            }
        }

        public StdTextProduct retrieveProduct() {
            StdTextProduct rval = null;

            if (file != null) {
                BufferedInputStream bufStream = null;

                try {
                    String xml = null;
                    synchronized (this) {
                        byte[] b = new byte[(int) file.length()];
                        bufStream = new BufferedInputStream(
                                new FileInputStream(file));
                        bufStream.read(b);
                        xml = new String(b);
                    }

                    rval = (StdTextProduct) SerializationUtil
                            .unmarshalFromXml(xml);
                } catch (Exception e) {
                    statusHandler.handle(Priority.PROBLEM,
                            "Retrieval of product failed", e);
                } finally {
                    if (bufStream != null) {
                        try {
                            bufStream.close();
                        } catch (IOException e) {
                            statusHandler.handle(Priority.VERBOSE,
                                    "Failed to close file stream", e);
                        }
                    }
                }

            }

            return rval;
        }

        public boolean stop() {
            boolean success = false;
            try {
                if (file != null) {
                    file.delete();
                    file = null;
                }

                success = true;
            } catch (Exception e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Failed to remove auto save file", e);
            }

            if (timer != null) {
                timer.cancel();
                timer = null;
            }

            return success;
        }

        public void stopTimer() {
            if (timer != null) {
                timer.cancel();
                timer = null;
            }
        }

        private void setupTimer() {
            if (timer != null) {
                timer.cancel();
            }

            timer = new Timer();

            TimerTask saveEditSessionTask = new TimerTask() {
                @Override
                public void run() {
                    getDisplay().syncExec(new Runnable() {
                        public void run() {
                            if (!shell.isDisposed()) {
                                if (autoSave == AutoSaveTask.this) {
                                    saveEditedProduct(true, false, false);
                                }
                            }
                        }
                    });
                }
            };

            timer.schedule(saveEditSessionTask, 600, 60000);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @seecom.raytheon.uf.viz.core.notification.INotificationObserver#
     * notificationArrived
     * (com.raytheon.uf.viz.core.notification.NotificationMessage[]) This is the
     * callback method called by the NotificationManagerJob when observations
     * are received. It parses the notifications and triggers a load of a
     * product that matches the currently loaded product. Short circuits are
     * provided for cases when the dialog is either not displaying a product or
     * is in edit mode.
     */
    @Override
    public void notificationArrived(NotificationMessage[] messages) {
        for (NotificationMessage message : messages) {

            try {
                Object payload = message.getMessagePayload();
                String msgPIL = null;
                if (payload instanceof AlarmAlertProduct) {
                    msgPIL = ((AlarmAlertProduct) payload).getProductId();
                } else if (payload instanceof String) {
                    msgPIL = (((String) payload).split("_"))[0];
                } else {
                    statusHandler.handle(Priority.EVENTA,
                            "received invalid message, class is "
                                    + payload.getClass().getSimpleName());
                    msgPIL = "";
                }
                if (isObsDisplayed(msgPIL)) {
                    updateCount.addAndGet(1);
                    ICommand command = CommandFactory.getAfosCommand(msgPIL);
                    UpdateObsRun run = new UpdateObsRun(command);
                    VizApp.runSync(run);
                }
            } catch (NotificationException e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Could not parse message from server", e);
            }
        }
    }

    /**
     * Determines is the edit window is currently displaying an observation. To
     * be valid, the PIL of the displayed product must represent an observation.
     * Note: From the OB9.0 TCL code, observations are products having an "NNN"
     * ID of "MRT" or "SAO".
     * 
     * @param displayedPIL
     *            AFOS PIL of the displayed product
     * 
     * @return true is an observation is displayed
     */
    private boolean isObsDisplayed(final String displayedPIL) {
        if (obsRegex != null) {
            return obsRegex.matcher(displayedPIL).matches();
        }
        return false;
    }

    /**
     * Generate and compile the regular expression for the observations in the
     * current list of displayed PILs.
     */
    private void makeObsRegex() {
        StringBuilder obsPat = new StringBuilder();
        boolean firstPil = true;
        for (String pil : displayedPils) {
            if (pil.matches(".{3}((MTR)|(SAO)).*")) {
                isSaoMetarFlag = true;
                if (firstPil) {
                    obsPat.append("(");
                    firstPil = false;
                } else {
                    obsPat.append("|(");
                }
                obsPat.append(pil);
                if (pil.length() == 6) {
                    obsPat.append(".{3})");
                } else {
                    obsPat.append(")");
                }
            } else if (pil.matches("((MTR)|(SAO)).{3}")) {
                isSaoMetarFlag = true;
                if (firstPil) {
                    obsPat.append("(.{3}");
                    firstPil = false;
                } else {
                    obsPat.append("|(.{3}");
                }
                obsPat.append(pil).append(")");
            }
        }
        if (obsPat.length() > 0) {
            obsRegex = Pattern.compile(obsPat.toString());
        } else {
            obsRegex = null;
        }
    }

    /**
     * Private class to isolate the action of updating the display from the
     * NotificationManagerJob's call back from it's thread.
     */
    private class UpdateObsRun implements Runnable {
        /** the AFOS command to run */
        ICommand command = null;

        /**
         * Constructor.
         * 
         * @param command
         *            the AFOS command to run
         */
        public UpdateObsRun(ICommand command) {
            this.command = command;
        }

        @Override
        public void run() {
            /*
             * DR15103 - set the flag to 'true' before executing AFOS command so
             * the AFOS command box is not cleared when obs are updated
             */
            executeCommand(command, true);
        }

    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.texteditor.msgs.IScriptRunnerObserver#scriptError()
     */
    @Override
    public void scriptError() {
        scriptHasError = true;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.texteditor.msgs.IScriptRunnerObserver#addStdErrMsg(java
     * .lang.String)
     */
    @Override
    public void addStdErrMsg(String errMsg) {
        if (scriptErrorBfr == null) {
            scriptErrorBfr = new StringBuffer();
        }
        scriptErrorBfr.append(errMsg);

    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.texteditor.msgs.IScriptRunnerObserver#isScriptError()
     */
    @Override
    public boolean isScriptError() {
        return scriptHasError;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.texteditor.msgs.IScriptRunnerObserver#writeErrMsg(java
     * .lang.String)
     */
    @Override
    public void writeErrMsg(String errMsg) {
        StringBuffer msg = new StringBuffer(errMsg);
        if (scriptHasError) {
            // manage display of script error
            if (scriptErrorBfr != null) {
                msg.append("\n");
                msg.append(scriptErrorBfr);
            }
            showErrorMessage(msg.toString(), null);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.texteditor.msgs.IScriptRunnerObserver#clearErrBuffer()
     */
    @Override
    public void clearErrBuffer() {
        scriptHasError = false;
        scriptErrorBfr = null;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.ui.dialogs.CaveSWTDialog#preOpened()
     */
    @Override
    protected void preOpened() {
        // Shell shell = ted.getShell();
        if (textWorkstationFlag) {
            Rectangle rect = getShell().getDisplay().getClientArea();
            int x = rect.width / 4;

            // account for dual monitor
            if (rect.width > rect.height * 2) {
                x /= 2;
            }

            int index = getText().indexOf(" ");
            int editorIndex = new Integer(getText().substring(index + 1))
                    .intValue();

            int offset = (editorIndex - 1) * 25;
            getShell().setLocation(x + offset, rect.height / 4 + offset);
        }

        inEditMode = false;

        getParent().addDisposeListener(new DisposeListener() {

            @Override
            public void widgetDisposed(DisposeEvent e) {
                // Clean up any active timer
                stopAutoSave();
            }
        });
    }

    /**
     * Stop auy active autosave timers. This is synchronized to prevent messing
     * up an active auto save. This the only place autoSave.stop() should be
     * called.
     */
    private synchronized void stopAutoSave() {
        if (autoSave != null) {
            autoSave.stop();
            autoSave = null;
        }
    }

    /*
     * This class handles sending a request to a Thrift Client.
     */
    private class ThriftClientRunnable implements Runnable {

        private final OUPRequest request;

        public ThriftClientRunnable(OUPRequest request) {
            this.request = request;
        }

        @Override
        public void run() {
            try {
                final OUPResponse response = (OUPResponse) ThriftClient
                        .sendRequest(request);

                // if (response.isAcknowledged()) {
                if (response.hasFailure()) {
                    Priority p = Priority.EVENTA;
                    if (!response.isAttempted()) {
                        // if was never attempted to send or store even locally
                        p = Priority.CRITICAL;
                    } else if (!response.isSendLocalSuccess()) {
                        // if send/store locally failed
                        p = Priority.CRITICAL;
                    } else if (!response.isSendWANSuccess()) {
                        // if send to WAN failed
                        if (response.getNeedAcknowledgment()) {
                            // if ack was needed, if it never sent then no ack
                            // was recieved
                            p = Priority.CRITICAL;
                        } else {
                            // if no ack was needed
                            p = Priority.EVENTA;
                        }
                    } else if (response.getNeedAcknowledgment()
                            && !response.isAcknowledged()) {
                        // if sent but not acknowledged when acknowledgement is
                        // needed
                        p = Priority.CRITICAL;
                    }

                    statusHandler.handle(p, response.getMessage());
                } else {
                    // no failure
                    // As of DR 15418, nothing is done with
                    // response.getChangedBBB()
                }

                Thread.interrupted();
            } catch (VizException e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Error transmitting text product", e);

            }
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.ui.dialogs.CaveSWTDialogBase#disposed()
     */
    @Override
    protected void disposed() {
        if (smlFont != null) {
            smlFont.dispose();
        }

        if (medFont != null) {
            medFont.dispose();
        }

        if (lrgFont != null) {
            lrgFont.dispose();
        }

        if (clipboard != null) {
            clipboard.dispose();
        }

        if (browser != null) {
            browser.close();
            browser = null;
        }

        inEditMode = false;
    }

    /**
     * checks if the offset is inside a style range, which indicates a lock
     * 
     * @param offset
     * @return
     */
    private boolean hasLockAtOffset(int offset) {
        boolean rval = false;
        StyleRange style = textEditor.getStyleRangeAtOffset(offset);
        if (style != null) {
            rval = true;
        }
        return rval;
    }

    /**
     * Gets the starting offset of the style range that contains the passed in
     * offset
     * 
     * @param offset
     * @return
     */
    private int getStartOfLockAtOffset(int offset) {
        int rval = -1;
        if (hasLockAtOffset(offset)) {
            StyleRange[] ranges = textEditor.getStyleRanges();
            for (StyleRange range : ranges) {
                if (offset >= range.start
                        && offset <= range.start + range.length) {
                    rval = range.start;
                    break;
                }
            }
        }
        return rval;
    }

    /**
     * get the length of the offset that contains the passed in offset
     * 
     * @param offset
     * @return
     */
    private int getLengthOfLockAtOffset(int offset) {
        int rval = -1;
        if (hasLockAtOffset(offset)) {
            StyleRange[] ranges = textEditor.getStyleRanges();
            for (StyleRange range : ranges) {
                if (offset >= range.start
                        && offset <= range.start + range.length) {
                    rval = range.length;
                    break;
                }
            }
        }
        return rval;
    }

    /**
     * check if a line contains any locks at all
     * 
     * @param lineNumber
     * @return
     */
    private boolean containsLock(int lineNumber) {
        boolean rval = false;

        int lineStart = textEditor.getOffsetAtLine(lineNumber);
        int lineLength = textEditor.getLine(lineNumber).length();

        StyleRange[] ranges = textEditor.getStyleRanges(lineStart, lineLength);
        if (ranges != null && ranges.length > 0) {
            rval = true;
        }
        return rval;
    }

    // Allow the verify listener on the text editor to ignore wrapping.
    // private boolean performingWrap = false;

    /**
     * rewrap the paragraph(s) starting from the line containing the character
     * at the offset start and ending in the pargraph with the line containing
     * end.
     * 
     * @param start
     * @param end
     */
    private void rewrap(int start, int end) {
        if (wordWrapEnabled == false) {
            return;
        }
        if (this.standardWrapRegex == null) {
            recompileRegex();
        }
        // performingWrap = true;
        int lineNumber = textEditor.getLineAtOffset(start);
        endWrapLine = textEditor.getLineAtOffset(end);
        /*
         * DR14889 - resetting isPreviousLineWrapped
         */
        isPreviousLineWrapped = false;
        rewrapInternal(lineNumber);

        // The rest of this method is adjusting the view of the display.
        int caret = textEditor.getCaretOffset();
        int lineStart = textEditor.getOffsetAtLine(textEditor
                .getLineAtOffset(caret));

        // Shrink the scroll bar to the new width of the text.
        textEditor.redraw();
        // Force display to the left to show start of the lines.
        textEditor.setCaretOffset(lineStart);
        textEditor.showSelection();
        // but make sure current caret position is visible.
        textEditor.setCaretOffset(caret);
        textEditor.showSelection();
        // performingWrap = false;
    }

    /**
     * starts the actual work of word-wrap
     * 
     * @param lineNumber
     */
    private void rewrapInternal(int lineNumber) {
        boolean inLocations = false;
        String padding = "";
        // get contents of line
        String line = textEditor.getLine(lineNumber);
        // check for special paragraph cases
        int paragraphStartLineNumber = findParagraphStart(lineNumber);
        String paragraphStart = null;
        if (paragraphStartLineNumber != lineNumber) {
            paragraphStart = textEditor.getLine(paragraphStartLineNumber);
        } else {
            paragraphStart = line;
        }

        paragraphStart = paragraphStart.toUpperCase();
        // is this the locations paragraph?
        if (paragraphStart.startsWith("* LOCATIONS")) {
            inLocations = true;
        }

        if (paragraphStart.matches(METAR_PARAGRAPH)) {
            padding = "     ";
        } else if (checkParagraphPadding(paragraphStart)) {
            // do we need to add or remove padding when we manipulate lines (
            // two
            // spaces )
            padding = "  ";
        }

        if (inLocations && paragraphStartLineNumber == lineNumber) {
            // Keep LOCATIONS first line short & don't paste more to it.
            if (line.indexOf("...") == line.lastIndexOf("...")) {
                return;
            }
            int offset = textEditor.getOffsetAtLine(lineNumber)
                    + line.indexOf("...") + 3;
            textEditor.replaceTextRange(offset, 0, "\r\n");
            ++endWrapLine;

            // Check padding for the new line and set up to wrap rest of
            // paragraph.
            offset += 2;
            if (" ".equals(textEditor.getText(offset, offset)) == false) {
                textEditor.replaceTextRange(offset, 0, "  ");
            } else {
                ++offset;
                if (" ".equals(textEditor.getText(offset, offset)) == false) {
                    textEditor.replaceTextRange(offset, 0, " ");
                }
            }
            ++lineNumber;
            line = textEditor.getLine(lineNumber);
        }

        if (line.length() <= charWrapCol) {
            extendShortLine(lineNumber, padding);
            if (textEditor.getLine(lineNumber).length() <= charWrapCol) {
                // extended line is still short enough do not wrap
                if (lineNumber < endWrapLine) {
                    // May have more lines to wrap.
                    int nextLine = lineNumber + 1;
                    while (nextLine <= endWrapLine
                            && textEditor.getLine(nextLine).trim().isEmpty()) {
                        ++nextLine;
                    }
                    if (nextLine <= endWrapLine) {
                        rewrapInternal(nextLine);
                    }
                }
                return;
            }
        }
        // now we can actually wrap
        if (inLocations) {
            wrapInLocations(lineNumber, padding);
        } else {
            wrapNormal(lineNumber, padding);
        }
    }

    /**
     * Concatenate lines following the passed in line(s) until the line is long
     * enough or there are no more lines in the paragraph.
     * 
     * @param lineNumber
     * @param padding
     */
    private void extendShortLine(int lineNumber, final String padding) {
        // if the line is too short move the next line up
        // if there is a next line
        String line = textEditor.getLine(lineNumber);

        // If the next line is part of the same paragraph and not empty make it
        // part of the current line.
        if (lineNumber + 1 < textEditor.getLineCount()) {
            // if the next line does not start a new paragraph
            if (!isParagraphStart(lineNumber + 1)) {
                // if the next line is not empty
                if (!textEditor.getLine(lineNumber + 1).trim().isEmpty()) {
                    // Determine what kind of end of line marker line has.
                    int deleteLen = 0;

                    try {
                        String allText = textEditor.getText();
                        int eol = textEditor.getOffsetAtLine(lineNumber)
                                + line.length();
                        if (allText.charAt(eol) == '\r'
                                && allText.charAt(eol + 1) == '\n') {
                            deleteLen = 2;
                        } else if (allText.charAt(eol) == '\n') {
                            deleteLen = 1;
                        } else {
                            return;
                        }
                    } catch (Exception e) {
                        return;
                    }
                    if (containsLock(lineNumber + 1)) {
                        // if the next line contains a lock, only bring up
                        // if the line does not start with a lock
                        int lineStart = textEditor
                                .getOffsetAtLine(lineNumber + 1);
                        if (padding.length() > 0
                                && textEditor.getLine(lineNumber + 1)
                                        .startsWith(padding)) {
                            // add two to skip over padding if it exists and
                            // this paragraph is padded
                            lineStart += padding.length();
                        }
                        int lockStart = getStartOfLockAtOffset(lineStart);
                        if (lockStart != -1) {
                            // the start of the next line is inside a lock,
                            // if the lock is too long to fit on this line do
                            // not bring up the next line
                            int lockLength = getLengthOfLockAtOffset(lockStart);
                            if (line.length() + lockLength > charWrapCol) {
                                // lock is too long, do not bring up next
                                // line
                                return;
                            }
                        }
                    }
                    // grab the next line
                    int lineStartOffset = textEditor
                            .getOffsetAtLine(lineNumber);
                    int newlinePosition = lineStartOffset + line.length();
                    if (padding.length() > 0
                            && textEditor.getLine(lineNumber + 1).startsWith(
                                    "  ")) {
                        deleteLen += padding.length();
                    }
                    String beforeReplace = textEditor.getText();
                    String endLine = textEditor.getText(newlinePosition - 1,
                            newlinePosition - 1);
                    String startNextLine = textEditor.getText(newlinePosition
                            + deleteLen, newlinePosition + deleteLen);
                    String wordSpace = "";
                    if (noSeparatorPattern.matcher(endLine).matches()
                            && noSeparatorPattern.matcher(startNextLine)
                                    .matches()) {
                        // Put a space between words when merging the lines.
                        wordSpace = " ";
                    }
                    textEditor.replaceTextRange(newlinePosition, deleteLen,
                            wordSpace);
                    String afterReplace = textEditor.getText();

                    // if the textEditor (StyledText) did not make the change
                    // then exit
                    if (beforeReplace.equals(afterReplace)) {
                        return;
                    }
                    --endWrapLine;

                    // is this line still too short?
                    if (textEditor.getLine(lineNumber).length() <= charWrapCol) {
                        extendShortLine(lineNumber, padding);
                    }
                }
            }
        }
    }

    /**
     * special wrap function for inside the locations paragraph
     * 
     * @param lineNumber
     * @param padding
     */
    private void wrapInLocations(int lineNumber, final String padding) {
        // are we on the locations line?
        String line = textEditor.getLine(lineNumber);
        int lineStartOffset = textEditor.getOffsetAtLine(lineNumber);
        Matcher m = this.locationsFirstRegex.matcher(line);

        if (m.find()) {
            // we are at the start of the locations paragraph, split on
            // whitespace, no periods
            wrapAtPositionOrLock(lineStartOffset + m.end(), padding);
        } else {
            m = this.locationsBodyRegex.matcher(line);
            if (m.find()) {
                // we are splitting on ... first, if no ... then we split on
                // whitespace
                wrapAtPositionOrLock(lineStartOffset + m.end(), padding);
            } else {
                // there is no whitespace or triple dots, split at charWrapCol
                wrapAtPositionOrLock(lineStartOffset + charWrapCol, padding);
            }
        }

        /*
         * DR14889 - add call to checkAndWrapPreviousLine
         */
        checkAndWrapPreviousLine(lineNumber);

        checkAndWrapNextLine(lineNumber);
    }

    /**
     * check that the position passed in is not in the middle of a locked area,
     * if it is the wrap before the locked text if possible
     * 
     * @param position
     * @param padding
     */
    private void wrapAtPositionOrLock(int position, final String padding) {
        if (hasLockAtOffset(position)) {
            // wrap at the lock start
            int lockStart = getStartOfLockAtOffset(position);
            int lineStart = textEditor.getOffsetAtLine(textEditor
                    .getLineAtOffset(position));
            // do not wrap if lockStart is before the start of the line
            // lineStart should never be less than 0
            if (lockStart > lineStart) {
                String replacement = "\r\n" + padding;
                textEditor.replaceTextRange(lockStart, 0, replacement);
                ++endWrapLine;
            }
        } else {
            // wrap at the position
            StringBuilder replacement = new StringBuilder("\r\n");
            int padLen = padding.length();
            if (padLen > 0) {
                int cnt = 0;
                while (cnt < padLen
                        && textEditor.getText(position + cnt, position + cnt)
                                .equals(" ")) {
                    ++cnt;
                }
                if (cnt < padLen) {
                    replacement.append(padding.substring(cnt));
                }
            }
            textEditor.replaceTextRange(position, 0, replacement.toString());
            ++endWrapLine;
        }
    }

    /**
     * checks if the next line is part of the same paragraph and continues
     * wrapping if it is
     * 
     * @param line
     */
    private void checkAndWrapNextLine(int line) {
        // if there is a next line
        if (line + 1 < textEditor.getLineCount()) {
            // if the next line does not start a new paragraph
            if (!isParagraphStart(line + 1)) {
                // if the next line is not empty ( marks the end of a paragraph
                // )
                if (!textEditor.getLine(line + 1).trim().isEmpty()) {
                    // rewrap the next line
                    rewrapInternal(line + 1);
                } else if (line + 1 < endWrapLine) {
                    // See if another paragraph needs to be wrapped.
                    int nextLine = line + 1;
                    while (nextLine <= endWrapLine
                            && textEditor.getLine(nextLine).trim().isEmpty()) {
                        ++nextLine;
                    }
                    if (nextLine <= endWrapLine) {
                        rewrapInternal(nextLine);
                    }
                }
            } else if (line + 1 <= endWrapLine) {
                rewrapInternal(line + 1);
            }
        }
    }

    /**
     * wrap with no special cases
     * 
     * @param lineNumber
     * @param padding
     */
    private void wrapNormal(int lineNumber, final String padding) {
        String line = textEditor.getLine(lineNumber);
        int lineStartOffset = textEditor.getOffsetAtLine(lineNumber);
        Matcher m = this.standardWrapRegex.matcher(line);

        if (m.find()) {
            wrapAtPositionOrLock(lineStartOffset + m.end(), padding);
        } else {
            // split at the column, no whitespace
            wrapAtPositionOrLock(lineStartOffset + charWrapCol, padding);
        }

        /*
         * DR14889 - add call to checkAndWrapPreviousLine
         */
        checkAndWrapPreviousLine(lineNumber);

        checkAndWrapNextLine(lineNumber);
    }

    /**
     * checks if the previous line is part of the same paragraph and continues
     * wrapping if it is
     * 
     * @param line
     */
    private void checkAndWrapPreviousLine(int line) {
        // if there is a previous line
        if (isPreviousLineWrapped) {
            return;
        }
        if (line - 1 > 0) {
            // if the previous line does not start a new paragraph
            if (!isParagraphStart(line - 1)) {
                // if the previous line is not empty ( marks the end of a
                // paragraph
                // )
                if (!textEditor.getLine(line - 1).trim().isEmpty()) {
                    // rewrap the previous line
                    isPreviousLineWrapped = true;
                    rewrapInternal(line - 1);
                } else if (line - 1 < endWrapLine) {
                    // See if another paragraph needs to be wrapped.
                    int nextLine = line - 1;
                    while (nextLine <= endWrapLine
                            && textEditor.getLine(nextLine).trim().isEmpty()) {
                        --nextLine;
                    }
                    if (nextLine <= endWrapLine) {
                        isPreviousLineWrapped = true;
                        rewrapInternal(nextLine);
                    }
                }
            } else if (line - 1 <= endWrapLine) {
                isPreviousLineWrapped = true;
                rewrapInternal(line - 1);
            }
        }

    }

    /**
     * checks if the paragraph starting at the line passed in uses two space
     * padding for subsequent lines
     * 
     * @param firstLine
     * @return
     */
    private boolean checkParagraphPadding(String firstLine) {
        boolean rval = false;
        if (firstLine.length() > 0
                && PADDED_PARAGRAPH_DELIMITERS.contains(firstLine.substring(0,
                        1))) {
            rval = true;
        }
        return rval;
    }

    /**
     * find and return the line number that is the start of the paragraph
     * containing the line number passed in
     * 
     * @param line
     * @return
     */
    private int findParagraphStart(int line) {
        while (!isParagraphStart(line)) {
            line--;
        }
        return line;
    }

    /**
     * checks if a line is the start of a paragraph
     * 
     * @param line
     * @return
     */
    private boolean isParagraphStart(int line) {
        boolean rval = false;
        // return true if this is line 0
        if (line == 0) {
            return true;
        }

        String lineText = textEditor.getLine(line);
        // return false if this line is empty
        if (lineText.trim().isEmpty()) {
            return false;
        }

        // check cases that indicate the start of a paragraph
        if (textEditor.getLine(line - 1).trim().isEmpty()) {
            rval = true;
        } else if (PARAGRAPH_DELIMITERS.contains(lineText.substring(0, 1))) {
            rval = true;
        } else if (isSaoMetarFlag && lineText.startsWith(" ") == false) {
            rval = true;
        }
        return rval;
    }

    /**
     * call recompileRegex when charWrapCol changes
     */
    private void recompileRegex() {
        this.standardWrapRegex = Pattern.compile("(  |..).{1,"
                + (charWrapCol - 3) + "}(\\s|-)");
        this.locationsFirstRegex = Pattern.compile("^\\* LOCATIONS [^\\.]{1,"
                + (charWrapCol - 13) + "}\\s");
        this.locationsBodyRegex = Pattern.compile("((  |..).{1,"
                + (charWrapCol - 5) + "}\\.\\.\\.)|((  |..).{1,"
                + (charWrapCol - 3) + "}\\s)");
    }

    /**
     * replace \r\n with \n
     * 
     * @param source
     * @return
     */
    private String removeSoftReturns(String source) {
        return source.replaceAll("\\r\\n", "\n");
    }

    /**
     * Set up tool tip dispaly with airport information.
     * 
     * @param st
     */
    private void displayAirportTooltip(StyledText st) {
        String word = parseProduct(st);
        String result = AfosBrowserModel.getInstance().getNodeHelp(word);
        if (result == null) {
            result = word;
        }

        textEditor.setToolTipText(result);
    }

    /**
     * Parse the METAR or SAO product based on the nearest line location of the
     * mouse cursor to locate the potential airport.
     * 
     * @param st
     *            -- the styled text widget containing the selection of text
     * @return result -- the start and finish positions of the selected range
     */
    private String parseProduct(StyledText st) {
        String lineText = getLineOffset(st);

        String result = new String("");
        char c = lineText.charAt(0);
        if ((c == 'M') || (c == 'S') || (c == 'T')) {
            // # Most obs start with METAR, SPECI, TESTM, or TESTS. Skip over
            // that tag,
            // # a space, and the K or P, to get to the 3-char station ID.
            if (lineText.length() > 10) {
                result = lineText.substring(7, 10);
            } else {
                result = lineText.substring(lineText.length() - 3);
            }
        } else if ((c == 'W') || (c == 'Y')) {
            // # Canadian SAOs have 3-character IDs, starting with W or Y. Grab
            // 'em.
            result = lineText.substring(0, 3);
        } else {
            // # Some military obs don't get tagged. Skip the K or P and get 3
            // chars.
            int wordLineStart = 1;
            result = lineText.substring(wordLineStart, wordLineStart + 4);
        }

        return result;
    }

    /**
     * Get the line of text is the styled text current caret location.
     * 
     * @param st
     * @return lineText
     */
    private String getLineOffset(StyledText st) {
        int caretOffset = st.getCaretOffset();
        int lineIndex = st.getLineAtOffset(caretOffset);
        String lineText = st.getLine(lineIndex);
        int lineOffset = st.getOffsetAtLine(lineIndex);

        if (lineOffset > 0) {
            boolean goBack = true;
            while (goBack && lineIndex > 0) {
                if (lineText.startsWith(" ") || lineText.length() == 0) {
                    lineIndex--;
                } else {
                    String tempLine = st.getLine(lineIndex);
                    if (tempLine.startsWith("&&") || tempLine.startsWith("$$")) {
                        lineIndex--;
                    } else {
                        goBack = false;
                    }
                }
                lineOffset = lineIndex;
                lineText = st.getLine(lineIndex);
            }
            st.setCaretOffset(caretOffset);
        }

        return lineText;
    }

    /**
     * Determine if the contents of a file contains a text field.
     * 
     * @param file
     * @return
     * @throws IOException
     */
    private static boolean isTextFile(File file) throws IOException {
        boolean result = false;

        byte[] bytesFromFile = getBytesFromFile(file);
        for (int i = 0; i < bytesFromFile.length; i++) {
            byte b = bytesFromFile[i];
            if (b == 0x09 || b == 0x0A || b == 0x0C || b == 0x0D
                    || (b >= 0x20 && b <= 0x7E)) {
                result = true;
                break;
            }
        }

        return result;
    }

    /**
     * Get the contents of file as a byte array.
     * 
     * @param file
     * @return
     * @throws IOException
     */
    private static byte[] getBytesFromFile(File file) throws IOException {
        InputStream is = new FileInputStream(file);

        // Get the size of the file
        long length = file.length();

        if (length > Integer.MAX_VALUE) {
            // File is too large
        }

        // Create the byte array to hold the data
        byte[] bytes = new byte[(int) length];

        // Read in the bytes
        int offset = 0;
        int numRead = 0;
        while (offset < bytes.length
                && (numRead = is.read(bytes, offset, bytes.length - offset)) >= 0) {
            offset += numRead;
        }

        // Ensure all the bytes have been read in
        if (offset < bytes.length) {
            throw new IOException("Could not completely read file "
                    + file.getName());
        }

        // Close the input stream and return bytes
        is.close();
        return bytes;
    }

    private void clearButtons() {
        if (autoWrapMenuItem != null) {
            Menu menu = autoWrapMenuItem.getMenu();
            for (MenuItem item : menu.getItems()) {
                item.setSelection(false);
            }
        }
    }

    /**
     * Set the enable state of the editor's send button.
     * 
     * @param state
     */
    public void enableSend(boolean state) {
        editorSendBtn.setEnabled(state);
    }

    /**
     * Queue a afosId and who to noify.
     * 
     * @param afosId
     * @param notify2
     */
    public void enqueue(String afosId, NotifyExpiration notify2) {
        queuedNotify = notify2;
        queuedProduct = afosId;
    }

    /**
     * Test to see if this is a GFE product that shouldn't be handled.
     * 
     * @param ccc
     * @param nnn
     * @return true forbidden GFE product
     */
    private boolean gfeForbidden(String ccc, String nnn) {
        boolean retval = false;
        if (ccc != null && nnn != null) {
            if (gfePils.contains(nnn) && !exceptionCCCs.contains(ccc)) {
                retval = true;
            }
        }
        return retval;
    }

    /**
     * Retrieve a product from a remote site instead of the local server.
     * 
     * @param req
     */
    private void sendRemoteRetrievalRequest(final RemoteRetrievalRequest req) {
        Job job = new Job("Remote Product Request") {
            @Override
            protected IStatus run(IProgressMonitor monitor) {
                try {
                    RemoteRetrievalResponse response = (RemoteRetrievalResponse) ThriftClient
                            .sendRequest(req);
                    statusHandler.handle(response.isOk() ? Priority.INFO
                            : Priority.ERROR, response.getStatusMessage());
                } catch (VizException e) {
                    statusHandler.error("Remote request failed", e);
                }
                return Status.OK_STATUS;
            }
        };
        job.setPriority(Job.LONG);
        job.setSystem(true);
        job.schedule();
    }

    /**
     * Check of the bbb string to make it the empty string when it is for a
     * normal product.
     * 
     * @param bbb
     * @return
     */
    private static String fixNOR(String bbb) {
        if ("NOR".equals(bbb))
            return "";
        else
            return bbb;
    }
}
