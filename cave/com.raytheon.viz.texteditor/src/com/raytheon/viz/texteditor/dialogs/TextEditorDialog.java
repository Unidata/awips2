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

import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.List;
import java.util.Set;
import java.util.TimeZone;
import java.util.Timer;
import java.util.TimerTask;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.regex.PatternSyntaxException;

import javax.xml.bind.JAXB;
import javax.xml.bind.JAXBException;

import org.apache.commons.lang3.StringUtils;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.commands.IExecutionListener;
import org.eclipse.core.commands.NotHandledException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.MenuManager;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.window.DefaultToolTip;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.ST;
import org.eclipse.swt.custom.StyleRange;
import org.eclipse.swt.custom.StyledText;
import org.eclipse.swt.dnd.Clipboard;
import org.eclipse.swt.dnd.TextTransfer;
import org.eclipse.swt.dnd.Transfer;
import org.eclipse.swt.events.FocusAdapter;
import org.eclipse.swt.events.FocusEvent;
import org.eclipse.swt.events.KeyAdapter;
import org.eclipse.swt.events.KeyEvent;
import org.eclipse.swt.events.MouseAdapter;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.events.ShellAdapter;
import org.eclipse.swt.events.ShellEvent;
import org.eclipse.swt.events.VerifyEvent;
import org.eclipse.swt.events.VerifyListener;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Cursor;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.FontMetrics;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.FileDialog;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.commands.ICommandService;
import org.eclipse.ui.menus.IMenuService;

import com.raytheon.uf.common.dataplugin.text.RemoteRetrievalResponse;
import com.raytheon.uf.common.dataplugin.text.alarms.AlarmAlertProduct;
import com.raytheon.uf.common.dataplugin.text.db.MixedCaseProductSupport;
import com.raytheon.uf.common.dataplugin.text.db.OperationalStdTextProduct;
import com.raytheon.uf.common.dataplugin.text.db.PracticeStdTextProduct;
import com.raytheon.uf.common.dataplugin.text.db.StdTextProduct;
import com.raytheon.uf.common.dataplugin.text.db.StdTextProductId;
import com.raytheon.uf.common.dataplugin.text.request.RemoteRetrievalRequest;
import com.raytheon.uf.common.dataplugin.text.request.StdTextProductServerRequest;
import com.raytheon.uf.common.dataplugin.text.request.TextProductInfoCreateRequest;
import com.raytheon.uf.common.dataplugin.text.util.AFOSParser;
import com.raytheon.uf.common.dissemination.OUPRequest;
import com.raytheon.uf.common.dissemination.OUPResponse;
import com.raytheon.uf.common.jms.notification.INotificationObserver;
import com.raytheon.uf.common.jms.notification.NotificationException;
import com.raytheon.uf.common.jms.notification.NotificationMessage;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.localization.SaveableOutputStream;
import com.raytheon.uf.common.localization.exception.LocalizationException;
import com.raytheon.uf.common.serialization.JAXBManager;
import com.raytheon.uf.common.serialization.comm.IServerRequest;
import com.raytheon.uf.common.site.SiteMap;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.ISimulatedTimeChangeListener;
import com.raytheon.uf.common.time.SimulatedTime;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.notification.jobs.NotificationManagerJob;
import com.raytheon.uf.viz.core.requests.ThriftClient;
import com.raytheon.uf.viz.spellchecker.dialogs.SpellCheckDlg;
import com.raytheon.uf.viz.ui.menus.DiscoverMenuContributions;
import com.raytheon.uf.viz.vtec.VtecObject;
import com.raytheon.uf.viz.vtec.VtecUtil;
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
import com.raytheon.viz.texteditor.msgs.IAfosBrowserCallback;
import com.raytheon.viz.texteditor.msgs.IAwipsBrowserCallback;
import com.raytheon.viz.texteditor.msgs.IRecoverEditSessionCallback;
import com.raytheon.viz.texteditor.msgs.IScriptRunnerObserver;
import com.raytheon.viz.texteditor.msgs.ITextEditorCallback;
import com.raytheon.viz.texteditor.msgs.IWmoBrowserCallback;
import com.raytheon.viz.texteditor.notify.NotifyExpiration;
import com.raytheon.viz.texteditor.print.PrintDisplay;
import com.raytheon.viz.texteditor.scripting.dialogs.IScriptEditorObserver;
import com.raytheon.viz.texteditor.scripting.dialogs.ScriptEditorDialog;
import com.raytheon.viz.texteditor.scripting.dialogs.ScriptOutputDlg;
import com.raytheon.viz.texteditor.scripting.runner.ITextWsScriptController;
import com.raytheon.viz.texteditor.scripting.runner.TextEditorScriptRunnerObserver;
import com.raytheon.viz.texteditor.scripting.runner.TextWsScriptThreadManager;
import com.raytheon.viz.texteditor.util.SiteAbbreviationUtil;
import com.raytheon.viz.texteditor.util.TextEditorUtil;
import com.raytheon.viz.ui.dialogs.CaveJFACEDialog;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;
import com.raytheon.viz.ui.dialogs.SWTMessageBox;
import com.raytheon.viz.ui.simulatedtime.SimulatedTimeOperations;

/**
 * Main Text Editor dialog.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer        Description
 * ------------- -------- --------------- --------------------------------------
 * Sep 13, 2007  368      lvenable        Initial creation.
 * Sep 28, 2007  368      grichard        Added script building.
 * Oct 10, 2007  459      grichard        Added `token' for display model.
 * Oct 11, 2007  482      grichard        Implemented build 9 features.
 * Oct 25, 2007  501      grichard        Implemented build 10 features.
 * Dec 07, 2007  582      grichard        Implemented build 12 features.
 * Dec 17, 2007  639      grichard        Added &quot;fxa&quot; parm to scripts.
 * Jan 03, 2008  637      grichard        Implemented build 13 features.
 * Jan 07, 2008  722      grichard        Implemented build 14 features.
 * Jan 08, 2008  681      grichard        Resize text window for large font.
 * Jan 10, 2008  722      grichard        Implemented localization.
 * Jan 10, 2008  722      grichard        Saved text product info.
 * Jan 11, 2008  749      grichard        Guarded auto-save / window close
 *                                        interaction.
 * Dec 02, 2008  1588     grichard        Made determineSelectionRange a static
 *                                        method.
 * Dec 02, 2008  1588     grichard        Highlight specific syntax errors in
 *                                        TAF editor.
 * Dec 09, 2008  1735     grichard        Format bulleted sections in warnings
 *                                        upon saving.
 * Dec 10, 2008  1735     grichard        Implement lockable text for warngen
 *                                        products.
 * Dec 17, 2008  1538     jkorman         TextDB plugin/Cave Text Workstation
 *                                        mods.
 * Dec 21, 2008  1803     grichard        Restored changes from December 2008.
 * Jan 05, 2009  1688     grichard        Update text model based on database
 *                                        dip.
 * Jan 07, 2009  1688     grichard        Backout executeAFOSCommand.
 * Mar 23, 2009  1956     bwoodle         Change SiteNode and WMO ID retrieval
 *                                        for hazard prods.
 * Jul 10, 2009  2374     MW Fegan        Hooking in script running
 *                                        capabilities.
 * Jul 28, 2009  2610     rjpeter         Changed error handling to AlertViz.
 * Oct 02, 2009  2646     rjpeter         Added required field validation.
 * Dec 30, 2009  3778     mhuang          Added no acknowledgment pop-up notice
 * Feb 16, 2010  4276     MW Fegan        Allow import/export of files w/o
 *                                        extensions.
 * Feb 23, 2010  3425     MW Fegan        Correct problems with sending
 *                                        products.
 * Mar 02, 2010  4765     MW Fegan        Hook in spell checker.
 * Mar 15, 2010  3317     MW Fegan        Implemented 'Update Obs' functionality
 * Mar 29, 2010  3331     MW Fegan        Corrected Cancel Editor functionality
 *                                        to match A-I.
 * May 24, 2010  2187     cjeanbap        Added StdTextProduct and ThriftClient
 *                                        functionality.
 * Jun 07, 2010  5004     cjeanbap        Handle threading blocking issue
 * Jun 09, 2010  4523     cjeanbap        Handle restoring of character Word
 *                                        Wrap.
 * Jun 15, 2010  766      cjeanbap        Handle cut/copy/paste in editor
 *                                        window.
 * Jun 22, 2010  5377     cjeanbap        Added Print Dialog confirmation.
 * Jun 22, 2010  5378     cjeanbap        Print Selection cleared after
 *                                        printing.
 * Jun 24, 2010  4001     cjeanbap        buttons on keyboard are
 *                                        non-functioning in the text
 *                                        workstation
 * Jun 29, 2010  5378     cjeanbap        Replaced JOptionPane with MessageBox.
 * Jun 29, 2010  4639     cjeanbap        Modified editHeader() place
 *                                        &quot;-&quot; when wmoId and/or siteid
 *                                        is empty
 * Jul 06, 2010  4773     cjeanbap        Add Go Ahead dialog when send button
 *                                        push when product comes from WarnGen.
 * Jul 06, 2010  4523     cjeanbap        Set Auto Wrap to 69 chars.
 * Jul 09, 2010  5508     cjeanbap        Return focus to text editor when
 *                                        forcaster must modify the selected
 *                                        region.
 * Jul 30, 2010  4773     cjeanbap        Added WarnGen Product confirmation
 *                                        dialog.
 * Aug 23, 2010  2187     cjeanbap        Changed from Dialog to CaveSWTDialog.
 * Sep 09, 2010  5468     rferrel         determineCurrentWord now works when
 *                                        word at end of the line
 * Nov 08, 2010  7433     cjeanbap        Remove initially added private
 *                                        variable and method; used existing
 *                                        inEditMode.
 * Nov 16, 2010  7433     cjeanbap        Fixed bug of inEditMode flag.
 * Nov 30, 2010  7433     cjeanbap        Call confirmation dialog if user
 *                                        cancels edit of TextEditor.
 * Dec 17, 2010  7221     Qinglu Lin      Added code to prevent locked lines or
 *                                        segments from sticking
 *                        D. Friedman     together after being moved.
 * Jan 08, 2011  7643     lvenable        Fix the closing and opening of the
 *                                        dialogs.
 * Jan 11, 2011  7375     cjeanbap        Fix multiple enter dialogs and
 *                                        disposed widget.
 * Jan 20, 2011  7826     Qinglu Lin      Sync Ins key on keypad with INS/OVR
 *                                        button and overStrikeItem on Text
 *                                        Editor.
 * Feb 08, 2011  7433     cjeanbap        Warngen generated Text Editor Windows
 *                                        being overwritten by new warnings.
 * Mar 30, 2011  8561     jdortiz         Added call to enterEditor() if flag in
 *                                        AFOS command is set.
 * Apr 05, 2011  8378     jdortiz         Fixed Document private class and
 *                                        printDocument method to print entire
 *                                        document.
 * Mar 05, 2011  8420     cjeanbap        Added nearest Airport display for
 *                                        Metars and Saos.
 * Apr 07, 2011  8394     cjeanbap        Added a shell addControlListener to
 *                                        handle the width resize.
 * Apr 13, 2011  5097     cjeanbap        Added file Attachment functionality.
 * May 16, 2011  7545     cjeanbap        Added enableVersionMenuItems().
 * May 17, 2011  8394     cjeanbap        Handle the width resize below the
 *                                        minimum size.
 * May 18, 2011  8420     cjeanbap        Enhanced nearest Airport functionality
 *                                        to mimick A1.
 * Jun 03, 2011  9742     cjeanbap        Change awips id length to 5 chars.
 * Jun 05, 2011  9741     cjeanbap        Enabled Version menu items; removing
 *                                        #7545 changes.
 * Jun 14, 2011  8394     cjeanbap        Handle the height resize below the
 *                                        minimum size.
 * Jul 19, 2011  10165    rferrel         rewrap now done after cut/paste.
 * Aug 31, 2011  10793    rferrel         setCurrentHeaderAndBody now removes
 *                                        DRAFT_PIL header.
 * Sep 15, 2011  10651    rferrel         Implemented clearUpdateFlags and have
 *                                        updateDispalyedProduct highlighted
 *                                        updated METARs.
 * Sep 15, 2011  10557    rferrel         Use class PrintDisplay to handle
 *                                        printing.
 * Sep 28, 2011  10800    rferrel         Fixed airport tooltip to work from
 *                                        Cave and stand alone.
 * Oct 03, 2011  10998    rferrel         No longer accumulate obs when update
 *                                        and accum checked.
 * Nov 02, 2011  11450    rferrel         Catch errors when parsing for lock
 *                                        tags, perform popup for queued WarnGen
 *                                        and fixed autosave.
 * Nov 11, 2011  11552    rferrel         Product no longer needs to be a RESEND
 *                                        in order to be sent.
 * Nov 14, 2011  11203    rferrel         Header included when exporting a file
 *                                        while in edit mode.
 * Mar 08, 2012  14553    mhuang          Add blank line between product header
 *                                        and body.
 * Apr 23, 2012  14783    rferrel         Allow line wrap at white space or
 *                                        hyphen.
 * Apr 24, 2012  14548    rferrel         Merging lines for wrap places a space
 *                                        beween words when needed.
 * Apr 27, 2012  14902    rferrel         No longer have blank line between
 *                                        AWIPS ID and UGC line.
 * Jun 19, 2012  14975    D. Friedman     Prevent zeroed-out WMO header times.
 * Jul 18, 2012  14457    rferrel         Add mouse listener to clear site's
 *                                        update obs when clicked on.
 * Jul 25, 2012  14459    rferrel         Strip WMH headers when getting all
 *                                        METARs.
 * Aug 13, 2012  14613    mgamazaychikov  Ensured the WMO and MND header times
 *                                        are the same.
 * Aug 20, 2012  15340    D. Friedman     Use callbacks for stop sign dialog.
 *                                        Prevent NOR in header.
 * Sep 10, 2012  15334    rferrel         No longer wrap text pasted to an empty
 *                                        text field.
 * Sep 10, 2012  15103    mgamazaychikov  DR15103 -do not clear AFOS command
 *                                        from the text box when obs are updated
 *                                        and refactored executeCommand
 * Sep 10, 2012  15401    D. Friedman     Fix QC problem caused by DR 15340.
 * Sep 20, 2012  1196     rferrel         Refactor dialogs to prevent blocking.
 * Sep 25, 2012  1196     lvenable        Refactor dialogs to prevent blocking.
 * Sep 26, 2012  1196     lvenable        Refactor dialogs to prevent blocking.
 * Sep 27, 2012  1196     rferrel         Changes for non-blocking
 *                                        ScriptOutputDlg.
 * Sep 27, 2012  15424    S.Naples        Set focus on AFOS command text field
 *                                        after executing retrieval of product.
 * Oct 09, 2012  14889    mgamazaychikov  Add call to checkAndWrapPreviousLine
 * Sep 26, 2012  1196     lvenable        Refactor dialogs to prevent blocking.
 * Sep 27, 2012  1196     rferrel         Changes for non-blocking
 *                                        ScriptOutputDlg.
 * Oct 01, 2012  1229     rferrel         Change WmoBrowserDlg to non-blocking
 * Oct 10, 2012  1229     rferrel         Changed AwipsBrowserDlg to
 *                                        non-blocking.
 * Oct 12, 2012  15418    D. Friedman     Do not store product when sending in
 *                                        operational mode. Do not use changed
 *                                        BBB from OUPResponse.
 * Oct 17, 2012  1229     rferrel         Changes for non-blocking
 *                                        SWTMessageBox.
 * Nov 05, 2012  15560    S. Naples       Added check to see if we are in edit
 *                                        mode before capturing keys.
 * Nov 28, 2012  14842    mgamazaychikov  Re-wrote processPopup method
 * Dec 13, 2012  1353     rferrel         Change to make edit cancel message not
 *                                        dispaly the red had kill job message.
 * Jan 10, 2013  15704    mgamazaychikov  Added setting userKeyPressed to false
 *                                        in verifyText method.
 * Jan 22, 2013  1496     rferrel         Query for loading products no longer
 *                                        on the UI thread.
 * Jan 31, 2013  14247    D. Friedman     Make spell check dialog child of
 *                                        editor window.
 * Jan 31, 2013  15580    D. Friedman     Prevent errors when window is
 *                                        disposed.
 * Jan 31, 2013  15610    mgamazaychikov  Added handling first line of text
 *                                        product which is not a valid WMO
 *                                        heading.
 * Jan 31, 2013  1563     rferrel         Force location of airport tooltip.
 * Jan 31, 2013  1568     rferrel         Spell checker now tied to this dialog
 *                                        instead of parent.
 * Apr 26, 2013  16123    snaples         Removed setFocus to TextEditor in
 *                                        postExecute method.
 * Jun 07, 2013  1981     mpduff          Add user id to OUPRequest as it is now
 *                                        protected.
 * Jun 20, 2013  15733    XHuang          Add functionalities that get Font
 *                                        size, Text colors from *.xml files in
 *                                        localization; add selection listener
 *                                        to catch the highlight words and set
 *                                        the highlight colors.
 * Jul 23, 2013  2176     jsanchez        Added a new confirmation message for
 *                                        emergency warnings.
 * Jul 25, 2013  15733    GHull           Read font and color prefs from
 *                                        TextEditorCfg.
 * Aug 23, 2013  16514    D. Friedman     Fix handling of completed product
 *                                        requests.  Do not change command
 *                                        history or close browser window for
 *                                        "update obs".
 * Sep 04, 2013  2176     jsanchez        Changed the order of the QC check
 *                                        dialogs.
 * Sep 12, 2013  2249     rferrel         Change Time stamp in file name created
 *                                        by warngen to use simulated time.
 * Sep 20, 2013  2394     lvenable        Fixed color memory leaks.
 * Nov 20, 2013  16777    D. Friedman     Check if OUPRequest will work before
 *                                        setting ETN.
 * Dec 10, 2013  2601     mpduff          Fix NullPointerException.
 * Jan 28, 2014  14595    mgamazaychikov  Added template loading and editing
 *                                        functionality.
 * Mar 14, 2014  17175    D. Friedman     Get correct time zone for MND header
 *                                        time sync.
 * May 08, 2014  16041    kshrestha       Save unofficial text products from
 *                                        text editor.
 * May 13, 2014  2536     bclement        moved WMO Header to common, switched
 *                                        from TimeTools to TimeUtil
 * Sep 11, 2014  3580     mapeters        Replaced SerializationTuil usage with
 *                                        JAXBManager, removed IQueryTransport
 *                                        usage (no longer exists).
 * Oct 20, 2014  3685     randerso        Made conversion to upper case
 *                                        conditional on product id
 * Feb 15, 2015  4001     dgilling        Ensure all fields are set in
 *                                        SendPracticeProductRequest.
 * Mar 05, 2015  15025    kshrestha       Fix to maintain the headers that they
 *                                        are saved with
 * Mar 10, 2015  14866    kshrestha       Disable QC GUI pop up for TextWS
 * Apr 06, 2015  14968    mgamazaychikov  Fix formatting for pathcast section
 * Jun 15, 2015  4441     randerso        Unconditionally convert text to upper
 *                                        case for QC
 * Jul 08, 2015  15044    dhuffman        Implemented tabbing and tabs to
 *                                        spaces.
 * Aug 31, 2015  4749     njensen         Changed setCloseCallback to
 *                                        addCloseCallback
 * Sep 02, 2015  4781     dgilling        Used different constructor for
 *                                        SpellCheckDlg.
 * Sep 29, 2015  4899     rferrel         Do not send product while in
 *                                        operational mode and simulated time.
 * Sep 30, 2015  4860     skorolev        Corrected misspelling.
 * Oct 07, 2015  18132    D. Friedman     Exclude certain phensigs from
 *                                        automatic ETN incrementing.
 * Oct 28, 2015  5054     randerso        Make Text Editor windows appear on
 *                                        same monitor as the parent. Removed
 *                                        hard coded offset for window
 *                                        placement.
 * Nov 05, 2015  5039     rferrel         Prevent wrapping text to a component
 *                                        name line and clean up of streams.
 * Nov 19, 2015  5141     randerso        Replace commas with ellipses if
 *                                        product not enabled for mixed case
 *                                        transmission
 * Dec 10, 2015  5206     randerso        Replace commas with ellipses only in
 *                                        WarnGen products
 * Dec 11, 2015  14752    mgamazaychikov  Fix problems with wrapping in the
 *                                        impact section, generalized the
 *                                        approach to padding paragraphs.
 * Jan 06, 2016  18452    mgamazaychikov  Fix NPE for null product in
 *                                        enterEditor
 * Jan 06, 2016  5225     randerso        Fix problem with mixed case not
 *                                        getting converted to upper case when
 *                                        multiple text editors are open on the
 *                                        same product.
 * Jan 27, 2016  5054     randerso        Removed ignored second ICON from
 *                                        Cancel confirmation
 * Jan 29, 2016  5289     tgurney         Add missing maximize button in trim
 * Feb 04, 2016  5076     dgilling        Prevent text editor from adding
 *                                        multiple copies of header to edited
 *                                        product after save/cancel cycling.
 * Feb 16, 2106  5391     randerso        Fixed button layouts so text is not
 *                                        cut off with larger fonts/higher DPI
 * Feb 12, 2016  4716     rferrel         Changes to use new Awips Browser
 *                                        Dialog. Localization File deprecation
 *                                        clean up.
 * Mar 01, 2016  17614    arickert        Updated MND, WMO, and VTEC should be
 *                                        reflected in text editor after warngen
 *                                        is submitted
 * Mar 01, 2016  13214    mgamazaychikov  Added verifyLineWidth method.
 * Mar 01, 2016  14803    mgamazaychikov  Added code to handle products without
 *                                        WMO header.
 * Mar 10, 2016  5460     dgilling        Fix StringIndexOutofBoundsException
 *                                        when a WRK product with a ZCZC header
 *                                        has spaces in the new line chars at
 *                                        the end of the header.
 * Mar 10, 2016  5411     randerso        Added flags to disable comma
 *                                        replacement and enable character set
 *                                        validation. Moved upper case
 *                                        conversion for QC checks into the
 *                                        specific checks that need it.
 * Mar 15, 2016  5339     dgilling        Ease up on word wrapping spaces at
 *                                        ends of lines.
 * Mar 21, 2016  5343     bkowal          Updated site retrieval for
 *                                        compatibility with non-standard
 *                                        products.
 * Mar 17, 2016  18727    D. Friedman     Fix use of verification listener when
 *                                        entering and exiting editor.
 * Apr 15, 2016  18870    D. Friedman     Replace commas with ellipses only at
 *                                        start of edit and then word-wrap.
 * Jun 14, 2016  17614    mgamazaychikov  Fix loading of product on exit from
 *                                        edit mode.
 * Jun 22, 2016  5710     rferrel         Keep Version menu enabled when WIP of
 *                                        TTAAII CCCC text fields gain focus.
 * Aug 17, 2016  13824    mgamazaychikov  Fix regex for updating METARs from
 *                                        geographic regions other than CONUS.
 * Aug 24, 2016  19246    mgamazaychikov  Disallow editing/sending of products
 *                                        with WarnGen PILs.
 * Aug 25, 2016  19250    Qinglu Lin      Log several pieces of information
 *                                        regarding Save, Send, Cancel buttons,
 *                                        etc.
 * Aug 28, 2016  5839     rferrel         Use IParser and changes to display
 *                                        ALL: products in editor.
 * Nov 03, 2016  5934     randerso        Moved VtecObject and VtecUtil to a
 *                                        separate plugin.
 * Nov 30, 2016  6016     randerso        Change timeChanged to call
 *                                        validateTime on the UI thread
 * Feb 14, 2017  6037     randerso        Ensure dialog does not appear over
 *                                        panels
 * Apr 28, 2017  6051     tgurney         Fix rare NPE on autosave
 * May 02, 2017  19971    Qinglu Lin      Updated isProductForbiddenToEdit().
 * May 12, 2017  19971    Qinglu Lin      Updated isProductForbiddenToEdit().
 * Jul 10, 2017  19530    jdynina         QC FTM to conform with RPG ICD
 *                                        requirements.
 * Jul 25, 2017  19826    D. Friedman     Set header fields based on the result
 *                                        of running LaunchToolAction.
 * Aug 04, 2017  6364     tgurney         Disable Save for WarnGen products
 * Sep 07, 2017  20284    Qinglu Lin      Updated isProductForbiddenToEdit() for
 *                                        GFE PILs.
 * Sep 27, 2017  19428    Qinglu Lin      Updated isProductForbiddenToEdit() for
 *                                        GFE PILs.
 * Dec 12, 2017  6225     randerso        Fix deletion of auto save files
 * Jan 03, 2018  6804     tgurney         Implement script multithreading
 * Jan 17, 2018  7193     bkowal          Prevent missing characters when
 *                                        backspacing by only removing
 *                                        characters equivalent to padding when
 *                                        padding is matched exactly.
 * Jan 24, 2018  6850     tgurney         Stop unchecking Update Obs on clear
 * Jan 18, 2018  7197     bkowal          No longer specify single product when
 *                                        retrieving the AWIPS command. The
 *                                        single product flag causes all of the
 *                                        text from multiple products to be
 *                                        merged into the first product.
 * Feb 02, 2018  7057     tgurney         Left/Right arrow fix to work for both
 *                                        AFOS and AWIPS commands
 * Feb 07, 2018  7104     dgilling        Prevent MND header time from being
 *                                        changed
 * Aug 15, 2018  7197     randerso        Set singleProduct to true for ALL
 *                                        commands
 * Mar 05, 2018  7006     tgurney         Fix insert/overwrite state tracking
 * Mar 20, 2018  7133     tgurney         Fix wrapping of text when pasting into
 *                                        empty text box
 * Mar 21, 2018  6754     tgurney         Don't cancel editor when hiding the
 *                                        dialog
 * Apr 04, 2018  6691     tgurney         AFOS cmd field, make typing semicolon
 *                                        insert a colon instead
 * May 09, 2018  7291     randerso        Changed signature for
 *                                        PrintDisplay.print.
 * May 10, 2018  7133     tgurney         Fix wrapping of certain problematic
 *                                        texts. Also wrap text when pasting via
 *                                        a global keybind or middle-click
 * Jun 21, 2018  7133     tgurney         Fix getting ICommandService for
 *                                        standalone TextWS
 * Aug 28, 2018  7134     randerso        Pass lastRemoteRetrievalRequest
 *                                        toRemoteSiteRequestDlg constructor
 * Sep 10, 2018  7133     tgurney         Paste command null check on dispose
 * Nov 01, 2018  7587     tgurney         Add getStatusMsg()
 * Nov 01, 2018  7588     tgurney         Add Ctrl-Up/Down to go to
 *                                        previous/next paragraph
 * Nov 01, 2018  7589     tgurney         Allow page up / page down of text area
 *                                        while AWIPS/AFOS text box is in focus
 * Nov 01, 2018  7599     tgurney         Reimplement "select to end of line"
 * Nov 05, 2018  6804     tgurney         executeTextScript return the script
 *                                        controller
 * Nov 9, 2018            mjames          Remove send button capability
 * Nov 09, 2018  7587     tgurney         Add "writeToLog" parameter to
 *                                        showScriptStatus()
 * Nov 13, 2018  7598     tgurney         Stop redraw of text editor during
 *                                        rewrap to prevent "phantom lines"
 * Nov 16, 2018  7625     tgurney         Make Fill function wrap the selected
 *                                        text
 * Dec 04, 2018  7665     tgurney         Rename getASCIIProduct() to
 *                                        getPrintableProduct()
 * Dec 14, 2018  20572    mgamazaychikov  Updated isProductForbiddenToEdit to
 *                                        allow editing/sending of
 * Jan 24, 2019  7721     randerso        Fix obsRegex when products loaded via
 *                                        script
 * Feb 26, 2019  7746     randerso        Change to use Path instead of String
 *                                        for TextWS script path. Allow scripts
 *                                        to be stored in any directory.
 * Feb 28, 2019  7601     tgurney         Consolidate "Continue" and "Skip Wait"
 *                                        script functions. Cleanup scripting
 *                                        code
 * Mar 26, 2019  20827    anilsonm        warngen gives line length warning then
 *                                        locks up
 * Apr 03, 2019  20828    anilsonm        product failed but incremented ETN
 * May 01, 2019  7831     randerso        Added support for nonWrapPils.
 *                                        Significantly refactored autoWrap
 *                                        configuration handling.
 * Jun 24, 2019  7864     randerso        Fixed setCurrentHeaderAndBody() to
 *                                        handle scratch product headers.
 * Jun 28, 2019  7625     randerso        Changed to use proper regex for
 *                                        finding soft newlines instead of using
 *                                        \r\n.
 * Jul 02, 2019  7891     randerso        Restore previous behavior when Text
 *                                        WarnGen window is closed.
 * Oct 22, 2019  7625     tgurney         Fix autowrap Enter key behavior
 * Oct 24, 2019  7625     tgurney         Stop removing soft newlines on copy
 * Nov 20, 2019  20827    kshrestha       Fix max line length from being
 *                                        exceeded.
 * Mar 13, 2020  21048    mgamazaychikov  Get wmoID and cccID from text product
 * May 20, 2020  8134     randerso        Change File Export dialog from OPEN to
 *                                        SAVE. Fixed extension string.
 * Jun 23, 2020  8186     bhurley         Update the MND time and VTEC start
 *                                        time at product transmission for the
 *                                        SQW and DSW products
 * Oct 13, 2020  8238     randerso        Set minimum size on dialog.
 * Jan 19, 2022  8742     randerso        Remove minimum size, suppress
 *                                        SonarQube warnings for intentially
 *                                        unlogged exceptions.
 *
 * </pre>
 *
 * @author lvenable
 */
public class TextEditorDialog extends CaveSWTDialog
        implements VerifyListener, IAfosBrowserCallback, IAwipsBrowserCallback,
        IWmoBrowserCallback, IRecoverEditSessionCallback, IScriptEditorObserver,
        INotificationObserver, IProductQueryCallback,
        ISimulatedTimeChangeListener {

    /**
     * Handler used for messages.
     */
    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(TextEditorDialog.class);

    private static volatile JAXBManager jaxb;

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

    private static final List<String> warngenPils = Arrays.asList("AWW", "EWW",
            "FFS", "FFW", "FLS", "FLW", "FRW", "MWS", "NOW", "SMW", "SPS",
            "SVR", "SVS", "TOR");

    /**
     * Default list of VTEC phenomena significance codes for which the ETN
     * should not be changed when sending a NEW-action product.
     */
    private static final List<String> defaultNoETNIncrementPhenSigs = Arrays
            .asList("HU.A", "HU.S", "HU.W", "TR.A", "TR.W", "SS.A", "SS.W",
                    "TY.A", "TY.W");

    /**
     * Path of ETN rules localization file
     */
    private static final String ETN_RULES_FILE = "textws/gui/EtnRules.xml";

    /**
     * System color to use for background color when an obs is updated.
     */
    private static final int UPDATE_BG = SWT.COLOR_BLACK;

    /**
     * System color to use for foreground color when an obs is updated.
     */
    private static final int UPDATE_FG = SWT.COLOR_WHITE;

    /**
     * Lockable text begin element tag
     */
    private static final String BEGIN_ELEMENT_TAG = "<L>";

    /**
     * Lockable text end element tag
     */
    private static final String END_ELEMENT_TAG = "</L>";

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

    private static final Pattern HEADER_SEPARATOR_PATTERN = Pattern
            .compile("\\n\\s*\\n");

    private static final Pattern SOFT_NEWLINE = Pattern
            .compile("\\p{Blank}*(?<!\n)\n(?!\n)");

    /**
     * Pattern to match WMO header that allows for scratch headers like "- -
     * DDHHMM\n"
     *
     */
    private static final Pattern HEADER_PATTERN = Pattern.compile(
            "(([A-Z]{3}[A-Z0-9](?:\\d{0,2}|[A-Z]{0,2})|-) ([A-Z0-9 ]{4}|-) "
                    + "(\\d{6}|DDHHMM)(?: [A-Z]{3})? *[\\r]*\\n)");

    /**
     * List of pils in that are being displayed.
     */
    private final List<String> displayedPils = new ArrayList<>();

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
     * Pattern for finding a date.
     */
    private static final Pattern datePtrn = Pattern.compile(
            "((\\d{1,2}\\d{2})(\\s(AM|PM)\\s(\\w{3})\\s\\w{3}\\s\\w{3}\\s{1,}\\d{1,2}\\s\\d{4}))");

    /**
     * Pattern used when merging line to determine if white space is needed.
     */
    private static final Pattern noSeparatorPattern = Pattern
            .compile("[^-\\s]");

    /**
     * Pattern to determine if the body of the product is a UGC line.
     */
    private static final Pattern UGC_FIRST_LINE_PATTERN = Pattern
            .compile("^[A-Z][A-Z0-9][CZ]\\d{3}[->].*-\\s*$");

    /**
     * Pattern used to determine if a line is a component name line.
     */
    private static final Pattern COMPONENT_NAME_PATTERN = Pattern
            .compile("^\\.[A-Za-z0-9][^.]*\\.{3}");

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
        AUTOSAVE_DATE_FORMAT.setTimeZone(TimeUtil.GMT_TIME_ZONE);
    }

    /*
     * The direction to go to find selection.
     */
    private enum SelectDirection {
        FORWARD, BACKWARD
    }

    /**
     * Number of lines scroll up or down based on select direction.
     */
    private static final int PAGE_SCROLL_LINES = 20;

    /**
     * Use to determine the width of the text field.
     */
    private static final int EDITOR_WIDTH = 80;

    /**
     * Use to determine the height of the text field.
     */
    private static final int EDITOR_HEIGHT = 30;

    /**
     * Filters for file import/export and attach functions.
     */
    private static final String[] FILTER_NAMES = { "All Files" };

    /**
     * Allowable extensions for import/export and attach functions.
     */
    private static final String[] FILTER_EXTS = { "*.*" };

    /* the alarm/alert topic */
    private static final String ALARM_ALERT_TOPIC = "edex.alarms.msg";

    /**
     * Airport information tool tip.
     */
    private DefaultToolTip airportToolTip;

    /**
     * Awips browser menu item.
     */
    private MenuItem AwipsBrowserItem;

    /**
     * AFOS browser menu item.
     */
    private MenuItem AFOSBrowserItem;

    /**
     * Enter editor mode menu item.
     */
    private MenuItem enterEditorItem;

    /**
     * Save menu item.
     */
    private MenuItem saveItem;

    /**
     * Cancel editor item.
     */
    private MenuItem cancelEditorItem;

    /**
     * Import from file editor menu item.
     */
    private MenuItem importFromFileItem;

    /**
     * Recover edit session menu item.
     */
    private MenuItem recoverEditSessionItem;

    /**
     * Close menu item.
     */
    private MenuItem closeItem;

    /**
     * Cut text menu item.
     */
    private MenuItem cutItem;

    /**
     * Paste text menu item.
     */
    private MenuItem pasteItem;

    /**
     * Fill (remove hard returns) menu item.
     */
    private MenuItem fillItem;

    /**
     * Delete sub-menu.
     */
    private MenuItem deleteSubMenuItem;

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
     * List of PILs that should not be wrapped
     */
    private Set<String> nonWrapPils;

    /**
     * Currently selected wrap configuration
     */
    private WrapButtonCfg currentWrapCfg;

    /**
     * Previously selected wrap configuration, saved when wrapping is disable
     * for a nonWrapPil
     */
    private WrapButtonCfg prevWrapCfg;

    private int wrapColumn = -1;

    /**
     * Overstrike (overwrite) menu item.
     */
    private MenuItem overStrikeItem;

    /**
     * Version menu.
     */
    private MenuItem versionMenuItem;

    /**
     * Products menu.
     */
    private MenuItem productsMenuItem;

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
     * Script->Cancel menu item
     */
    private MenuItem scriptsCancelItem = null;

    /**
     * the cancel script button
     */
    private Button scriptCancelBtn = null;

    /**
     * Insert text combo index;
     */
    private static final int INSERT_TEXT = 0;

    /**
     * Overwrite text combo index;
     */
    private static final int OVERWRITE_TEXT = 1;

    /**
     * AWIPS browser button to launch the AWIPS browser.
     */
    private Button awipsBrowserBtn;

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
    private static final String currentDateId = "DDHHMM";

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
     * Styled text editor.
     */
    private StyledText textEditor;

    /**
     * default font
     */
    private Font dftFont;

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
    private volatile boolean inEditMode = false;

    /**
     * Search and replace dialog.
     */
    private SearchReplaceDlg searchReplaceDlg;

    /**
     * Flag indicating if the overwrite mode has been set for template editing.
     */
    private boolean isTemplateOverwriteModeSet = false;

    /**
     * Flag to indicate if the document being edited has been saved.
     */
    private boolean saved = false;

    /**
     * Flag indicating if the editor is in overwrite mode.
     */
    private boolean overwriteMode = false;

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
     * The AWIPS browser associated with this dialog.
     */
    private AwipsBrowserDlg awipsBrowser;

    /**
     * Flag to indicate the AFOS browser is opened.
     */
    private boolean displayAfosBrowser = false;

    /**
     * Flag set to true when user try close editor with unsaved changes.
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
     * Job to handle query for products off the UI thread.
     */
    private final ProductQueryJob productQueryJob = new ProductQueryJob(this);

    /**
     * Flag to indicate if the dialog is in wait mode.
     */
    private boolean busy = false;

    /**
     * Determines if marked text is uneditable -- equates to the text coming
     * from warngen
     */
    private boolean verifyUndeditableText = false;

    /**
     * the script output window
     */
    private ScriptOutputDlg scriptOutput = null;

    /**
     * the Script Editor window
     */
    private ScriptEditorDialog scriptEditor = null;

    /**
     * Controller for the currently running script.
     */
    private ITextWsScriptController runningScript = null;

    /**
     * Error in script flag
     */
    private volatile boolean scriptHasError = false;

    private StringBuilder scriptErrorBfr = null;

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
     * Indicates this instance of dialog is for a warnGen.
     */
    private boolean warnGenFlag = false;

    /**
     * Indicates the instance of the dialog is for text work station.
     */
    private boolean textWorkstationFlag = false;

    /**
     * When true this is a WarnGen dialog.
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
    private MouseAdapter updateObsListener = null;

    /** Text character wrap dialog */
    private TextCharWrapDlg textCharWrapDlg;

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

    /** Text foregraound color. */
    private Color textForegroundClr;

    /** Text background color. */
    private Color textBackgroundClr;

    /** Highlight foreground color. */
    private Color highlightForegroundClr;

    /** Highlight background color. */
    private Color highlightBackgroundClr;

    private IExecutionListener pasteCommandListener;

    private static volatile List<Pattern> paddingPatternList = new ArrayList<>();

    private static final String PARAGRAPH_PADDING_PATTERN_FILENAME = "textws/gui/ParagraphPaddingPattern.txt";

    // derived from /data/fxa/nationalData/textQC.config
    private static final List<String> updateHeaderTimesPils = Arrays.asList(
            "EWW", "FFS", "FFW", "FLS", "FLW", "MWS", "NOW", "SMW", "SPS",
            "SVR", "SVS", "TOR", "SQW", "DSW");

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
        super(parent, SWT.DIALOG_TRIM | SWT.MIN | SWT.MAX | SWT.RESIZE,
                CAVE.DO_NOT_BLOCK | CAVE.INDEPENDENT_SHELL
                        | additionalCaveStyle);

        winTitle = title;
        setText(winTitle);

        this.token = token;
        this.hasScripting = hasScripting;

        this.disposeOnExit = disposeOnExit;

        callbackClient = cbClient;
        this.textWorkstationFlag = textWorkstationFlag;
        SimulatedTime.getSystemTime().addSimulatedTimeChangeListener(this);
    }

    private static JAXBManager getJaxbManager() throws JAXBException {
        if (jaxb == null) {
            jaxb = new JAXBManager(OperationalStdTextProduct.class,
                    PracticeStdTextProduct.class);
        }
        return jaxb;
    }

    @Override
    protected Layout constructShellLayout() {
        // Create the main layout for the shell.
        GridLayout mainLayout = new GridLayout(1, true);
        mainLayout.marginHeight = 0;
        mainLayout.marginWidth = 0;
        return mainLayout;
    }

    @Override
    protected void initializeComponents(final Shell shell) {
        clipboard = new Clipboard(getDisplay());

        if (textWorkstationFlag || isWarnGenDlg) {
            shell.addShellListener(new ShellAdapter() {
                @Override
                public void shellClosed(ShellEvent event) {
                    if (isWarnGenDlg && inEditMode) {
                        event.doit = false;
                        cancelDoClose = true;
                        bringToTop();
                        logInfo("Window close button clicked");
                        cancelEditor(true);
                        return;
                    }

                    // If the disposeDialog flag is true then return so this
                    // dialog will get disposed.
                    if (disposeDialog) {
                        return;
                    }

                    // If the disposeOnExit is true then return since this
                    // dialog
                    // will be modal and we can't prevent the dialog from
                    // disposing.
                    if (disposeOnExit) {
                        return;
                    }

                    if (afosBrowser != null && afosBrowser.isBrowserActive()) {
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

            getParent().addDisposeListener(e -> {
                inEditMode = false;
                close();
            });
        }

        commandHistory = new CommandHistory();

        // Create the menus
        createMenus();

        // Initialize all of the controls and layouts
        createTopButtonRow();

        Label sepLbl = new Label(shell, SWT.SEPARATOR | SWT.HORIZONTAL);
        sepLbl.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));

        createTextRow();
        createHeaderTextField();
        createEditorControlButtons();
        createTextAreaEditor();
        createScriptRunnerControlBar();
        createStatusBar();

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
        AwipsBrowserItem = new MenuItem(fileMenu, SWT.NONE);
        AwipsBrowserItem.setText("AWIPS Browser...");
        AwipsBrowserItem.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                displayAwipsBrowser();
            }
        });

        AFOSBrowserItem = new MenuItem(fileMenu, SWT.NONE);
        AFOSBrowserItem.setText("AFOS Browser...");
        AFOSBrowserItem.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                displayAfosBrowser();
            }
        });

        new MenuItem(fileMenu, SWT.SEPARATOR);

        MenuItem printAllItem = new MenuItem(fileMenu, SWT.NONE);
        printAllItem.setText("Print All\tCtrl+P");
        printAllItem.setAccelerator(SWT.CTRL + 'P');
        printAllItem.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                printAllText();
            }
        });

        MenuItem printSelectionItem = new MenuItem(fileMenu, SWT.NONE);
        printSelectionItem.setText("Print Selection");
        printSelectionItem.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                printSelectedText();
            }
        });

        new MenuItem(fileMenu, SWT.SEPARATOR);

        enterEditorItem = new MenuItem(fileMenu, SWT.NONE);
        enterEditorItem.setText("Enter Editor");
        enterEditorItem.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                inEditMode = true;
                logInfo("File -> Enter Editor clicked");
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

        cancelEditorItem = new MenuItem(fileMenu, SWT.NONE);
        cancelEditorItem.setText("Cancel Editor");
        cancelEditorItem.setEnabled(false);
        cancelEditorItem.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                logInfo("File -> Cancel Editor clicked");
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

        MenuItem exportFromFileItem = new MenuItem(fileMenu, SWT.NONE);
        exportFromFileItem.setText("Export To File...");
        exportFromFileItem.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                exportFile();
            }
        });

        new MenuItem(fileMenu, SWT.SEPARATOR);

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

        closeItem = new MenuItem(fileMenu, SWT.NONE);
        closeItem.setText("Close\tAlt+F4");
        closeItem.setAccelerator(SWT.ALT | SWT.F4);
        closeItem.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                if (disposeOnExit) {
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

        MenuItem copyItem = new MenuItem(editMenu, SWT.NONE);
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
        MenuItem selectSubMenuItem = new MenuItem(editMenu, SWT.CASCADE);
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

        MenuItem searchItem = new MenuItem(editMenu, SWT.NONE);
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
    public void clearTextEditor() {
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
        MenuItem fontSizeMenuItem = new MenuItem(optionsMenu, SWT.CASCADE);
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
                if (!AFOSParser.isTemplate) {
                    setOverwriteMode(!overwriteMode);
                }
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

        MenuItem versionPreviousItem = new MenuItem(versionMenu, SWT.NONE);
        versionPreviousItem.setText("Previous\tAlt+Left Arrow");
        versionPreviousItem.setAccelerator(SWT.ALT | SWT.ARROW_LEFT);
        versionPreviousItem.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                CommandType type = prepareHistory();
                ICommand command = commandHistory.getPreviousCommand(type);
                if (command != null) {
                    ICommand cmd = CommandFactory
                            .getPreviousForCommand(command);
                    if (cmd.isValid()) {
                        executeCommand(cmd);
                    }
                }
            }
        });

        MenuItem versionNextItem = new MenuItem(versionMenu, SWT.NONE);
        versionNextItem.setText("Next\tAlt+Right Arrow");
        versionNextItem.setAccelerator(SWT.ALT | SWT.ARROW_RIGHT);
        versionNextItem.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                CommandType type = prepareHistory();
                ICommand command = commandHistory.getPreviousCommand(type);
                if (command != null) {
                    ICommand cmd = CommandFactory.getNextForCommand(command);
                    if (cmd.isValid()) {
                        executeCommand(cmd);
                    }
                }
            }
        });

        MenuItem versionLatestItem = new MenuItem(versionMenu, SWT.NONE);
        versionLatestItem.setText("Latest");
        versionLatestItem.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                CommandType type = prepareHistory();
                ICommand command = commandHistory.getPreviousCommand(type);
                if (command != null) {
                    ICommand cmd = CommandFactory.getLatestForCommand(command);
                    if (cmd.isValid()) {
                        executeCommand(cmd);
                    }
                }
            }
        });

        MenuItem versionAllItem = new MenuItem(versionMenu, SWT.NONE);
        versionAllItem.setText("All");
        versionAllItem.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                CommandType type = prepareHistory();
                ICommand command = commandHistory.getPreviousCommand(type);
                if (command != null) {
                    ICommand cmd = CommandFactory.getAllForCommand(command);
                    if (cmd.isValid()) {
                        executeCommand(cmd);
                    }
                }
            }
        });
    }

    /**
     * Set up command history for the newest entry and return entries command
     * type.
     *
     * @return
     */
    private CommandType prepareHistory() {
        List<ICommand> l = commandHistory.getRecentHistory();
        CommandType type = l.get(l.size() - 1).getType();
        commandHistory.resetIndex(type);
        return type;
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
        IMenuService ms = PlatformUI.getWorkbench()
                .getService(IMenuService.class);
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
                if (object instanceof MenuItem) {
                    MenuItem source = (MenuItem) object;
                    setShowScriptOutput(source.getSelection());
                }
            }
        });

        new MenuItem(scriptsMenu, SWT.SEPARATOR);

        scriptsContinueItem = new MenuItem(scriptsMenu, SWT.NONE);
        scriptsContinueItem.setText("Continue/Skip Wait");
        scriptsContinueItem.setEnabled(false);
        scriptsContinueItem.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                continueScript();
            }

        });

        scriptsCancelItem = new MenuItem(scriptsMenu, SWT.NONE);
        scriptsCancelItem.setText("Cancel");
        scriptsCancelItem.setEnabled(false);
        scriptsCancelItem.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                cancelScript();
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
        DiscoverMenuContributions.discoverContributions("menus/textws");
        // -----------------------------------------
        // Create all the items in the Products menu
        // -----------------------------------------
        MenuManager productsMenuMgr = new MenuManager("Products");
        IMenuService ms = PlatformUI.getWorkbench()
                .getService(IMenuService.class);
        ms.populateContributionManager(productsMenuMgr,
                "menu:#texteditor.products.menu");
        productsMenuMgr.fill(menuBar, -1);
        Menu productsMenu = productsMenuMgr.getMenu();
        productsMenu.setData("Dialog", this);
        productsMenuItem = productsMenu.getParentItem();
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
     * Method to create selection sub-menu.
     *
     * @param selectSubMenu
     */
    private void createSelectSubMenu(Menu selectSubMenu) {
        MenuItem toPreviousWordItem = new MenuItem(selectSubMenu, SWT.NONE);
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

        MenuItem toNextWordItem = new MenuItem(selectSubMenu, SWT.NONE);
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

        MenuItem toBeginningOfLineItem = new MenuItem(selectSubMenu, SWT.NONE);
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

        MenuItem toEndOfLineItem = new MenuItem(selectSubMenu, SWT.NONE);
        toEndOfLineItem.setText("To end of line\tShift+End");
        toEndOfLineItem.setAccelerator(SWT.SHIFT | SWT.END);
        toEndOfLineItem.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                // Select to the end of the current line
                int start = TextEditorUtil.determineSelectionStart(textEditor);
                String text = textEditor.getText();
                int charCount = textEditor.getCharCount();
                int finish;
                for (finish = start; finish < charCount; finish++) {
                    char c = text.charAt(finish);
                    if (c == '\n') {
                        break;
                    }
                }

                textEditor.setSelection(start, finish);
            }
        });

        MenuItem toPreviousPageItem = new MenuItem(selectSubMenu, SWT.NONE);
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

        MenuItem toNextPageItem = new MenuItem(selectSubMenu, SWT.NONE);
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

        MenuItem toTopOfProductItem = new MenuItem(selectSubMenu, SWT.NONE);
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

        MenuItem toEndOfProductItem = new MenuItem(selectSubMenu, SWT.NONE);
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

        MenuItem selectAllItem = new MenuItem(selectSubMenu, SWT.NONE);
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
        MenuItem deleteCharacterItem = new MenuItem(deleteSubMenu, SWT.NONE);
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

        MenuItem deleteWordItem = new MenuItem(deleteSubMenu, SWT.NONE);
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
                textEditor.replaceTextRange(result[0], deletedWord.length(),
                        "");
            }
        });

        MenuItem deleteLineItem = new MenuItem(deleteSubMenu, SWT.NONE);
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
     * Create the Autowrap sub menu items. The Autowrap menu item is located
     * under the Options menu on the top menu bar.
     *
     * @param autoWrapSubMenu
     *            The Autowrap sub menu.
     */
    private void createAutoWrapSubMenu(Menu autoWrapSubMenu) {
        SelectionListener autoWrapSelectionListener = new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                MenuItem item = (MenuItem) event.getSource();
                if (item.getSelection()) {
                    WrapButtonCfg buttonCfg = (WrapButtonCfg) item.getData();
                    configureAutoWrap(buttonCfg);
                }
            }
        };

        MenuItem autoWrapOffItem = new MenuItem(autoWrapSubMenu, SWT.RADIO);
        autoWrapOffItem.setText("Off");
        autoWrapOffItem.setData(new WrapButtonCfg());
        autoWrapOffItem.addSelectionListener(autoWrapSelectionListener);

        AutoWrapCfg autoWrapCfg = AutoWrapCfg.loadAutoWrapCfg();
        this.nonWrapPils = autoWrapCfg.getNonWrapPils();

        WrapButtonCfg selectedConfig = null;
        for (WrapButtonCfg buttonCfg : autoWrapCfg.getButtons()) {
            MenuItem item;
            if (!buttonCfg.isWrapEnabled()) {
                // Off menu item cannot be overridden
                continue;

            }
            item = new MenuItem(autoWrapSubMenu, SWT.RADIO);
            item.setData(buttonCfg);
            item.setText(buttonCfg.getLabelName());
            item.setSelection(false);
            item.addSelectionListener(autoWrapSelectionListener);

            if (buttonCfg.isSelected()) {
                selectedConfig = buttonCfg;
            }
        }

        if (selectedConfig == null) {
            selectedConfig = (WrapButtonCfg) autoWrapOffItem.getData();
        }

        final int rangeStart = autoWrapCfg.getRangeStart();
        final int rangeEnd = autoWrapCfg.getRangeEnd();

        MenuItem autoWrapOtherItem = new MenuItem(autoWrapSubMenu, SWT.RADIO);
        autoWrapOtherItem.setText("Other...");
        autoWrapOtherItem.setData(new WrapButtonCfg(rangeEnd, false));
        autoWrapOtherItem.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                MenuItem item = (MenuItem) event.getSource();

                if (item.getSelection()) {
                    // Open the text character wrap dialog.
                    openTextCharWrapDialog(item, rangeStart, rangeEnd);
                }
            }
        });

        configureAutoWrap(selectedConfig);
    }

    /**
     * Dialog to allow user to create non-standard word wrap column.
     *
     * @param rangeStart
     * @param rangeEnd
     */
    private void openTextCharWrapDialog(MenuItem item, final int rangeStart,
            final int rangeEnd) {
        // Create the text character wrap dialog.
        if (textCharWrapDlg == null || textCharWrapDlg.isDisposed()) {
            WrapButtonCfg buttonCfg = (WrapButtonCfg) item.getData();
            textCharWrapDlg = new TextCharWrapDlg(shell, buttonCfg.getWrapCol(),
                    rangeStart, rangeEnd);

            textCharWrapDlg.addCloseCallback(returnValue -> {
                if (returnValue != null) {
                    buttonCfg.setWrapCol((Integer) returnValue);
                    currentWrapCfg = buttonCfg;

                    item.setText(String.format("Other...  (%d)", returnValue));
                }

                configureAutoWrap(currentWrapCfg);
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

        FontSizeCfg fontSizeCfg = TextEditorCfg.getTextEditorCfg()
                .getFontSizeCfg();
        SizeButtonCfg seldFontBtn = TextEditorCfg.getSelectedFontButton();

        for (SizeButtonCfg buttonCfg : fontSizeCfg.getButtons()) {
            MenuItem item = new MenuItem(fontSizeSubMenu, SWT.RADIO);
            item.setText(buttonCfg.getLabelName());
            item.setSelection(false);
            item.setData(buttonCfg);

            // if this button is the initial selection.
            if (seldFontBtn.getLabelName().equals(buttonCfg.getLabelName())) {
                item.setSelection(true);
                setDefaultFont(seldFontBtn.getFontSize(),
                        seldFontBtn.getFontName());
            }

            item.addSelectionListener(new SelectionAdapter() {
                @Override
                public void widgetSelected(SelectionEvent event) {
                    MenuItem item = (MenuItem) event.getSource();
                    if (item.getSelection()) {
                        int selectFontSize = ((SizeButtonCfg) item.getData())
                                .getFontSize();
                        String seldFontName = ((SizeButtonCfg) item.getData())
                                .getFontName();

                        setDefaultFont(selectFontSize, seldFontName);

                        textEditor.setFont(dftFont);
                        headerTF.setFont(dftFont);
                    }
                }
            });
        }

    }

    public void setDefaultFont(int fontSize, String fontName) {
        dftFont = new Font(getDisplay(), fontName, fontSize, SWT.NORMAL);
    }

    /**
     * Create the top row of buttons on the display.
     */
    private void createTopButtonRow() {
        // Create the composite to contain the row of buttons.
        Composite topBtnRowComp = new Composite(shell, SWT.NONE);
        GridLayout layout = new GridLayout(7, false);
        topBtnRowComp.setLayout(layout);

        // Add the AWIPS Browser button.
        GridData rd = new GridData(SWT.DEFAULT, SWT.DEFAULT);
        awipsBrowserBtn = new Button(topBtnRowComp, SWT.PUSH);
        awipsBrowserBtn.setText("AWIPS Browser");
        awipsBrowserBtn.setLayoutData(rd);
        awipsBrowserBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                displayAwipsBrowser();
            }
        });

        // Add the Load History button.
        rd = new GridData(SWT.DEFAULT, SWT.DEFAULT);
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
                if (!commands.isEmpty()) {
                    menuMgr = new MenuManager("#PopupMenu");

                    for (int i = commands.size() - 1; i >= 0; i--) {
                        final ICommand cmd = commands.get(i);
                        menuMgr.add(
                                new Action(TextEditorUtil.getCommandText(cmd)) {
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
        rd = new GridData(SWT.DEFAULT, SWT.DEFAULT);
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
        rd = new GridData(SWT.DEFAULT, SWT.DEFAULT);
        enterEditorBtn = new Button(topBtnRowComp, SWT.PUSH);
        enterEditorBtn.setText("Enter Editor");
        enterEditorBtn.setLayoutData(rd);
        enterEditorBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                enterEditor1();
            }
        });

        // Add the Accumulate text check button.
        rd = new GridData(SWT.DEFAULT, SWT.DEFAULT);
        accumChkBtn = new Button(topBtnRowComp, SWT.CHECK);
        accumChkBtn.setText("Accum  ");
        accumChkBtn.setLayoutData(rd);

        // Add the Update Observation check button.
        rd = new GridData(SWT.DEFAULT, SWT.DEFAULT);
        updateObsChkBtn = new Button(topBtnRowComp, SWT.CHECK);
        updateObsChkBtn.setText("Update Obs  ");
        updateObsChkBtn.setLayoutData(rd);
        updateObsChkBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                handleUpdateObsChkBtn(updateObsChkBtn.getSelection());
            }
        });

        // Add the Clear button.
        rd = new GridData(SWT.DEFAULT, SWT.DEFAULT);
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
        GridLayout layout = new GridLayout(7, false);
        textRowComp.setLayout(layout);
        GC gc = new GC(textRowComp.getDisplay());
        gc.setFont(textRowComp.getFont());
        int charWidth = gc.textExtent("M").x;
        gc.dispose();

        createAwipsIdTf(textRowComp, charWidth);
        createWmoTtaaiiTF(textRowComp, charWidth);
        createCcccTF(textRowComp, charWidth);
        createAfosCmdTf(textRowComp, charWidth);
    }

    private void createAfosCmdTf(Composite textRowComp, int charWidth) {
        // Create a AFOS command label that is vertically aligned.
        Label label = new Label(textRowComp, SWT.NONE);
        GridData layoutData = new GridData(SWT.CENTER, SWT.DEFAULT);
        label.setText("AFOS Cmd:");

        // Add the AFOS command text field.
        afosCmdTF = new Text(textRowComp, SWT.BORDER);
        afosCmdTF.setTextLimit(13);
        layoutData = new GridData(SWT.DEFAULT, SWT.DEFAULT);
        layoutData.widthHint = 13 * charWidth;
        afosCmdTF.setLayoutData(layoutData);
        afosCmdTF.addFocusListener(new FocusAdapter() {
            @Override
            public void focusGained(FocusEvent event) {
                clearWmoTF();
                clearAwipsIdTF();
            }

            @Override
            public void focusLost(FocusEvent event) {
                String tmp = afosCmdTF.getText();
                tmp = tmp.trim();
                tmp = tmp.toUpperCase();
                afosCmdTF.setText(tmp);
            }
        });

        afosCmdTF.addVerifyListener(
                e -> e.text = e.text.toUpperCase().replace(';', ':'));

        afosCmdTF.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetDefaultSelected(SelectionEvent event) {
                String tmp = afosCmdTF.getText();
                tmp = tmp.trim();
                afosCmdTF.setText(tmp);

                AFOSParser parser = new AFOSParser(afosCmdTF.getText(),
                        AfosBrowserModel.getInstance().getLocalSite());
                if (!parser.isValidCommand()) {
                    userInformation("AFOSCMD is invalid");
                    if (!afosCmdTF.isDisposed()) {
                        afosCmdTF.setFocus();
                    }
                    return;
                }

                TextDisplayModel.getInstance().setAfosCommand(token,
                        parser.getAfosCommand());

                // Perform the query of the product identified by the Afos
                // Command.
                executeCommand(
                        CommandFactory.getAfosCommand(afosCmdTF.getText()));

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
                } else if (e.keyCode == SWT.PAGE_DOWN) {
                    textEditor.invokeAction(ST.PAGE_DOWN);
                    e.doit = false;
                } else if (e.keyCode == SWT.PAGE_UP) {
                    textEditor.invokeAction(ST.PAGE_UP);
                    e.doit = false;
                }
            }
        });
    }

    private void createWmoTtaaiiTF(Composite textRowComp, int charWidth) {
        // Create a WMO label that is vertically aligned.
        Label label = new Label(textRowComp, SWT.NONE);
        GridData layoutData = new GridData(SWT.CENTER, SWT.DEFAULT);
        label.setText("WMO TTAAii CCCC:");

        // Add the WMO data type and area indicator text field.
        wmoTtaaiiTF = new Text(textRowComp, SWT.BORDER);
        wmoTtaaiiTF.setTextLimit(6);
        layoutData = new GridData(SWT.DEFAULT, SWT.DEFAULT);
        layoutData.widthHint = 6 * charWidth;
        wmoTtaaiiTF.setLayoutData(layoutData);
        wmoTtaaiiTF.addFocusListener(new FocusAdapter() {
            @Override
            public void focusGained(FocusEvent event) {
                clearAfosCmdTF();
                clearAwipsIdTF();
            }

            @Override
            public void focusLost(FocusEvent event) {
            }
        });

        wmoTtaaiiTF.addVerifyListener(e -> e.text = e.text.toUpperCase());

        wmoTtaaiiTF.addModifyListener(event -> {
            if (wmoTtaaiiTF.getCaretPosition() == wmoTtaaiiTF.getTextLimit()) {
                ccccTF.setFocus();
            }
        });

        wmoTtaaiiTF.addSelectionListener(new SelectionAdapter() {

            @Override
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
    }

    private void createCcccTF(Composite textRowComp, int charWidth) {
        // Add the International location indicator text field.
        ccccTF = new Text(textRowComp, SWT.BORDER);
        ccccTF.setTextLimit(4);
        GridData layoutData = new GridData(SWT.DEFAULT, SWT.DEFAULT);
        layoutData.widthHint = 4 * charWidth;
        ccccTF.setLayoutData(layoutData);
        ccccTF.addFocusListener(new FocusAdapter() {
            @Override
            public void focusGained(FocusEvent event) {
                clearAfosCmdTF();
                clearAwipsIdTF();
            }

            @Override
            public void focusLost(FocusEvent event) {
            }
        });

        ccccTF.addVerifyListener(e -> e.text = e.text.toUpperCase());

        ccccTF.addSelectionListener(new SelectionAdapter() {

            @Override
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
    }

    private void createAwipsIdTf(Composite textRowComp, int charWidth) {
        // Create an AWIPS ID label that is vertically aligned.
        Label label = new Label(textRowComp, SWT.NONE);
        GridData layoutData = new GridData(SWT.CENTER, SWT.DEFAULT);
        label.setText("AWIPS ID:");

        // Add the AWIPS ID text field.
        awipsIdTF = new Text(textRowComp, SWT.BORDER);
        awipsIdTF.setTextLimit(14);
        layoutData = new GridData(SWT.DEFAULT, SWT.DEFAULT);
        layoutData.widthHint = 14 * charWidth;
        awipsIdTF.setLayoutData(layoutData);
        awipsIdTF.addFocusListener(new FocusAdapter() {
            @Override
            public void focusGained(FocusEvent event) {
                clearAfosCmdTF();
                clearWmoTF();
            }

            @Override
            public void focusLost(FocusEvent event) {
            }
        });

        awipsIdTF.addVerifyListener(e -> e.text = e.text.toUpperCase());

        awipsIdTF.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetDefaultSelected(SelectionEvent event) {
                awipsIdTF.setText(awipsIdTF.getText().trim().toUpperCase());
                String cmd = awipsIdTF.getText();
                String hdrTime = null;
                String pastVersion = null;
                String site = null;
                boolean error = false;
                boolean singleProduct = false;

                if (cmd.startsWith("ALL:")) {
                    site = "0000";
                    cmd = cmd.substring(4);
                    hdrTime = "000000";
                    singleProduct = true;
                } else if (cmd.startsWith("-")) {
                    int end = cmd.indexOf(':');
                    if (end == -1) {
                        error = true;
                    } else {
                        pastVersion = cmd.substring(1, end);
                        cmd = cmd.substring(end + 1);
                    }
                } else {
                    hdrTime = null;
                }
                int charCount = cmd.length();
                if (error || charCount < 3 || charCount > 10) {
                    userInformation(
                            "Optional \"ALL:\" or previous version number\nExample: \"-1:\" \nthen a 3 to 10 character AWIPS ID");
                    if (!awipsIdTF.isDisposed()) {
                        awipsIdTF.setFocus();
                    }
                    return;
                } else {
                    if (charCount >= 7) {
                        site = cmd.substring(0, 4);
                        cmd = cmd.substring(4);
                    }
                    TextDisplayModel.getInstance().setProductCategory(token,
                            cmd.substring(0, 3));
                    TextDisplayModel.getInstance().setProductDesignator(token,
                            cmd.length() > 3 ? cmd.substring(3) : "");
                }

                // Highlight the text contained in the Awips ID Field.
                awipsIdTF.selectAll();

                /*
                 * Perform the query for all hdr times products identified by
                 * the Awips ID and possibly restrict to a site.
                 */
                ICommand command = CommandFactory.getAwipsCommand(cmd, null,
                        site, hdrTime, pastVersion, null, singleProduct);
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
                } else if (e.keyCode == SWT.PAGE_DOWN) {
                    textEditor.invokeAction(ST.PAGE_DOWN);
                    e.doit = false;
                } else if (e.keyCode == SWT.PAGE_UP) {
                    textEditor.invokeAction(ST.PAGE_UP);
                    e.doit = false;
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
                updateObsListener = new MouseAdapter() {

                    @Override
                    public void mouseUp(MouseEvent e) {
                        try {
                            int offset = textEditor
                                    .getOffsetAtLocation(new Point(e.x, e.y));
                            clearUpdateFlag(offset);
                        } catch (@SuppressWarnings("squid:S1166")
                        IllegalArgumentException ex) {
                            /* bad mouse location ignore */
                        }
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
            if (range.start <= offset && offset < range.start + range.length) {
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
        GridData layoutData = new GridData(GridData.FILL_HORIZONTAL);
        headerTFComp = new Composite(shell, SWT.NONE);
        GridLayout gridLayout = new GridLayout(1, false);
        headerTFComp.setLayout(gridLayout);
        layoutData.exclude = true;
        headerTFComp.setLayoutData(layoutData);
        headerTFComp.setVisible(false);

        headerTF = new Text(headerTFComp,
                SWT.BORDER | SWT.MULTI | SWT.READ_ONLY);
        layoutData = new GridData(GridData.FILL_HORIZONTAL);
        layoutData.heightHint = headerTF.computeTrim(0, 0, SWT.DEFAULT,
                headerTF.getLineHeight() * 2).height;
        headerTF.setLayoutData(layoutData);

        headerTF.setFont(dftFont);
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
        gd = new GridData(SWT.DEFAULT, SWT.DEFAULT);
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
        gd = new GridData(SWT.DEFAULT, SWT.DEFAULT);
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
        gd = new GridData(SWT.DEFAULT, SWT.DEFAULT);
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
        gd = new GridData(SWT.DEFAULT, SWT.DEFAULT);
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
        gd = new GridData(SWT.DEFAULT, SWT.DEFAULT);
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
        gd = new GridData(SWT.DEFAULT, SWT.DEFAULT);
        Button editorEditHeaderBtn = new Button(editorBtnRowComp, SWT.PUSH);
        editorEditHeaderBtn.setText("Edit Header");
        editorEditHeaderBtn.setLayoutData(gd);
        editorEditHeaderBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                // Edit the header block of the text product.
                editHeader("", false);
            }
        });

        // Add the Cancel button.
        gd = new GridData(SWT.DEFAULT, SWT.DEFAULT);
        Button editorCancelBtn = new Button(editorBtnRowComp, SWT.PUSH);
        editorCancelBtn.setText("Cancel");
        editorCancelBtn.setLayoutData(gd);
        editorCancelBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                logInfo("Cancel button clicked");
                cancelEditor(true);
            }
        });

        // Add the Attach button.
        gd = new GridData(SWT.DEFAULT, SWT.DEFAULT);
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
        gd = new GridData(SWT.DEFAULT, SWT.DEFAULT);
        editorInsertCmb = new Combo(editorBtnRowComp,
                SWT.DROP_DOWN | SWT.READ_ONLY);
        editorInsertCmb.add("INS");
        editorInsertCmb.add("OVR");
        editorInsertCmb.select(INSERT_TEXT);
        editorInsertCmb.setLayoutData(gd);
        editorInsertCmb.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                if (!AFOSParser.isTemplate) {
                    if (editorInsertCmb.getSelectionIndex() == INSERT_TEXT) {
                        logInfo("INS clicked");
                        setOverwriteMode(false);
                    } else if (editorInsertCmb
                            .getSelectionIndex() == OVERWRITE_TEXT) {
                        logInfo("OVR clicked");
                        setOverwriteMode(true);
                    }
                    textEditor.setFocus();
                }
            }
        });

        editorBtnRowComp.layout();
    }

    /**
     * Create the text editor (styled text) control.
     */
    private void createTextAreaEditor() {
        GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        Composite textEditorComp = new Composite(shell, SWT.NONE);
        GridLayout gridLayout = new GridLayout(1, false);
        // TextColorsCfg textColorCfg = null;

        textEditorComp.setLayout(gridLayout);
        textEditorComp.setLayoutData(gd);

        textEditor = new StyledText(textEditorComp,
                SWT.BORDER | SWT.MULTI | SWT.V_SCROLL | SWT.H_SCROLL);
        gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        textEditor.setFont(dftFont);

        GC gc = new GC(textEditor);
        FontMetrics fm = gc.getFontMetrics();
        gc.dispose();
        Rectangle size = textEditor.computeTrim(0, 0,
                EDITOR_WIDTH * fm.getAverageCharWidth(),
                EDITOR_HEIGHT * textEditor.getLineHeight());
        gd.widthHint = size.width;
        gd.heightHint = size.height;

        textEditor.setLayoutData(gd);
        textEditor.setWordWrap(false);
        textEditor.setEditable(false);
        airportToolTip = new DefaultToolTip(textEditor, SWT.DEFAULT, true);
        // DR 7826
        textEditor.setKeyBinding(SWT.INSERT, SWT.NULL);

        // textColorCfg = getTextColorCfg();
        setDefaultTextColor(TextEditorCfg.getTextEditorCfg());
        textEditor.setForeground(textForegroundClr);
        textEditor.setBackground(textBackgroundClr);

        textEditor.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected(SelectionEvent e) {

                StyledText stylText = (StyledText) e.getSource();

                stylText.setSelectionBackground(highlightBackgroundClr);
                stylText.setSelectionForeground(highlightForegroundClr);

            }
        });

        textEditor.addKeyListener(new KeyAdapter() {
            @Override
            public void keyPressed(KeyEvent e) {
                if (e.keyCode == SWT.ARROW_LEFT && !textEditor.getEditable()) {
                    CommandType type = prepareHistory();
                    ICommand command = commandHistory.getPreviousCommand(type);
                    if (command != null) {
                        ICommand cmd = CommandFactory
                                .getPreviousForCommand(command);
                        if (cmd.isValid()) {
                            executeCommand(cmd);
                        }
                    }
                } else if (e.keyCode == SWT.ARROW_RIGHT
                        && !textEditor.getEditable()) {
                    CommandType type = prepareHistory();
                    ICommand command = commandHistory.getPreviousCommand(type);
                    if (command != null) {
                        ICommand cmd = CommandFactory
                                .getNextForCommand(command);
                        if (cmd.isValid()) {
                            executeCommand(cmd);
                        }
                    }
                }
            }
        });

        textEditor.addExtendedModifyListener(event -> {
            eventStart = event.start;
            if (userKeyPressed) {
                userKeyPressed = false;
                rewrap(eventStart, eventStart);
            }
        });

        textEditor.addVerifyKeyListener(event -> {
            // Ignore edit keys when not in edit mode.
            if (!textEditor.getEditable()) {
                return;
            }
            if (event.keyCode == SWT.DEL || event.keyCode == SWT.BS
                    || event.keyCode == SWT.SHIFT) {
                // Do nothing...
                // We need to capture the Delete, Backspace and Shift
                // keystrokes...
            } else if (event.keyCode == SWT.HOME || event.keyCode == SWT.END) {
                if (!textEditor.getEditable()) {
                    int offset = 0;

                    if (event.keyCode == SWT.END) {
                        offset = textEditor.getCharCount();
                    }

                    textEditor.setCaretOffset(offset);
                    textEditor.showSelection();
                    event.doit = false;
                }
            } else if (event.keyCode == SWT.ARROW_DOWN
                    && event.stateMask == SWT.CTRL) {
                cursorToNextParagraphStart();
            } else if (event.keyCode == SWT.ARROW_UP
                    && event.stateMask == SWT.CTRL) {
                cursorToPreviousParagraphStart();
            } else if (event.keyCode == SWT.PAGE_UP
                    && event.stateMask == SWT.CTRL) {
                // Ignore Ctrl + PageUp
                event.doit = false;
            } else if (event.keyCode == SWT.PAGE_DOWN
                    && event.stateMask == SWT.CTRL) {
                // Ignore Ctrl + PageDown
                event.doit = false;
            } else if (event.keyCode == SWT.PAGE_UP
                    && event.stateMask == (SWT.CTRL | SWT.SHIFT)) {
                // Ignore Ctrl+Shift+PageUp
                event.doit = false;
            } else if (event.keyCode == SWT.PAGE_DOWN
                    && event.stateMask == (SWT.CTRL | SWT.SHIFT)) {
                // Ignore Ctrl+Shift+PageDown
                event.doit = false;
            } else if (event.keyCode == SWT.INSERT) {
                // Ins key on the keypad
                if (AFOSParser.isTemplate) {
                    setOverwriteMode(!overwriteMode);
                }
            } else if (event.keyCode > 500) {
                // Do nothing...
                // We need to capture the non-alphanumeric editing-related
                // keystrokes...
            }

            // if some event is going to happen and the key was not enter
            // then set userKeyPressed to true
            if (event.doit && event.character != 0 && event.character != '\n'
                    && event.character != '\r') {
                userKeyPressed = true;
            }
            if (AFOSParser.isTemplate) {

                if (event.keyCode == SWT.BS) {
                    event.doit = false;
                    int currentPos1 = textEditor.getCaretOffset();
                    String textUpToCaret1 = textEditor.getText().substring(0,
                            currentPos1);
                    int leftMost = textUpToCaret1.lastIndexOf('[') + 1;
                    int rightMost = textEditor.getText().indexOf(']',
                            currentPos1);
                    int editableTextWidth = rightMost - leftMost;
                    String leftPart = "";
                    String rightPart = "";
                    if (currentPos1 == leftMost) {
                        leftPart = "";
                        rightPart = textEditor.getText().substring(currentPos1,
                                rightMost);
                        textEditor.setCaretOffset(leftMost);
                    } else if (currentPos1 > leftMost
                            && currentPos1 <= rightMost) {
                        leftPart = textEditor.getText().substring(leftMost,
                                currentPos1 - 1);
                        rightPart = textEditor.getText().substring(currentPos1,
                                rightMost);
                    } else if (currentPos1 == rightMost) {
                        leftPart = textEditor.getText().substring(leftMost,
                                currentPos1 - 1);
                        rightPart = "";
                    }
                    String newString = leftPart + rightPart;
                    int neededPadSpaces = editableTextWidth
                            - newString.length();
                    String newPaddedString = String.format(
                            "%1$-" + (neededPadSpaces + 1) + "s", newString);
                    String spacedoutString = String
                            .format("%1$-" + editableTextWidth + "s", " ");
                    textEditor.replaceTextRange(leftMost,
                            spacedoutString.length(), spacedoutString);
                    textEditor.replaceTextRange(leftMost,
                            newPaddedString.length(), newPaddedString);
                    textEditor.setCaretOffset(currentPos1 - 1);

                } else if (event.keyCode == SWT.TAB) {
                    if (!isTemplateOverwriteModeSet) {
                        if (overwriteMode) {
                            textEditor.invokeAction(ST.TOGGLE_OVERWRITE);
                        } else {
                        }
                        isTemplateOverwriteModeSet = true;
                    }
                    event.doit = false;
                    int currentPos2 = textEditor.getCaretOffset();
                    String textUpToCaret2 = textEditor.getText().substring(0,
                            currentPos2);
                    int openBracketPos = textUpToCaret2.lastIndexOf('[');
                    openBracketPos = textEditor.getText().indexOf('[',
                            currentPos2);
                    textEditor.setCaretOffset(openBracketPos + 1);
                } else if (event.keyCode >= 97 && event.keyCode <= 122
                        || event.keyCode >= 48 && event.keyCode <= 57) {
                    event.doit = true;
                }
            } else if (event.keyCode == SWT.TAB) {
                int lineStartOffset = textEditor.getOffsetAtLine(textEditor
                        .getLineAtOffset(textEditor.getCaretOffset()));
                int caretOffsetOnLine = textEditor.getCaretOffset()
                        - lineStartOffset;
                int numberOfSpaces = textEditor.getTabs()
                        - caretOffsetOnLine % textEditor.getTabs();
                StringBuilder spaces = new StringBuilder();
                for (int x = 0; x < numberOfSpaces; x++) {
                    spaces.append(' ');
                }
                textEditor.insert(spaces.toString());
                textEditor.setCaretOffset(
                        textEditor.getCaretOffset() + numberOfSpaces);
                event.doit = false;
            }
        });

        textEditor.addMouseListener(new MouseAdapter() {
            @Override
            public void mouseDown(org.eclipse.swt.events.MouseEvent e) {
                if (e.button == 2) {
                    int caretOffset = textEditor.getCaretOffset();
                    try {
                        /*
                         * intentionally convert the current mouse location
                         * relative to textEditor component. if an exception is
                         * thrown then the mouse cursor is at an invalid
                         * location; outside the scope the text within the
                         * textEditor component.
                         */
                        textEditor.getOffsetAtLocation(new Point(e.x, e.y));
                        if (isSaoMetarFlag) {
                            Point p = new Point(e.x, e.y);
                            displayAirportTooltip(p);
                        }
                    } catch (@SuppressWarnings("squid:S1166")
                    IllegalArgumentException iae) {
                        textEditor.setCaretOffset(caretOffset);
                    }
                    // rewrap to handle middle-click pasted text
                    if (inEditMode && textEditor.getCharCount() > 0) {
                        rewrap(eventStart, textEditor.getCaretOffset());
                    }
                } else if (e.button == 3) {
                    processPopup();
                }
            }

            @Override
            public void mouseUp(org.eclipse.swt.events.MouseEvent e) {
                airportToolTip.setText(null);
                airportToolTip.hide();
            }
        });

        textEditor.addVerifyListener(TextEditorDialog.this);

        ICommandService service = PlatformUI.getWorkbench()
                .getService(ICommandService.class);

        // Listen for the Eclipse paste command and wrap text after it executes
        pasteCommandListener = new IExecutionListener() {

            private int wrapStartPos = -1;

            @Override
            public void notHandled(String commandId,
                    NotHandledException exception) {
            }

            @Override
            public void postExecuteFailure(String commandId,
                    ExecutionException exception) {
            }

            @Override
            public void postExecuteSuccess(String commandId,
                    Object returnValue) {
                if (wrapStartPos >= 0) {
                    rewrap(wrapStartPos, textEditor.getCaretOffset());
                    wrapStartPos = -1;
                }
            }

            @Override
            public void preExecute(String commandId, ExecutionEvent event) {
                Object trigger = event.getTrigger();
                if (trigger instanceof Event
                        && ((Event) trigger).widget == textEditor) {
                    if (textEditor.getSelectionCount() == 0) {
                        wrapStartPos = textEditor.getCaretOffset();
                    } else {
                        wrapStartPos = textEditor.getSelectionRange().x;
                    }
                }
            }
        };

        service.getCommand("org.eclipse.ui.edit.paste")
                .addExecutionListener(pasteCommandListener);
    }

    private void setDefaultTextColor(TextEditorCfg txtClrCfg) {
        textBackgroundClr = new Color(shell.getDisplay(),
                txtClrCfg.getTextBackgroundColor());
        textForegroundClr = new Color(shell.getDisplay(),
                txtClrCfg.getTextForegroundColor());
        highlightBackgroundClr = new Color(shell.getDisplay(),
                txtClrCfg.getHighlightTextBackgroundColor());
        highlightForegroundClr = new Color(shell.getDisplay(),
                txtClrCfg.getHighlightTextForegroundColor());
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
            mi.addListener(SWT.Selection, event -> handleSelection(event));
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
            if (!textEditor.isDisposed()) {
                textEditor.update();
            }
        }
    }

    /**
     * creates the bar containing the script runner controls.
     */
    private void createScriptRunnerControlBar() {
        scriptRunnerComp = new Composite(shell, SWT.NONE);
        GridLayout layout = new GridLayout(2, true);
        scriptRunnerComp.setLayout(layout);
        scriptRunnerComp
                .setLayoutData(new GridData(GridData.HORIZONTAL_ALIGN_END));

        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        scriptContinueBtn = new Button(scriptRunnerComp, SWT.PUSH);
        scriptContinueBtn.setText("Continue/Skip Wait");
        scriptContinueBtn.setLayoutData(gd);
        scriptContinueBtn.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected(SelectionEvent e) {
                continueScript();
            }
        });

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        gd.horizontalAlignment = SWT.FILL;
        scriptCancelBtn = new Button(scriptRunnerComp, SWT.PUSH);
        scriptCancelBtn.setText("Cancel");
        scriptCancelBtn.setLayoutData(gd);
        scriptCancelBtn.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected(SelectionEvent e) {
                cancelScript();
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

    private void enterEditor1() {
        logInfo("Enter Editor button clicked");
        if (editorInsertCmb.getSelectionIndex() == 0) {
            logInfo("editorInsertCmb status: INS");
        } else {
            logInfo("editorInsertCmb status: OVR");
        }
        enterEditor();
    }

    /**
     * Enter the text editor mode.
     */
    private void enterEditor() {
        if (!validateTime()) {
            return;
        }
        StdTextProduct product = TextDisplayModel.getInstance()
                .getStdTextProduct(token);
        if (isProductForbiddenToEdit(product)) {
            inEditMode = false;
            return;
        }

        initTemplateOverwriteMode();

        // Reset the saved
        saved = false;
        // Set the edit mode flag to true.
        inEditMode = true;
        int ranges[] = textEditor.getRanges();
        if (ranges == null || ranges.length == 0) {
            originalText = textEditor.getText();
        } else {
            logInfo("Original product:\n" + originalText);
            textEditor.setText(originalText);
        }
        // Capture the contents of the current header section and the body
        // section
        setCurrentHeaderAndBody();

        // Mark the uneditable warning text
        if (markUneditableText(textEditor)) {
            // Enable listener to monitor attempt to edit locked text
            verifyUndeditableText = true;
        }

        // if product is a WarnGen product and is not enabled for mixed case
        // transmission, replace all commas with ellipses
        if (TextEditorCfg.getTextEditorCfg().getReplaceCommasWithEllipses()
                && product != null && warngenPils.contains(product.getNnnid())
                && !MixedCaseProductSupport.isMixedCase(product.getNnnid())) {
            replaceCommasWithEllipses(product);
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

    private void configureAutoWrap(WrapButtonCfg buttonCfg) {
        if (buttonCfg != null) {
            currentWrapCfg = buttonCfg;
        }

        for (MenuItem item : autoWrapMenuItem.getMenu().getItems()) {
            item.setSelection(item.getData() == currentWrapCfg);
            item.setEnabled(true);
        }

        if (currentWrapCfg.isWrapEnabled()) {
            recompileRegex();
        }

        logInfo("Options -> Autowrap -> " + currentWrapCfg.getLabelName()
                + " selected");
    }

    private void disableAutoWrap() {
        prevWrapCfg = currentWrapCfg;
        for (MenuItem item : autoWrapMenuItem.getMenu().getItems()) {
            Object obj = item.getData();
            if (obj instanceof WrapButtonCfg) {
                WrapButtonCfg buttonCfg = (WrapButtonCfg) obj;
                if (buttonCfg.isWrapEnabled()) {
                    item.setSelection(false);
                    item.setEnabled(false);
                } else {
                    item.setSelection(true);
                    currentWrapCfg = buttonCfg;
                }
            }
        }
    }

    private void replaceCommasWithEllipses(StdTextProduct product) {
        boolean wasVerifying = verifyUndeditableText;
        try {
            verifyUndeditableText = false;
            /*
             * Performing wrapping as few times as possible to reduce the
             * chances of breaking the product format. Also, the location list
             * does not wrap properly unless all commas in the paragraph have
             * been changed to ellipses.
             */
            Pattern p = Pattern.compile(", {0,1}");
            int pendingParagraphLineStart = -1;
            while (true) {
                String text = textEditor.getText();
                Matcher m = p.matcher(text);
                if (!m.find()) {
                    break;
                }
                int line = textEditor.getLineAtOffset(m.start());
                int paragraphLineStart = findParagraphStart(line);
                String lineText = textEditor.getLine(line);
                boolean lineNeedsWrap = lineText.length()
                        - (m.end() - m.start())
                        + 3 > currentWrapCfg.getWrapCol();
                if (pendingParagraphLineStart >= 0
                        && paragraphLineStart != pendingParagraphLineStart
                        && lineNeedsWrap) {
                    wrapWholeParagraphAtLine(pendingParagraphLineStart);
                    pendingParagraphLineStart = -1;
                    // Line numbers may have changed so restart.
                    continue;
                }
                textEditor.replaceTextRange(m.start(), m.end() - m.start(),
                        "...");
                if (lineNeedsWrap) {
                    pendingParagraphLineStart = paragraphLineStart;
                }
            }
            if (pendingParagraphLineStart >= 0) {
                wrapWholeParagraphAtLine(pendingParagraphLineStart);
            }
        } finally {
            verifyUndeditableText = wasVerifying;
        }
    }

    void wrapWholeParagraphAtLine(int paragraphLineStart) {
        String line = textEditor.getLine(paragraphLineStart);
        // Avoid rewrapInternal early bailout check.
        if (line.length() < currentWrapCfg.getWrapCol()
                && line.indexOf("...") == line.lastIndexOf("...")) {
            paragraphLineStart++;
        }
        int offset = textEditor.getOffsetAtLine(paragraphLineStart);
        rewrap(offset, offset);
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
                    SWT.ICON_QUESTION | SWT.YES | SWT.NO);
            mb.addCloseCallback(returnValue -> {
                if (returnValue instanceof Integer) {
                    int rval = (Integer) returnValue;
                    if (rval == SWT.YES) {
                        doCancelEditor();
                        logInfo("Yes click");
                        if (cancelDoClose) {
                            hide();
                        }
                    } else {
                        logInfo("No click");
                    }
                    cancelDoClose = false;
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

        // restore the word wrap settings
        configureAutoWrap(currentWrapCfg);

        // Hide the header text field.
        headerTFComp.setVisible(false);
        ((GridData) headerTFComp.getLayoutData()).exclude = true;

        // Hide the editor control buttons.
        editorBtnRowComp.setVisible(false);
        ((GridData) editorBtnRowComp.getLayoutData()).exclude = true;

        headerTFComp.getParent().layout();
        editorBtnRowComp.layout();

        // Set the search and replace dialog to reflect the edit state.
        if (searchReplaceDlg != null) {
            searchReplaceDlg.setEditMode(inEditMode);
        }

        // Only set text to originalText for unsaved products
        if (!saved) {
            if (originalText != null) {
                textEditor.setText(originalText);
            }
        }

        markUneditableText(textEditor);

        // Disable the lockable text listener since the application is no
        // longer in edit mode for the warning product that was being edited.
        verifyUndeditableText = false;

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
        if (headerEditSession != null) {
            return;
        }

        // Create and display the AWIPS header block dialog.
        AWIPSHeaderBlockDlg awipsHeaderBlockDlg = new AWIPSHeaderBlockDlg(shell,
                this);
        if (isWarnGen()) {
            editorSaveBtn.setEnabled(false);
            saveItem.setEnabled(false);
        }

        headerEditSession = closeEditorOnCancel
                ? HeaderEditSession.CLOSE_ON_EXIT
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

        if (dialogResult) {

            TextDisplayModel tdm = TextDisplayModel.getInstance();

            // Update the buttonology.
            updateButtonology(tdm.getAfosPil(token));
            String bbbid = tdm.getBbbId(token);

            String nnnxxx = workProductId != null ? workProductId
                    : tdm.getProductCategory(token)
                            + tdm.getProductDesignator(token);
            // Set the header text field.
            if ("NOR".equals(bbbid)
                    || bbbid.isEmpty() && tdm.getAfosPil(token) != null) {
                String wmoId = tdm.getWmoId(token);
                wmoId = wmoId.length() > 0 ? wmoId : "-";
                String siteId = tdm.getSiteId(token);
                siteId = siteId.length() > 0 ? siteId : "-";
                setHeaderTextField(wmoId, siteId, currentDateId, "\n", nnnxxx);
            } else {
                setHeaderTextField(tdm.getWmoId(token), tdm.getSiteId(token),
                        bbbid.length() > 0 ? currentDateId + " " + bbbid
                                : currentDateId,
                        "\n", nnnxxx);
            }

            // Special case to handle the products with no WMO header -
            // remove NNNXXX from product body
            if (textEditor.getLine(0).trim().equals(nnnxxx)) {
                int nnnxxxOffset = nnnxxx.length();
                if (textEditor.getLineCount() > 1) {
                    nnnxxxOffset = nnnxxxOffset + 1;
                    // skip the empty line if it follows NNNXXX
                    if (textEditor.getLine(1).trim().isEmpty()) {
                        nnnxxxOffset = nnnxxxOffset + 1;
                    }
                }
                String replaceText = textEditor.getText()
                        .substring(nnnxxxOffset);
                textEditor.setText(replaceText);
            }

            // Update the "now editing" title of the text editor window.
            updateNowEditingTitle();

            editing = true;
        } else if (lastSession == HeaderEditSession.CLOSE_ON_EXIT) {
            editing = !cancelEditor(false);
        }

        if (lastSession == HeaderEditSession.CLOSE_ON_EXIT) {
            if (editing) {
                StdTextProduct product = TextDisplayModel.getInstance()
                        .getStdTextProduct(token);
                if (product == null) {
                    return;
                }
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
        awipsBrowserBtn.setEnabled(!inEditMode);
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
        AwipsBrowserItem.setEnabled(!inEditMode);
        AFOSBrowserItem.setEnabled(!inEditMode);
        enterEditorItem.setEnabled(!inEditMode);
        importFromFileItem.setEnabled(!inEditMode);
        recoverEditSessionItem.setEnabled(!inEditMode);

        // ---------------------------------
        // File Menu menu items
        // Enabled when in editor mode
        // ---------------------------------
        saveItem.setEnabled(inEditMode);
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
            userInformation("Cut Error", "You must first select text to\n"
                    + "cut (by dragging, for example).");
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
            userInformation("Copy Error", "You must first select text to\n"
                    + "copy (by dragging, for example).");
            return;
        }

        String selection = textEditor.getSelectionText();
        TextTransfer textTransfer = TextTransfer.getInstance();
        clipboard.setContents(new Object[] { selection },
                new Transfer[] { textTransfer });
    }

    private String unWrapLines(String s) {
        return SOFT_NEWLINE.matcher(s).replaceAll(" ");
    }

    /**
     * Print all text from the text editor to the default printer.
     */
    private void printAllText() {
        try {
            PrintDisplay.print(textEditor.getText());
        } catch (VizException e) {
            statusHandler.error(e.getLocalizedMessage(), e);
        }
    }

    /**
     * Print selected text from the text editor to the default printer.
     */
    private void printSelectedText() {
        if (textEditor.getSelectionCount() == 0) {
            userInformation("Print Selection Error",
                    "You must first select text to\n"
                            + "print (by dragging, for example).");
            return;
        }

        int response = TextWSMessageBox.open(shell, "",
                "Do you want to print the current selection?",
                SWT.ICON_QUESTION | SWT.YES | SWT.NO);
        if (response == SWT.YES) {
            String tmpText = textEditor.getText();
            Point point = textEditor.getSelection();
            try {
                PrintDisplay.print(textEditor.getSelectionText());
            } catch (VizException e) {
                statusHandler.error(e.getLocalizedMessage(), e);
            }
            textEditor.setText(tmpText);
            textEditor.setSelection(point);
        }
    }

    /**
     * Paste text from the clip board into the text editor.
     */
    private void pasteText() {
        // AWIPS I just does the pasted in both overwrite and insert mode.
        try {
            int start = -1;
            if (textEditor.getSelectionCount() == 0) {
                start = textEditor.getCaretOffset();
            } else {
                start = textEditor.getSelectionRange().x;
            }
            textEditor.paste();
            rewrap(start, textEditor.getCaretOffset());
        } catch (@SuppressWarnings("squid:S1166")
        IllegalArgumentException ex) {
            /* Ignore */
        }
    }

    /**
     * Fill selected text, that is, remove line separators from selected text.
     */
    private void fillText() {
        Point selection = textEditor.getSelection();
        String text = textEditor.getText();

        // adjust selection to eliminate leading and trailing new lines
        while (selection.x < selection.y && text.charAt(selection.x) == '\n') {
            selection.x++;
        }
        while (selection.x < selection.y
                && text.charAt(selection.y - 1) == '\n') {
            selection.y--;
        }

        textEditor.setSelection(selection);

        int selectionLength = textEditor.getSelectionCount();
        if (selectionLength > 0) {
            String replacementText = unWrapLines(textEditor.getSelectionText());
            textEditor.replaceTextRange(selection.x, selectionLength,
                    replacementText);
        }
        if (this.standardWrapRegex == null) {
            recompileRegex();
        }
        endWrapLine = textEditor.getLineAtOffset(selection.y);
        rewrapInternal(textEditor.getLineAtOffset(selection.x));
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
            try (BufferedReader in = new BufferedReader(new FileReader(fn))) {
                String s;
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
            int endIndex = statusBarLabel.getText().lastIndexOf(File.separator)
                    + 1;
            String filterPath = statusBarLabel.getText().substring(startIndex,
                    endIndex);
            dlg.setFilterPath(filterPath);
            dlg.setFileName(statusBarLabel.getText().substring(startIndex));
        }
        String fn = dlg.open();
        if (fn != null) {
            try {
                File file = new File(fn);
                if (file.exists() && file.length() <= 50_000
                        && isTextFile(file)) {
                    try (FileInputStream in = new FileInputStream(file)) {
                        byte[] bytes = new byte[(int) file.length()];
                        int offset = 0;
                        int numRead = 0;
                        while (offset < bytes.length
                                && (numRead = in.read(bytes, offset,
                                        bytes.length - offset)) >= 0) {
                            offset += numRead;
                        }
                        attachedFile = bytes;
                        attachedFilename = fn
                                .substring(fn.lastIndexOf(File.separator) + 1);
                        statusBarLabel.setText("Attachment: " + fn);
                    }
                } else {
                    StringBuilder sb = new StringBuilder();
                    if (!file.exists()) {
                        sb.append("File does NOT exist!\n\n");
                    } else if (file.length() > 50_000) {
                        sb.append("File size is too large!\n\n");
                    } else {
                        sb.append("File is NOT a text file!\n\n");
                    }
                    userInformation(sb.toString() + fn);
                }
            } catch (Exception e) {
                statusHandler.handle(Priority.PROBLEM, "Error attaching file",
                        e);
            }
        } else if (statusBarLabel.getText().startsWith("Attachment: ")) {
            statusBarLabel.setText("");
            attachedFile = null;
            attachedFilename = null;
        }
    }

    /**
     * Export data to a file.
     */
    private void exportFile() {
        FileDialog dlg = new FileDialog(shell, SWT.SAVE);
        dlg.setText("Export To File");
        dlg.setFilterNames(FILTER_NAMES);
        dlg.setFilterExtensions(FILTER_EXTS);
        String fn = dlg.open();
        if (fn != null) {
            try (BufferedWriter out = new BufferedWriter(new FileWriter(fn))) {
                StringBuilder s = new StringBuilder();
                if (inEditMode) {
                    s.append(headerTF.getText());
                    s.append("\n\n");
                }
                s.append(textEditor.getText());
                int eolIndex = s.indexOf("\n");
                int ddhhmmIndex = s.indexOf("DDHHMM");
                if (ddhhmmIndex > 0 && ddhhmmIndex < eolIndex) {
                    s.replace(ddhhmmIndex, ddhhmmIndex + 6, "000000");
                }
                out.append(s);
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

    private void displayAwipsBrowser() {
        if (awipsBrowser == null || awipsBrowser.isDisposed()) {
            awipsBrowser = new AwipsBrowserDlg(shell, shell.getText(), this,
                    token);
            awipsBrowser.open();
        } else {
            awipsBrowser.bringToTop();
        }
    }

    /**
     * Determine if dialog needs to be open or just bring to top.
     */
    public void showDialog() {
        if (disposeOnExit) {
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
        if (!disposeOnExit) {
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

    /**
     * Bring up an error dialog to display a problem to the user.
     *
     * @param title
     * @param information
     */
    private void userInformation(String title, String information) {
        logInfo("message: " + information);
        TextWSMessageBox.open(shell, title, information);
    }

    /**
     * Display a notice to the user.
     *
     * @param information
     */
    private void userInformation(String information) {
        userInformation("Notice", information);
    }

    private EtnRules getETNRules() throws Exception {
        LocalizationFile lf = PathManagerFactory.getPathManager()
                .getStaticLocalizationFile(ETN_RULES_FILE);
        if (lf == null) {
            throw new Exception(
                    "ETN rules file (" + ETN_RULES_FILE + ") not found.");
        }
        try (InputStream stream = lf.openInputStream()) {
            return JAXB.unmarshal(stream, EtnRules.class);
        }
    }

    private boolean shouldSetETNtoNextValue(StdTextProduct prod) {
        List<String> excludedPhenSigs = null;
        try {
            excludedPhenSigs = getETNRules().getExcludePhenSigs();
        } catch (Exception e) {
            statusHandler.handle(Priority.WARN,
                    "Error loading ETN assignment rules.  Will use default rules.",
                    e);
            excludedPhenSigs = defaultNoETNIncrementPhenSigs;
        }
        boolean result = true;
        VtecObject vo = VtecUtil.parseMessage(prod.getProduct());
        if (vo != null && excludedPhenSigs != null
                && excludedPhenSigs.contains(vo.getPhensig())) {
            result = false;
        }
        return result;
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

        if (AFOSParser.isTemplate) {
            return removePreformat(body.toString());
        }

        return body.toString();
    }

    /**
     * Callback method called when the user requests a save of the currently
     * editing product. If the save is successful, the restore text following a
     * cancel is updated to the saved text.
     */
    private synchronized void saveProduct() {
        StdTextProduct product = TextDisplayModel.getInstance()
                .getStdTextProduct(token);
        logInfo("Save button clicked");
        if (isProductForbiddenToEdit(product)) {
            inEditMode = false;
            return;
        }
        String savedProduct = saveEditedProduct(product, false, false, false);
        if (!savedProduct.isEmpty()) {
            logInfo("Saved product:\n" + product.getProduct());
            // reset the editor status flags
            saved = true;
            replaceWorkProductId();
            originalText = savedProduct;
        }
    }

    /**
     * Saves the edited product.
     *
     * @param product
     *            StdTextProduct to be saved
     * @param isAutoSave
     *            true if auto save operation
     * @param resend
     *            true if product is to be resent
     * @param isOperationalSend
     *            true if operational send
     *
     * @return The text of the saved product if successful. Empty string if not.
     */
    private synchronized String saveEditedProduct(StdTextProduct product,
            boolean isAutoSave, boolean resend, boolean isOperationalSend) {
        if (isProductForbiddenToEdit(product, true)) {
            inEditMode = false;
            return StringUtils.EMPTY;
        }
        boolean successful = false;

        /*
         * DR14613 - string currectDate is derived from Date now ensuring the
         * same time in WMO heading and in the MND heading.
         */
        Date now = TimeUtil.newDate();
        String currentDate = getCurrentDate(now);
        TextDisplayModel tdmInst = TextDisplayModel.getInstance();

        // Convert the text in the text editor to uppercase
        if (!isAutoSave) {
            if (!verifyRequiredFields()) {
                return StringUtils.EMPTY;
            }
            replaceWorkProductId();

            String header = headerTF.getText().toUpperCase();
            String body = MixedCaseProductSupport.conditionalToUpper(
                    product.getNnnid(), textEditor.getText());
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
        StdTextProduct storedProduct = tdmInst.getStdTextProduct(token, true);
        String productText = combineOriginalMessage();

        if (!isAutoSave) {
            if (!resend) {
                // If not a resend, set the DDHHMM field to the current time
                if (productText.startsWith("- -")
                        && productText.contains("DDHHMM")) {
                    productText = getUnofficeProduct(currentDate);
                } else {
                    productText = replaceDDHHMM(productText, currentDate);
                }
                // Sync VTEC and MND header times with the header time for
                // certain products
                if (updateHeaderTimesPils.contains(storedProduct.getNnnid())) {
                    productText = updateHeaderTimes(productText, now);
                    VtecObject vtecObj = VtecUtil.parseMessage(productText);
                    if (vtecObj != null) {
                        productText = updateVtecTimes(productText, vtecObj,
                                now);
                    }
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
                productText = productText.replace(
                        productText.substring(start, start + 6),
                        storedProduct.getProdId().getNnnid()
                                + storedProduct.getProdId().getXxxid());
            }
        }

        if (statusBarLabel.getText().startsWith("Attachment:")) {
            int startIndex = statusBarLabel.getText().indexOf(":") + 2;
            productText += ATTACHMENT_STR
                    + statusBarLabel.getText().substring(startIndex);
        }

        if (AFOSParser.isTemplate) {
            productText = removePreformat(productText);
        }

        storedProduct.setProduct(productText.trim());

        if (isAutoSave) {
            autoSave.saveProduct(storedProduct);
        } else if (isOperationalSend || resend) {
            // OUPRequest will update the StdTextProduct table.
            successful = true;
        } else {
            if (!saveStoredTextProduct(storedProduct)) {
                return StringUtils.EMPTY;
            }

            // Update the TextProductInfo table within the Text Database when a
            // successful database update has occurred to the StdTextProduct
            // table of the Text Database unless the update happened as a result
            // of an auto-save operation. The TextProductInfo table is used by
            // the AFOS Browser to generate the list of products that are
            // retrievable.
            successful = saveTextProductInfo();
        }

        return successful ? productText.trim() : StringUtils.EMPTY;
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
            if (headerParts.length >= 3) {
                headerParts[2] = ddhhmm;
            }
            // TODO: else raise error?
            StringBuilder sb = new StringBuilder(productText.length());
            boolean first = true;
            for (String s : headerParts) {
                if (first) {
                    first = false;
                } else {
                    sb.append(' ');
                }
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
        List<StyleRange> locks = new ArrayList<>();
        for (StyleRange range : textEditor.getStyleRanges()) {
            StyleRange lock = (StyleRange) range.clone();
            locks.add(lock);
        }

        // Temporarily disable verify listener to stop lock text checking
        boolean wasVerifying = verifyUndeditableText;
        try {
            verifyUndeditableText = false;
            textEditor.setText(body);
        } finally {
            verifyUndeditableText = wasVerifying;
        }

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
        if (isProductForbiddenToEdit(product)) {
            inEditMode = false;
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
        boolean operationalMode = CAVEMode.OPERATIONAL.equals(mode)
                || CAVEMode.TEST.equals(mode) ? true : false;
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
        if (isProductForbiddenToEdit(product)) {
            inEditMode = false;
            return false;
        }
        try {
            ThriftClient.sendRequest(
                    createStdTextProductServerRequest(storedProduct));
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
        boolean result = CAVEMode.OPERATIONAL.equals(mode)
                || CAVEMode.TEST.equals(mode) ? true : false;
        request.setOperationalFlag(result);

        return request;
    }

    /**
     * Method to mark uneditable warning text. Only applies to VTEC warning
     * (vtecAfosProductEnum) type products.
     */
    private boolean markUneditableText(StyledText st) {
        // Capture the input from the styled text widget.
        StringBuilder sb = new StringBuilder(st.getText());
        String errMsg = null;
        int currentIndex = 0;
        int startIndex = 0;
        int endIndex = 0;
        boolean markedTextUndeditable = false;
        try {
            while (sb.indexOf(BEGIN_ELEMENT_TAG, 0) >= 0) {
                currentIndex = 0;
                startIndex = 0;
                endIndex = 0;

                // Looks for the most inner <L></L> tags
                do {
                    startIndex = sb.indexOf(BEGIN_ELEMENT_TAG, currentIndex);
                    endIndex = sb.indexOf(END_ELEMENT_TAG, currentIndex);
                } while (startIndex > 0 && endIndex > 0
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
                st.setStyleRange(new StyleRange(startIndex,
                        endIndex - startIndex - BEGIN_ELEMENT_TAG_LEN,
                        getDisplay().getSystemColor(SWT.COLOR_BLUE), null,
                        SWT.BOLD));
                markedTextUndeditable = true;
            }
        } catch (@SuppressWarnings("squid:S1166")
                IllegalArgumentException | StringIndexOutOfBoundsException ex) {
            errMsg = ex.toString();
        }
        if (errMsg != null) {
            Status status = new Status(IStatus.ERROR,
                    "com.raytheon.viz.texteditor",
                    String.format(
                            "Bad Lock Tag pairing:\ncurrentIndex %d\nstartIndex %d\nendIndex %d",
                            currentIndex, startIndex, endIndex));
            ErrorDialog.openError(shell, "Product Problems",
                    "Problem parsing product to determine uneditable text\nPlease capture this dialog informaion and the contents of the Wargen screen prior to any changes.",
                    status);
        }
        return markedTextUndeditable;
    }

    @Override
    public void verifyText(VerifyEvent event) {
        // Enforces uneditability of lockable text in warning products
        if (!verifyUndeditableText) {
            return;
        }
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
                        if (event.start > rangeStart
                                && event.start < rangeEnd) {
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
            statusHandler.handle(Priority.PROBLEM, "Problem verifying text. ",
                    e);
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
            ICommand command = CommandFactory
                    .getWmoCommand(wmoTtaaiiTF.getText(), ccccTF.getText());
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
        // first three characters is AFOS NNN
        int afosNnnLimit = 2;
        // second three characters is AFOS XXX
        int afosXxxLimit = 5;
        String prodText = textEditor.getText();

        if (!prodText.startsWith("ZCZC")) {
            /*
             * DR15610 - Make sure that if the first line of the text product is
             * not a WMO heading it is treated as part of the text body.
             */
            String[] pieces = prodText.split("\n", 2);
            if (pieces.length > 1) {
                // WMOHeader expects this
                pieces[0] += "\n";
            }

            if (!HEADER_PATTERN.matcher(pieces[0]).matches()) {
                headerTF.setText("");
                try {
                    textEditor.setText(originalText);
                    textEditor.setEditable(true);
                    textEditor.setEditable(false);
                } catch (@SuppressWarnings("squid:S1166")
                IllegalArgumentException e) {
                    /*
                     * There is no text product body, so set it to the empty
                     * string.
                     */
                    textEditor.setText("");
                }
            } else {
                // TODO FIX PARSING

                // First, set the current header by assuming that it usually
                // consists of the first two lines of text in the text product,
                // though there will be exceptions to that "rule" as handled
                // below.
                // So, obtain the AFOS NNNxxx. If it's where it is supposed to
                // be
                // in the new format, then the existing header is already an
                // AWIPS
                // text product identifier. Otherwise it is a legacy AFOS
                // identifier.
                if (TextDisplayModel.getInstance().hasStdTextProduct(token)) {
                    StdTextProduct textProd = TextDisplayModel.getInstance()
                            .getStdTextProduct(token);
                    StdTextProductId prodId = textProd.getProdId();
                    try {
                        // start of second line of text
                        start = textEditor.getOffsetAtLine(thisLine + 1);
                        if (textEditor.getText(start, start + afosNnnLimit)
                                .equals(prodId.getNnnid())
                                && textEditor
                                        .getText(start + afosNnnLimit + 1,
                                                start + afosXxxLimit)
                                        .equals(prodId.getXxxid())) {
                            // Text matches the products nnnid and xxxid
                            numberOfLinesOfHeaderText = 2;
                        } else if (AFOSParser.DRAFT_PIL.equals(textEditor
                                .getText(start, start + afosNnnLimit + 2))
                                || "TTAA0".equals(textEditor.getText(start,
                                        start + afosNnnLimit + 2))) {
                            // Text matches temporary WRKWG#
                            numberOfLinesOfHeaderText = 2;
                        } else {
                            // Assume this header block is a legacy AFOS
                            // identifier.
                            numberOfLinesOfHeaderText = 1;
                        }
                    } catch (@SuppressWarnings("squid:S1166")
                    IllegalArgumentException e) {
                        /*
                         * Assume this header block is a legacy AFOS identifier.
                         */
                        numberOfLinesOfHeaderText = 1;
                    }
                }

                try {
                    start = 0;
                    finish = textEditor.getOffsetAtLine(
                            thisLine + numberOfLinesOfHeaderText) - 1;
                } catch (@SuppressWarnings("squid:S1166")
                IllegalArgumentException e) {
                    /*
                     * The text does not span enough lines so use the full
                     * extent of the product.
                     */
                    finish = textEditor.getCharCount() - 1;
                }

                // Set the content of the header block to consist of just the
                // header
                // of
                // the text product... it will get reunited with the body when
                // it is
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
                        line = textEditor
                                .getLine(thisLine + numberOfLinesOfHeaderText
                                        + numberOfBlankLines);
                    } while (line.isEmpty());
                    // Note: 'st' is a reference to 'textEditor'...
                    // delelete the header from the text in 'textEditor'
                    finish = textEditor.getOffsetAtLine(thisLine
                            + numberOfLinesOfHeaderText + numberOfBlankLines);
                    textEditor.setSelection(start, finish);
                    textEditor.setEditable(true);
                    textEditor.invokeAction(SWT.DEL);
                    textEditor.setEditable(false);
                } catch (@SuppressWarnings("squid:S1166")
                IllegalArgumentException e) {
                    /*
                     * There is no text product body, so set it to the empty
                     * string.
                     */
                    textEditor.setText("");
                }
            }
        } else {
            /**
             * If the first word begins with "ZCZC", it is a two-line header at
             * least, it is "ZCZC CCNNNXXX adr\nTTAA00 KCCC DDHHMM bbb\n"
             */
            Matcher headerMatcher = HEADER_SEPARATOR_PATTERN.matcher(prodText);
            if (headerMatcher.find()) {
                int newLineIndex = headerMatcher.start();
                String header = prodText.substring(0, newLineIndex);

                if (header.length() > 10) {
                    String rest = prodText.substring(newLineIndex + 1);

                    headerTF.setText(header);
                    String cccnnnxxx = header.substring(5, 14);
                    setCurrentSiteId("");
                    setCurrentWmoId("");
                    setCurrentWsfoId(cccnnnxxx.substring(0, 3));
                    setCurrentProdCategory(cccnnnxxx.substring(3, 6));
                    setCurrentProdDesignator(cccnnnxxx.substring(6, 9));

                    try {
                        textEditor.setText(rest.trim());
                        textEditor.setEditable(true);
                        textEditor.setEditable(false);
                    } catch (@SuppressWarnings("squid:S1166")
                    IllegalArgumentException e) {
                        /*
                         * There is no text product body, so set it to the empty
                         * string.
                         */
                        textEditor.setText("");
                    }
                }
            }
        }
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
        headerTF.setText(
                wmoId + " " + siteId + " " + dateId + separator + nnnxxx);

        // disable word wrap if non-wrap pil
        if (nnnxxx != null && nnnxxx.length() >= 3) {
            String pil = nnnxxx.substring(0, 3);
            if (nonWrapPils.contains(pil)) {
                disableAutoWrap();
            } else {
                configureAutoWrap(prevWrapCfg);
            }
        }

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
        return formatter.format(now);
    }

    /**
     * Update the VTEC time using the Date now.
     *
     * @param product
     * @param vtecObj
     * @param now
     * @return
     */
    private String updateVtecTimes(String product, VtecObject vtecObj,
            Date now) {

        if (vtecObj == null || "COR".equals(vtecObj.getAction())) {
            return product;
        }
        // Update the vtec start time
        if ("NEW".equals(vtecObj.getAction())) {
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
            TimeZone tz = TextWarningConstants.timeZoneShortNameMap
                    .get(m.group(5));
            if (tz != null) {
                headerFormat.setTimeZone(tz);
                product = product.replace(m.group(1), headerFormat.format(now));
            } else {
                statusHandler.warn(
                        "Could not sync MND header time because the time zone could not be determined.  Will proceed with save/send.");
            }
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
    @Override
    public void setAfosCmdField(String cmd) {
        afosCmdTF.setText(cmd);
        clearAwipsIdTF();
        clearWmoTF();
        TextDisplayModel.getInstance().setAfosCommand(token, cmd);
    }

    @Override
    public void setAwipsCmdField(String cmd) {
        awipsIdTF.setText(cmd);
        clearAfosCmdTF();
        clearWmoTF();

        TextDisplayModel.getInstance().setAfosCommand(token, "AWIPS:" + cmd);
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
     * Convenience method to execute command without updating ObsUpdated.
     */
    @Override
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
        // Reset the saved
        saved = false;
        if (isDisposed()) {
            return;
        }

        if (!isObsUpdated) {
            if (browser != null) {
                browser.close();
                browser = null;
            }

            commandHistory.addCommand(command);
        }

        statusBarLabel
                .setText("Loading " + TextEditorUtil.getCommandText(command));
        statusBarLabel.update();
        setBusy(true);
        productQueryJob.addRequest(command, isObsUpdated,
                accumChkBtn.getSelection());
    }

    /**
     * Request for product(s) is finish now update the display with the
     * information.
     */
    @Override
    public void requestDone(ICommand command,
            final List<StdTextProduct> prodList, final boolean isObsUpdated) {
        boolean enterEditor = false;
        boolean hasAttachment = false;
        String attachedFilename = new String();
        boolean validExecuteCommand = command != null;

        if (validExecuteCommand) {
            if (prodList != null && !prodList.isEmpty()) {
                if (prodList.size() > 1) {
                    if (CommandType.WMO.equals(command.getType())) {
                        final boolean hasAtt = hasAttachment;
                        final boolean enterEd = enterEditor;
                        final boolean validExecuteCmd = validExecuteCommand;
                        final String attachedFN = attachedFilename;
                        browser = new WmoBrowserDlg(getShell(), this, prodList);
                        browser.addCloseCallback(returnValue -> {
                            postProductCheck(isObsUpdated, prodList);
                            postExecute(hasAtt, enterEd, validExecuteCmd,
                                    attachedFN);
                            browser = null;
                        });
                        browser.setBlockOnOpen(false);
                        browser.open();
                        return;
                    } else if (CommandType.AWIPS.equals(command.getType())) {
                        final boolean hasAtt = hasAttachment;
                        final boolean enterEd = enterEditor;
                        final boolean validExecuteCmd = validExecuteCommand;
                        final String attachedFN = attachedFilename;
                        browser = new AwipsProductBrowserDlg(getShell(), this,
                                prodList);
                        browser.addCloseCallback(returnValue -> {
                            postProductCheck(isObsUpdated, prodList);
                            postExecute(hasAtt, enterEd, validExecuteCmd,
                                    attachedFN);
                            browser = null;
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
                        attachedFilename = prod.getProduct()
                                .substring(endIndex + ATTACHMENT_STR.length());
                        prod.setProduct(product);
                        hasAttachment = true;
                    }

                    String commandText = command.getCommandTextFields()[0];

                    StdTextProductId stdProdId = prod.getProdId();

                    if ("MTR".equals(stdProdId.getNnnid())
                            && (commandText.startsWith("ALL:")
                                    || commandText.startsWith("A:")
                                    || commandText.endsWith("000"))) {

                        stripWMOHeaders(prod);
                    }

                    if (isObsUpdated) {
                        updateDisplayedProduct(prodList.get(0));
                    } else {
                        setDisplayedProduct(prodList.get(0));
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
                userInformation(
                        "No product in the database matches your request.");

                if (!accumChkBtn.isDisposed() && !accumChkBtn.getSelection()) {
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
        if ("WRK".equals(nnnxxx[0])) {
            String siteNode = TextDisplayModel.getSiteNode(warning, nnnxxx[1]);
            if (siteNode.isEmpty()) {
                // look up failed use current siteNode.
                siteNode = prodList.get(0).getCccid();
            }
            String ttaaii = SiteAbbreviationUtil
                    .getTtaaii(siteNode + nnnxxx[0] + nnnxxx[1]);
            final String w = warning.replace(TextWarningConstants.TTAAII,
                    ttaaii);
            TextDisplayModel.getInstance().createStdTextProduct(token, w,
                    siteNode);
        } else if (!"xxx".equals(nnnxxx[1])) {
            String siteNode = TextDisplayModel.getSiteNode(warning, nnnxxx[1]);
            String ttaaii = SiteAbbreviationUtil
                    .getTtaaii(siteNode + nnnxxx[0] + nnnxxx[1]);
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
            if (!productQueryJob.isExpectingRequests()) {
                if (hasAttachment) {
                    statusBarLabel.setText("Attachment: " + attachedFilename);
                } else {
                    statusBarLabel.setText("");
                }
                statusBarLabel.update();
                setBusy(false);
            }
            // Automatically open the editor window with returned data.
            if (enterEditor) {
                enterEditor();
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
            String siteNode = TextDisplayModel.getSiteNode(warning, nnnxxx[1]);
            String ttaaii = SiteAbbreviationUtil
                    .getTtaaii(siteNode + nnnxxx[0] + nnnxxx[1]);
            final String w = warning.replace(TextWarningConstants.TTAAII,
                    ttaaii);

            TextDisplayModel.getInstance().createStdTextProduct(token, w,
                    siteNode);

            workProductId = afosId.substring(3);
            warnGenFlag = true;
            VizApp.runAsync(() -> {
                long t0 = System.currentTimeMillis();
                // For VTEC related warning messages, turn off wordwrap by
                // default.
                if (textEditor == null) {
                    openDialog();
                }

                if (textEditor.isDisposed()) {
                    return;
                }

                // Set the text editor's contents to the warning message.
                verifyUndeditableText = false;
                textEditor.setText(w);

                showDialog();
                long t1 = System.currentTimeMillis();
                SimpleDateFormat sdf = new SimpleDateFormat(
                        "yyyy-MM-dd HH:mm:ss.SSS");
                sdf.setTimeZone(TimeZone.getTimeZone("GMT"));
                statusHandler.debug(
                        sdf.format(new Date()) + ": Text Workstation took "
                                + (t1 - t0) + "ms to show dialog");
                enterEditor1();

                // TODO: is this necessary?
                Menu menu = autoWrapMenuItem.getMenu();
                for (MenuItem item : menu.getItems()) {
                    if (item.getSelection()) {
                        currentWrapCfg = (WrapButtonCfg) item.getData();
                        configureAutoWrap(currentWrapCfg);
                    }
                }
                saved = false;
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
            String site = SiteMap.getInstance()
                    .getSite4LetterId(textProd.getCccid());
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

    /**
     * Update the buttonology of the text workstation if this text editor dialog
     * corresponds to one that belongs to the text workstation (TextWS) and not
     * the D-2D.
     *
     * @param buttonology
     */
    private void updateButtonology(String buttonology) {
        if (!"0".equals(token) && !"9".equals(token)) {
            // Update the title of this TextWS text window
            winTitle = "Text " + token + ": " + buttonology;
            shell.setText(winTitle);
            callbackClient.updateText(Integer.parseInt(token) - 1, winTitle);
        } else if ("0".equals(token)) {
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
        if (!"0".equals(token) && !"9".equals(token)) {
            // Update the title of this TextWS text window
            winTitle = "Text " + token;
            shell.setText(winTitle);
            callbackClient.restoreText(Integer.parseInt(token) - 1);
        } else if ("0".equals(token)) {
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
        } else if (winTitle.indexOf('(') != -1) {
            winTitle = winTitle.substring(0, winTitle.indexOf('('));
            shell.setText(winTitle);
        }
    }

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
        StdTextProduct product = getStdTextProduct();
        SpellCheckDlg spellCheckDlg = new SpellCheckDlg(shell, textEditor,
                MixedCaseProductSupport.isMixedCase(product.getNnnid()));
        spellCheckDlg.open();
    }

    @Override
    public void setShowScriptOutput(boolean showScriptOutput) {
        if (showScriptOutput) {
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
                scriptOutput.addCloseCallback(returnValue -> {
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
                });
                scriptOutput.open();
            } else {
                scriptOutput.bringToTop();
            }
        } else if (scriptOutput != null) {
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

    public void setAccumulation(boolean flag) {
        this.accumChkBtn.setSelection(flag);
    }

    public void showScriptStatus(String statusMsg, boolean writeToLog) {
        statusBarLabel.setText(statusMsg);
        if (writeToLog) {
            statusHandler.handle(Priority.EVENTA, statusMsg);
        }
    }

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

        scriptContinueBtn.setVisible(flag);
        setContinueEnabled(false);

        scriptCancelBtn.setVisible(flag);
        scriptsCancelItem.setEnabled(flag);
        scriptCancelBtn.setEnabled(flag);

        shell.layout();
    }

    @Override
    public ITextWsScriptController executeTextScript(Path scriptPath) {
        try {
            IScriptRunnerObserver observer = new TextEditorScriptRunnerObserver(
                    this);
            runningScript = TextWsScriptThreadManager.getInstance()
                    .runScript(scriptPath, observer, token);
            return runningScript;
        } catch (Exception e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Unable to execute script (SCRP)", e);
            return null;
        }

    }

    public void scriptStarted() {
        setScriptMenuControls(false);
        setScriptControls(true);
        if (scriptEditor != null) {
            scriptEditor.scriptStarted();
        }
    }

    private String adjustedPil(String pil) {
        if (pil.endsWith("000")) {
            return pil.substring(0, pil.length() - 3);
        } else {
            return pil;
        }
    }

    public void postProductToEditor(final String[] products,
            final String[] pils) {
        versionMenuItem.setEnabled(false);
        if (accumChkBtn.getSelection() && !textEditor.getText().isEmpty()) {
            textEditor.append("\n");
            for (String pil : pils) {
                displayedPils.add(adjustedPil(pil));
            }
            makeObsRegex();
        } else {
            textEditor.setText("");
            displayedPils.clear();
            isSaoMetarFlag = false;
            for (String pil : pils) {
                displayedPils.add(adjustedPil(pil));
            }
            makeObsRegex();
        }
        String textResult = String.join("\n", products);
        textEditor.append(textResult);
        String[] nnnxxx = TextDisplayModel.getNnnXxx(textResult);
        String siteNode = TextDisplayModel.getSiteNode(textResult, nnnxxx[1]);
        TextDisplayModel.getInstance().createStdTextProduct(token, textResult,
                siteNode);
        StdTextProduct p = getStdTextProduct();
        if (p != null) {
            String ccc = p.getCccid();
            String nnn = p.getNnnid();
            String xxx = p.getXxxid();
            if (ccc != null && ccc.trim().length() == 3 && nnn != null
                    && nnn.length() == 3 && xxx != null && xxx.length() > 0) {
                String pil = ccc + nnn + xxx;
                ICommand command = CommandFactory.getAfosCommand(pil);
                commandHistory.addCommand(command);
                commandHistory.resetIndex(command.getType());
                versionMenuItem.setEnabled(true);
            }
        }
    }

    /**
     * Provides actions needed by the 'Script -> Run...' menu item. It uses a
     * file open dialog to request a script from the user and then executes the
     * script.
     */
    private void loadAndRunScript() {
        FileDialog fd = new FileDialog(shell, SWT.OPEN);
        fd.setText("Select script to run");
        fd.setFilterExtensions(ScriptEditorDialog.SCRIPT_EXTNS);
        fd.setFilterNames(ScriptEditorDialog.SCRIPT_NAMES);
        fd.setFilterPath(Activator.getDefault().getMostRecentDir().toString());
        String result = fd.open();
        if (result != null) {
            Path scriptPath = Paths.get(result);
            Activator.getDefault().saveMostRecentDir(scriptPath);
            executeTextScript(scriptPath);
        }
    }

    /**
     * Open up the script editor with the desired values.
     */
    private void onDisplayScriptEditor() {
        scriptsEditItem.setEnabled(false);
        scriptsRunItem.setEnabled(false);
        getDisplay().asyncExec(() -> {
            if (scriptEditor == null) {
                scriptEditor = new ScriptEditorDialog(shell,
                        TextEditorDialog.this, token,
                        scriptsShowOutputItem.getSelection());
            }
            scriptEditor.open();
        });
    }

    public void scriptComplete() {
        runningScript = null;
        setScriptControls(false);
        if (scriptEditor != null) {
            scriptEditor.scriptComplete();
        } else {
            setScriptMenuControls(true);
        }
        statusBarLabel.setText("");
    }

    @Override
    public void scriptEditorClosed() {
        if (runningScript == null) {
            setScriptMenuControls(true);
        }
        scriptEditor = null;
    }

    /**
     * Enables/disables the Script=>Edit and script=>Run menu items.
     *
     * @param enabled
     *            true to enable, false to disable
     */
    private void setScriptMenuControls(boolean enabled) {
        scriptsEditItem.setEnabled(enabled);
        scriptsRunItem.setEnabled(enabled);
    }

    public void setContinueEnabled(boolean enabled) {
        scriptsContinueItem.setEnabled(enabled);
        scriptContinueBtn.setEnabled(enabled);
        if (scriptEditor != null) {
            scriptEditor.setContinueEnabled(enabled);
        }
    }

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
        String textProduct = product.getPrintableProduct();
        if ((product.getNnnid() + product.getXxxid())
                .startsWith(AFOSParser.DRAFT_PIL)
                || (product.getNnnid() + product.getXxxid())
                        .startsWith(AFOSParser.MCP_NNN)) {
            String[] nnnxxx = TextDisplayModel.getNnnXxx(textProduct);
            String operationalPil = nnnxxx[0] + nnnxxx[1];
            String siteNode = TextDisplayModel.getSiteNode(textProduct,
                    nnnxxx[1]);
            String ttaaii = SiteAbbreviationUtil
                    .getTtaaii(siteNode + operationalPil);
            textProduct = textProduct
                    .replace(TextWarningConstants.TTAAII, ttaaii)
                    .replace(operationalPil,
                            product.getNnnid() + product.getXxxid());
            originalText = textProduct;
        }

        textEditor.append(textProduct);

        // if product is a WarnGen product and is not enabled for mixed case
        // transmission, replace all commas with ellipses
        if (TextEditorCfg.getTextEditorCfg().getReplaceCommasWithEllipses()
                && warngenPils.contains(product.getNnnid())
                && !MixedCaseProductSupport.isMixedCase(product.getNnnid())) {
            textEditor
                    .setText(textEditor.getText().replaceAll(", {0,1}", "..."));
        }

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
            String site = product.getSite();
            String geographicRegionCode = "";
            if (site.length() > 0) {
                geographicRegionCode = site.substring(0, 1);
            } else {
                statusHandler.handle(Priority.INFO, "Cannot update METAR "
                        + product.getNnnid() + product.getXxxid());
                return;
            }
            regex = "(METAR |SPECI )" + geographicRegionCode
                    + product.getXxxid();
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
                        Matcher m = METAR_PATTERN
                                .matcher(textEditor.getLine(i - 1));
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
                textEditor.replaceTextRange(start, end - start,
                        productText + "\n");
            } else {
                textEditor.replaceTextRange(start, end - start, productText);
            }
        }

        // if updating, we need to highlight the site name. If the first word of
        // the line is "METAR" or "SPECI", we need to highlight the second word
        if (haveWMOHeader) {
            // need to skip the WMO header
            lineIndex++;
        }
        String line = textEditor.getLine(lineIndex);
        int startIndex = textEditor.getOffsetAtLine(lineIndex);
        if (line.startsWith("METAR") || line.startsWith("SPECI")) {
            // skip first word plus a space
            startIndex += 6;
        }
        int endIndex = textEditor.getText().indexOf(" ", startIndex);
        textEditor.setStyleRange(new StyleRange(startIndex,
                endIndex - startIndex, getDisplay().getSystemColor(UPDATE_FG),
                getDisplay().getSystemColor(UPDATE_BG), SWT.NORMAL));
        textEditor.setSelection(startIndex, endIndex);

        // Update text display model with the product that was
        // retrieved for display in this text editor dialog
        // instance.
        TextDisplayModel.getInstance().setStdTextProduct(token, product);
    }

    @Override
    public void setCCCCField(String cccc) {
        ccccTF.setText(cccc);
    }

    @Override
    public void setTTAAiiField(String ttaaii) {
        wmoTtaaiiTF.setText(ttaaii);
    }

    @Override
    public void setCommandText(String commandText) {
        updateButtonology(commandText);
    }

    private boolean validateCharacterSet(String nnn) {
        boolean rval = true;

        Pattern UPPER_PATTERN = Pattern.compile("[^" + Pattern.quote(
                TextEditorCfg.getTextEditorCfg().getUpperCaseValidCharcters())
                + "]");

        Pattern MIXED_PATTERN = Pattern.compile("[^" + Pattern.quote(
                TextEditorCfg.getTextEditorCfg().getMixedCaseValidCharacters())
                + "]");

        String body = textEditor.getText();
        Pattern pattern;
        if (MixedCaseProductSupport.isMixedCase(nnn)) {
            pattern = MIXED_PATTERN;
        } else {
            body = body.toUpperCase();
            pattern = UPPER_PATTERN;
        }

        String[] separatedLines = body.split("\n");
        int lineNum = 0;
        for (String line : separatedLines) {
            lineNum++;
            Matcher matcher = pattern.matcher(line);
            if (matcher.find()) {
                rval = false;

                String errorMsg = "Illegal character '" + matcher.group()
                        + "' on line " + lineNum + ", column "
                        + (matcher.start() + 1);
                userInformation(errorMsg);
                if (!textEditor.isDisposed()) {
                    int offset = body.indexOf(matcher.group());
                    textEditor.setSelection(offset, offset + 1);
                    textEditor.redraw();
                    textEditor.setFocus();
                }
            }
        }
        return rval;
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
                userInformation(
                        "You must modify the selected region before sending or saving the product.");
                if (!textEditor.isDisposed()) {
                    textEditor.setSelection(startIndex, endIndex + 3);
                    textEditor.setFocus();
                }
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
        } else if (isProductForbiddenToEdit(stdTextProduct,
                "Resend Warning Product Error", false)) {
            resend = false;
        }

        return resend;
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

        private String filename = null;

        public AutoSaveTask(String ttaaii, String cccc) {
            this.filenameIdentifier = ttaaii + "_" + cccc;
            setupTimer();
        }

        public AutoSaveTask(String filename) {
            if (filename != null) {
                this.filename = filename;
            }

            StdTextProduct prod = retrieveProduct();
            this.filenameIdentifier = prod.getWmoid() + "_" + prod.getSite();

            setupTimer();
        }

        private LocalizationFile getFile(String filename) {
            LocalizationFile lFile = pathManager.getLocalizationFile(lc,
                    SAVED_SESSION_DIR + filename);
            return lFile;
        }

        public void saveProduct(StdTextProduct stdTextProduct) {
            try {
                // delete and write new file, rename didn't always work
                // rename would end up writing a new file every time and
                // kept the original in sync
                LocalizationFile file = null;
                if (this.filename != null) {
                    file = getFile(this.filename);
                }

                if (file != null && file.exists()) {
                    file.delete();
                }

                this.filename = "window_" + token + "_" + filenameIdentifier
                        + "_" + AUTOSAVE_DATE_FORMAT.format(
                                TimeUtil.newGmtCalendar().getTime())
                        + ".txt";
                file = getFile(this.filename);

                if (file == null) {
                    statusHandler.warn(
                            "Auto save failed.  See server for details...");
                } else {
                    synchronized (this) {
                        try (SaveableOutputStream out = file.openOutputStream();
                                BufferedOutputStream bufStream = new BufferedOutputStream(
                                        out)) {
                            getJaxbManager().marshalToStream(stdTextProduct,
                                    out);
                            out.save();
                        }
                    }
                }

            } catch (Exception e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Auto save failed to " + filename, e);
            }
        }

        public StdTextProduct retrieveProduct() {
            StdTextProduct rval = null;

            if (this.filename != null) {
                LocalizationFile file = getFile(this.filename);
                synchronized (this) {
                    try (InputStream in = file.openInputStream();
                            BufferedInputStream bufStream = new BufferedInputStream(
                                    in)) {

                        rval = (StdTextProduct) getJaxbManager()
                                .unmarshalFromInputStream(bufStream);
                    } catch (Exception e) {
                        statusHandler.handle(Priority.PROBLEM,
                                "Retrieval of product failed:" + file.getPath(),
                                e);
                    }
                }
            }

            return rval;
        }

        public boolean stop() {
            boolean success = false;
            try {
                if (filename != null) {
                    getFile(filename).delete();
                    filename = null;
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

        private void setupTimer() {
            if (timer != null) {
                timer.cancel();
            }

            timer = new Timer();

            TimerTask saveEditSessionTask = new TimerTask() {
                @Override
                public void run() {
                    getDisplay().syncExec(() -> {
                        if (!shell.isDisposed()) {
                            if (autoSave == AutoSaveTask.this) {
                                StdTextProduct product = TextDisplayModel
                                        .getInstance().getStdTextProduct(token);
                                saveEditedProduct(product, true, false, false);
                            }
                        }
                    });
                }
            };

            timer.schedule(saveEditSessionTask, 600, 60_000);
        }
    }

    /**
     * This is the callback method called by the NotificationManagerJob when
     * observations are received. It parses the notifications and triggers a
     * load of a product that matches the currently loaded product. Short
     * circuits are provided for cases when the dialog is either not displaying
     * a product or is in edit mode.
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
                    msgPIL = ((String) payload).split("_")[0];
                } else {
                    statusHandler.handle(Priority.EVENTA,
                            "received invalid message, class is "
                                    + payload.getClass().getSimpleName());
                    msgPIL = "";
                }
                if (isObsDisplayed(msgPIL)) {
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
        private ICommand command = null;

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

    public void scriptError() {
        scriptHasError = true;
    }

    public void addStdErrMsg(String errMsg) {
        if (scriptErrorBfr == null) {
            scriptErrorBfr = new StringBuilder();
        }
        scriptErrorBfr.append(errMsg);

    }

    public boolean isScriptError() {
        return scriptHasError;
    }

    public void writeErrMsg(String errMsg) {
        StringBuilder msg = new StringBuilder(errMsg);
        if (scriptHasError) {
            // manage display of script error
            if (scriptErrorBfr != null) {
                msg.append("\n");
                msg.append(scriptErrorBfr);
            }
            statusHandler.warn(msg.toString());
        }
    }

    public void clearErrBuffer() {
        scriptHasError = false;
        scriptErrorBfr = null;
    }

    @Override
    protected void preOpened() {
        // Shell shell = ted.getShell();
        if (textWorkstationFlag) {

            // get bounds of monitor containing parent shell
            Rectangle rect = getParent().getMonitor().getClientArea();
            int x = rect.x + rect.width / 4;
            int y = rect.y;

            int index = getText().indexOf(" ");
            int editorIndex = Integer.parseInt(getText().substring(index + 1));

            Rectangle bounds = getShell().getBounds();
            Rectangle clientArea = getShell().getClientArea();

            /*
             * NOTE: this offset includes the height of the title bar and the
             * menu. There appears to be no way to get just the title bar in
             * Eclipse 3.8 We may be able to do this in Eclipse 4
             */
            int xOffset = (editorIndex - 1) * (bounds.width - clientArea.width);
            int yOffset = (editorIndex - 1)
                    * (bounds.height - clientArea.height);
            getShell().setLocation(x + xOffset, y + yOffset);
        }

        inEditMode = false;

        getParent().addDisposeListener(e -> stopAutoSave());
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
    private static class ThriftClientRunnable implements Runnable {

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

    @Override
    protected void disposed() {
        textEditor.setFont(shell.getFont());
        headerTF.setFont(shell.getFont());

        cancelScript();

        if (dftFont != null) {
            dftFont.dispose();
        }

        if (clipboard != null) {
            clipboard.dispose();
        }

        if (browser != null) {
            browser.close();
            browser = null;
        }

        if (textForegroundClr != null) {
            textForegroundClr.dispose();
        }

        if (textBackgroundClr != null) {
            textBackgroundClr.dispose();
        }

        if (highlightForegroundClr != null) {
            highlightForegroundClr.dispose();
        }

        if (highlightBackgroundClr != null) {
            highlightBackgroundClr.dispose();
        }

        SimulatedTime.getSystemTime().removeSimulatedTimeChangeListener(this);

        inEditMode = false;

        ICommandService service = PlatformUI.getWorkbench()
                .getService(ICommandService.class);
        if (service != null && pasteCommandListener != null) {
            service.getCommand("org.eclipse.ui.edit.paste")
                    .removeExecutionListener(pasteCommandListener);
        }

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
        if (!currentWrapCfg.isWrapEnabled()) {
            return;
        }
        if (this.standardWrapRegex == null) {
            recompileRegex();
        }
        try {
            textEditor.setRedraw(false);
            int lineNumber = textEditor.getLineAtOffset(start);
            endWrapLine = textEditor.getLineAtOffset(end);
            /*
             * DR14889 - resetting isPreviousLineWrapped
             */
            isPreviousLineWrapped = false;
            rewrapInternal(lineNumber);

        } finally {
            textEditor.setRedraw(true);
        }
        // The rest of this method is adjusting the view of the display.
        int caret = textEditor.getCaretOffset();
        int lineStart = textEditor
                .getOffsetAtLine(textEditor.getLineAtOffset(caret));

        // Force display to the left to show start of the lines.
        textEditor.setCaretOffset(lineStart);
        textEditor.showSelection();
        // but make sure current caret position is visible.
        textEditor.setCaretOffset(caret);
        textEditor.showSelection();
    }

    /**
     * starts the actual work of word-wrap
     *
     * @param lineNumber
     */
    private void rewrapInternal(int lineNumber) {
        if (lineNumber < 0) {
            statusHandler.debug("rewrapInternal invalid arg: " + lineNumber);
            return;
        }

        if (wrapColumn == -1) {
            statusHandler.debug("No wrap column set.");
            return;
        }

        boolean inLocations = false;
        boolean inPathcast = false;
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
        if (paragraphStart.startsWith("* LOCATIONS")
                || paragraphStart.startsWith("* SOME LOCATIONS")
                || paragraphStart.startsWith("LOCATIONS IMPACTED")
                || paragraphStart.startsWith("SOME LOCATIONS THAT")) {
            inLocations = true;
        }

        // is this the pathcast paragragh?
        if (paragraphStart.startsWith("* THIS")
                && paragraphStart.endsWith("WILL BE NEAR...")) {
            inPathcast = true;
        }

        // get the padding for the paragraph
        String padding = getParagraphPadding(paragraphStart);

        if ((inLocations || inPathcast)
                && paragraphStartLineNumber == lineNumber) {
            // Keep LOCATIONS and PATHCAST first line short & don't paste more
            // to it.
            if (line.indexOf("...") == line.lastIndexOf("...")) {
                if (line.length() > wrapColumn) {
                    if (inLocations) {
                        wrapInLocations(lineNumber, padding);
                    } else {
                        wrapNormal(lineNumber, padding);
                    }
                }
                return;
            }
            int offset = textEditor.getOffsetAtLine(lineNumber)
                    + line.indexOf("...") + 3;
            textEditor.replaceTextRange(offset, 0, "\n");
            ++endWrapLine;

            // Check padding for the new line and set up to wrap rest of
            // paragraph.
            offset += 2;
            if (!" ".equals(textEditor.getText(offset, offset))) {
                textEditor.replaceTextRange(offset, 0, "  ");
            } else {
                ++offset;
                if (!" ".equals(textEditor.getText(offset, offset))) {
                    textEditor.replaceTextRange(offset, 0, " ");
                }
            }
            ++lineNumber;
            line = textEditor.getLine(lineNumber);
        }

        if (line.length() <= wrapColumn) {
            extendShortLine(lineNumber, padding, inLocations);
            if (textEditor.getLine(lineNumber).length() <= wrapColumn) {
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
     * @param inLocations
     */
    private void extendShortLine(int lineNumber, final String padding,
            boolean inLocations) {
        // if the line is too short move the next line up
        // if there is a next line
        String line = textEditor.getLine(lineNumber);

        // If the next line is part of the same paragraph and not empty make it
        // part of the current line.
        if (lineNumber + 1 < textEditor.getLineCount()) {
            // if the next line does not start a new paragraph
            if (!isParagraphStart(lineNumber + 1)) {
                // if the next line is not empty
                if (!textEditor.getLine(lineNumber + 1).trim().isEmpty()
                        || textEditor.getLine(lineNumber + 1)
                                .length() == padding.length() + 1) {
                    // Determine what kind of end of line marker line has.
                    int deleteLen = 0;

                    try {
                        String allText = textEditor.getText();
                        int eol = textEditor.getOffsetAtLine(lineNumber)
                                + line.length();
                        if (allText.charAt(eol) == '\n') {
                            if (allText.charAt(eol - 1) == '.'
                                    && allText.charAt(eol - 2) != '.') {
                                // do not extend this line.
                                return;
                            } else {
                                deleteLen = 1;
                            }
                        } else {
                            return;
                        }
                    } catch (@SuppressWarnings("squid:S1166")
                    Exception e) {
                        return;
                    }
                    if (containsLock(lineNumber + 1)) {
                        // if the next line contains a lock, only bring up
                        // if the line does not start with a lock
                        int lineStart = textEditor
                                .getOffsetAtLine(lineNumber + 1);
                        if (padding.length() > 0 && textEditor
                                .getLine(lineNumber + 1).startsWith(padding)) {
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
                            if (line.length() + lockLength > currentWrapCfg
                                    .getWrapCol()) {
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
                    if (padding.length() > 0 && textEditor
                            .getLine(lineNumber + 1).startsWith(padding)) {
                        deleteLen += padding.length();
                    }
                    String beforeReplace = textEditor.getText();
                    String endLine = textEditor.getText(newlinePosition - 1,
                            newlinePosition - 1);
                    String startNextLine = textEditor.getText(
                            newlinePosition + deleteLen,
                            newlinePosition + deleteLen);
                    String wordSpace = "";
                    if (noSeparatorPattern.matcher(endLine).matches()
                            && noSeparatorPattern.matcher(startNextLine)
                                    .matches()
                            && (!inLocations || !line.endsWith("..."))) {
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
                    if (textEditor.getLine(lineNumber)
                            .length() <= currentWrapCfg.getWrapCol()) {
                        extendShortLine(lineNumber, padding, inLocations);
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
                wrapAtPositionOrLock(
                        lineStartOffset + currentWrapCfg.getWrapCol(), padding);
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
            int lineStart = textEditor
                    .getOffsetAtLine(textEditor.getLineAtOffset(position));
            // do not wrap if lockStart is before the start of the line
            // lineStart should never be less than 0
            if (lockStart > lineStart) {
                String replacement = "\n" + padding;
                textEditor.replaceTextRange(lockStart, 0, replacement);
                ++endWrapLine;
            }
        } else {
            // wrap at the position
            StringBuilder replacement = new StringBuilder("\n");
            int padLen = padding.length();
            if (padLen > 0) {
                int cnt = 0;
                while (cnt < padLen && " ".equals(
                        textEditor.getText(position + cnt, position + cnt))) {
                    cnt++;
                }
                if (cnt < padLen) {
                    replacement.append(padding.substring(cnt));
                }
            }
            textEditor.replaceTextRange(position, 0, replacement.toString());

            // remove extra whitespace
            int lineNum = textEditor.getLineAtOffset(position);
            String wrappedLine = textEditor.getLine(lineNum);
            String trimmedLine = wrappedLine.replaceAll("\\p{Blank}+$", " ");
            if (!trimmedLine.equals(wrappedLine)) {
                textEditor.replaceTextRange(textEditor.getOffsetAtLine(lineNum),
                        wrappedLine.length(), trimmedLine);
            }

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
            wrapAtPositionOrLock(lineStartOffset + currentWrapCfg.getWrapCol(),
                    padding);
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
                    while (nextLine <= endWrapLine && nextLine >= 0
                            && textEditor.getLine(nextLine).trim().isEmpty()) {
                        --nextLine;
                    }
                    if (nextLine >= 0 && nextLine <= endWrapLine) {
                        isPreviousLineWrapped = true;
                        rewrapInternal(nextLine);
                    }
                }
            } else if (line - 1 >= 0 && line - 1 <= endWrapLine) {
                isPreviousLineWrapped = true;
                rewrapInternal(line - 1);
            }
        }

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
     * @param line
     * @return true if line is the start of a paragraph, or is line 0
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
        } else if (isSaoMetarFlag && !lineText.startsWith(" ")) {
            rval = true;
        } else if (isComponentNameLine(line - 1)) {
            rval = true;
        }
        return rval;
    }

    private void cursorToPreviousParagraphStart() {
        int lineNumber = textEditor
                .getLineAtOffset(textEditor.getCaretOffset());
        if (isParagraphStart(lineNumber) && textEditor
                .getOffsetAtLine(lineNumber) == textEditor.getCaretOffset()) {
            lineNumber = Math.max(lineNumber - 1, 0);
        }
        while (!isParagraphStart(lineNumber)) {
            lineNumber--;
        }
        textEditor.setCaretOffset(textEditor.getOffsetAtLine(lineNumber));
        textEditor.showSelection();
    }

    private void cursorToNextParagraphStart() {
        int lineNumber = textEditor
                .getLineAtOffset(textEditor.getCaretOffset());
        int lineCount = textEditor.getLineCount();
        if (isParagraphStart(lineNumber)) {
            lineNumber = Math.min(lineNumber + 1, lineCount - 1);
        }
        while (!isParagraphStart(lineNumber) && lineNumber < lineCount - 1) {
            lineNumber++;
        }
        if (isParagraphStart(lineNumber)) {
            // Don't go to last line in the editor unless it starts a paragraph
            textEditor.setCaretOffset(textEditor.getOffsetAtLine(lineNumber));
            textEditor.showSelection();
        }
    }

    /**
     * @param lineNumber
     * @return true when line number is a component name line
     */
    private boolean isComponentNameLine(int lineNumber) {
        boolean result = false;
        if (lineNumber > 0
                && textEditor.getLine(lineNumber - 1).trim().isEmpty()) {
            result = COMPONENT_NAME_PATTERN
                    .matcher(textEditor.getLine(lineNumber)).find();
        }
        return result;
    }

    /**
     * call recompileRegex when charWrapCol changes
     */
    private void recompileRegex() {
        this.wrapColumn = currentWrapCfg.getWrapCol();
        this.standardWrapRegex = Pattern.compile(
                "(  |..).{1," + (currentWrapCfg.getWrapCol() - 3) + "}(\\s|-)");
        this.locationsFirstRegex = Pattern.compile(
                "^(?:\\* (?:SOME )?LOCATIONS|LOCATIONS IMPACTED|SOME LOCATIONS THAT) [^\\.]{1,"
                        + (currentWrapCfg.getWrapCol() - 13) + "}\\s");
        this.locationsBodyRegex = Pattern.compile("((  |..).{1,"
                + (currentWrapCfg.getWrapCol() - 5) + "}\\.\\.\\.)|((  |..).{1,"
                + (currentWrapCfg.getWrapCol() - 3) + "}\\s)");
    }

    /**
     * Set airport tool tip to the information at location and display the
     * information.
     *
     * @param location
     */
    private void displayAirportTooltip(Point location) {
        String word = parseProduct(textEditor, location.y);
        if (word != null) {
            String result = AfosBrowserModel.getInstance().getNodeHelp(word);
            if (result != null) {
                // dispaly below and to the right of location.
                location.x += 5;
                location.y += 5;
                airportToolTip.setText(result);
                airportToolTip.show(location);
            }
        }
    }

    /**
     * Parse the METAR or SAO product based on the nearest line location of the
     * mouse cursor to locate the potential airport.
     *
     * @param st
     *            -- the styled text widget containing the selection of text
     * @param y
     *            -- The cursor's y location
     * @return result -- the start and finish positions of the selected range
     */
    private String parseProduct(StyledText st, int y) {
        String lineText = getLineOffset(st, st.getLineIndex(y));

        String result = "";
        try {
            char c = lineText.charAt(0);
            if (c == 'M' || c == 'S' || c == 'T') {
                // # Most obs start with METAR, SPECI, TESTM, or TESTS. Skip
                // over
                // that tag,
                // # a space, and the K or P, to get to the 3-char station ID.
                if (lineText.length() > 10) {
                    result = lineText.substring(7, 10);
                } else {
                    result = lineText.substring(lineText.length() - 3);
                }
            } else if (c == 'W' || c == 'Y') {
                // # Canadian SAOs have 3-character IDs, starting with W or Y.
                // Grab
                // 'em.
                result = lineText.substring(0, 3);
            } else {
                // # Some military obs don't get tagged. Skip the K or P and get
                // 3
                // chars.
                int wordLineStart = 1;
                result = lineText.substring(wordLineStart, wordLineStart + 4);
            }
        } catch (@SuppressWarnings("squid:S1166")
        StringIndexOutOfBoundsException ex) {
            /* User has non METAR/SAO products and the parsing failed. */
            result = null;
        }

        return result;
    }

    /**
     * For the give line index get the line that is the start of the product.
     *
     * @param st
     * @param lineIndex
     * @return line
     */
    private String getLineOffset(StyledText st, int lineIndex) {
        String lineText = st.getLine(lineIndex);
        int lineOffset = st.getOffsetAtLine(lineIndex);

        if (lineOffset > 0) {
            boolean goBack = true;
            while (goBack && lineIndex > 0) {
                if (lineText.startsWith(" ") || lineText.length() == 0) {
                    lineIndex--;
                } else {
                    String tempLine = st.getLine(lineIndex);
                    if (tempLine.startsWith("&&")
                            || tempLine.startsWith("$$")) {
                        lineIndex--;
                    } else {
                        goBack = false;
                    }
                }
                lineOffset = lineIndex;
                lineText = st.getLine(lineIndex);
            }
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
    private boolean isTextFile(File file) throws IOException {
        boolean result = false;

        byte[] bytesFromFile = getBytesFromFile(file);
        for (byte b : bytesFromFile) {
            if (b == 0x09 || b == 0x0A || b == 0x0C || b == 0x0D
                    || b >= 0x20 && b <= 0x7E) {
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
     * @return bytes
     * @throws IOException
     */
    private byte[] getBytesFromFile(File file) throws IOException {
        byte[] bytes = null;

        try (InputStream is = new FileInputStream(file)) {

            // Get the size of the file
            long length = file.length();

            if (length > Integer.MAX_VALUE) {
                // File is too large
            }

            // Create the byte array to hold the data
            bytes = new byte[(int) length];

            // Read in the bytes
            int offset = 0;
            int numRead = 0;
            while (offset < bytes.length && (numRead = is.read(bytes, offset,
                    bytes.length - offset)) >= 0) {
                offset += numRead;
            }

            // Ensure all the bytes have been read in
            if (offset < bytes.length) {
                throw new IOException(
                        "Could not completely read file " + file.getName());
            }
        } catch (Exception ex) {
            statusHandler.handle(Priority.PROBLEM,
                    "Error opening input stream.", ex);
        }

        return bytes;
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
     * Test to see if this product shouldn't be handled in Text Workstation.
     *
     * @param product
     *
     * @return true if product is forbidden to be handled by Text Workstation.
     */
    private boolean isProductForbiddenToEdit(StdTextProduct product) {
        return isProductForbiddenToEdit(product, "", false);
    }

    private boolean isProductForbiddenToEdit(StdTextProduct product,
            boolean b) {
        return isProductForbiddenToEdit(product, "", b);
    }

    private boolean isProductForbiddenToEdit(StdTextProduct product,
            String prefix, boolean supressUserInfo) {
        boolean retval = false;
        if (product != null) {
            String ccc = product.getCccid();
            String nnn = product.getNnnid();
            // Site SJU issues Spanish version of products. For Spanish version,
            // XXX is SPN
            // in the PIL. When XXX is SPN, don't forbid editing (DR 19971 and
            // DR 20284).
            if (!"SPN".equals(product.getXxxid().trim())) {
                if (ccc != null && nnn != null) {
                    if (gfePils.contains(nnn) && !exceptionCCCs.contains(ccc)) {
                        if (!supressUserInfo) {
                            userInformation(prefix
                                    + "This product MUST be edited in GFE!"
                                    + "\n Please exit and return to GFE."
                                    + ". \n Action Aborted!");
                        }
                        return true;
                    }
                    if (!warnGenFlag && warngenPils.contains(nnn)) {
                        // In Practice mode Warngen products should be allowed
                        // to edit/store.
                        if (CAVEMode.PRACTICE.equals(CAVEMode.getMode())) {
                            return false;
                        }
                        if (!"000000".equals(product.getHdrtime())) {
                            if (!supressUserInfo) {
                                userInformation(prefix
                                        + "This product may only be edited as a scratch product generated by WarnGen.");
                            }
                            return true;
                        }
                    }
                }
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
                    statusHandler.handle(
                            response.isOk() ? Priority.INFO : Priority.ERROR,
                            response.getStatusMessage());
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
        if ("NOR".equals(bbb)) {
            return "";
        } else {
            return bbb;
        }
    }

    private void setOverwriteMode(boolean overwrite) {
        if (overwrite != overwriteMode) {
            textEditor.invokeAction(ST.TOGGLE_OVERWRITE);
        }
        overwriteMode = overwrite;
        editorInsertCmb.select(overwrite ? OVERWRITE_TEXT : INSERT_TEXT);
        overStrikeItem.setSelection(overwrite);
    }

    private void initTemplateOverwriteMode() {
        if (AFOSParser.isTemplate) {
            editorInsertCmb.setEnabled(false);
            editorCutBtn.setEnabled(false);
            editorCopyBtn.setEnabled(false);
            editorPasteBtn.setEnabled(false);
            editorFillBtn.setEnabled(false);
            editorAttachBtn.setEnabled(false);
            overStrikeItem.setEnabled(false);
            if (!isTemplateOverwriteModeSet) {
                setOverwriteMode(true);
                isTemplateOverwriteModeSet = true;
            }

        } else {
            editorInsertCmb.setEnabled(true);
            overStrikeItem.setEnabled(true);
            editorCutBtn.setEnabled(true);
            editorCopyBtn.setEnabled(true);
            editorPasteBtn.setEnabled(true);
            editorFillBtn.setEnabled(true);
            editorAttachBtn.setEnabled(true);
            if (isTemplateOverwriteModeSet && !overwriteMode) {
                setOverwriteMode(true);
                isTemplateOverwriteModeSet = false;
            }
            if (!isTemplateOverwriteModeSet && overwriteMode) {
                setOverwriteMode(false);
            }
        }
    }

    private String removePreformat(String preformattedText) {
        String modifiedText = preformattedText.replaceAll("\\[|\\]", " ");
        return modifiedText;
    }

    private String getUnofficeProduct(String currDate) {
        StdTextProduct textProd = TextDisplayModel.getInstance()
                .getStdTextProduct(token);

        String header = headerTF.getText();

        String nnn = textProd.getNnnid();
        String xxx = textProd.getXxxid();
        String nnnXxx = nnn + xxx;
        String site = SiteMap.getInstance()
                .getSite4LetterId(textProd.getCccid());
        String wmoId = textProd.getCccid() + nnnXxx + " " + getAddressee()
                + "\nTTAA00 " + site;

        header = header.replaceFirst("\n" + nnnXxx, "");
        header = header.replaceFirst("-", "ZCZC");
        header = header.replaceFirst("-", wmoId);

        if (currDate != null) {
            header = header.replaceFirst("DDHHMM", currDate);
        } else {
            header = header.replaceFirst("DDHHMM", textProd.getHdrtime());
        }

        String body = MixedCaseProductSupport.conditionalToUpper(nnn,
                textEditor.getText());

        header = header + "\n\n" + body + "\n!--not sent--!";

        return header;
    }

    /**
     * Validate CAVE is in a state to allow sending of text product; and display
     * a warning when unable to send.
     *
     * @return true when able to send text product
     */
    private boolean validateTime() {
        if (shell != null && !shell.isDisposed() && shell.isVisible()
                && !SimulatedTimeOperations.isTransmitAllowed()) {
            SimulatedTimeOperations.displayFeatureLevelWarning(shell,
                    "Send Text Product");
            return false;
        }
        return true;
    }

    @Override
    public void timechanged() {
        VizApp.runAsync(() -> validateTime());
    }

    /**
     * Reads the contents of PARAGRAPH_PADDING_PATTERN_FILENAME into
     * paddingPatternList.
     */
    private void loadPaddingPattern() {
        IPathManager pathMgr = PathManagerFactory.getPathManager();
        LocalizationFile lf = pathMgr.getStaticLocalizationFile(
                LocalizationType.CAVE_STATIC,
                PARAGRAPH_PADDING_PATTERN_FILENAME);
        if (lf != null && lf.exists()) {
            try (BufferedReader br = new BufferedReader(
                    new InputStreamReader(lf.openInputStream()))) {
                String line = null;
                List<Pattern> patternList = new ArrayList<>();
                while ((line = br.readLine()) != null) {
                    if (!line.startsWith("#")) {
                        try {
                            Pattern ptrn = Pattern.compile(line);
                            patternList.add(ptrn);
                        } catch (PatternSyntaxException e) {
                            statusHandler.handle(Priority.PROBLEM,
                                    "Could not compile regex for line " + line
                                            + " from Padding Pattern file "
                                            + lf.toString(),
                                    e);
                        }
                    }
                }
                paddingPatternList = patternList;

            } catch (IOException e) {
                statusHandler
                        .handle(Priority.PROBLEM,
                                "Could not read Padding Pattern file "
                                        + PARAGRAPH_PADDING_PATTERN_FILENAME,
                                e);
            } catch (LocalizationException e) {
                statusHandler
                        .handle(Priority.PROBLEM,
                                "Could not find Padding Pattern file "
                                        + PARAGRAPH_PADDING_PATTERN_FILENAME,
                                e);
            }
        }
    }

    /**
     * Sets the padding: - according to padding pattern - to empty string
     *
     * @param paragraphStart
     */
    private String getParagraphPadding(String paragraphStart) {
        String defaultParagraphPadding = "";
        for (Pattern paddingPtrn : getPaddingPatternList()) {
            Matcher m = paddingPtrn.matcher(paragraphStart);
            if (m.matches()) {
                int paragraphOffset = m.group(1).length();
                StringBuilder sb = new StringBuilder(paragraphOffset);
                for (int i = 0; i < paragraphOffset; i++) {
                    sb.append(" ");
                }
                return sb.toString();
            }
        }
        return defaultParagraphPadding;
    }

    private List<Pattern> getPaddingPatternList() {
        // load padding pattern file
        if (paddingPatternList.isEmpty()) {
            loadPaddingPattern();
        }
        return paddingPatternList;
    }

    private int findLineToWrap() {
        int rval = -1;
        for (int i = 0; i < textEditor.getLineCount(); ++i) {
            String line = textEditor.getLine(i).trim();
            if (line.length() > currentWrapCfg.getWrapCol()) {
                return i;
            }
        }
        return rval;
    }

    private boolean verifyBufferSize() {
        if (textEditor.getCharCount() <= 3840) {
            // 3840 is max characters allowed in RPG
            return true;
        }

        RadarTextCheckConfirmationMsg radarTextCheckConfirmationMsg = new RadarTextCheckConfirmationMsg(
                shell);
        radarTextCheckConfirmationMsg.open();
        return false;
    }

    private void logInfo(String s) {
        statusHandler.handle(Priority.INFO,
                "Text " + token + " TextEditorDialog (UI Trace) - " + s);
    }

    public final String getToken() {
        return token;
    }

    public boolean isWarnGen() {
        return warnGenFlag;
    }

    public void continueScript() {
        if (runningScript != null) {
            runningScript.doContinue();
        }
    }

    public void cancelScript() {
        if (runningScript != null) {
            runningScript.cancel();
        }
    }

    /**
     * @return Current contents of the status message bar (may be empty)
     */
    public String getStatusMsg() {
        return statusBarLabel.getText();
    }
}
