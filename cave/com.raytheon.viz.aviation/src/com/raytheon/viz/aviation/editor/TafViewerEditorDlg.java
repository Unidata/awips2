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

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.io.Writer;
import java.lang.reflect.Constructor;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.TimeZone;

import javax.xml.bind.JAXB;

import org.apache.commons.configuration.ConfigurationException;
import org.apache.commons.lang.StringUtils;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.SWTError;
import org.eclipse.swt.custom.ST;
import org.eclipse.swt.custom.SashForm;
import org.eclipse.swt.custom.StyleRange;
import org.eclipse.swt.custom.StyledText;
import org.eclipse.swt.dnd.Clipboard;
import org.eclipse.swt.events.MouseAdapter;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.MouseTrackAdapter;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Cursor;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.printing.Printer;
import org.eclipse.swt.printing.PrinterData;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.FileDialog;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.TabFolder;
import org.eclipse.swt.widgets.TabItem;
import org.eclipse.ui.progress.UIJob;

import com.raytheon.uf.common.aviation.avnconfig.ITafSiteConfig;
import com.raytheon.uf.common.aviation.avnconfig.TafSiteConfigFactory;
import com.raytheon.uf.common.aviation.avnconfig.TafSiteData;
import com.raytheon.uf.common.dataplugin.taf.TafRecord;
import com.raytheon.uf.common.localization.ILocalizationFile;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.localization.SaveableOutputStream;
import com.raytheon.uf.common.localization.exception.LocalizationException;
import com.raytheon.uf.common.python.PyUtil;
import com.raytheon.uf.common.python.PythonScript;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.ISimulatedTimeChangeListener;
import com.raytheon.uf.common.time.SimulatedTime;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.viz.core.RGBColors;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.uf.viz.core.localization.LocalizationManager;
import com.raytheon.viz.aviation.AviationDialog;
import com.raytheon.viz.aviation.editor.tools.AvnSmartToolFinishedListener;
import com.raytheon.viz.aviation.editor.tools.AvnSmartToolJob;
import com.raytheon.viz.aviation.editor.tools.AvnSmartToolRequest;
import com.raytheon.viz.aviation.guidance.MetarViewer;
import com.raytheon.viz.aviation.guidance.ViewerTab;
import com.raytheon.viz.aviation.monitor.AvnPyUtil;
import com.raytheon.viz.aviation.monitor.TafUtil;
import com.raytheon.viz.aviation.observer.SendDialog;
import com.raytheon.viz.aviation.observer.TafMonitorDlg;
import com.raytheon.viz.aviation.resource.ResourceConfigMgr;
import com.raytheon.viz.aviation.resource.ResourceConfigMgr.ResourceTag;
import com.raytheon.viz.aviation.utility.AviationTextUtility;
import com.raytheon.viz.aviation.xml.TafViewerEditorConfig;
import com.raytheon.viz.aviation.xml.ViewerTabConfig;
import com.raytheon.viz.avncommon.AvnMessageMgr.StatusMessageType;
import com.raytheon.viz.avncommon.SyntaxMonitorCfg;
import com.raytheon.viz.avnconfig.FindReplaceDlg;
import com.raytheon.viz.avnconfig.HelpUsageDlg;
import com.raytheon.viz.avnconfig.IStatusSettable;
import com.raytheon.viz.avnconfig.MessageStatusComp;
import com.raytheon.viz.core.mode.CAVEMode;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;
import com.raytheon.viz.ui.dialogs.ICloseCallback;
import com.raytheon.viz.ui.simulatedtime.SimulatedTimeOperations;

import jep.JepConfig;
import jep.JepException;

/**
 * This class displays the TAF Viewer and Editor dialog.
 *
 * <pre>
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer     Description
 * ------------- -------- ------------ -----------------------------------------
 * Feb 28, 2008  938      lvenable     Initial creation.
 * Mar 26, 2008  933      grichard     Added ITafSettable interface.
 * Apr 02, 2008  934      grichard     Added localization usage.
 * May 07, 2008  1121     grichard     Hot fix to upper case current site.
 * May 12, 2008  1119     grichard     Made tabComp private instance variable.
 * May 12, 2008  1119     grichard     Convert 3-letter site to conus 4-letter
 *                                     site.
 * May 16, 2008  1119     grichard     Added support for IAviationObserver.
 * May 29, 2008  937      grichard     Taf refactor first cut.
 * Jun 16, 2008  937      grichard     Improved viewer/editor interaction.
 * Jun 19, 2008  937      grichard     Added edit features for taf editor.
 * Jul 28, 2008  1336     grichard     Made Taf editable in edit mode.
 * Sep 04, 2008  1444     grichard     Add Taf syntax check capability.
 * Sep 12, 2008  1444     grichard     Accommodate separate message logs.
 * Sep 19, 2008  1444     grichard     Add Taf wx quality check capability.
 * Dec 02, 2008  1588     grichard     Clarified naming of result of findInLine.
 * Dec 02, 2008  1588     grichard     Highlight specific syntax errors in TAF
 *                                     editor.
 * Dec 03, 2008  1588     grichard     Added wmo header display option for
 *                                     metars.
 * Jan 08, 2009  1840     grichard     Select tab before applying function.
 * Jan 09, 2009  1779     grichard     Highlight syntax errors in optional
 *                                     fields.
 * Jan 14, 2009  1842     grichard     Revise QC functionality.
 * Jan 14, 2009  1843     grichard     Display hover text containing syntax
 *                                     error.
 * Jan 14, 2009  1841     grichard     Set background color of syntax error
 *                                     black.
 * Jan 15, 2009  1816     grichard     Correct TAF to conform to 10-813 NWSI.
 * Apr 15, 2009  1982     grichard     Set background color of syntax error
 *                                     orange.
 * Apr 15, 2009  1982     grichard     Provide feedback when saving a working
 *                                     TAF.
 * May 06, 2009  1982     grichard     Set flight category balloon messages.
 * May 28, 2009  1982     grichard     Correct apply button functionality.
 * Jun 19, 2009  1982     grichard     Delegate syntax checking to Python codes.
 * Jul 24, 2009  2669     njensen      Hooked up TAF guidance
 * Aug 17, 2009  2837     grichard     Amend TAF when selected during Adjust
 *                                     Times.
 * Aug 17, 2009  2842     grichard     Amend/Correct TAF when Syntax check
 *                                     performed.
 * Aug 19, 2009  2832     grichard     Update times on format after checking
 *                                     syntax.
 * Aug 20, 2009  2839     grichard     Check for syntax performed before sending
 *                                     TAF.
 * Sep 04, 2009  3027     njensen      Major refactor and cleanup.
 * Jun 28, 2010  6547     lvenable
 * Jul 13, 2010  5555     rferrel      Correct text edit button to load TAF as
 *                                     AMD.
 * Jul 23, 2010  6696     rferrel      Removed extra pop window when restoring a
 *                                     TAF.
 * Aug 12, 2010  5326     rferrel      Changed syntax error message in status
 *                                     line.
 * Aug 23, 2010  5330     zhao         Removed &quot;Press Syntax before
 *                                     QC'ing&quot; msgBox
 * Sep 02, 2010  4022     rferrel      Highlight alerts.
 * Sep 09, 2010  5468     rferrel      Added setMessageStatusOK &amp;
 *                                     setMessageStatusError
 * Sep 28, 2010  5719     rferrel      Use resources to determine text editor
 *                                     field's height and width.
 * Nov 01, 2010  7407     rferrel      In checkSyntaxUsingPython fix parsing to
 *                                     get line # and column offset correctly.
 * Nov 02, 2010  3804     bgonzale     Updated printAllText to prepend user,
 *                                     date, time, and wmo header.
 * Dec 03, 2010  6223     rferrel      Disallow always now works and displays
 *                                     proper message.
 * Dec 09, 2010  7380     rferrel      Fixed getting view tabs text field size.
 *                                     Now compute the weights for sashFrom for
 *                                     proper display size for text fields.
 * Dec 14, 2010  5672     rferrel      populateNumTafsCombo now uses resources
 *                                     to populate and select default value.
 * Jan 06, 2011  7697     rferrel      Changes to always call the python code
 *                                     syntax and formating.
 * Jan 17, 2011  7782     rferrel      Created qcCheckSetup to simplify QC
 *                                     button.
 * Feb 09, 2011  7786     rferrel      Highlighting range now determined using
 *                                     the result's from/to information returned
 *                                     with the error message.
 * Apr 18, 2011  7888     rferrel      The saveFile/restoreFile now save/parse
 *                                     the first line of the TAF file for the
 *                                     the wmo, wmo site and issue time.
 * Apr 14, 2011  8065     rferrel      Populate View Tab only when active.
 * Apr 29, 2011  7888     rferrel      Make clear work the same as AWISP 1
 * Jun 01, 2011  9673     rferrel      Added fltCatFontColor.
 * Jun 27, 2011  9940     rferrel      sendTafToEditor now loads just the latest
 *                                     TAF.
 * Aug 12, 2011  10612    rferrel      saveFile will now always push file back
 *                                     to the server.
 * Nov 29, 2011  11612    rferrel      Added getViewerTabList.
 * Jul 20, 2012  14570    gzhang/zhao  Highlight correct time groups in TAF
 *                                     Viewer
 * 08AGU2012     15613    zhao         Modified highlightTAF()
 * Oct 04, 2012  1229     rferrel      Changes for non-blocking LoaderDialog.
 * Oct 09, 2012  1229     rferrel      Changes for non-blocking QcDialog.
 * Oct 09, 2012  1229     rferrel      Changes for non-blocking SendDialog.
 * Oct 11, 2012  1229     rferrel      Converted to a subclass of CaveSWTDialog
 *                                     and
 * Oct 12, 2012  1229     rferrel      Changes for non-blocking FindReplaceDlg.
 *                                     made non-blocking.
 * Oct 15, 2012  1229     rferrel      Changes for non-blocking HelpUsageDlg.
 * Nov 05, 2012  15477    zhao         Trim blank lines in text in Editor when
 *                                     check Syntax
 * Jan 09, 2013  15528    zhao         Modified saveFile() and restoreFile()
 * Aug 09, 2013  2033     mschenke     Switched File.separator to
 *                                     IPathManager.SEPARATOR
 * Sep 04, 2013  2322     lvenable     Added CAVE style so this dialog is
 *                                     perspective independent
 * Oct 24, 2013  16478    zhao         add syntax check for extra '=' sign
 * Jan 13, 2014  16153    zhao         Modified qcCheck().
 * Jan 15, 2014  16458    zhao         Check for extra '=' sign before 'regular
 *                                     syntax check'
 * Feb 12, 2014  17076    lvenable     Mark guidance tabs as not current so they
 *                                     get refreshed
 * Feb 19, 2014  16980    zhao         add code to ensure the Alt flag is false
 *                                     after the Alt kay is released
 * Mar 21, 2014  2925     lvenable     Fixed NPE error found during testing.
 * Apr 09, 2014  3005     lvenable     Added calls to mark the tabs as not
 *                                     current when the tabs are changed. This
 *                                     will show the tab as updating in the
 *                                     header and data text controls.
 * May 06, 2014  3091     rferrel      Use OUP authorization to bring up send
 *                                     dialog.
 * Jul 23, 2014  15645    zhao         modified checkBasicSyntaxError()
 * May 15, 2014  3002     bgonzale     Moved common taf code to
 *                                     com.raytheon.uf.common.dataplugin.taf.
 * Aug 13, 2014  3497     njensen      Refactored syntax checking to prevent
 *                                     potential infinite loop
 * Dec 02, 2014  15007    zhao         Added restoreFrom() for the "Restore
 *                                     From..." menu option
 * Apr 07, 2015  17332    zhao         Added code to handle case of "Cancel" in
 *                                     "Restore From..."
 * Jun 23, 2015  2282     skorolev     Corrected "CLEAR" case in updateSettings.
 * Jun 26, 2015  4588     skorolev     Fixed Insert/Overwrite issue.
 * Sep 15, 2015  4880     njensen      Removed dead code
 * Sep 28, 2015  4898     rferrel      Disable sending of TAF when CAVE not in
 *                                     real time.
 * Oct 05, 2015  4855     skorolev     Fixed an unhandled event loop exception
 *                                     in createErrorStyleRange.
 * Oct 16, 2015  4645     skorolev     Added updateWordWrap.
 * Oct 23, 2015  18061    zhao         Fixed a bug in checkBaiscSyntaxError()
 * Nov 12, 2015  4834     njensen      Changed LocalizationOpFailedException to
 *                                     LocalizationException
 * Dec 09, 2015  4645     skorolev     Initiated wrapChk using ResourceTag.
 *                                     Removed popup menu persistance.
 * Jan 07, 2016  4860     skorolev     Initiated TextDisplayModel at start of
 *                                     dialog.
 * Feb 11, 2016  5242     dgilling     Remove calls to deprecated Localization
 *                                     APIs.
 * Feb 15, 2016  4860     njensen      Use AviationTextUtility directly
 * May 19, 2016  5458     dgilling     Move AutoSave off UI thread.
 * Aug 25, 2016  17295    zhao         Added code to allow saving TAF to a user
 *                                     selected location
 * Nov 30, 2016  6016     randerso     Change timeChanged to call validateTime
 *                                     on the UI thread
 * Jan 13, 2017  5959     njensen      Cleaned up some warnings
 * Mar 22, 2017  6183     tgurney      Move localization files to common_static
 * Aug 09, 2017  6361     tgurney      Set issuance type from "Restore From"
 *                                     file
 * Dec 19, 2017  6946     tgurney      Check for multiple '=' before running any
 *                                     tool
 * Feb 21, 2018  6946     tgurney      Catch and ignore errors that appear when
 *                                     mouse hovering over text area
 * Feb 28, 2018  6870     tgurney      Remove state tracking of the Alt key
 * Jul 25, 2018  6748     randerso     Fixed to work with changes in
 *                                     CaveSWTDialog.shouldClose(). Code
 *                                     cleanup.
 * Oct 30, 2018  7525     randerso     Cleanup printing code.
 * May 15, 2019 20693   mgamazaychikov ITafSiteConfig, TafSiteConfigFactory, TafSiteData refactor
 * Jun 03, 2019  7852     dgilling     Update code for jep 3.8.
 *
 * </pre>
 *
 * @author lvenable
 *
 */
public class TafViewerEditorDlg extends CaveSWTDialog
        implements ITafSettable, ISimulatedTimeChangeListener, IEditActions {

    /**
     * Eclipse {@link Job} to read the current values from the TAF editor for
     * emergency auto save. This job needs to run from the UI thread because SWT
     * will only allow access to the data within the widgets from that thread.
     * After reading all of the data from the UI, this data will be passed to
     * the {@link AutoSaveWriteTafJob} for writing to the Localization store.
     * Job auto re-schedules itself every minute.
     */
    private class AutoSavePrepJob extends UIJob {

        public AutoSavePrepJob() {
            super("Preparing TAF data for Auto Save");
            setSystem(true);
        }

        @Override
        public IStatus runInUIThread(IProgressMonitor monitor) {
            if (!isDisposed()
                    && tabFolder.getSelectionIndex() != VIEWER_TAB_SELECTED) {
                String tafContents = editorTafTabComp.getTextEditorControl()
                        .getText();

                if (StringUtils.isNotEmpty(tafContents)
                        && !monitor.isCanceled()) {
                    String wmoId = editorTafTabComp.getWmoId();
                    String siteId = editorTafTabComp.getWmoSiteId();
                    String issueTime = editorTafTabComp.getLargeTF();
                    autoSaveWriteJob = new AutoSaveWriteTafJob(wmoId, siteId,
                            issueTime, tafContents);
                    autoSaveWriteJob.schedule();
                }
            }

            if (!monitor.isCanceled()) {
                schedule(TimeUtil.MILLIS_PER_MINUTE);
                return Status.OK_STATUS;
            }

            return Status.CANCEL_STATUS;
        }
    }

    /**
     * Eclipse {@link Job} to write the current state of the TAF editor to
     * Localization for emergency backup purposes.
     */
    private class AutoSaveWriteTafJob extends Job {

        private final String wmoId;

        private final String siteId;

        private final String issueTime;

        private final String taf;

        public AutoSaveWriteTafJob(String wmoId, String siteId,
                String issueTime, String taf) {
            super("Auto saving TAF");
            setSystem(true);

            this.wmoId = wmoId;
            this.siteId = siteId;
            this.issueTime = issueTime;
            this.taf = taf;
        }

        @Override
        protected IStatus run(IProgressMonitor monitor) {
            if (!monitor.isCanceled()) {
                String fileName = String.format("taf.%s.taf", VizApp.getWsId());
                try {
                    saveFile(fileName, wmoId, siteId, issueTime, taf);
                } catch (IOException | LocalizationException e) {
                    statusHandler
                            .error("An Exception occured while saving file "
                                    + fileName, e);
                }
                return Status.OK_STATUS;
            }

            return Status.CANCEL_STATUS;
        }
    }

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(TafViewerEditorDlg.class);

    /**
     * Developer can use this for debug purposes by making it true. This can go
     * away once AlertViz respects the Priorty setting of the status handler.
     */
    private static final boolean trace = false;

    private static final String SPLIT_REGEX = "=+[\\s\n]*|\n{2,}|\n$";

    /**
     * The number of editor tabs
     */
    private static final int numEditorTabs = 4;

    private static final IPathManager PATH_MANAGER = PathManagerFactory
            .getPathManager();

    /**
     * The indices for a range array.
     */
    private static final int frLineIndex = 0, frColIndex = 1, toLineIndex = 2,
            toColIndex = 3;

    private static final LocalizationContext BASE_COMMON_CTX = PATH_MANAGER
            .getContext(LocalizationType.COMMON_STATIC, LocalizationLevel.BASE);

    private static final String TEMP_TAF_PATH = "aviation"
            + IPathManager.SEPARATOR + "tmp";

    private String getCommonPythonDir() {
        return PATH_MANAGER.getFile(BASE_COMMON_CTX, "python").getPath();
    }

    /**
     * The display control.
     */
    private Display display;

    /**
     * Return value when the shell is disposed.
     */
    private Composite mainComp;

    /**
     * TabItem used to select the tab of the viewer/editor.
     */
    private TabItem ti;

    /**
     * Sash Form used to adjust the area a composite takes up.
     */
    private SashForm sashForm;

    /**
     * Show headers check box.
     */
    private Button showHeadersChk;

    /**
     * Number of TAFs check box.
     */
    private Combo numTafsCbo;

    /**
     * TAF viewer text control.
     */
    private StyledText tafViewerStTxt;

    /**
     * TAF viewer cursor.
     */
    private Cursor tafViewerCursor;

    /**
     * The Style Range color.
     */
    private Color errorColor;

    /**
     * Colors for the QC check
     */
    private Color[] qcColors;

    /**
     * Current maximum syntax error Level.
     */
    private int syntaxErrorLevel = 0;

    /**
     * Array of TAF editor tab items.
     */
    private ArrayList<TabItem> editorTafTabs;

    /**
     * Blank text for "empty" tabs.
     */
    private static final String tabFillText = "               ";

    /**
     * Tools combo box.
     */
    private Combo toolsCbo;

    /**
     * Insert check box.
     */
    private Button insertChk;

    /**
     * Wrap check box.
     */
    private Button wrapChk;

    /**
     * Site ID combo box.
     */
    private Combo siteIdCbo;

    /**
     * The tab folder on the TAF VIEWER/EDITOR Dialog;
     */
    private TabFolder tabFolder;

    /**
     * The tab folder on the GUIDANCE Dialog;
     */
    private TabFolder guidanceViewerFolder;

    /**
     * TAF editor tab folder. This contains all of the tab items, say, per ICAO.
     */
    private TabFolder editorTabFolder;

    /**
     * The EDITOR tab item of the tab folder in the TAF Viewer Editor Dialog.
     */
    private TabItem editorTab;

    /**
     * the VIEWER tab item of the tab folder in the TAF Viewer Editor Dialog.
     */
    private TabItem viewerTab;

    /**
     * Tab composite containing the TAF Viewer and the TAF Editor.
     */
    private EditorTafTabComp editorTafTabComp;

    /**
     * Clipboard for copying text to paste when in overwrite mode.
     */
    private Clipboard clipboard;

    /**
     * Flight Category Font color.
     */
    private Color fltCatFontColor;

    /**
     * Indicator of viewer tab selected.
     */
    private static final int VIEWER_TAB_SELECTED = 0;

    /**
     * The currently selected station identifier.
     */
    private String stationName;

    /**
     * The metar viewer to check for highlighting.
     */
    private MetarViewer metarViewer;

    /**
     * The list of Viewer tabs configured for display.
     */
    private final List<ViewerTab> modelsTabs = new ArrayList<>();

    /**
     * Current active Viewer tab.
     */
    private ViewerTab currentTab;

    /**
     * Send multiple TAFs individually or as a collective forecast.
     */
    private MenuItem sendCollectMI;

    /**
     * TAF records for the TAFs in the viewer
     */
    private TafRecord[] tafsInViewer;

    private FindReplaceDlg findDlg;

    /**
     * TAF editor enumeration
     */
    public static enum TafSettings {
        /* Select Edit Tab */
        /* Select View Tab */
        /* Select Routine Edit */
        /* Select Amended Edit */
        /* Select Routine Delayed Edit */
        /* Select Corrected Edit */
        OPEN_EDIT,
        OPEN_VIEW,
        OPEN_RTN,
        OPEN_AMD,
        OPEN_RTD,
        OPEN_COR,
        CLEAR,
        UPDATE_VIEW;
    }

    /**
     * Indicator of whether to dispose dialog.
     */
    private boolean disposeDialog = false;

    /**
     * Message status composite.
     */
    private IStatusSettable msgStatComp;

    /**
     * The text editor control
     */
    private StyledText st;

    /**
     * Popup menu.
     */
    private Menu popupMenu;

    /**
     * The station list.
     */
    private final List<String> stationList;

    /**
     * Saved state of the QC check items
     */
    private Map<String, String> savedQcItems;

    private final Job autoSaveReadJob;

    private Job autoSaveWriteJob;

    /**
     * The update times on format menu item.
     */
    private MenuItem updateTimesFormatMI;

    private MenuItem autoPrintMI;

    /**
     * Flag if sending AMD and COR TAFs requires confirmation.
     */
    private boolean confirmSend;

    /**
     * The error level on which to disallow transmissions.
     */
    private String disallowSend;

    private PythonScript parsePythonScript;

    private LoaderDialog loadDlg;

    private QcDialog qcDlg;

    private SendDialog sendDlg;

    private HelpUsageDlg usageDlg;

    private HelpUsageDlg keyBindingUsageDlg;

    /**
     * Constructor.
     *
     * @param parent
     *            Parent Shell.
     */
    public TafViewerEditorDlg(Shell parent, List<String> stationList,
            int caveStyle) {
        super(parent, SWT.DIALOG_TRIM | SWT.RESIZE | SWT.MODELESS,
                caveStyle | CAVE.DO_NOT_BLOCK | CAVE.PERSPECTIVE_INDEPENDENT);

        this.stationList = stationList;

        this.autoSaveReadJob = new AutoSavePrepJob();

        setText("AvnFPS TAF Editor");
        SimulatedTime.getSystemTime().addSimulatedTimeChangeListener(this);
    }

    /**
     * Initialize the components on the display.
     */
    public void init() {
        Shell parent = getParent();
        display = parent.getDisplay();
        clipboard = new Clipboard(display);

        ResourceConfigMgr configMgr = ResourceConfigMgr.getInstance();

        // Create the main layout for the shell.
        GridLayout gl = new GridLayout(1, true);
        gl.marginHeight = 0;
        gl.marginWidth = 0;
        shell.setLayout(gl);

        mainComp = new Composite(shell, SWT.NONE);
        gl = new GridLayout(1, true);
        gl.marginHeight = 3;
        gl.marginWidth = 3;
        mainComp.setLayout(gl);
        mainComp.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));

        setupColors();

        // Initialize all of the controls and layouts
        initializeComponents();

        confirmSend = configMgr.getDataAsBoolean(ResourceTag.ConfirmSend);
        disallowSend = configMgr.getDataAsString(ResourceTag.DisallowSend);
    }

    @Override
    protected void disposed() {
        tafViewerCursor.dispose();

        if (errorColor != null) {
            errorColor.dispose();
        }

        if (qcColors != null) {
            for (Color qcColor : qcColors) {
                if (qcColor != null) {
                    qcColor.dispose();
                }
            }
        }

        stopAutoSaveTimer();
    }

    @Override
    public boolean shouldClose() {
        /*
         * If the disposeDialog flag is true then return true so this dialog
         * will get disposed.
         */
        if (disposeDialog) {
            return true;
        }

        // Block the disposal of this dialog.
        hideDialog();
        return false;
    }

    /**
     * Show Dialog method for Taf Monitor Dialog's use.
     */
    @Override
    public void updateSettings(TafSettings setting, String stationName) {
        checkDlg();

        String previousStationName = this.stationName;
        this.stationName = stationName;

        switch (setting) {

        case OPEN_EDIT:
            // Select the editor tab on the tab folder.
            tabFolder.setSelection(editorTab);

            /*
             * Queue the validation so that this dialog will be displayed prior
             * to the validation. This allows any validate warning to be
             * displayed over this dialog.
             */
            VizApp.runAsync(new Runnable() {

                @Override
                public void run() {
                    validateTime();
                }
            });
            // Do not set editorTafTabComp radial buttons here it can corrupt a
            // populated selected tab.
            // Any changes must be done after a open tab is found and populated.
            break;

        case OPEN_VIEW:
            // Select the viewer tab on the tab folder.
            tabFolder.setSelection(viewerTab);
            // Update the taf in the viewer tab.
            populateTafViewer();
            // Update the metar and mos guidance in the viewer tab.
            updateViewerTab(stationName);

            // Mark the tabs as not current so they get updated.
            markTabsAsNotCurrent();
            break;

        case OPEN_AMD:
            // Select the editor tab on the tab folder.
            tabFolder.setSelection(editorTab);
            if (findEditOpenTab()) {
                sendTafToEditor(setting);
            }
            break;

        case OPEN_RTD:
            // Select the editor tab on the tab folder.
            tabFolder.setSelection(editorTab);
            if (findEditOpenTab()) {
                sendTafToEditor(setting);
            }
            break;

        case OPEN_COR:
            // Select the editor tab on the tab folder.
            tabFolder.setSelection(editorTab);
            if (findEditOpenTab()) {
                sendTafToEditor(setting);
            }
            break;

        case CLEAR:
            // Clear the selected tab item within the editor tab on the tab
            // folder.
            tabFolder.setSelection(editorTab);
            ti.setText(tabFillText);
            // Awips 1 doesn't touch the radio buttons when doing a clear.
            // editorTafTabComp.setRtnRdo(true);
            // editorTafTabComp.setAmdRdo(false);
            // editorTafTabComp.setRtdRdo(false);
            // editorTafTabComp.setCorRdo(false);
            editorTafTabComp.setWmoIdLbl("");
            editorTafTabComp.setWmoSiteLbl("");
            editorTafTabComp.setLargeTF("");
            // editorTafTabComp.setSmallTF("");
            editorTafTabComp.getTextEditorControl().setText("");
            if (editorTafTabComp.isTafSent()) {
                editorTafTabComp.updateTafSent(false);
            }
            break;

        case UPDATE_VIEW:
            if (!this.stationName.equals(previousStationName)) {
                // not the current station is the view restore name and do
                // nothing
                this.stationName = previousStationName;
            } else {
                // Update the taf in the viewer tab.
                populateTafViewer();
                // Update the metar and mos guidance in the viewer tab.
                updateViewerTab(stationName);
            }
            break;

        default:
            // Select the editor tab on the tab folder.
            tabFolder.setSelection(editorTab);
            break;
        }
    }

    /**
     * Mark the tabs as not current so they get refreshed.
     */
    private void markTabsAsNotCurrent() {
        for (TabItem tbi : guidanceViewerFolder.getItems()) {
            if (tbi.getControl() instanceof ViewerTab) {
                ((ViewerTab) tbi.getControl()).setDisplayCurrent(false);
                ((ViewerTab) tbi.getControl()).markTextAsUpdating();
            }
        }
    }

    @Override
    public void clearAll() {
        if (shell == null) {
            return;
        }
        // Clear all tab items within all editor tabs on the tab folder.
        tabFolder.setSelection(editorTab);

        // Use local variables so class variables stay in sync with selected
        // tab.
        EditorTafTabComp editorTTComp = null;
        for (TabItem tItem : editorTafTabs) {
            tItem.setText(tabFillText);
            editorTTComp = (EditorTafTabComp) tItem.getControl();
            editorTTComp.setRtnRdo(true);
            editorTTComp.setAmdRdo(false);
            editorTTComp.setRtdRdo(false);
            editorTTComp.setCorRdo(false);
            editorTTComp.setWmoIdLbl("------");
            editorTTComp.setWmoSiteLbl("----");
            editorTTComp.setLargeTF("");
            editorTTComp.setSmallTF("");
        }
        setEditTabSelection(0);
    }

    private void setupColors() {

        SyntaxMonitorCfg syntaxMonCfg = getSyntaxMonitorCfg();

        RGB syntaxFatalRGB = RGBColors
                .getRGBColor(syntaxMonCfg.getSyntaxFatalColor());
        RGB syntaxErrorRGB = RGBColors
                .getRGBColor(syntaxMonCfg.getSyntaxErrorColor());
        RGB syntaxWarningRGB = RGBColors
                .getRGBColor(syntaxMonCfg.getSyntaxWarningColor());

        errorColor = new Color(display, syntaxErrorRGB);

        qcColors = new Color[4];
        qcColors[0] = shell.getBackground();
        qcColors[1] = new Color(display, syntaxWarningRGB);
        qcColors[2] = new Color(display, syntaxErrorRGB);
        qcColors[3] = new Color(display, syntaxFatalRGB);
    }

    /**
     * Method to inform user when an action is to be taken prior to another
     * action
     *
     * @param msg
     *            -- the message to the user
     */
    private void userInformation(String msg) {
        MessageBox mb = new MessageBox(shell, SWT.ICON_INFORMATION | SWT.OK);
        mb.setText("Notice");
        mb.setMessage(msg);
        mb.open();
    }

    /**
     * Show dialog
     */
    @Override
    public void showDialog() {

        checkDlg();

        if (!shell.isVisible()) {
            setVisible(true);
        }

        if (!mustCreate(qcDlg)) {
            qcDlg.bringToTop();
        }

        if (!mustCreate(sendDlg)) {
            sendDlg.bringToTop();
        }

        if (!mustCreate(findDlg)) {
            findDlg.bringToTop();
        }

        if (!mustCreate(keyBindingUsageDlg)) {
            keyBindingUsageDlg.bringToTop();
        }

        if (!mustCreate(usageDlg)) {
            usageDlg.bringToTop();
        }

        shell.setActive();
    }

    /**
     * Hide dialog
     */
    @Override
    public void hideDialog() {

        if (shell.isVisible()) {
            setVisible(false);
        }
        if (!mustCreate(qcDlg)) {
            qcDlg.hide();
        }
        if (!mustCreate(sendDlg)) {
            sendDlg.hide();
        }

        if (!mustCreate(findDlg)) {
            findDlg.hide();
        }

        if (!mustCreate(keyBindingUsageDlg)) {
            keyBindingUsageDlg.hide();
        }

        if (!mustCreate(usageDlg)) {
            usageDlg.hide();
        }
    }

    private void setVisible(boolean state) {
        shell.setVisible(state);
        if (loadDlg != null && loadDlg.getShell() != null
                && !loadDlg.isDisposed()) {
            loadDlg.getShell().setVisible(state);
        }
    }

    /**
     * Dispose dialog
     */
    @Override
    public void disposeDialog() {
        disposeDialog = true;
        SimulatedTime.getSystemTime().removeSimulatedTimeChangeListener(this);
        AvnSmartToolJob.shutdown();
        for (ViewerTab viewerTab : modelsTabs) {
            viewerTab.dispose();
        }
        if (fltCatFontColor != null) {
            fltCatFontColor.dispose();
            fltCatFontColor = null;
        }

        close();

        if (clipboard != null) {
            clipboard.dispose();
        }
    }

    @Override
    protected void initializeComponents(Shell shell) {
        init();
    }

    /**
     * Initialize components on the display.
     */
    private void initializeComponents() {

        ResourceConfigMgr configMgr = ResourceConfigMgr.getInstance();
        configMgr.setDefaultColors(mainComp);

        // ---------------------------------------------
        // Create the menus at the top of the dialog.
        // ---------------------------------------------
        createMenus();

        // -----------------------------------------------------
        // Create the SashForm that will be used to manually
        // control the amount of space the top and bottom
        // composites occupy.
        // -----------------------------------------------------
        GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        Composite sashComp = new Composite(mainComp, SWT.NONE);
        GridLayout gl = new GridLayout(1, false);
        sashComp.setLayout(gl);
        sashComp.setLayoutData(gd);
        configMgr.setDefaultColors(sashComp);

        int orientationInt = SWT.VERTICAL;
        int numCols = 1;

        String orientationStr = configMgr
                .getDataAsString(ResourceTag.Orientation);
        if (orientationStr.compareTo("horizontal") == 0) {
            orientationInt = SWT.HORIZONTAL;
            numCols = 2;
        }

        sashForm = new SashForm(sashComp, orientationInt);
        sashForm.setLayout(new GridLayout(numCols, false));
        sashForm.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));
        sashForm.SASH_WIDTH = 10;
        sashForm.setBackground(configMgr.getDefaultBackgroundOffsetColor());

        // ---------------------------------------------
        // Create the controls on the dialog.
        // ---------------------------------------------
        createViewEditTabs(configMgr);

        createGuidanceViewer(configMgr);

        createBottomMessageControls(configMgr);

        Control[] control = sashForm.getChildren();
        int[] weights = new int[control.length];
        int total = 0;

        if (orientationInt == SWT.HORIZONTAL) {
            for (int i = 0; i < control.length; ++i) {
                int wt = control[i].computeSize(SWT.DEFAULT, SWT.DEFAULT).x;
                weights[i] = wt * 100;
                total += wt;
            }
        } else {
            for (int i = 0; i < control.length; ++i) {
                int wt = control[i].computeSize(SWT.DEFAULT, SWT.DEFAULT).y;
                weights[i] = wt * 100;
                total += wt;
            }
        }

        for (int i = 0; i < weights.length; ++i) {
            weights[i] /= total;
        }

        sashForm.setWeights(weights);
    }

    /**
     * Create the menus at the top of the display.
     */
    private void createMenus() {
        Menu menuBar = new Menu(shell, SWT.BAR);

        createFileMenu(menuBar);
        createOptionsMenu(menuBar);
        createEditMenu(menuBar);
        createHelpMenu(menuBar);

        shell.setMenuBar(menuBar);
    }

    /**
     * Create the File menu.
     *
     * @param menuBar
     *            Menu bar.
     */
    private void createFileMenu(Menu menuBar) {
        // -------------------------------------
        // Create the file menu
        // -------------------------------------
        MenuItem fileMenuItem = new MenuItem(menuBar, SWT.CASCADE);
        fileMenuItem.setText("&File");

        Menu fileMenu = new Menu(menuBar);
        fileMenuItem.setMenu(fileMenu);

        // -------------------------------------------------
        // Create all the items in the File dropdown menu
        // -------------------------------------------------

        // Print menu item
        MenuItem printMI = new MenuItem(fileMenu, SWT.NONE);
        printMI.setText("&Print");
        printMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                printAllText();
            }
        });

        // Clear Errors menu item
        MenuItem clearErrorsMI = new MenuItem(fileMenu, SWT.NONE);
        clearErrorsMI.setText("&Clear Errors");
        clearErrorsMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                // Clear errors in the bulletin.
                // Assume editorTafTabComp is for the active tab.
                editorTafTabComp.clearErrorsInBulletin();
                if (editorTafTabComp.isTafSent()) {
                    editorTafTabComp.updateTafSent(false);
                }
                editorTafTabComp.getTextEditorControl().setStyleRange(null);
                editorTafTabComp.update();
            }
        });

        // Update Times menu item
        MenuItem updateTimesMI = new MenuItem(fileMenu, SWT.NONE);
        updateTimesMI.setText("&Update Times");
        updateTimesMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                updateTimes();
            }
        });

        // Save As menu item
        MenuItem saveAsMI = new MenuItem(fileMenu, SWT.NONE);
        saveAsMI.setText("&Save As...");
        saveAsMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                handleSaveAs();
            }
        });

        // Restore From menu item
        MenuItem restoreFromMI = new MenuItem(fileMenu, SWT.NONE);
        restoreFromMI.setText("&Restore From...");
        restoreFromMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                restoreFrom();
            }
        });

        // Store in DB menu item
        MenuItem storeInDbMI = new MenuItem(fileMenu, SWT.NONE);
        storeInDbMI.setText("S&tore in DB");
        storeInDbMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                storeInDb();
            }
        });

        new MenuItem(fileMenu, SWT.SEPARATOR);

        // Close menu item
        MenuItem closeMI = new MenuItem(fileMenu, SWT.NONE);
        closeMI.setText("C&lose");
        closeMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                hideDialog();
            }
        });
    }

    /**
     * Create the Options menu.
     *
     * @param menuBar
     *            Menu bar.
     */
    private void createOptionsMenu(Menu menuBar) {

        ResourceConfigMgr configMgr = ResourceConfigMgr.getInstance();

        // ----------------------------------------
        // Create the options menu
        // ----------------------------------------
        MenuItem optionsMenuItem = new MenuItem(menuBar, SWT.CASCADE);
        optionsMenuItem.setText("&Options");

        Menu optionsMenu = new Menu(menuBar);
        optionsMenuItem.setMenu(optionsMenu);

        // ----------------------------------------------------
        // Create all the items in the Options dropdown menu
        // ----------------------------------------------------

        // Auto save menu item
        boolean autoSave = configMgr.getResourceAsBoolean(ResourceTag.AutoSave);
        MenuItem autoSaveMI = new MenuItem(optionsMenu, SWT.CHECK);
        autoSaveMI.setText("&Auto Save");
        autoSaveMI.setSelection(autoSave);
        autoSaveMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                MenuItem mi = (MenuItem) event.getSource();
                if (mi.getSelection()) {
                    startAutoSaveTimer();
                } else {
                    stopAutoSaveTimer();
                }
            }
        });

        if (autoSave) {
            startAutoSaveTimer();
        }

        // Auto Print menu item
        autoPrintMI = new MenuItem(optionsMenu, SWT.CHECK);
        autoPrintMI.setText("A&uto Print");
        autoPrintMI.setSelection(
                configMgr.getResourceAsBoolean(ResourceTag.AutoPrint));

        // Update Times on Format menu item
        updateTimesFormatMI = new MenuItem(optionsMenu, SWT.CHECK);
        updateTimesFormatMI.setText("U&pdate Times on Format");
        updateTimesFormatMI.setSelection(
                configMgr.getResourceAsBoolean(ResourceTag.UpdateTimes));

        // Send Collective menu item
        sendCollectMI = new MenuItem(optionsMenu, SWT.CHECK);
        sendCollectMI.setText("&Send in Collective");
    }

    /**
     * Create the Edit menu.
     *
     * @param menuBar
     *            Menu bar.
     */
    private void createEditMenu(Menu menuBar) {
        // -------------------------------------
        // Create the edit menu
        // -------------------------------------
        MenuItem editMenuItem = new MenuItem(menuBar, SWT.CASCADE);
        editMenuItem.setText("&Edit");

        Menu editMenu = new Menu(menuBar);
        editMenuItem.setMenu(editMenu);

        // -------------------------------------------------
        // Create all the items in the Edit dropdown menu
        // -------------------------------------------------

        // Cut menu item
        MenuItem cutMI = new MenuItem(editMenu, SWT.NONE);
        cutMI.setText("&Cut");
        cutMI.setAccelerator(SWT.CTRL + 'X');
        cutMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                cutText();
            }
        });

        // Copy menu item
        MenuItem copyMI = new MenuItem(editMenu, SWT.NONE);
        copyMI.setText("C&opy");
        copyMI.setAccelerator(SWT.CTRL + 'C');
        copyMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                copyText();
            }
        });

        // Paste menu item
        MenuItem pasteMI = new MenuItem(editMenu, SWT.NONE);
        pasteMI.setText("&Paste");
        pasteMI.setAccelerator(SWT.CTRL + 'V');
        pasteMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                pasteText();
            }
        });

        // Find menu item
        MenuItem findMI = new MenuItem(editMenu, SWT.NONE);
        findMI.setText("&Find...");
        findMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                if (mustCreate(findDlg)) {
                    findDlg = new FindReplaceDlg(shell,
                            editorTafTabComp.getTextEditorControl());
                    findDlg.open();
                } else {
                    findDlg.bringToTop();
                }
            }
        });

        // Undo menu item
        MenuItem undoMI = new MenuItem(editMenu, SWT.NONE);
        undoMI.setText("&Undo");
        undoMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                undoText();
            }
        });

        // Redo menu item
        MenuItem redoMI = new MenuItem(editMenu, SWT.NONE);
        redoMI.setText("&Redo");
        redoMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                redoText();
            }
        });
    }

    /**
     * Create the Help menu.
     *
     * @param menuBar
     *            Menu bar.
     */
    private void createHelpMenu(Menu menuBar) {
        // -------------------------------------
        // Create the Help menu
        // -------------------------------------
        MenuItem helpMenuItem = new MenuItem(menuBar, SWT.CASCADE);
        helpMenuItem.setText("&Help");

        Menu helpMenu = new Menu(menuBar);
        helpMenuItem.setMenu(helpMenu);

        // -------------------------------------------------
        // Create all the items in the Help dropdown menu
        // -------------------------------------------------

        // Key Binding menu item
        MenuItem keyBindingMI = new MenuItem(helpMenu, SWT.NONE);
        keyBindingMI.setText("&Key Bindings");
        keyBindingMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                if (mustCreate(keyBindingUsageDlg)) {
                    String description = "Key Bindings";
                    String helpText = "Ctrl-u               Undo changes.\n"
                            + "Ctrl-r               Redo changes.\n"
                            + "Insert               Toggles insert/overwrite mode.\n"
                            + "Any-Key              Insert normal printing characters.\n"
                            + "Button1              Sets the insert point, clear the selection, set focus.\n"
                            + "Ctrl-Button1         Set the insert point without affecting the selection.\n"
                            + "Button1-Motion       Sweep out a selection from the insert point.\n"
                            + "Double-Button1       Select the word under the mouse.\n"
                            + "Triple-Button1       Select the line under the mouse.\n"
                            + "Shift-Button1        Adjust the end of selection closest to the mouse.\n"
                            + "Shift-Button1-Motion Continue to adjust the selection.\n"
                            + "Button2              Paste the selection, or set the scrolling anchor.\n"
                            + "Button2-Motion       Scroll the window.\n"
                            + "Left or Ctrl-b       Move the cursor left one character. Clear selection.\n"
                            + "Shift-Left           Move the cursor and extend the selection.\n"
                            + "Ctrl-Left            Move the cursor by words. Clear the selection.\n"
                            + "Ctrl-Shift-Left      Move the cursor by words. Extend the selection.\n"
                            + "Right or Ctrl-f      Right bindings are analogous to Left bindings.\n"
                            + "Alt-b or Alt         Same as Ctrl-Left, Ctrl-Right.\n"
                            + "Up or Ctrl-p         Move the cursor up one line. Clear the selection.\n"
                            + "Ctrl-Up              Move the cursor by paragraph which are group of lines separated by a blank line.\n"
                            + "Ctrl-Shift-Up        Move the cursor by paragraph. Extend selection.\n"
                            + "Down or Ctrl-n       All Down bindings are analogous to Up bindings.\n"
                            + "PgUp, PgDn           Move the cursor by one screen. Clear the selection.\n"
                            + "Shift-PgUp,          Move the cursor by one screen. Extend the selection.\n"
                            + "Shift-PgDn\n"
                            + "Home or Ctrl-a       Move the cursor to line start. Clear the selection.\n"
                            + "Shift-Home           Move the cursor to line start. Extend the selection.\n"
                            + "End or Ctrl-e        Move the cursor to line end. Clear the selection.\n"
                            + "Shift-End            Move the cursor to line end. Extend the selection.\n"
                            + "Ctrl-Home            Move the cursor to the beginning of text. Clear the selection.\n"
                            + "Ctrl-End             Move the cursor to the beginning of text. Extend the selection.\n"
                            + "Ctrl-/               Select everything in the text widget.\n"
                            + "Ctrl-\\               Clear the selection.\n"
                            + "Delete               Delete the selection, if any. Otherwise delete the character to the right of the cursor.\n"
                            + "Backspace or Ctrl-h Delete the selection, if any. Otherwise delete the character to the left of the cursor.\n"
                            + "Ctrl-d              Delete character to the right of the cursor.\n"
                            + "Alt-d               Delete word to the right of the cursor.\n"
                            + "Ctrl-k              Delete from cursor to the end of the line. If you are at the end of the line, delete the newline\n"
                            + "                    character.\n"
                            + "Ctrl-o              Insert a newline but do not advance the cursor.\n"
                            + "Alt-Delete          Delete the word to the left of the cursor.\n"
                            + "Ctrl-t              Transpose the characters on either side of the cursor.";
                    keyBindingUsageDlg = new HelpUsageDlg(shell, description,
                            helpText);
                    keyBindingUsageDlg.open();
                } else {
                    keyBindingUsageDlg.bringToTop();
                }
            }
        });

        // Usage menu item
        MenuItem usageMI = new MenuItem(helpMenu, SWT.NONE);
        usageMI.setText("&Usage");
        usageMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                if (mustCreate(usageDlg)) {
                    String description = "Usage";
                    String helpText = "This is a text editor specialized for composing and checking TAFs.\n"
                            + "\n"
                            + "The dialog consists of two areas. The top part is for viewing and\n"
                            + "editing TAFs, the bottom part displays guidance data.\n"
                            + "\n"
                            + "The text editor consists of 4 independent pages. Each one displays WMO\n"
                            + "header. The 'Rtn', ..., 'Cor' toggles change forecast type. Use\n"
                            + "'Clear' button to clear the text window.  The 'Tools' combo box\n"
                            + "displays list of site-specific utilities to modify the forecast.\n"
                            + "\n" + "Menus:\n"
                            + "    Most of the items are relevant when the 'Editor' tab is selected. \n"
                            + "    File:\n"
                            + "    Print         - call print dialog\n"
                            + "    Clear Errors  - clears error tags set by the formatting \n"
                            + "                    (quality check) action. Can be used to force \n"
                            + "                    transmission of forecasts that did not pass\n"
                            + "                    the Syntax check.  It also reverses colors in\n"
                            + "                    the editor window to normal after forecast is\n"
                            + "                    sent\n"
                            + "    Update Times  - updates issue and valid times\n"
                            + "    Save As       - allows to save edited forecast to a file. \n"
                            + "    Restore From  - use to restore forecast from a backup file\n"
                            + "    Store in DB   - use to store forecast in AWIPS text database\n"
                            + "    Close         - closes the editor window\n"
                            + "\n" + "    Options: \n"
                            + "    Auto Save     - toggles auto-save feature\n"
                            + "    Auto Print    - toggles automatic printout of sent forecasts\n"
                            + "    Update Times on Format - if selected, the issue and valid times in \n"
                            + "                    the forecast are updated before quality control\n"
                            + "                    checks\n"
                            + "    Send in Collective - Toggles collective versus split bulletin\n"
                            + "                    transmission. Intended for OCONUS sites only.\n"
                            + "\n" + "    Edit:\n"
                            + "    Provides the usual editing functions (i.e. Cut, Copy, Paste, \n"
                            + "    and Find/Replace)\n" + "\n"
                            + "TAF editor area:\n" + "\n" + "Buttons:\n"
                            + "    Load: invokes forecast selection dialog. Bulletin (or product) \n"
                            + "        is selected from 'Bulletins' menu. To load bulletin from \n"
                            + "        previously saved file, set the 'From file' toggle.\n"
                            + "        Otherwise the forecasts will be loaded depending on \n"
                            + "        the 'Load Order' selecton:\n"
                            + "        Latest: first an attempt is made to access the most \n"
                            + "            recent previous forecast. If one cannot be found,\n"
                            + "            a template file is loaded.\n"
                            + "        Merge:  loads previous forecast, then appends template.\n"
                            + "            The intent is to allow phrases such as\n"
                            + "            AMD NOT SKED AFT D1HHZ.\n"
                            + "        Template: loads forecasts from template file.\n"
                            + "        'Forecast Type' selection is used to initialize WMO\n"
                            + "        header (DDHHMM and BBB) fields. These fields will be \n"
                            + "        updated when forecast is sent. \n" + "\n"
                            + "    Syntax: Performs syntax check and assures proper indentation \n"
                            + "        and maximum line length. If Syntax Check fails the forecast, \n"
                            + "        the problem areas will be highlighted. The color \n"
                            + "        corresponds to the severity of the problem. Red means \n"
                            + "        the forecast could not be parsed sucessfully. Orange \n"
                            + "        means error according to NWSI 10-813. Green is a warning.\n"
                            + "\n"
                            + "    QC: Performs selected quality control checks\n"
                            + "\n"
                            + "    Send:   Splits the bulletin into separate files, one per site, \n"
                            + "        which are written to directory 'xmit/pending'. \n"
                            + "        The transmission program running on the data server is \n"
                            + "        responsible for actual transmission.\n"
                            + "        The program will check whether a regular forecast is \n"
                            + "        sent within the transmission time window. If not, an \n"
                            + "        error dialog is displayed.\n" + "\n"
                            + "    Save:   Stores bulletin as a work TAF in a file\n"
                            + "\n"
                            + "    Restore: Restores bulletin from the work file\n"
                            + "\n" + "Toggles:\n"
                            + "    Insert        - toggles insert/overwrite mode\n"
                            + "    Wrap          - if selected, the line is folded when its length\n"
                            + "                    exceedes window width. Has no effect on \n"
                            + "                    the final format.\n" + "\n"
                            + "Viewer area:\n"
                            + "    Use 'Site ID' combo box to view site data from the list of \n"
                            + "    currently monitored sites. \n"
                            + "    Select page in the notebook for a specific data source. The list \n"
                            + "    of data sources is configurable. A set of display options is \n"
                            + "    available, depending on the data source.";
                    usageDlg = new HelpUsageDlg(shell, description, helpText);
                    usageDlg.open();
                } else {
                    usageDlg.bringToTop();
                }
            }
        });
    }

    /**
     * Create the Viewer and Editor tabs.
     */
    private void createViewEditTabs(ResourceConfigMgr configMgr) {
        GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        Composite tabComp = new Composite(sashForm, SWT.NONE);
        GridLayout gl = new GridLayout(1, false);
        tabComp.setLayout(gl);
        tabComp.setLayoutData(gd);
        configMgr.setDefaultColors(tabComp);

        gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        tabFolder = new TabFolder(tabComp, SWT.NONE);
        tabFolder.setLayoutData(gd);
        configMgr.setDefaultColors(tabFolder);

        viewerTab = new TabItem(tabFolder, SWT.NONE);
        viewerTab.setText("Viewer");
        viewerTab.setControl(createViewerComp(tabFolder, configMgr));

        editorTab = new TabItem(tabFolder, SWT.NONE);
        editorTab.setText("Editor");
        editorTab.setControl(createEditorComp(tabFolder, configMgr));

        // Make the editor tab the default selection on the tab folder.
        tabFolder.setSelection(editorTab);
        tabFolder.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected(SelectionEvent e) {
                super.widgetSelected(e);
                if (e.item == editorTab) {
                    validateTime();
                }
            }
        });
    }

    /**
     * Create the Viewer composite/controls for the Viewer tab.
     *
     * @param parentComp
     *            Parent composite.
     * @return The Viewer composite.
     */
    private Composite createViewerComp(Composite parentComp,
            ResourceConfigMgr configMgr) {
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, true);
        Composite viewerComp = new Composite(parentComp, SWT.NONE);
        GridLayout gl = new GridLayout(1, false);
        viewerComp.setLayout(gl);
        viewerComp.setLayoutData(gd);
        configMgr.setDefaultColors(viewerComp);

        // ------------------------------------
        // Add controls to the top composite
        // ------------------------------------
        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Composite topComp = new Composite(viewerComp, SWT.NONE);
        gl = new GridLayout(4, false);
        topComp.setLayout(gl);
        topComp.setLayoutData(gd);
        configMgr.setDefaultColors(topComp);

        gd = new GridData(SWT.DEFAULT, SWT.DEFAULT, true, false);
        Button textEditorBtn = new Button(topComp, SWT.PUSH);
        textEditorBtn.setText("Text Editor");
        textEditorBtn.setLayoutData(gd);
        configMgr.setDefaultFontAndColors(textEditorBtn);
        textEditorBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                // rferrel DR 5555
                sendTafToEditor(TafSettings.OPEN_AMD);
            }
        });

        showHeadersChk = new Button(topComp, SWT.CHECK);
        showHeadersChk.setText("Show Headers   ");
        configMgr.setDefaultFontAndColors(showHeadersChk);

        // mmaron DR 4949
        boolean showHeaders = configMgr
                .getResourceAsBoolean(ResourceTag.ShowHeaders);
        showHeadersChk.setSelection(showHeaders);

        showHeadersChk.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                // Display the guidance for tafs.
                populateTafViewer();
            }
        });

        Label numTafsLbl = new Label(topComp, SWT.NONE);
        numTafsLbl.setText("Num TAFs:");
        configMgr.setDefaultFontAndColors(numTafsLbl);

        numTafsCbo = new Combo(topComp, SWT.DROP_DOWN | SWT.READ_ONLY);
        configMgr.setDefaultFont(numTafsCbo);
        populateNumTafsCombo(configMgr);
        numTafsCbo.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                populateTafViewer();
            }
        });

        // ------------------------------------
        // Add controls to the top composite
        // ------------------------------------
        gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        Composite bottomComp = new Composite(viewerComp, SWT.NONE);
        gl = new GridLayout(1, false);
        gl.marginHeight = 0;
        gl.marginWidth = 0;
        bottomComp.setLayout(gl);
        bottomComp.setLayoutData(gd);
        configMgr.setDefaultColors(bottomComp);

        gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        gd.heightHint = configMgr.getDataAsInt(ResourceTag.TextHeight);
        gd.widthHint = configMgr.getDataAsInt(ResourceTag.TextWidth);
        tafViewerStTxt = new StyledText(bottomComp,
                SWT.BORDER | SWT.MULTI | SWT.V_SCROLL | SWT.H_SCROLL);
        tafViewerStTxt.setWordWrap(true);
        tafViewerStTxt.setEditable(false);
        tafViewerStTxt.setLayoutData(gd);
        configMgr.setTextFontAndColors(tafViewerStTxt);

        String cursorStr = configMgr
                .getResourceAsString(ResourceTag.TextCursor);
        int cursorInt = configMgr.getCursorAsInt(cursorStr);
        tafViewerCursor = new Cursor(display, cursorInt);
        tafViewerStTxt.setCursor(tafViewerCursor);

        tafViewerStTxt.addMouseListener(new MouseAdapter() {
            @Override
            public void mouseDown(MouseEvent e) {
                if (e.button == 3) {
                    popupMenu.setVisible(true);
                }
            }
        });

        createEditorPopupMenu(bottomComp);

        return viewerComp;
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
                tafViewerStTxt.copy();
            }
        });

        parentComp.setMenu(popupMenu);
    }

    /**
     * Create the Editor composite/controls for the Editor tab.
     *
     * @param parentComp
     *            Parent composite.
     * @return The Editor composite.
     */
    private Composite createEditorComp(Composite parentComp,
            ResourceConfigMgr configMgr) {
        GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        Composite editorComp = new Composite(parentComp, SWT.NONE);
        GridLayout gl = new GridLayout(1, false);
        editorComp.setLayout(gl);
        editorComp.setLayoutData(gd);
        configMgr.setDefaultColors(editorComp);

        // ---------------------------------------------
        // Create the top buttons in the Editor tab
        // ---------------------------------------------
        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Composite buttonComp = new Composite(editorComp, SWT.NONE);
        gl = new GridLayout(6, true);
        buttonComp.setLayout(gl);
        buttonComp.setLayoutData(gd);
        configMgr.setDefaultColors(buttonComp);

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Button loadBtn = new Button(buttonComp, SWT.PUSH);
        loadBtn.setText("Load");
        loadBtn.setLayoutData(gd);
        configMgr.setDefaultFontAndColors(loadBtn);
        loadBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                if (mustCreate(loadDlg)) {
                    loadDlg = new LoaderDialog(shell, TafViewerEditorDlg.this);
                    loadDlg.open();
                } else {
                    loadDlg.bringToTop();
                }
            }
        });

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Button syntaxBtn = new Button(buttonComp, SWT.PUSH);
        syntaxBtn.setText("Syntax");
        syntaxBtn.setLayoutData(gd);
        configMgr.setDefaultFontAndColors(syntaxBtn);
        syntaxBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                if (checkBasicSyntaxError(true)) {
                    return;
                }
                syntaxCheck();
            }
        });

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Button qcBtn = new Button(buttonComp, SWT.PUSH);
        qcBtn.setText("QC");
        qcBtn.setLayoutData(gd);
        configMgr.setDefaultFontAndColors(qcBtn);
        qcBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                qcCheckSetup(false);
            }
        });

        qcBtn.addMouseListener(new MouseAdapter() {
            @Override
            public void mouseDown(MouseEvent e) {
                if (e.button == 3) {
                    qcCheckSetup(true);
                }
            }

        });

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Button sendBtn = new Button(buttonComp, SWT.PUSH);
        sendBtn.setText("Send");
        sendBtn.setLayoutData(gd);
        configMgr.setDefaultFontAndColors(sendBtn);
        sendBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                sendAction();
            }
        });

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Button saveBtn = new Button(buttonComp, SWT.PUSH);
        saveBtn.setText("Save");
        saveBtn.setLayoutData(gd);
        configMgr.setDefaultFontAndColors(saveBtn);
        saveBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                // Assume editorTafTabComp is for the active tab.
                String ccc = LocalizationManager.getInstance().getSite();
                String bbb = editorTafTabComp.getBBB();
                String type;

                if (ti.getText().equals(tabFillText)
                        || editorTafTabComp.getTextEditorControl().getText()
                                .trim().length() == 0) {
                    MessageDialog.openWarning(getShell(), "Save TAF",
                            "Cannot save Empty TAF!");
                    return;
                }

                if (bbb.startsWith("AA")) {
                    type = "AMD";
                } else if (bbb.startsWith("RR")) {
                    type = "RTD";
                } else if (bbb.startsWith("CC")) {
                    type = "COR";
                } else {
                    type = "RTN";
                }

                String fileName = ccc + "WRKTAF." + type;
                try {
                    setWaitCursor(true);
                    saveFile(fileName, editorTafTabComp.getWmoId(),
                            editorTafTabComp.getWmoSiteId(),
                            editorTafTabComp.getLargeTF(),
                            editorTafTabComp.getTextEditorControl().getText());
                    setMessageStatusOK(
                            "File " + fileName + " saved successfully.");
                } catch (IOException | LocalizationException e) {
                    String message = "An Exception occured while saving file "
                            + fileName;
                    setMessageStatusError(message);
                    statusHandler.error(message, e);
                } finally {
                    setWaitCursor(false);
                }
            }
        });

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Button restoreBtn = new Button(buttonComp, SWT.PUSH);
        restoreBtn.setText("Restore");
        restoreBtn.setLayoutData(gd);
        configMgr.setDefaultFontAndColors(restoreBtn);
        restoreBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                String ccc = LocalizationManager.getInstance().getSite();
                String type;

                if (editorTafTabComp.isAmdRdoSelected()) {
                    type = "AMD";
                } else if (editorTafTabComp.isRtdRdoSelected()) {
                    type = "RTD";
                } else if (editorTafTabComp.isCorRdoSelected()) {
                    type = "COR";
                } else {
                    type = "RTN";
                }

                restoreFile(ccc + "WRKTAF." + type);
            }
        });

        // ---------------------------------------------
        // Create the Tab Folder in the Editor tab
        // ---------------------------------------------

        gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        editorTabFolder = new TabFolder(editorComp, SWT.NONE);
        editorTabFolder.setLayoutData(gd);
        configMgr.setDefaultColors(editorTabFolder);

        editorTafTabs = new ArrayList<>();

        for (int i = 0; i < numEditorTabs; i++) {
            TabItem editorTafTab = new TabItem(editorTabFolder, SWT.NONE);
            editorTafTab.setText(tabFillText);
            editorTafTab.setControl(
                    new EditorTafTabComp(editorTabFolder, this, this));

            editorTafTabs.add(editorTafTab);
        }

        setEditTabSelection(0);
        editorTabFolder.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                // Keep in sync with selected tab so other methods can
                // assume these are for the current tab.
                setEditTabSelection(editorTabFolder.getSelectionIndex());

                String site = ti.getText().substring(0, 4);

                if (!site.trim().isEmpty()) {
                    populateViewerStation(site);
                    guidanceSiteIdSelectionEventAction();
                }
            }
        });

        // ---------------------------------------------
        // Create the controls below the Tab Folder in
        // the Editor tab
        // ---------------------------------------------

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Composite controlsComp = new Composite(editorComp, SWT.NONE);
        gl = new GridLayout(5, false);
        controlsComp.setLayout(gl);
        controlsComp.setLayoutData(gd);
        configMgr.setDefaultColors(controlsComp);

        Label toolsLbl = new Label(controlsComp, SWT.NONE);
        toolsLbl.setText("Tools: ");
        configMgr.setDefaultFontAndColors(toolsLbl);

        toolsCbo = new Combo(controlsComp, SWT.DROP_DOWN | SWT.READ_ONLY);
        configMgr.setDefaultFont(toolsCbo);
        toolsCbo.setItems(AvnSmartToolJob.getTafTools());
        toolsCbo.select(0);

        Button applyBtn = new Button(controlsComp, SWT.PUSH);
        applyBtn.setText("Apply");
        configMgr.setDefaultFontAndColors(applyBtn);
        applyBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                if (editorTafTabComp.getTextEditorControl().getText() != null
                        && !editorTafTabComp.getTextEditorControl().getText()
                                .isEmpty()) {
                    List<String> tafs = new ArrayList<>();
                    String[] split = editorTafTabComp.getTextEditorControl()
                            .getText().split(SPLIT_REGEX);
                    for (String s : split) {
                        tafs.add(s);
                    }
                    String toolName = toolsCbo
                            .getItem(toolsCbo.getSelectionIndex());
                    String bbb = editorTafTabComp.getBBB();

                    if (checkBasicSyntaxError(true)) {
                        return;
                    }

                    // Setup for python request
                    AvnSmartToolRequest req = new AvnSmartToolRequest();
                    req.setToolName(toolName);
                    req.setBbb(bbb);
                    req.setFcsts(tafs);
                    AvnSmartToolFinishedListener finish = new AvnSmartToolFinishedListener();
                    req.setListener(finish);

                    // Prepare Runnable for updating editor after request is
                    // done.
                    TextUpdater updater = new TextUpdater();
                    updater.setFinishListerner(finish);

                    // Make busy and start up the threads.
                    setWaitCursor(true);
                    new Thread(updater).start();
                    AvnSmartToolJob.getInstance().enqueue(req);
                }
            }
        });

        insertChk = new Button(controlsComp, SWT.CHECK);
        insertChk.setText("Insert");
        configMgr.setDefaultFontAndColors(insertChk);
        insertChk.setSelection(configMgr.getDataAsBoolean(ResourceTag.Insert));
        insertChk.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                updateInsert(false);
            }
        });

        /*
         * If the insert check box is set to false then set the insert on the
         * text editor controls.
         */
        if (!insertChk.getSelection()) {
            for (TabItem editTafTabItem : editorTafTabs) {
                EditorTafTabComp tafTabComp = (EditorTafTabComp) editTafTabItem
                        .getControl();
                tafTabComp.getTextEditorControl().invokeAction(SWT.INSERT);
            }
        }

        // WordWrap checkbox
        wrapChk = new Button(controlsComp, SWT.CHECK);
        wrapChk.setText("Wrap");
        configMgr.setDefaultFontAndColors(wrapChk);
        if ("word".equals(configMgr.getDataAsString(ResourceTag.Wrap))) {
            wrapChk.setSelection(true);
        } else {
            wrapChk.setSelection(false);
        }
        wrapChk.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                updateWordWrap(wrapChk.getSelection());
            }
        });
        return editorComp;
    }

    /**
     * Check if there is an extra '=' sign in a TAF
     *
     * @param doLogMessage
     * @return true if error found, otherwise false
     */
    private boolean checkBasicSyntaxError(boolean doLogMessage) {

        boolean errorFound = false;

        String in = editorTafTabComp.getTextEditorControl().getText();

        clearSyntaxErrorLevel();

        st = editorTafTabComp.getTextEditorControl();

        in = in.toUpperCase().replaceAll("TAF", "\n\nTAF").trim();
        while (in.contains("\n\n\n")) {
            in = in.replace("\n\n\n", "\n\n");
        }

        st.setText(in);

        final Map<StyleRange, String> syntaxMap = new HashMap<>();

        st.addMouseTrackListener(new MouseTrackAdapter() {
            @Override
            public void mouseHover(MouseEvent e) {
                st = editorTafTabComp.getTextEditorControl();
                Point p = new Point(e.x, e.y);
                try {
                    int offset = st.getOffsetAtLocation(p);
                    StyleRange[] srs = st.getStyleRanges();
                    StyleRange sr = null;
                    for (StyleRange range : srs) {
                        if (offset >= range.start
                                && offset <= range.start + range.length) {
                            sr = range;
                            break;
                        }
                    }
                    if (sr != null) {
                        st.setToolTipText(syntaxMap.get(sr));
                    } else {
                        st.setToolTipText(null);
                    }
                } catch (IllegalArgumentException ex) {
                    // No text under cursor
                    st.setToolTipText(null);
                }
            }
        });

        String msg = "Syntax error: There is an extra '=' sign or 'TAF' is missing at beginning of TAF";
        String[] tafs = in.split("\n\n");
        int tafStartIndex = 0;
        for (String taf : tafs) {
            int firstEqualSignIndex = taf.indexOf('=');
            if (firstEqualSignIndex == -1) {
                tafStartIndex += taf.length() + 2;
                continue;
            }
            int secondEqualSignIndex = taf.indexOf('=',
                    firstEqualSignIndex + 1);
            if (secondEqualSignIndex == -1) {
                tafStartIndex += taf.length() + 2;
                continue;
            }
            while (secondEqualSignIndex > -1) {
                int secondEqualSignIndexInEditorText = tafStartIndex
                        + secondEqualSignIndex;
                StyleRange sr = new StyleRange(secondEqualSignIndexInEditorText,
                        1, null, qcColors[3]);
                syntaxMap.put(sr, msg);
                st.setStyleRange(sr);
                secondEqualSignIndex = taf.indexOf('=',
                        secondEqualSignIndex + 1);
            }
            errorFound = true;
            tafStartIndex += taf.length() + 2;
        }

        if (doLogMessage && errorFound) {
            msgStatComp.setMessageText(msg, qcColors[3].getRGB());
        }

        return errorFound;
    }

    private void sendAction() {
        if (!SendDialog.isAuthorized()) {
            putMessageToForecaster("Not authorized to send TAF.");
            return;
        }

        if (!validateTime()) {
            return;
        }
        // Assume editorTafTabComp is for the active tab.
        if (editorTafTabComp.isTafSent()) {
            putMessageToForecaster(
                    "Cannot send forecast: Forecast already sent");
            return;
        } else if (editorTafTabComp.isSyntaxChecked()) {
            if ("always".equals(disallowSend)) {
                putMessageToForecaster(
                        "Cannot send forecast: Send is disabled");
                return;
            }
            // Flag to allow sending if the syntax error threshold is
            // met.
            boolean okToSend = true;
            if (editorTafTabComp.isErrorsInBulletin()) {
                if ("warning".equals(disallowSend)
                        && editorTafTabComp.getErrorLevel() >= 1) {
                    okToSend = false;
                } else if ("error".equals(disallowSend)
                        && editorTafTabComp.getErrorLevel() >= 2) {
                    okToSend = false;
                } else if ("fatal".equals(disallowSend)
                        && editorTafTabComp.getErrorLevel() >= 3) {
                    okToSend = false;
                }
            }

            if (okToSend) {
                if (confirmSend) {
                    String bbb = editorTafTabComp.getBBB();

                    if (bbb.startsWith("AA") || bbb.startsWith("CC")) {
                        MessageBox mb = new MessageBox(shell,
                                SWT.ICON_QUESTION | SWT.OK | SWT.CANCEL);
                        mb.setMessage(
                                "Attempting to send an AMD or COR TAF, please confirm.");
                        if (mb.open() == SWT.CANCEL) {
                            return;
                        }
                    }
                }
            } else {
                putMessageToForecaster(
                        "Cannot send forecast: Bulletin has errors" + "\n"
                                + "Use Clear Errors to send it without changes");
                return;
            }

            if (autoPrintMI.getSelection()) {
                printForecast(
                        editorTafTabComp.getTextEditorControl().getText());
            }

            if (mustCreate(sendDlg)) {
                sendDlg = new SendDialog(shell, editorTafTabComp, msgStatComp,
                        sendCollectMI.getSelection());
                sendDlg.addCloseCallback(new ICloseCallback() {

                    @Override
                    public void dialogClosed(Object returnValue) {
                        // sendDlg sets the "taf sent" field only
                        if (editorTafTabComp.isTafSent()) {
                            editorTafTabComp.updateTafSent(true);
                        }
                        sendDlg = null;
                    }
                });
                sendDlg.open();
            } else {
                sendDlg.bringToTop();
            }

        } else {
            putMessageToForecaster(
                    "Cannot send forecast: Press Syntax before transmission");
        }

    }

    private void syntaxCheck() {
        // Assume editorTafTabComp is for the active tab.
        st = editorTafTabComp.getTextEditorControl();
        st.setText(st.getText().toUpperCase());
        String in = st.getText();
        if (in.trim().length() == 0) {
            msgStatComp.setMessageText("No TAF(s)",
                    qcColors[syntaxErrorLevel].getRGB());
            return;
        }
        editorTafTabComp.clearErrorsInBulletin();

        try {
            setWaitCursor(true);
            if (!checkSyntaxInEditor(true)) {
                setMessageStatusOK("Syntax OK");
                st.setToolTipText("");
            } else {
                msgStatComp.setMessageText("Errors found",
                        qcColors[syntaxErrorLevel].getRGB());
            }
            // Update the syntax flag.
            editorTafTabComp.setSyntaxChecked();
            // After checking syntax, update the TAF times when the
            // update
            // times on format checkbox is selected.
            if (updateTimesFormatMI.getSelection()) {
                editorTafTabComp.updateTimes();
                // The updateTimes clears error highlights and tool
                // tips.
                // Rerun the check but skip logging errors.
                st.setText(in);
                checkSyntaxInEditor(false);
                editorTafTabComp.setSyntaxChecked();
            }
        } finally {
            setWaitCursor(false);
            if (parsePythonScript != null) {
                try {
                    parsePythonScript.dispose();
                } catch (JepException e) {
                    statusHandler.debug("Failed to dispose script instance.",
                            e);
                }
                parsePythonScript = null;
            }
        }
    }

    /**
     * Change the state of all cursors of the Editor.
     *
     * @param wait
     *            true wait cursor otherwise default cursor
     */
    private void setWaitCursor(boolean wait) {
        EditorTafTabComp editorTTComp = null;
        ViewerTab modelComp = null;
        if (wait) {
            Cursor cursor = display.getSystemCursor(SWT.CURSOR_WAIT);
            shell.setCursor(cursor);
            // Styled Text areas must have cursor explicitly set.
            tafViewerStTxt.setCursor(cursor);
            for (TabItem ti : editorTafTabs) {
                editorTTComp = (EditorTafTabComp) ti.getControl();
                editorTTComp.getTextEditorControl().setCursor(cursor);
            }

            for (TabItem modelItem : guidanceViewerFolder.getItems()) {
                modelComp = (ViewerTab) modelItem.getControl();
                modelComp.getTextComp().getDataStTxt().setCursor(cursor);
            }
        } else {
            shell.setCursor(null);
            // Styled Text areas must have cursor explicitly set.
            tafViewerStTxt.setCursor(null);
            for (TabItem ti : editorTafTabs) {
                editorTTComp = (EditorTafTabComp) ti.getControl();
                editorTTComp.getTextEditorControl().setCursor(null);
            }

            for (TabItem modelItem : guidanceViewerFolder.getItems()) {
                modelComp = (ViewerTab) modelItem.getControl();
                modelComp.getTextComp().getDataStTxt().setCursor(null);
            }
        }
    }

    /**
     * Create the Guidance Viewer (below the Viewer/Editor tabs).
     */
    private void createGuidanceViewer(ResourceConfigMgr configMgr) {
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Composite guidanceViewerComp = new Composite(sashForm, SWT.NONE);
        GridLayout gl = new GridLayout(1, false);
        guidanceViewerComp.setLayout(gl);
        guidanceViewerComp.setLayoutData(gd);
        configMgr.setDefaultColors(guidanceViewerComp);

        // --------------------------------------------------
        // Create the controls and label at the top of the
        // guidance viewer.
        // --------------------------------------------------
        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Composite topControlsComp = new Composite(guidanceViewerComp, SWT.NONE);
        gl = new GridLayout(7, false);
        topControlsComp.setLayout(gl);
        topControlsComp.setLayoutData(gd);
        configMgr.setDefaultColors(topControlsComp);

        Label siteIdLbl = new Label(topControlsComp, SWT.NONE);
        siteIdLbl.setText("Site ID: ");
        configMgr.setDefaultFontAndColors(siteIdLbl);

        siteIdCbo = new Combo(topControlsComp, SWT.DROP_DOWN | SWT.READ_ONLY);
        configMgr.setDefaultFont(siteIdCbo);
        populateStations();
        siteIdCbo.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                guidanceSiteIdSelectionEventAction();
            }
        });

        gd = new GridData(SWT.RIGHT, SWT.CENTER, true, false);
        Label flightCatLbl = new Label(topControlsComp, SWT.NONE);
        flightCatLbl.setText("Flight Categories: ");
        flightCatLbl.setLayoutData(gd);
        configMgr.setDefaultFontAndColors(flightCatLbl);

        fltCatFontColor = new Color(display, RGBColors.getRGBColor("white"));

        gd = new GridData(40, SWT.DEFAULT);
        Label vfrLbl = new Label(topControlsComp, SWT.CENTER);
        vfrLbl.setToolTipText("vis > 5SM or cig > 3000ft");
        configMgr.setDefaultFontAndColors(vfrLbl, "VFR", gd);
        vfrLbl.setForeground(fltCatFontColor);
        configMgr.setVfrColor(vfrLbl);

        gd = new GridData(40, SWT.DEFAULT);
        Label mvfrLbl = new Label(topControlsComp, SWT.CENTER);
        mvfrLbl.setToolTipText("3 <= vis <= 5SM or 1000 <= cig <= 3000ft");
        configMgr.setDefaultFontAndColors(mvfrLbl, "MVFR", gd);
        mvfrLbl.setForeground(fltCatFontColor);
        configMgr.setMvfrColor(mvfrLbl);

        gd = new GridData(40, SWT.DEFAULT);
        Label ifrLbl = new Label(topControlsComp, SWT.CENTER);
        // mmaron - DR 4339
        ifrLbl.setToolTipText("1 <= vis < 3SM or 500 <= cig < 1000ft");
        configMgr.setDefaultFontAndColors(ifrLbl, "IFR", gd);
        ifrLbl.setForeground(fltCatFontColor);
        configMgr.setIfrColor(ifrLbl);

        gd = new GridData(40, SWT.DEFAULT);
        Label lifrLbl = new Label(topControlsComp, SWT.CENTER);
        lifrLbl.setToolTipText("1/2 <= vis < 1SM or 200 <= cig < 500ft");
        configMgr.setDefaultFontAndColors(lifrLbl, "LIFR", gd);
        lifrLbl.setForeground(fltCatFontColor);
        configMgr.setLifrColor(lifrLbl);

        // ---------------------------------------------
        // Create the Guidance Viewer tab folder
        // ---------------------------------------------
        gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        guidanceViewerFolder = new TabFolder(guidanceViewerComp, SWT.NONE);
        guidanceViewerFolder.setLayoutData(gd);
        guidanceViewerFolder.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected(SelectionEvent e) {
                if (!(e.item instanceof TabItem)) {
                    return;
                }
                TabItem item = (TabItem) e.item;
                if (!(item.getControl() instanceof ViewerTab)) {
                    return;
                }
                currentTab.setCurrentTab(false);
                currentTab = (ViewerTab) item.getControl();
                currentTab.setCurrentTab(true);
                if (!currentTab.isDisplayCurrent()) {
                    String siteID = siteIdCbo
                            .getItem(siteIdCbo.getSelectionIndex());
                    String site = currentTab.getSite(siteID);
                    currentTab.generateGuidance(site);
                    currentTab.markTextAsUpdating();
                }
            }
        });

        configMgr.setDefaultColors(guidanceViewerFolder);

        List<ViewerTabConfig> viewTabCfgArray = getTafViewerEditorCfg();

        for (ViewerTabConfig vtc : viewTabCfgArray) {
            TabItem modelsTab = new TabItem(guidanceViewerFolder, SWT.NONE);
            modelsTab.setText(vtc.getLabelName());

            String className = vtc.getClassName();

            try {
                Class<?> clazz = Class.forName(className);
                Constructor<?> ctor = clazz
                        .getDeclaredConstructor(Composite.class, String.class);

                ViewerTab obj = (ViewerTab) ctor
                        .newInstance(guidanceViewerFolder, vtc.getModelName());
                obj.setStationList(stationList);
                obj.generateCache(stationList);
                if (obj instanceof MetarViewer) {
                    metarViewer = (MetarViewer) obj;
                    metarViewer.addHighlightAlertSelectionListener(
                            new SelectionAdapter() {

                                @Override
                                public void widgetSelected(SelectionEvent e) {
                                    hightlightTAF();
                                }
                            });
                }
                modelsTab.setControl(obj);
                obj.setTafViewerEditorDlg(this);
                modelsTabs.add(obj);
            } catch (Exception e) {
                statusHandler.error(e.getLocalizedMessage(), e);
            }
        }
        currentTab = modelsTabs.get(0);
        stationName = siteIdCbo.getItem(siteIdCbo.getSelectionIndex());
        updateViewerTab(stationName);
        guidanceViewerFolder.setSelection(0);
    }

    /**
     * Create the message status composite.
     */
    private void createBottomMessageControls(ResourceConfigMgr configMgr) {
        msgStatComp = new MessageStatusComp(mainComp,
                StatusMessageType.TafViewerEditor,
                configMgr.getDefaultBackgroundColor().getRGB(),
                configMgr.getMsgBarBackground());

        for (ViewerTab tab : modelsTabs) {
            tab.setMsgStatComp(msgStatComp);
        }
    }

    /**
     * Save the current TAF bulletin to a tmp file.
     *
     * @throws LocalizationException
     * @throws IOException
     */
    private void saveFile(String filename, String wmoId, String siteId,
            String issueTime, String taf)
            throws IOException, LocalizationException {
        LocalizationContext context = PATH_MANAGER.getContext(
                LocalizationType.COMMON_STATIC, LocalizationLevel.SITE);
        ILocalizationFile lFile = PATH_MANAGER.getLocalizationFile(context,
                TEMP_TAF_PATH + IPathManager.SEPARATOR + filename);

        try (SaveableOutputStream outStream = lFile.openOutputStream();
                Writer out = new BufferedWriter(
                        new OutputStreamWriter(outStream))) {
            // Save WMO, wmo site, issue time, and BBB on first line
            out.write(wmoId);
            out.write("\t");
            out.write(siteId);
            out.write("\t");
            out.write(issueTime);
            out.write(System.getProperty("line.separator"));
            out.write(taf);
            out.close();
            outStream.save();
        }
    }

    /**
     * restore from a file a user selects
     */
    private void restoreFrom() {

        if (tabFolder.getSelectionIndex() == VIEWER_TAB_SELECTED) {
            tabFolder.setSelection(editorTab);
        }

        String tempTafPath = "aviation/tmp/";
        IPathManager pm = PathManagerFactory.getPathManager();
        LocalizationContext context = pm.getContext(
                LocalizationType.COMMON_STATIC, LocalizationLevel.SITE);
        String path = pm.getFile(context, tempTafPath).getAbsolutePath();
        FileDialog dlg = new FileDialog(shell, SWT.OPEN);
        dlg.setFilterPath(path);
        String filepath = dlg.open();
        if (filepath == null) {
            // if "Cancel"; do nothing
            return;
        }

        String errorMsg = null;

        setWaitCursor(true);
        File file = new File(filepath);
        try (FileReader reader = new FileReader(file);
                BufferedReader input = new BufferedReader(reader)) {
            StringBuilder contents = new StringBuilder();
            String line = null;
            line = input.readLine();

            if (line == null) {
                errorMsg = "empty file";
            } else {
                String[] values = line.split("\t");
                // Assume first line contains wmo, wmo site and issue time.
                if (values.length != 3) {
                    errorMsg = "parse error";
                    contents.append(line);
                    contents.append(System.getProperty("line.separator"));
                } else {
                    editorTafTabComp.setWmoIdLbl(values[0].trim());
                    editorTafTabComp.setWmoSiteLbl(values[1].trim());
                    editorTafTabComp.setLargeTF(values[2].trim());
                }
            }

            while ((line = input.readLine()) != null && line.isEmpty()) {
                // eat empty lines
            }
            // Get the issuance type
            String issuanceType = "";
            if (line != null) {
                contents.append(line);
                contents.append(System.getProperty("line.separator"));
                String[] split = line.split("\\s");
                if (split.length > 1) {
                    issuanceType = split[1];
                }
            }

            while ((line = input.readLine()) != null) {
                contents.append(line);
                contents.append(System.getProperty("line.separator"));
            }

            input.close();

            String tafText = contents.toString();
            List<String> sitesInTaf = getSitesInTaf(tafText);
            String icao = "----";
            String bbb = "---";

            if (errorMsg == null) {
                bbb = "   ";
                if ("COR".equals(issuanceType)) {
                    bbb = "CCX";
                } else if ("AMD".equals(issuanceType)) {
                    bbb = "AAX";
                } else if ("RTD".equals(issuanceType)) {
                    bbb = "RRX";
                }
                editorTafTabComp.setBBB(bbb);
            }
            if (!sitesInTaf.isEmpty()) {
                icao = sitesInTaf.get(0);
            } else if (errorMsg == null) {
                errorMsg = "uable to determine station";
            }

            ti.setText(icao + " " + bbb);
            editorTafTabComp.getTextEditorControl().setText(tafText);

            if (editorTafTabComp.isTafSent()) {
                editorTafTabComp.updateTafSent(false);
            }
        } catch (FileNotFoundException e) {
            String msg = "File " + filepath + " not found.";
            setMessageStatusError(msg);
            statusHandler.handle(Priority.PROBLEM, msg, e);
        } catch (IOException e) {
            String msg = "An IOException occured while opening file "
                    + filepath;
            setMessageStatusError(msg);
            statusHandler.handle(Priority.PROBLEM, msg, e);
        } finally {
            if (errorMsg != null) {
                setMessageStatusError("File " + filepath + ": " + errorMsg);
            } else {
                setMessageStatusOK(
                        "File " + filepath + " opened successfully.");
            }
            setWaitCursor(false);
        }
    }

    /**
     * Restore a TAF bulletin from a tmp file.
     */
    private void restoreFile(String filename) {
        if (tabFolder.getSelectionIndex() != VIEWER_TAB_SELECTED) {
            if (editorTafTabComp != null) {
                // Select the editor tab on the tab folder.
                tabFolder.setSelection(editorTab);
                // Use the current tab
                if (!ti.getText().equals(tabFillText)) {
                    if (!editorTafTabComp.isTafSent()
                            && !editorTafTabComp.getTextEditorControl()
                                    .getText().trim().isEmpty()) {
                        MessageBox questionMB = new MessageBox(shell,
                                SWT.ICON_WARNING | SWT.OK | SWT.CANCEL);
                        questionMB.setText("Restore TAF");
                        questionMB.setMessage(
                                "Forecast not saved. Do you want to continue?");
                        int result = questionMB.open();

                        if (result == SWT.CANCEL) {
                            return;
                        }
                    }
                }

                String tempTafPath = "aviation/tmp/";
                IPathManager pm = PathManagerFactory.getPathManager();
                LocalizationContext context = pm.getContext(
                        LocalizationType.COMMON_STATIC, LocalizationLevel.SITE);
                String path = pm.getFile(context, tempTafPath)
                        .getAbsolutePath();
                String filepath = null;

                File tmp = new File(path);
                if (!tmp.exists()) {
                    tmp.mkdirs();
                }

                if (filename == null || filename.isEmpty()) {
                    FileDialog dlg = new FileDialog(shell, SWT.OPEN);
                    dlg.setFilterPath(path);
                    filepath = dlg.open();
                } else {
                    filepath = path + "/" + filename;
                }

                if (filepath != null) {
                    String errorMsg = null;

                    try {
                        setWaitCursor(true);

                        String fname = tempTafPath + filepath
                                .substring(filepath.lastIndexOf('/') + 1);
                        ILocalizationFile lFile = pm
                                .getLocalizationFile(context, fname);

                        StringBuilder contents = new StringBuilder();
                        try (InputStream inStream = lFile.openInputStream();
                                BufferedReader in = new BufferedReader(
                                        new InputStreamReader(inStream))) {
                            String line = null;
                            line = in.readLine();

                            if (line == null) {
                                errorMsg = "empty file";
                            } else {
                                String[] values = line.split("\t");
                                // Assume first line contains wmo, wmo site
                                // and issue time.
                                if (values.length != 3) {
                                    errorMsg = "parse error";
                                    contents.append(line);
                                    contents.append(System
                                            .getProperty("line.separator"));
                                } else {
                                    editorTafTabComp
                                            .setWmoIdLbl(values[0].trim());
                                    editorTafTabComp
                                            .setWmoSiteLbl(values[1].trim());
                                    editorTafTabComp
                                            .setLargeTF(values[2].trim());
                                }
                            }

                            while ((line = in.readLine()) != null) {
                                contents.append(line);
                                contents.append(
                                        System.getProperty("line.separator"));
                            }
                        }

                        String tafText = contents.toString();
                        List<String> sitesInTaf = getSitesInTaf(tafText);
                        String icao = "----";
                        String bbb = "---";

                        if (errorMsg == null) {
                            bbb = editorTafTabComp.getBBB();
                        }

                        if (!sitesInTaf.isEmpty()) {
                            icao = sitesInTaf.get(0);
                        } else if (errorMsg == null) {
                            errorMsg = "uable to determine station";
                        }

                        ti.setText(icao + " " + bbb);
                        editorTafTabComp.getTextEditorControl()
                                .setText(tafText);

                        if (editorTafTabComp.isTafSent()) {
                            editorTafTabComp.updateTafSent(false);
                        }

                        setMessageStatusOK(
                                "File " + filepath + " opened successfully.");
                    } catch (IOException | LocalizationException e) {
                        String msg = "An Exception occured while reading file "
                                + filepath;
                        statusHandler.handle(Priority.PROBLEM, msg, e);
                        setMessageStatusError(msg);
                    } finally {
                        if (errorMsg != null) {
                            setMessageStatusError(
                                    "File " + filepath + ": " + errorMsg);
                        }
                        setWaitCursor(false);
                    }
                }
            }
        }
    }

    /**
     * Put an error message on the status line making the status box red. This
     * is protected to allow child dialogs to display specfic errors.
     *
     * @param message
     *            Message to display
     */
    protected void setMessageStatusError(String message) {
        msgStatComp.setMessageText(message,
                display.getSystemColor(SWT.COLOR_RED).getRGB());
    }

    private void setMessageStatusOK(String message) {
        msgStatComp.setMessageText(message,
                display.getSystemColor(SWT.COLOR_GREEN).getRGB());
    }

    /**
     * Store the bulletin as a temporary taf in the text database.
     */
    private void storeInDb() {
        if (tabFolder.getSelectionIndex() != VIEWER_TAB_SELECTED) {
            try {
                AviationTextUtility.saveTafBulletin(
                        editorTafTabComp.getTextEditorControl().getText());
                setMessageStatusOK("Temporary WRKTAF Stored in DB");
            } catch (Exception e) {
                statusHandler.error(
                        "Problem storing temporary WRKTAF in text product database.",
                        e);
            }
        }
    }

    /**
     * Populate the number of Tafs combo and select default value.
     *
     * @param configMgr
     */
    private void populateNumTafsCombo(ResourceConfigMgr configMgr) {
        configMgr.getComboValues(ResourceTag.NumTafs);
        for (String value : configMgr.getComboValues(ResourceTag.NumTafs)) {
            numTafsCbo.add(value);
        }
        numTafsCbo.select(numTafsCbo
                .indexOf(configMgr.getResourceAsString(ResourceTag.NumTafs)));
    }

    /**
     * Populate the stations
     */
    private void populateStations() {
        siteIdCbo.removeAll();
        for (String s : stationList) {
            siteIdCbo.add(s);
        }
        siteIdCbo.select(0);
    }

    /**
     * Populate the viewer station
     */
    @Override
    public void populateViewerStation(String theStation) {
        checkDlg();
        siteIdCbo.select(siteIdCbo.indexOf(theStation));
    }

    /**
     * Cut selected text from the taf editor and put it in the clipboard.
     */
    @Override
    public void cutText() {
        if (tabFolder.getSelectionIndex() != VIEWER_TAB_SELECTED) {
            // Assume editorTafTabComp is for the active tab.
            if (editorTafTabComp.getTextEditorControl()
                    .getSelectionCount() == 0) {
                MessageBox mb = new MessageBox(shell, SWT.ICON_ERROR | SWT.OK);
                mb.setText("Cut Error");
                mb.setMessage("You must first select text to\n"
                        + "cut (by dragging, for example).");
                mb.open();
                return;
            }
            editorTafTabComp.getTextEditorControl().cut();
            // Remove popup menu.
            editorTafTabComp.getTextEditorControl().redraw();
        }
    }

    /**
     * Copy Selected text from the taf editor and put it in the clipboard.
     */
    @Override
    public void copyText() {
        if (tabFolder.getSelectionIndex() != VIEWER_TAB_SELECTED) {
            boolean copyFromGuidanceViewerTab = false;

            TabItem modelItem = guidanceViewerFolder
                    .getItem(guidanceViewerFolder.getSelectionIndex());

            ViewerTab modelComp = (ViewerTab) modelItem.getControl();

            // Assume editorTafTabComp is for the active tab.
            if (editorTafTabComp.getTextEditorControl()
                    .getSelectionCount() == 0) {
                if (modelComp.getTextComp().getDataStTxt()
                        .getSelectionCount() != 0
                        || modelComp.getTextComp().getHeaderStTxt()
                                .getSelectionCount() != 0) {
                    copyFromGuidanceViewerTab = true;
                } else {
                    MessageBox mb = new MessageBox(shell,
                            SWT.ICON_ERROR | SWT.OK);
                    mb.setText("Copy Error");
                    mb.setMessage("You must first select text to\n"
                            + "copy (by dragging, for example).");
                    mb.open();
                    return;
                }
            }

            if (copyFromGuidanceViewerTab) {
                if (modelComp.getTextComp().getDataStTxt()
                        .getSelectionCount() != 0) {
                    modelComp.getTextComp().getDataStTxt().copy();
                } else {
                    modelComp.getTextComp().getHeaderStTxt().copy();
                }
            } else {
                editorTafTabComp.getTextEditorControl().copy();
                // Remove popup menu.
                editorTafTabComp.getTextEditorControl().redraw();
            }
        }
    }

    /**
     * Paste text from the clipboard into the taf editor.
     */
    @Override
    public void pasteText() {
        if (tabFolder.getSelectionIndex() != VIEWER_TAB_SELECTED) {
            // Assume editorTafTabComp is for the active tab.
            editorTafTabComp.getTextEditorControl().paste();
            // Remove popup menu after pasting.
            editorTafTabComp.getTextEditorControl().redraw();
        }
    }

    /**
     * Print all text from the taf editor to the default printer.
     */
    private void printAllText() {
        if (tabFolder.getSelectionIndex() != VIEWER_TAB_SELECTED) {
            DateFormat dt = new SimpleDateFormat("MM/dd/yy HH:mm:ss");
            TafRecord tf = this.tafsInViewer[0];
            StyledText editorStyledText = editorTafTabComp
                    .getTextEditorControl();
            StringBuilder sb = new StringBuilder("Printed by ");

            sb.append(AviationDialog.getForecaster());
            sb.append(" on ");
            sb.append(dt.format(Calendar
                    .getInstance(TimeZone.getTimeZone("GMT")).getTime()));
            sb.append('\n');
            sb.append(tf.getWmoHeader());
            sb.append(editorStyledText.getText());

            try {
                printForecast(sb.toString());
            } catch (SWTError ex) {
                String msg = "Unable to determine default printer.";
                statusHandler.handle(Priority.PROBLEM, msg, ex);
                MessageBox box = new MessageBox(shell,
                        SWT.ICON_WARNING | SWT.OK);
                box.setText("Print Error");
                box.setMessage(msg + "\n" + ex.toString());
                box.open();
            }
        }
    }

    /**
     * Undo the very last text edit from the taf editor via the undo scratch
     * pad.
     */
    @Override
    public void undoText() {
        if (tabFolder.getSelectionIndex() != VIEWER_TAB_SELECTED) {
            // Assume editorTafTabComp is for the active tab.
            editorTafTabComp.undo();
            // Remove popup menu.
            editorTafTabComp.getTextEditorControl().redraw();
        }
    }

    /**
     * Redo the very last text edit from the taf editor via the redo scratch
     * pad.
     */
    @Override
    public void redoText() {
        if (tabFolder.getSelectionIndex() != VIEWER_TAB_SELECTED) {
            // Assume editorTafTabComp is for the active tab.
            editorTafTabComp.redo();
            // Remove popup menu.
            editorTafTabComp.getTextEditorControl().redraw();
        }
    }

    /**
     * Check TAF syntax within the taf editor within the selected tab using the
     * legacy Python code for decoding.
     *
     * @param doLogMessage
     *            when true place error messages in message status.
     * @return errorInTaf true when syntax error found otherwise false
     */
    private boolean checkSyntaxInEditor(boolean doLogMessage) {
        clearSyntaxErrorLevel();
        st = editorTafTabComp.getTextEditorControl();

        final Map<StyleRange, String> syntaxMap = new HashMap<>();
        st.addMouseTrackListener(new MouseTrackAdapter() {
            @Override
            public void mouseHover(MouseEvent e) {
                st = editorTafTabComp.getTextEditorControl();
                Point p = new Point(e.x, e.y);
                try {
                    int offset = st.getOffsetAtLocation(p);
                    StyleRange[] srs = st.getStyleRanges();
                    StyleRange sr = null;
                    for (StyleRange range : srs) {
                        if (offset >= range.start
                                && offset <= range.start + range.length) {
                            sr = range;
                            break;
                        }
                    }
                    if (sr != null) {
                        st.setToolTipText(syntaxMap.get(sr));
                    } else {
                        st.setToolTipText(null);
                    }
                } catch (IllegalArgumentException ex) {
                    // No text under cursor
                    st.setToolTipText(null);
                }
            }
        });

        // Get the content of the Taf Editor.
        // Assume editorTafTabComp is for the active tab.
        // DR15477: trim blank lines before Syntax Checking
        String in = editorTafTabComp.getTextEditorControl().getText().trim();
        checkSyntax(in, syntaxMap, doLogMessage);
        // reset everything since checkSyntax may have altered the TAF
        st.setStyleRange(null);
        syntaxMap.clear();

        /*
         * TODO Refactor all of this to be smarter. Right now it's kind of dumb
         * in that the python syntax check can potentially alter the datetime of
         * the TAF and/or the whitespace/spacing of the TAF. If that occurs, the
         * python does NOT return the map of syntax problems detected, so you
         * have to run the syntax check again against the properly formatted
         * TAF.
         *
         * Due to the way the code is currently structured, there's not an easy
         * way to cleanly do the second syntax check only if necessary while
         * keeping the style ranges correctly lined up. Therefore, for now we
         * will run the syntax check once against the TAF(s) (ie the code
         * above), and just presume that it altered the TAF's datetime or
         * whitespace. Then we will run it a second time (ie the code below)
         * since it should be guaranteed at that point to return a syntax map if
         * there were any syntax issues detected.
         */
        in = editorTafTabComp.getTextEditorControl().getText().trim();
        boolean errorInTaf = checkSyntax(in, syntaxMap, doLogMessage);
        st.setStyleRange(null);
        Set<StyleRange> srs = syntaxMap.keySet();
        for (StyleRange sr : srs) {
            st.setStyleRange(sr);
        }

        return errorInTaf;
    }

    private boolean checkSyntax(String in, Map<StyleRange, String> syntaxMap,
            boolean doLogMessage) {
        boolean errorInTaf = false;
        List<String> checkedTafs = new ArrayList<>();
        List<String> tList = new ArrayList<>();
        Map<StyleRange, String> sMap = new HashMap<>();

        /*
         * Separate each TAF individually and syntax check it, and then
         * reassemble the set of TAFs each iteration to ensure the line numbers
         * and style range indices will line up correctly.
         */
        int idx1 = 0;
        while (idx1 > -1) {
            int idx2 = in.indexOf("TAF", idx1 + 1);
            String taf;
            sMap.clear();
            tList.clear();
            if (idx2 > -1) {
                taf = in.substring(idx1, idx2);
            } else {
                taf = in.substring(idx1);
            }

            int currentLineNo = st.getLineAtOffset(idx1);
            errorInTaf |= checkSyntaxUsingPython(taf, currentLineNo, sMap,
                    tList, doLogMessage);
            for (StyleRange skey : sMap.keySet()) {
                syntaxMap.put(skey, sMap.get(skey));
            }

            String tafAfterCheck = tList.get(0);
            checkedTafs.add(tafAfterCheck);
            StringBuilder sb = new StringBuilder();
            for (String checkedTaf : checkedTafs) {
                sb.append(checkedTaf);
                sb.append("\n");
            }
            int lengthChecked = sb.length();

            if (idx2 > -1) {
                sb.append(in.substring(idx2));
            }
            in = sb.toString();
            st.setText(in);

            /*
             * Set idx1 to the next TAF after all the text that has already been
             * checked. This ensures we won't hit the very rare infinite loop
             * that occurs if tafAfterCheck comes back with two TAFS inside it.
             */
            idx1 = in.indexOf("TAF", lengthChecked);
        }

        return errorInTaf;
    }

    /**
     * Check TAF syntax within the taf editor within the selected tab using the
     * legacy Python code for decoding. When the Python code modifies the TAF
     * parsing is halted to allow the caller to handle the corrected TAF.
     *
     * @param in
     *            The TAF string to parse
     * @param currentLineNo
     *            The line number in the edirot where the TAF starts
     * @param syntaxMap
     *            Where syntax error style range information is placed
     * @param tafList
     *            Place to add TAF returned by the Python code
     * @param doLogMessage
     *            when true log syntax error
     * @return errorInTaf true when syntax error found otherwise false
     */
    @SuppressWarnings("unchecked")
    private boolean checkSyntaxUsingPython(String in, int currentLineNo,
            Map<StyleRange, String> syntaxMap, List<String> tafList,
            boolean doLogMessage) {
        boolean errorInTaf = false;
        int[] range = new int[] { 0, 0, 0, 0 };

        in = in.trim();
        Map<String, Object> resultMap = parseText(in,
                editorTafTabComp.getBBB());
        String newText = (String) resultMap.get("text");
        Map<String, Object> parsedText = (Map<String, Object>) resultMap
                .get("result");
        String newTime = (String) resultMap.get("headerTime");
        tafList.add(newText);

        editorTafTabComp.setLargeTF(newTime);
        List<String> results;
        StringBuilder errorMsg = new StringBuilder();
        int errorLevel;

        if (parsedText.keySet().size() == 2) {
            for (Entry<String, Object> e : parsedText.entrySet()) {
                results = (List<String>) e.getValue();
                if (results != null) {
                    if ("index".equals(e.getKey())) {
                        getRange(range, results);
                    } else {
                        for (String s : results) {
                            errorMsg.append(s);
                        }
                        errorLevel = createErrorStyleRange(
                                "'" + errorMsg.toString() + "'"
                                        + " detected at line "
                                        + (currentLineNo + range[frLineIndex])
                                        + " in column " + range[frColIndex],
                                e.getKey(), syntaxMap, currentLineNo, range,
                                doLogMessage);
                        editorTafTabComp.setErrorsInBulletin(errorLevel);
                        errorInTaf = true;
                    }
                }
            }
        } else {

            // The keys are: index, fatal, ident, amd, itime, vtime, group, and
            // bbb
            for (Entry<String, Object> e : parsedText.entrySet()) {
                String k = e.getKey();
                if (traceEnabled()) {
                    statusHandler.debug("The outer key is " + k);
                }
                if (!"group".equals(k) && !"bbb".equals(k)) {
                    Map<String, List<String>> m = (Map<String, List<String>>) e
                            .getValue();
                    for (Entry<String, List<String>> e2 : m.entrySet()) {
                        String k2 = e2.getKey();
                        if (traceEnabled()) {
                            statusHandler.debug("The inner key is " + k2);
                        }
                        if ("warning".equals(k2) || "error".equals(k2)
                                || "fatal".equals(k2) || "index".equals(k2)) {
                            results = e2.getValue();
                            if (results != null) {
                                if ("index".equals(k2)) {
                                    getRange(range, results);
                                } else {

                                    for (String s : results) {
                                        errorMsg.append(s);
                                    }
                                    errorLevel = createErrorStyleRange(
                                            "'" + errorMsg.toString() + "'"
                                                    + " detected at line "
                                                    + (currentLineNo
                                                            + range[frLineIndex])
                                                    + " in column "
                                                    + range[frColIndex],
                                            k2, syntaxMap, currentLineNo, range,
                                            doLogMessage);
                                    editorTafTabComp
                                            .setErrorsInBulletin(errorLevel);
                                    errorInTaf = true;
                                }
                            }
                            errorMsg.setLength(0);
                        }
                    }
                } else if ("group".equals(k)) {
                    Object o1 = e.getValue();
                    if (traceEnabled()) {
                        statusHandler.debug(o1.getClass().getName());
                    }
                    List<Map<String, Object>> lm1 = (List<Map<String, Object>>) o1;
                    for (Map<String, Object> m1 : lm1) {
                        for (Entry<String, Object> e2 : m1.entrySet()) {
                            String k2 = e2.getKey();
                            if (traceEnabled()) {
                                statusHandler
                                        .debug("The next inner key is " + k);
                            }
                            Object o2 = e2.getValue();
                            if (traceEnabled()) {
                                statusHandler.debug(o2.getClass().getName());
                            }
                            Map<String, Object> m2 = (Map<String, Object>) o2;
                            for (Entry<String, Object> e3 : m2.entrySet()) {
                                if (traceEnabled()) {
                                    statusHandler
                                            .debug("The next next inner key is "
                                                    + e3.getKey());
                                }
                                Object o3 = e3.getValue();
                                if (traceEnabled()) {
                                    statusHandler
                                            .debug(o3.getClass().getName());
                                }
                                if (o3 instanceof Map) {
                                    Map<String, Object> m3 = (Map<String, Object>) o3;
                                    for (Entry<String, Object> e4 : m3
                                            .entrySet()) {
                                        String k4 = e4.getKey();
                                        if (traceEnabled()) {
                                            statusHandler
                                                    .debug("The next next next inner key is "
                                                            + k4);
                                            Object o4 = e4.getValue();
                                            statusHandler.debug(
                                                    o4.getClass().getName());
                                        }
                                        if ("warning".equals(k4)
                                                || "error".equals(k4)
                                                || "fatal".equals(k4)
                                                || "index".equals(k4)) {
                                            results = (List<String>) e4
                                                    .getValue();
                                            if (results != null) {
                                                if ("index".equals(k4)) {
                                                    getRange(range, results);
                                                } else {
                                                    for (String s : results) {
                                                        errorMsg.append(s);
                                                    }
                                                    if (traceEnabled()) {
                                                        StringBuilder sb = new StringBuilder();
                                                        sb.append(
                                                                "The result is: At line ")
                                                                .append(range[frLineIndex])
                                                                .append(" in column ")
                                                                .append(range[frColIndex])
                                                                .append("...\n")
                                                                .append(errorMsg);
                                                        statusHandler.debug(
                                                                sb.toString());
                                                    }
                                                    errorLevel = createErrorStyleRange(
                                                            "'" + errorMsg
                                                                    .toString()
                                                                    + "'"
                                                                    + " detected at line "
                                                                    + (currentLineNo
                                                                            + range[frLineIndex])
                                                                    + " in column "
                                                                    + range[frColIndex],
                                                            k4, syntaxMap,
                                                            currentLineNo,
                                                            range,
                                                            doLogMessage);
                                                    editorTafTabComp
                                                            .setErrorsInBulletin(
                                                                    errorLevel);
                                                    errorInTaf = true;
                                                }
                                            }
                                            errorMsg.setLength(0);
                                        }
                                    }
                                }
                            }

                        }
                    }
                } else if ("bbb".equals(k) && traceEnabled()) {
                    statusHandler.debug(e.getValue().getClass().getName());
                }
            }
        }

        return errorInTaf;
    }

    /**
     * This parses strings to get the from line/column and to line/column as
     * integer values. This assumes range is an array of length four and that
     * results contains two strings in the form:
     * <li>"line#.column#"</li>
     * <li>thus results ["2.4", "3.10"] would place in range the values [2, 4,
     * 3, 10]</li>
     *
     * @param range
     *            - An array to place the parsed results indices in the order
     *            from line/column then to line/column
     * @param results
     *            - The strings to parse to get the range
     */
    private void getRange(int[] range, List<String> results) {
        String[] fixedNumber = results.get(0).split("\\.");
        range[frLineIndex] = Integer.parseInt(fixedNumber[0]);
        range[frColIndex] = Integer.parseInt(fixedNumber[1]);
        fixedNumber = results.get(1).split("\\.");
        range[toLineIndex] = Integer.parseInt(fixedNumber[0]);
        range[toColIndex] = Integer.parseInt(fixedNumber[1]);
    }

    /**
     * Use the Python updateTimes to adjust timestamps in the TAFs.
     */
    @SuppressWarnings("unchecked")
    private void updateTimes() {
        // Assume editorTafTabComp is for the active tab.
        st = editorTafTabComp.getTextEditorControl();
        String text = st.getText();
        int idx1 = 0;

        if (text.indexOf("TAF", idx1) <= -1) {
            msgStatComp.setMessageText("Keyword 'TAF' not found",
                    errorColor.getRGB());
            return;
        }

        List<String> updatedTafs = new ArrayList<>();

        while (idx1 > -1) {
            int idx2 = text.indexOf("TAF", idx1 + 1);
            String taf;
            if (idx2 > -1) {
                taf = text.substring(idx1, idx2);
            } else {
                taf = text.substring(idx1);
            }

            // update the time in taf
            String bbb = editorTafTabComp.getBBB().trim();

            IPathManager pm = PathManagerFactory.getPathManager();
            LocalizationContext baseContext = pm.getContext(
                    LocalizationType.COMMON_STATIC, LocalizationLevel.BASE);
            File baseFile = pm.getFile(baseContext,
                    "aviation/python/TafDecoder.py");
            LocalizationContext baseCtx = pm.getContext(
                    LocalizationType.COMMON_STATIC, LocalizationLevel.BASE);
            // need getFile to pull whole python dir from localization server
            pm.getLocalizationFile(baseCtx, "aviation/python").getFile();
            File baseDir = pm.getFile(baseContext, "aviation/python");
            Map<String, Object> resultMap = null;
            Map<String, Object> args = new HashMap<>();
            String includePath = PyUtil.buildJepIncludePath(
                    getCommonPythonDir(), baseDir.getPath());
            JepConfig config = new JepConfig().setIncludePath(includePath)
                    .setClassLoader(TafViewerEditorDlg.class.getClassLoader());
            try (PythonScript python = new PythonScript(config,
                    baseFile.getPath())) {
                python.instantiatePythonClass("parser", "Decoder", null);
                args.put("text", taf);
                args.put("bbb", bbb);
                Object com = python.execute("updateTime", "parser", args);
                resultMap = (Map<String, Object>) com;
            } catch (JepException e) {
                String msg = "An error occured while running TafDecoder.py: "
                        + e.getMessage();
                msgStatComp.setMessageText(msg, errorColor.getRGB());
                statusHandler.handle(Priority.PROBLEM, msg, e);
                return;
            }

            // if taf is the first taf in text, update the issue time textbox
            if (updatedTafs.isEmpty()) {
                String newTime = (String) resultMap.get("headerTime");
                editorTafTabComp.setLargeTF(newTime);
            }

            // add the updated taf tex to the updateTafs list
            updatedTafs.add((String) resultMap.get("text"));

            idx1 = idx2;
        }

        StringBuilder sb = new StringBuilder();

        for (String taf : updatedTafs) {
            sb.append(taf);
            sb.append("\n");
        }

        st.setText(sb.toString());
    }

    /**
     * Syntax checker parser/decoder for TAF using Python.
     *
     * @param text
     *            -- the TAF
     * @return -- the decoded TAF
     */
    @SuppressWarnings("unchecked")
    private Map<String, Object> parseText(String text, String bbb) {

        IPathManager pm = PathManagerFactory.getPathManager();

        LocalizationContext baseContext = pm.getContext(
                LocalizationType.COMMON_STATIC, LocalizationLevel.BASE);
        File baseFile = pm.getFile(baseContext,
                "aviation" + IPathManager.SEPARATOR + "python"
                        + IPathManager.SEPARATOR + "TafDecoder.py");
        File baseDir = pm.getFile(baseContext,
                "aviation" + IPathManager.SEPARATOR + "python");
        LocalizationContext baseCtx = pm.getContext(
                LocalizationType.COMMON_STATIC, LocalizationLevel.BASE);
        // need getFile to pull whole python dir from localization server
        pm.getLocalizationFile(baseCtx, "aviation/python").getFile();

        Map<String, Object> resultMap = null;
        Map<String, Object> argMap = new HashMap<>();
        try {
            if (parsePythonScript == null) {
                String includePath = PyUtil.buildJepIncludePath(
                        getCommonPythonDir(), baseDir.getPath());
                JepConfig config = new JepConfig().setIncludePath(includePath)
                        .setClassLoader(
                                TafViewerEditorDlg.class.getClassLoader());
                parsePythonScript = new PythonScript(config,
                        baseFile.getPath());
            }
            parsePythonScript.instantiatePythonClass("parser", "Decoder", null);
            argMap.put("text", text);
            argMap.put("bbb", bbb);
            Object com = parsePythonScript.execute("parseFromJava", "parser",
                    argMap);
            resultMap = (Map<String, Object>) com;
        } catch (JepException e) {
            statusHandler.error("Error parsing TAF", e);
        }
        return resultMap;
    }

    private List<String> getSitesInTaf(String text) {
        List<String> result = new ArrayList<>();
        String[] fcsts = text.split(SPLIT_REGEX);
        for (String f : fcsts) {
            String[] split = f.split("\n");
            for (int i = 0; i < split.length; i++) {
                if (split[i].startsWith("TAF")) {
                    if (i + 1 < split.length) {
                        String site = split[i + 1].substring(0, 4);
                        result.add(site);
                    }
                    break;
                } else if (i == 0 && split[i].matches("^[A-Z]{4}\\s.*$")) {
                    String site = split[i].substring(0, 4);
                    result.add(site);
                    break;
                }
            }
        }
        return result;
    }

    /**
     * Method to log a message when an error is detected by the syntax checker
     * and set up text highlighting.
     *
     * @param msg
     *            - error message to dispaly
     * @param key
     *            - error type
     * @param syntaxMap
     *            - Map to add error highlighting to
     * @param currentLineNo
     *            - Current line number range values are relative to
     * @param range
     *            array of from line/column and to line/column of error location
     *            in the text
     * @param doLogMessage
     *            - flag to indicate if error message should be displayed
     * @return errorLevel serverity level of this error
     */
    private int createErrorStyleRange(String msg, String key,
            Map<StyleRange, String> syntaxMap, int currentLineNo, int[] range,
            boolean doLogMessage) {
        int errorLevel;
        Color color;

        if ("warning".equals(key)) {
            errorLevel = 1;
            color = qcColors[errorLevel];
        } else if ("error".equals(key)) {
            errorLevel = 2;
            color = qcColors[errorLevel];
        } else if ("fatal".equals(key)) {
            errorLevel = 3;
            color = qcColors[errorLevel];
        } else {
            color = errorColor;
            errorLevel = 2;
        }

        setSyntaxErrorLevel(errorLevel);
        StyledText st = editorTafTabComp.getTextEditorControl();
        if (st.getContent().getLineCount() > currentLineNo + range[frLineIndex]
                - 1) {
            int start = st.getOffsetAtLine(
                    currentLineNo + range[frLineIndex] - 1) + range[frColIndex];
            int end = st.getOffsetAtLine(currentLineNo + range[toLineIndex] - 1)
                    + range[toColIndex];
            StyleRange sr = new StyleRange(start, end - start, null, color);

            if (syntaxMap != null) {
                syntaxMap.put(sr, msg);
            }
        }

        if (doLogMessage) {
            msgStatComp.setMessageText(msg, color.getRGB());
        }

        return errorLevel;
    }

    @Override
    public void updateInsert(boolean updateInsertChk) {
        if (updateInsertChk) {
            // Change the state of the insert check
            insertChk.setSelection(!insertChk.getSelection());
        }
        // Loop and set all of the editors to the update insert state
        for (TabItem editTafTabItem : editorTafTabs) {
            EditorTafTabComp tafTabComp = (EditorTafTabComp) editTafTabItem
                    .getControl();
            tafTabComp.getTextEditorControl().invokeAction(ST.TOGGLE_OVERWRITE);
        }
    }

    @Override
    public void updateWordWrap(boolean wrap) {
        // Loop and set all of the editors to the update WordWrap state
        for (TabItem editTafTabItem : editorTafTabs) {
            EditorTafTabComp tafTabComp = (EditorTafTabComp) editTafTabItem
                    .getControl();
            tafTabComp.getTextEditorControl().setWordWrap(wrap);
        }
    }

    /**
     * Update METAR and MOS data in the view tab of the viewer/editor.
     */
    private void updateViewerTab(String siteID) {
        // Loop and update the guidance viewers.
        TabItem tbi = guidanceViewerFolder
                .getItem(guidanceViewerFolder.getSelectionIndex());
        if (tbi.getControl() instanceof ViewerTab) {
            String id = ((ViewerTab) tbi.getControl()).getSite(siteID);
            ((ViewerTab) tbi.getControl()).generateGuidance(id);
        }
    }

    @SuppressWarnings("unchecked")
    public void qcCheckSetup(boolean doQcDialog) {
        // Assume editorTafTabComp is for the active tab.
        if (!editorTafTabComp.isSyntaxChecked()) {
            putMessageToForecaster("Press Syntax before QC'ing");
            return;
        }

        if (!editorTafTabComp.isQcSkipCheck()
                && editorTafTabComp.isErrorsInBulletin()) {
            MessageBox bulletinContinueMB = new MessageBox(shell,
                    SWT.ICON_QUESTION | SWT.OK | SWT.CANCEL);
            bulletinContinueMB
                    .setMessage("Bulletin has errors\n" + "Continue?");
            int result = bulletinContinueMB.open();

            if (result != SWT.OK) {
                return;
            }

            editorTafTabComp.setQcSkipCheck();
        }

        if (doQcDialog) {
            if (mustCreate(qcDlg)) {
                qcDlg = new QcDialog(shell, savedQcItems);
                qcDlg.addCloseCallback(new ICloseCallback() {

                    @Override
                    public void dialogClosed(Object returnValue) {
                        String tafText = editorTafTabComp.getTextEditorControl()
                                .getText();
                        List<String> sitesInTaf = getSitesInTaf(tafText);
                        if (returnValue instanceof HashMap<?, ?>) {
                            Map<String, String> qcItems = (Map<String, String>) returnValue;
                            Map<String, Map<String, String>> qcMap = new HashMap<>();
                            for (String site : sitesInTaf) {
                                qcMap.put(site, qcItems);
                            }
                            qcCheck(qcMap);
                        }
                    }
                });
                qcDlg.open();
            } else {
                qcDlg.bringToTop();
            }
        } else {
            String tafText = editorTafTabComp.getTextEditorControl().getText();
            List<String> sitesInTaf = getSitesInTaf(tafText);
            Map<String, Map<String, String>> qcMap = null;
            qcMap = new HashMap<>();

            for (String site : sitesInTaf) {
                qcMap.put(site, null);
            }
            qcCheck(qcMap);
        }
    }

    /**
     * Perform the quality checks.
     */
    @SuppressWarnings("unchecked")
    private void qcCheck(Map<String, Map<String, String>> qcMap) {
        try {
            setWaitCursor(true);
            ITafSiteConfig config = TafSiteConfigFactory.getInstance();
            List<Object> tafs = new ArrayList<>();
            Map<String, Object> siteInfo = new HashMap<>();
            Map<String, Object> qcItems = new HashMap<>();

            // Assume editorTafTabComp is for the active tab.
            st = editorTafTabComp.getTextEditorControl();
            st.setStyleRange(null);
            st.setToolTipText(null);
            String bbb;

            bbb = editorTafTabComp.getBBB().trim();

            if (bbb.length() == 0) {
                bbb = null;
            }

            final Map<StyleRange, String> qcResultMap = new HashMap<>();
            st.addMouseTrackListener(new MouseTrackAdapter() {
                @Override
                public void mouseHover(MouseEvent e) {
                    Point p = new Point(e.x, e.y);
                    try {
                        int offset = st.getOffsetAtLocation(p);
                        StyleRange[] srs = st.getStyleRanges();

                        if (srs.length > 0) {
                            StyleRange sr = null;
                            for (StyleRange range : srs) {
                                if (offset >= range.start
                                        && offset <= range.start
                                                + range.length) {
                                    sr = range;
                                    break;
                                }
                            }
                            if (sr != null) {
                                st.setToolTipText(qcResultMap.get(sr));
                            } else {
                                st.setToolTipText(null);
                            }
                        } else {
                            st.setToolTipText(null);
                        }
                    } catch (IllegalArgumentException ex) {
                        // No text under cursor
                        st.setToolTipText(null);
                    }
                }
            });

            Set<String> sites = qcMap.keySet();

            for (String site : sites) {
                TafSiteData data = config.getSite(site);
                Map<String, Object> info = new HashMap<>();
                Map<String, Object> temp = new HashMap<>();

                // qc
                temp = new HashMap<>();
                Map<String, String> items = qcMap.get(site);

                if (items == null) {
                    String wx;
                    String climate;
                    String impact;

                    if (data.currentWxQc) {
                        wx = "1";
                    } else {
                        wx = "0";
                    }

                    if (data.climateQc) {
                        climate = "1";
                    } else {
                        climate = "0";
                    }

                    if (data.impactQc) {
                        impact = "1";
                    } else {
                        impact = "0";
                    }

                    temp.put("currentwx", wx);
                    temp.put("climate", climate);
                    temp.put("impact", impact);
                } else {
                    temp.put("currentwx", items.get("currentwx"));
                    temp.put("climate", items.get("climate"));
                    temp.put("impact", items.get("impact"));
                    savedQcItems = items;
                }

                info.put("qc", temp);
                qcItems.put(site, temp);
                siteInfo.put(site, info);
            }

            String in = editorTafTabComp.getTextEditorControl().getText()
                    .trim();
            int idx = 0;
            int idx2 = 0;

            if (in.indexOf("TAF", idx) <= -1) {
                msgStatComp.setMessageText("Keyword 'TAF' not found",
                        errorColor.getRGB());
            } else {
                // TODO use the SPLIT_REGEX instead
                while (idx > -1 && idx2 > -1) {
                    idx = in.indexOf("TAF", idx);
                    idx2 = in.indexOf("TAF", idx + 3);
                    String tafStr;

                    if (idx > -1 && idx2 > -1) {
                        tafStr = in.substring(idx, idx2);
                        idx += 3;
                    } else {
                        tafStr = in.substring(idx);
                    }

                    tafs.add(tafStr.trim());
                }

                // Run the QC checks from python
                Map<List<String>, Object> results = runPythonQC(tafs, sites,
                        qcItems, bbb);

                if (results == null) {
                    setMessageStatusError(
                            "An Error occured while performing the QC check.");
                    return;
                }

                // Display the QC results
                int beginIndex = 0;
                int endIndex = in.indexOf('\n');
                if (endIndex == -1) {
                    endIndex = in.length();
                }
                String thisSite = "";
                String lastLine = "";
                String line = in.substring(beginIndex, endIndex);
                int lineNumber = 1;
                Set<List<String>> keySet = results.keySet();
                int maxLevel = 0;

                while (endIndex < in.length()) {
                    String text = null;
                    int level = 0;
                    int start = -1;
                    int length = -1;

                    if (line.startsWith("TAF")) {
                        thisSite = in.substring(endIndex + 1, endIndex + 5);
                    }

                    if (!line.startsWith("TAF")) {
                        if (line.trim().length() == 0) {
                            beginIndex = endIndex + 1;
                            endIndex = in.indexOf('\n', beginIndex);

                            if (beginIndex >= in.length()) {
                                beginIndex = in.length() - 1;
                            }

                            if (endIndex == -1) {
                                endIndex = in.length();
                            }

                            lastLine = line;
                            line = in.substring(beginIndex, endIndex);
                            continue;
                        }

                        // Get the QC result text
                        for (List<String> key : keySet) {
                            String temp = key.get(0);
                            int keyLineNum = Integer.parseInt(
                                    temp.substring(0, temp.indexOf('.')));
                            Map<String, Object> result = (Map<String, Object>) results
                                    .get(key);

                            if (lineNumber == keyLineNum) {
                                if (!isWrappingLine(line, thisSite)) {
                                    text = result.get("text").toString() + "\n";
                                    level = Integer.parseInt(
                                            result.get("level").toString());
                                    start = Integer.parseInt(temp
                                            .substring(temp.indexOf('.') + 1));
                                    temp = key.get(1);
                                    length = Integer.parseInt(temp
                                            .substring(temp.indexOf('.') + 1));
                                    length = length - start;
                                    start = beginIndex + start;
                                    break;
                                } else {
                                    // a PROB30 group is wrapped in two lines
                                    text = result.get("text").toString() + "\n";
                                    level = Integer.parseInt(
                                            result.get("level").toString());
                                    start = beginIndex - 1 - lastLine.length()
                                            + lastLine.indexOf("PROB30");
                                    length = lastLine
                                            .substring(
                                                    lastLine.indexOf("PROB30"))
                                            .length() + 1 + line.length();
                                    break;
                                }
                            }
                        }

                        // Create a StyleRange
                        if (text != null) {
                            if (start < 0 && length < 1) {
                                length = line.length() - 1;
                                start = 13;
                                length = length - start;
                                start = beginIndex + start;
                            }

                            StyleRange sr = new StyleRange(start, length, null,
                                    qcColors[level]);
                            st.setStyleRange(sr);
                            qcResultMap.put(sr, text);

                            if (level > maxLevel) {
                                maxLevel = level;
                            }
                        }
                    }

                    beginIndex = endIndex + 1;
                    endIndex = in.indexOf('\n', beginIndex);

                    if (beginIndex >= in.length()) {
                        beginIndex = in.length() - 1;
                    }

                    if (endIndex == -1) {
                        endIndex = in.length() - 1;
                    }

                    if (beginIndex == endIndex
                            && beginIndex == in.length() - 1) {
                        beginIndex = endIndex = in.length();
                        line = "";
                    } else {
                        lastLine = line;
                        line = in.substring(beginIndex, endIndex);
                    }

                    lineNumber++;
                }

                if (maxLevel > 0) {
                    setMessageStatusError("Inconsistencies found.");
                } else {
                    setMessageStatusOK("Checks successful.");
                }
            }
        } catch (Exception e) {
            String msg = "An Error occured while performing the QC check.";
            setMessageStatusError(msg);
            statusHandler.handle(Priority.PROBLEM, msg, e);
        } finally {
            setWaitCursor(false);
        }
    }

    private boolean isWrappingLine(String line, String site) {
        String tempLine = line.trim();
        if (tempLine.startsWith(site) || tempLine.startsWith("TEMPO")
                || tempLine.startsWith("FM") || tempLine.startsWith("PROB30")) {
            return false;
        }
        return true;
    }

    /**
     * Read in the TAF viewer editor config XML.
     *
     * @return An array of viewer tab configuration data.
     */
    private ArrayList<ViewerTabConfig> getTafViewerEditorCfg() {
        TafViewerEditorConfig tvec = null;
        String fs = IPathManager.SEPARATOR;

        try {
            IPathManager pm = PathManagerFactory.getPathManager();
            File path = pm.getStaticFile("aviation" + fs + "config" + fs + "gui"
                    + fs + "TafViewerEditorCfg.xml");

            tvec = JAXB.unmarshal(path, TafViewerEditorConfig.class);
        } catch (Exception e) {
            statusHandler.error("Error reading taf viewer editor config", e);
        }

        return tvec.getViewerTabs();
    }

    private SyntaxMonitorCfg getSyntaxMonitorCfg() {
        SyntaxMonitorCfg syntaxMonCfg = null;
        String fs = IPathManager.SEPARATOR;

        try {
            IPathManager pm = PathManagerFactory.getPathManager();
            File path = pm.getStaticFile("aviation" + fs + "config" + fs + "gui"
                    + fs + "SyntaxMonitorCfg.xml");

            syntaxMonCfg = JAXB.unmarshal(path, SyntaxMonitorCfg.class);

        } catch (Exception e) {
            statusHandler.error("Error reading syntax monitor config", e);
        }

        return syntaxMonCfg;
    }

    /**
     * Call the python QC check method
     *
     * @param tafs
     *            ArrayList<Object> of decoded TAFs.
     * @param siteinfo
     *            HashMap<String, Object> containing TAF site info for each
     *            site.
     * @param items
     *            HashMap<String, Object> containing information about which QC
     *            checks to run for each site.
     * @return results map
     */
    @SuppressWarnings("unchecked")
    private Map<List<String>, Object> runPythonQC(List<Object> tafs,
            Set<String> sites, Map<String, Object> items, String bbb) {

        IPathManager pm = PathManagerFactory.getPathManager();
        File runner = pm.getStaticFile("aviation/python/TafQC.py");
        LocalizationContext baseCtx = pm.getContext(
                LocalizationType.COMMON_STATIC, LocalizationLevel.BASE);
        // need getFile to pull whole python dir from localization server
        pm.getLocalizationFile(baseCtx, "aviation/python").getFile();
        String filePath = runner.getPath();
        String includePath = PyUtil.buildJepIncludePath(
                runner.getParentFile().getPath(), AvnPyUtil.getPointDataDir(),
                AvnPyUtil.getLoggingHandlerDir(),
                AvnPyUtil.getCommonPythonDir());
        String dataDir = VizApp.getDataDir() + "/aviation/";
        JepConfig config = new JepConfig().setIncludePath(includePath)
                .setClassLoader(TafViewerEditorDlg.class.getClassLoader());

        try (PythonScript python = new PythonScript(config, filePath)) {
            Map<String, Object> args = new HashMap<>();
            args.put("text", tafs);
            args.put("sites", sites);
            args.put("items", items);
            args.put("bbb", bbb);
            args.put("dataDir", dataDir);
            python.instantiatePythonClass("tafqc", "TafQC", null);
            Object map = python.execute("qcFromJava", "tafqc", args);
            Map<List<String>, Object> resultMap = (Map<List<String>, Object>) map;
            return resultMap;
        } catch (JepException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Python error occurred while executing QC - see message details.",
                    e);
            return null;
        }
    }

    public void populateTafViewer() {
        tafViewerStTxt.setText("");
        int n = Integer
                .valueOf(numTafsCbo.getItem(numTafsCbo.getSelectionIndex()));
        if (stationName != null) {
            tafsInViewer = TafUtil.getLatestTafs(stationName, n);
        }
        StringBuilder sb = new StringBuilder();
        boolean showHeaders = showHeadersChk.getSelection();
        if (tafsInViewer != null) {
            for (TafRecord t : tafsInViewer) {
                sb.append(TafUtil.safeFormatTaf(t, showHeaders));
                sb.append("\n");
            }
        }

        tafViewerStTxt.setText(sb.toString());
        hightlightTAF();
    }

    /**
     * Highlight desired values in the first TAF in the viewer.
     */
    private void hightlightTAF() {
        tafViewerStTxt.setStyleRange(null);

        // Determine if metar viewer is in a state to do highlight.
        if (metarViewer == null) {
            return;
        }

        if (!metarViewer.highlightAlerts()) {
            return;
        }

        Map<String, String[]> alertMap = TafMonitorDlg
                .getCurrentAlertMap(stationName);

        if (alertMap == null || alertMap.size() == 0) {
            return;
        }

        ResourceConfigMgr configMgr = ResourceConfigMgr.getInstance();
        String taf = tafViewerStTxt.getText();
        int offset = taf.indexOf("TAF");
        if (showHeadersChk.getSelection()) {
            offset = taf.indexOf("TAF", offset + 3);
        }
        try {
            int end = taf.indexOf("TAF", offset + 3);
            if (end > 0) {
                taf = taf.substring(offset, end);
            } else {
                taf = taf.substring(offset);
            }
        } catch (IndexOutOfBoundsException ex) {
            // Assume no TAF in the viewer
            return;
        }

        // DR 14570
        Map<String, String> alertTimeMap = TafMonitorDlg
                .getCurrentAlertTimeMap(stationName);

        // 20120712 for TEMPO
        String TEMPO_TXT = "TEMPO";

        if (taf.contains(TEMPO_TXT)) {

            Map<String, String[]> tempoMap = TafMonitorDlg
                    .getCurrentTempoMap(stationName);// 20120711
            if (tempoMap != null) {
                int tempoStart = taf.indexOf(TEMPO_TXT);
                // end of the TEMPO line
                int tempoEnd = taf.indexOf(TafUtil.LINE_BREAK, tempoStart);

                StringBuilder str = new StringBuilder(" ");

                for (Entry<String, String[]> tempoEntry : tempoMap.entrySet()) {
                    String alertKey = tempoEntry.getKey();
                    if (traceEnabled()) {
                        statusHandler.debug("2___alertKey: " + alertKey);
                    }
                    for (String value : tempoEntry.getValue()) {
                        if (traceEnabled()) {
                            statusHandler.debug("3___value: " + value);
                        }
                        str.setLength(1);
                        str.append(value);
                        int len = str.length();
                        str.append(" ");

                        // for tempo only
                        int startIndex = taf.indexOf(str.toString(),
                                tempoStart);

                        if (startIndex < 0) {
                            str.setLength(len);
                            str.append("\n");
                            startIndex = taf.indexOf(str.toString());
                        }
                        /* within tempo line */
                        if (startIndex >= 0 && startIndex < tempoEnd) {
                            StyleRange sr = new StyleRange(
                                    offset + startIndex + 1, len - 1, null,
                                    configMgr.getViwerAlertColor());

                            tafViewerStTxt.setStyleRange(sr);
                        }
                    }
                }
            }
        }
        // END 20120712 for TEMPO

        StringBuilder str = new StringBuilder(" ");
        for (Entry<String, String[]> alertEntry : alertMap.entrySet()) {
            String alertKey = alertEntry.getKey();
            for (String value : alertEntry.getValue()) {
                str.setLength(1);
                str.append(value);
                int len = str.length();
                str.append(" ");
                // DR 14570
                String time = alertTimeMap.get(alertKey);
                // DR 14570
                int idx = taf.indexOf(time);
                /*
                 * DR 14570: highlight after the correct time group
                 */
                int startIndex = taf.indexOf(str.toString(), idx);
                /*
                 * DR 14570: a line ends with a line_break
                 */
                int endIndex = taf.indexOf(TafUtil.LINE_BREAK, idx);
                if (startIndex < 0) {
                    str.setLength(len);
                    str.append("\n");
                    startIndex = taf.indexOf(str.toString());
                    if (startIndex < 0) {
                        str.setLength(len);
                        str.append("=");
                        startIndex = taf.indexOf(str.toString());
                    }
                }

                /* within the same line */
                if (startIndex >= 0 && startIndex < endIndex) {
                    StyleRange sr = new StyleRange(offset + startIndex + 1,
                            len - 1, null, configMgr.getViwerAlertColor());

                    tafViewerStTxt.setStyleRange(sr);
                } else {
                    // Should not get here. The first TAF in the viewer and the
                    // values in the alertMap should both be from the latest
                    // TAF. This indicates a program bug.
                    statusHandler.warn("highlightTAF unable to find: \""
                            + str.toString() + "\" in the first TAF");
                }

            }
        }
    }

    private void startAutoSaveTimer() {
        autoSaveReadJob.schedule();
    }

    private void stopAutoSaveTimer() {
        autoSaveReadJob.cancel();
        if (autoSaveWriteJob != null) {
            autoSaveWriteJob.cancel();
        }
    }

    private void putMessageToForecaster(String information) {
        MessageBox mb = new MessageBox(shell, SWT.ICON_ERROR | SWT.OK);
        mb.setMessage(information);
        mb.open();
    }

    public String getSelectedEditorText() {
        return editorTafTabComp.getTextEditorControl().getText();
    }

    public void setSelectedEditorText(String text) {
        editorTafTabComp.getTextEditorControl().setText(text);
    }

    public void populateEditorTab(String tafText, String wmoId,
            String stationId, String issueTime, String bbb) {
        if (!findEditOpenTab()) {
            populateViewerStation(ti.getText().substring(0, 4));
            guidanceSiteIdSelectionEventAction();
            return;
        }

        String siteId = tafText.substring(tafText.indexOf('\n') + 1,
                tafText.indexOf('\n') + 5);

        ti.setText(siteId + " " + bbb);
        editorTafTabComp.setWmoIdLbl(wmoId);
        editorTafTabComp.setWmoSiteLbl(stationId);
        editorTafTabComp.setLargeTF(issueTime);
        editorTafTabComp.getTextEditorControl().setText(tafText);
        editorTafTabComp.setBBB(bbb);
        populateViewerStation(siteId);
        guidanceSiteIdSelectionEventAction();
        if (editorTafTabComp.isTafSent()) {
            editorTafTabComp.updateTafSent(false);
        }
    }

    private void printForecast(String forecast) {
        PrinterData printerData = Printer.getDefaultPrinterData();

        if (printerData != null) {
            Printer printer = new Printer(printerData);
            printer.startJob("metar text");

            Rectangle clientArea = printer.getClientArea();
            Rectangle trim = printer.computeTrim(0, 0, 0, 0);
            Point dpi = printer.getDPI();
            int leftMargin = dpi.x + trim.x;
            int rightMargin = clientArea.width - dpi.x + trim.x + trim.width;
            int topMargin = dpi.y + trim.y;
            int bottomMargin = clientArea.height - dpi.y + trim.y + trim.height;

            int tabSize = 4;
            StringBuilder tabBuffer = new StringBuilder(tabSize);
            for (int i = 0; i < tabSize; i++) {
                tabBuffer.append(' ');
            }
            String tabs = tabBuffer.toString();

            GC gc = new GC(printer);

            Font printerFont = new Font(printer, "Courier", 9, SWT.NORMAL);
            gc.setFont(printerFont);
            int tabWidth = gc.stringExtent(tabs).x;
            int lineHeight = gc.getFontMetrics().getHeight();

            Color printerForegroundColor = new Color(printer,
                    RGBColors.getRGBColor("black"));
            gc.setForeground(printerForegroundColor);

            Color printerBackgroundColor = new Color(printer,
                    RGBColors.getRGBColor("white"));
            gc.setBackground(printerBackgroundColor);

            printer.startPage();
            StringBuilder wordBuffer = new StringBuilder();
            int x = leftMargin;
            int y = topMargin;
            int index = 0;
            int end = forecast.length();
            while (index < end) {
                char c = forecast.charAt(index);
                index++;
                if (c != 0) {
                    if (c == 0x0a || c == 0x0d) {
                        if (c == 0x0d && index < end
                                && forecast.charAt(index) == 0x0a) {
                            index++;
                        }
                        if (wordBuffer.length() > 0) {
                            String word = wordBuffer.toString();
                            int wordWidth = gc.stringExtent(word).x;
                            if (x + wordWidth > rightMargin) {
                                x = leftMargin;
                                y += lineHeight;
                                if (y + lineHeight > bottomMargin) {
                                    printer.endPage();
                                    if (index + 1 < end) {
                                        y = topMargin;
                                        printer.startPage();
                                    }
                                }
                            }
                            gc.drawString(word, x, y, false);
                            x += wordWidth;
                            wordBuffer = new StringBuilder();
                        }
                        x = leftMargin;
                        y += lineHeight;
                        if (y + lineHeight > bottomMargin) {
                            printer.endPage();
                            if (index + 1 < end) {
                                y = topMargin;
                                printer.startPage();
                            }
                        }
                    } else {
                        if (c != '\t') {
                            wordBuffer.append(c);
                        }
                        if (Character.isWhitespace(c)) {
                            if (wordBuffer.length() > 0) {
                                String word = wordBuffer.toString();
                                int wordWidth = gc.stringExtent(word).x;
                                if (x + wordWidth > rightMargin) {
                                    x = leftMargin;
                                    y += lineHeight;
                                    if (y + lineHeight > bottomMargin) {
                                        printer.endPage();
                                        if (index + 1 < end) {
                                            y = topMargin;
                                            printer.startPage();
                                        }
                                    }
                                }
                                gc.drawString(word, x, y, false);
                                x += wordWidth;
                                wordBuffer = new StringBuilder();
                            }
                            if (c == '\t') {
                                x += tabWidth;
                            }
                        }
                    }
                }
            }
            if (y + lineHeight <= bottomMargin) {
                printer.endPage();
            }

            printer.endJob();
            printerForegroundColor.dispose();
            printerBackgroundColor.dispose();
            gc.dispose();
            printerFont.dispose();
            printer.dispose();
        } else {
            putMessageToForecaster(
                    "Auto Print failed. No default printer set.");
        }
    }

    public void setSendCollective(boolean sendCollective) {
        sendCollectMI.setSelection(sendCollective);
    }

    private void sendTafToEditor(TafSettings type) {
        try {
            StringBuilder sb = new StringBuilder();
            if (tafsInViewer != null && tafsInViewer.length > 0) {
                String site = siteIdCbo.getItem(siteIdCbo.getSelectionIndex());
                updateSettings(TafSettings.OPEN_EDIT, site);
                // Only load the latest TAF, and assume it is the first one in
                // the Viewer.
                sb.append(TafUtil.safeFormatTaf(tafsInViewer[0], false));
                String[] header = tafsInViewer[0].getWmoHeader().split(" ");

                ITafSiteConfig config = TafSiteConfigFactory.getInstance();
                TafSiteData siteData = config.getSite(site);

                String wmoId = siteData.wmo.split(" ")[0];
                String stationId = siteData.wmo.split(" ")[1];
                String issueTime = header[2];

                String bbb = "";
                if (type == null) {
                    // Do nothing
                } else if (type == TafSettings.OPEN_AMD) {
                    bbb = "AAX";
                } else if (type == TafSettings.OPEN_RTD) {
                    bbb = "RRX";
                } else if (type == TafSettings.OPEN_COR) {
                    bbb = "CCX";
                }

                populateEditorTab(sb.toString().trim(), wmoId, stationId,
                        issueTime, bbb);
            }
            showDialog();
        } catch (ConfigurationException | IOException e) {
            String msg = "Error: Unable to load site configuration data.";
            setMessageStatusError(msg);
            statusHandler.handle(Priority.PROBLEM, msg, e);
        }
    }

    private void guidanceSiteIdSelectionEventAction() {
        if (stationName == null || !stationName
                .equals(siteIdCbo.getItem(siteIdCbo.getSelectionIndex()))) {
            // Get the selection from the combo box and update forecast
            // model.
            stationName = siteIdCbo.getItem(siteIdCbo.getSelectionIndex());
            // Update the taf in the viewer tab.
            populateTafViewer();

            // Mark tab displays no longer current.
            markTabsAsNotCurrent();

            // Update the metar and mos guidance in the viewer tab.
            updateViewerTab(stationName);
        }
    }

    /**
     * Finds first open tab and select it for use. Send error message when no
     * available tab.
     *
     * @return true when open tab found otherwise false
     */
    private boolean findEditOpenTab() {
        boolean foundOpenTab = false;
        for (int i = 0; i < editorTafTabs.size(); ++i) {
            TabItem tabItem = editorTafTabs.get(i);
            EditorTafTabComp tafTabComp = (EditorTafTabComp) tabItem
                    .getControl();
            if (tabItem.getText().equals(tabFillText)
                    || tafTabComp.isTafSent()) {
                foundOpenTab = true;
                setEditTabSelection(i);
                break;
            }
        }
        if (!foundOpenTab) {
            userInformation("Clear page first");
        }
        return foundOpenTab;
    }

    /**
     * TabFolder's setSelection cannot be overridden and it doesn't notify its
     * listeners when called; use this instead so needed class variables stay
     * syncronized with selected tab.
     *
     * @param tabIndex
     */
    private synchronized void setEditTabSelection(int tabIndex) {
        editorTabFolder.setSelection(tabIndex);
        ti = editorTafTabs.get(editorTabFolder.getSelectionIndex());
        editorTafTabComp = (EditorTafTabComp) ti.getControl();
    }

    class TextUpdater implements Runnable {
        private AvnSmartToolFinishedListener finish;

        public void setFinishListerner(AvnSmartToolFinishedListener fin) {
            this.finish = fin;
        }

        @Override
        public void run() {
            final EditorTafTabComp myEditorTafTabComp = editorTafTabComp;
            try {
                while (true) {
                    if (!finish.isDone()) {
                        Thread.sleep(100L);
                    } else {
                        if (finish.getResult() != null) {
                            if (!TafViewerEditorDlg.this.display.isDisposed()) {
                                VizApp.runSync(new Runnable() {
                                    @Override
                                    public void run() {
                                        myEditorTafTabComp
                                                .getTextEditorControl()
                                                .setText(finish.getResult());
                                    }
                                });
                            }
                        }
                        break;
                    }
                }
            } catch (InterruptedException e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Avn Taf Viewer updater thread interrupted", e);
            } finally {
                if (!TafViewerEditorDlg.this.shell.isDisposed()) {
                    VizApp.runSync(new Runnable() {
                        @Override
                        public void run() {
                            setWaitCursor(false);
                        }
                    });
                }
            }
        }
    }

    @Override
    public String getCurrentViewerStation() {
        return stationName;
    }

    @Override
    public List<ViewerTab> getViewerTabList() {
        return modelsTabs;
    }

    /**
     * Set synatax error level to errorLevel when errorLevel is higher then the
     * current syntax error level.
     *
     * @param errorLevel
     */
    private void setSyntaxErrorLevel(int errorLevel) {
        if (syntaxErrorLevel < errorLevel) {
            syntaxErrorLevel = errorLevel;
        }
    }

    /**
     * Reset syntax error level to lowest value.
     */
    private void clearSyntaxErrorLevel() {
        syntaxErrorLevel = 0;
    }

    /**
     * This dialog is created but not immediately displayed thus components have
     * not been created. Some of the ITafSettable methods attempt to access
     * components prior to showing the dialog. This check must be done to force
     * the compoents creation.
     */
    private final void checkDlg() {
        if (shell == null) {
            open();
        }
    }

    /**
     * Validate CAVE can send a TAF.
     *
     * @return true when in real time.
     */
    private boolean validateTime() {
        if (shell != null && !shell.isDisposed() && shell.isVisible()) {
            /*
             * Currently practice mode not supported.
             */
            if (CAVEMode.getMode() == CAVEMode.PRACTICE) {
                putMessageToForecaster(
                        "Not allowed to send TAF when in practice mode.");
                return false;
            } else if (!SimulatedTimeOperations.isTransmitAllowed()) {
                SimulatedTimeOperations.displayFeatureLevelWarning(shell,
                        "Send TAF");
                return false;
            }
        }
        return true;
    }

    @Override
    public void timechanged() {
        VizApp.runAsync(new Runnable() {

            @Override
            public void run() {
                validateTime();
            }
        });
    }

    /**
     *
     * @return
     */
    private boolean traceEnabled() {
        /*
         * TODO When alertviz respects Prioirty change this to use the
         * statusHander priority debug enabled.
         */
        return trace;
    }

    private void handleSaveAs() {
        String tempTafPath = "aviation/tmp/";
        IPathManager pm = PathManagerFactory.getPathManager();
        LocalizationContext context = pm.getContext(
                LocalizationType.COMMON_STATIC, LocalizationLevel.SITE);
        String path = pm.getFile(context, tempTafPath).getAbsolutePath();
        String filepath = null;

        File tmp = new File(path);
        if (!tmp.exists()) {
            tmp.mkdirs();
        }

        FileDialog dlg = new FileDialog(shell, SWT.SAVE);
        dlg.setFilterPath(path);
        filepath = dlg.open();

        /**
         * If a user selected a location other than the "aviation/tmp"
         * directory, save the file to the location the user selected
         */
        String dirPath = filepath.substring(0, filepath.lastIndexOf('/'));
        if (!dirPath.equals(path)) {
            saveFileToUserSelectedLocation(filepath);
            return;
        }

        if (filepath != null) {
            try {
                setWaitCursor(true);
                String fname = tempTafPath
                        + filepath.substring(filepath.lastIndexOf('/') + 1);
                LocalizationFile lFile = pm.getLocalizationFile(context, fname);
                File file = lFile.getFile();

                if (file.exists()) {
                    MessageBox questionMB = new MessageBox(shell,
                            SWT.ICON_WARNING | SWT.OK | SWT.CANCEL);
                    questionMB.setText("Save TAF");
                    questionMB.setMessage(
                            "File already exists. Do you want to overwrite it?");
                    int result = questionMB.open();

                    if (result == SWT.CANCEL) {
                        return;
                    }
                }

                try (FileWriter writer = new FileWriter(file);
                        BufferedWriter output = new BufferedWriter(writer)) {
                    // Save WMO, wmo site, issue time, and BBB on first line
                    output.write(editorTafTabComp.getWmoId());
                    output.write("\t");
                    output.write(editorTafTabComp.getWmoSiteId());
                    output.write("\t");
                    output.write(editorTafTabComp.getLargeTF());
                    output.write(System.getProperty("line.separator"));
                    output.write(
                            editorTafTabComp.getTextEditorControl().getText());
                    output.close();

                    lFile.save();

                    setMessageStatusOK(
                            "File " + filepath + " saved successfully.");
                }
            } catch (FileNotFoundException e) {
                String msg = "Unable to open file " + filepath
                        + " for writing.";
                statusHandler.handle(Priority.DEBUG, msg, e);
                setMessageStatusError(msg);

            } catch (IOException e) {
                String msg = "An IOException occured while saving file "
                        + filepath;
                statusHandler.handle(Priority.DEBUG, msg, e);
                setMessageStatusError(msg);
            } catch (LocalizationException e) {
                String msg = "A LocalizationException occured while saving file "
                        + filepath;
                statusHandler.handle(Priority.DEBUG, msg, e);
                setMessageStatusError(msg);
            } finally {
                setWaitCursor(false);
            }
        }
    }

    private void saveFileToUserSelectedLocation(String filepath) {
        try {
            setWaitCursor(true);

            File file = new File(filepath);

            if (file.exists()) {
                MessageBox questionMB = new MessageBox(shell,
                        SWT.ICON_WARNING | SWT.OK | SWT.CANCEL);
                questionMB.setText("Save TAF");
                questionMB.setMessage(
                        "File already exists. Do you want to overwrite it?");
                int result = questionMB.open();

                if (result == SWT.CANCEL) {
                    return;
                }
            }

            try (FileWriter writer = new FileWriter(file);
                    BufferedWriter output = new BufferedWriter(writer)) {
                // Save WMO, wmo site, issue time, and BBB on first line
                output.write(editorTafTabComp.getWmoId());
                output.write("\t");
                output.write(editorTafTabComp.getWmoSiteId());
                output.write("\t");
                output.write(editorTafTabComp.getLargeTF());
                output.write(System.getProperty("line.separator"));
                output.write(editorTafTabComp.getTextEditorControl().getText());
                output.close();

                setMessageStatusOK("File " + filepath + " saved successfully.");
            }
        } catch (FileNotFoundException e) {
            String msg = "Unable to open file " + filepath + " for writing.";
            statusHandler.handle(Priority.DEBUG, msg, e);
            setMessageStatusError(msg);

        } catch (IOException e) {
            String msg = "An IOException occured while saving file " + filepath;
            statusHandler.handle(Priority.DEBUG, msg, e);
            setMessageStatusError(msg);
        } finally {
            setWaitCursor(false);
        }
    }
}
