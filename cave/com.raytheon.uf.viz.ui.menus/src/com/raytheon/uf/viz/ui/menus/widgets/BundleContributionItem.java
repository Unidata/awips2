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
package com.raytheon.uf.viz.ui.menus.widgets;

import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.TimeZone;

import org.eclipse.core.commands.Command;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.jface.action.ContributionItem;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.commands.ICommandService;

import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.menus.xml.CommonBundleMenuContribution;
import com.raytheon.uf.common.menus.xml.VariableSubstitution;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.BinOffset;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.VariableSubstitutionUtil;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.jobs.JobPool;
import com.raytheon.uf.viz.core.procedures.Bundle;
import com.raytheon.uf.viz.core.procedures.BundleUtil;
import com.raytheon.uf.viz.core.procedures.BundleUtil.BundleDataItem;
import com.raytheon.uf.viz.core.rsc.URICatalog;
import com.raytheon.uf.viz.core.rsc.URICatalog.IURIRefreshCallback;
import com.raytheon.uf.viz.ui.menus.xml.BundleMenuContribution;
import com.raytheon.viz.ui.BundleLoader;
import com.raytheon.viz.ui.BundleLoader.BundleInfoType;
import com.raytheon.viz.ui.BundleProductLoader;
import com.raytheon.viz.ui.UiUtil;
import com.raytheon.viz.ui.editor.AbstractEditor;

/**
 * Provides an Eclipse menu contribution that loads a bundle, and is decorated
 * with bundle availability times.
 * 
 * The dataURIs are utilized for the bundle availability times (this is
 * redundant in the bundle, but is necessary for performance reasons).
 * 
 * The bundle availability times are updated at two times:
 * <UL>
 * <LI>when the menu is pulled down, the times are checked to guarantee
 * consistency
 * <LI>while the menu is open, a callback is utilized to keep the menu up to
 * date
 * </UL>
 * 
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 12, 2009            chammack     Initial creation
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1.0
 */

public class BundleContributionItem extends ContributionItem {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(BundleContributionItem.class);

    protected static JobPool prepareBundleJobPool = new JobPool(
            "Preparing menu entries", 4);

    /** The bundle menu item widget */
    protected MenuItem widget;

    /** Variable substitutions from the menu files */
    protected Map<String, String> substitutions;

    protected Listener menuItemListener;

    protected Set<BundleDataItem> pdoMapList;

    protected BundleMenuContribution menuContribution;

    protected static final SimpleDateFormat DATE_FORMATTER = new SimpleDateFormat(
            "dd.HHmm");

    protected String menuText;

    protected boolean shownBefore;

    protected final static String NOT_AVAILABLE = "--.----";

    protected final static String UNKNOWN = "??.????";

    protected DataTime lastUsedTime;

    protected boolean queryPerformed = false;

    protected boolean performQuery = true;

    public BundleContributionItem(CommonBundleMenuContribution contribution,
            VariableSubstitution[] includeSubstitutions) throws VizException {
        super(VariableSubstitutionUtil.processVariables(contribution.id,
                VariableSubstitution.toMap(VariableSubstitution.combine(
                        contribution.substitutions, includeSubstitutions))));
        this.performQuery = contribution.timeQuery;
        this.menuContribution = new BundleMenuContribution();
        this.menuContribution.xml = contribution;
        // this.keySet = new HashSet<URIKey>();
        // this.adjustedKeyMap = new HashMap<URIKey, URIKey>();

        // Build the substitutions:
        // Everything defaults to the include value
        // Fill in contribution substitutions from include and possible override
        this.substitutions = VariableSubstitution.toMap(includeSubstitutions);
        if (contribution.substitutions != null) {
            HashMap<String, String> includeSubstitutionsMap = VariableSubstitution
                    .toMap(includeSubstitutions);
            for (VariableSubstitution vs : contribution.substitutions) {
                this.substitutions.put(vs.key, VariableSubstitutionUtil
                        .processVariables(vs.value, includeSubstitutionsMap));
            }
        }
        if (contribution.suppressErrors != null) {
            contribution.suppressErrors = String.valueOf(this.substitutions
                    .get(contribution.suppressErrors.substring(2,
                            contribution.suppressErrors.length() - 1)));
        }
        // Substitute the menu text
        this.menuText = VariableSubstitutionUtil.processVariables(
                menuContribution.xml.text, this.substitutions);
        if (contribution.dataURIs != null) {
            for (int i = 0; i < contribution.dataURIs.length; i++) {
                contribution.dataURIs[i] = VariableSubstitutionUtil
                        .processVariables(contribution.dataURIs[i],
                                this.substitutions);
            }
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.jface.action.ContributionItem#fill(org.eclipse.swt.widgets
     * .Menu, int)
     */
    @Override
    public void fill(Menu parent, int index) {
        if (this.menuContribution == null) {
            return;
        }

        if (widget != null || parent == null) {
            return;
        }

        MenuItem item = null;
        if (index >= 0) {
            item = new MenuItem(parent, SWT.PUSH, index);
        } else {
            item = new MenuItem(parent, SWT.PUSH);
        }

        item.setData(this);

        item.addListener(SWT.Dispose, getItemListener());
        item.addListener(SWT.Selection, getItemListener());
        item.addListener(SWT.Activate, getItemListener());
        item.getParent().addListener(SWT.Show, getItemListener());

        widget = item;
        updateMenuText();
        update(null);
    }

    protected void updateMenuTextAsync() {
        VizApp.runAsync(new Runnable() {
            @Override
            public void run() {
                updateMenuText();
            }
        });
    }

    protected synchronized void updateMenuText() {
        if (widget == null)
            return;

        if (!performQuery) {
            widget.setText(menuText);

            // notify things of menu update times
            Event event = new Event();
            event.data = widget;
            event.widget = widget;
            widget.notifyListeners(SWT.Modify, event);
            return;
        }

        String dateStr = UNKNOWN;

        boolean useReferenceTime = this.menuContribution.xml.useReferenceTime;

        if (lastUsedTime != null) {
            // We have a
            DATE_FORMATTER.setTimeZone(TimeZone.getTimeZone("UTC"));

            Date timeToUse;
            if (useReferenceTime) {
                timeToUse = lastUsedTime.getRefTime();
            } else {
                timeToUse = lastUsedTime.getValidTime().getTime();
            }

            dateStr = DATE_FORMATTER.format(timeToUse);
        } else if (this.queryPerformed) {
            // indicates that query has completed, and data is not there
            dateStr = NOT_AVAILABLE;
        }

        String labelStr = this.menuText + " \t" + dateStr;

        widget.setText(labelStr);

        // notify things of menu update times
        Event event = new Event();
        event.data = widget;
        event.widget = widget;
        widget.notifyListeners(SWT.Modify, event);
    }

    protected void updateTime(DataTime time, BinOffset offset) {
        BundleContributionItem.this.queryPerformed = true;

        if (time != null) {
            boolean useReferenceTime = BundleContributionItem.this.menuContribution.xml.useReferenceTime;

            if (offset != null) {
                time = offset.getNormalizedTime(time);
            }
            // compare refTime
            // set mostRecentProduct to key.dataTime if key.dataTime
            // is larger or mostRecentProduct is null
            if (BundleContributionItem.this.lastUsedTime == null) {
                BundleContributionItem.this.lastUsedTime = time.clone();

            } else {
                if (useReferenceTime) {
                    if (BundleContributionItem.this.lastUsedTime.getRefTime()
                            .compareTo(time.getRefTime()) < 0) {
                        BundleContributionItem.this.lastUsedTime = time.clone();
                    }
                } else {
                    if (BundleContributionItem.this.lastUsedTime
                            .compareTo(time) < 0) {
                        BundleContributionItem.this.lastUsedTime = time.clone();
                    }
                }
            }
        }

        updateMenuTextAsync();
    }

    private Listener getItemListener() {
        if (menuItemListener == null) {
            menuItemListener = new Listener() {
                @Override
                public void handleEvent(Event event) {
                    switch (event.type) {
                    case SWT.Dispose:
                        handleWidgetDispose(event);
                        break;
                    case SWT.Selection:
                        if (event.widget != null) {
                            loadBundle(event);
                        }
                        break;
                    case SWT.Show:
                        onShow();
                        break;
                    }
                }

            };
        }
        return menuItemListener;
    }

    /**
     * Called when the menu is about to be shown
     * 
     * First see if the item has ever been shown before. If not, prepare the
     * bundle (parse the metadata maps out)
     * 
     */
    protected void onShow() {
        if (performQuery) {
            if (!shownBefore) {
                shownBefore = true;
                prepareBundleJobPool.schedule(new PrepareBundleJob());
            }
        }

        if (widget != null
                && (widget.getText() == null || widget.getText().equals(""))) {
            updateMenuText();
        }
    }

    private void handleWidgetDispose(Event event) {
        if (event.widget == widget) {
            widget.removeListener(SWT.Selection, getItemListener());
            widget.removeListener(SWT.Dispose, getItemListener());
            widget = null;
        }
    }

    private void loadBundle(Event event) {
        try {
            Bundle bundle = BundleLoader.getBundle(
                    this.menuContribution.xml.bundleFile, substitutions,
                    BundleInfoType.FILE_LOCATION);
            AbstractEditor editor = UiUtil.createOrOpenEditor(
                    this.menuContribution.xml.editorType, bundle.getDisplays());
            BundleLoader loader;
            if (this.menuContribution.xml.fullBundleLoad == null
                    || this.menuContribution.xml.fullBundleLoad == false) {
                loader = new BundleProductLoader(editor, bundle);
            } else {
                loader = new BundleLoader(editor, bundle);
            }
            loader.schedule();

            if (this.menuContribution.xml.command != null) {
                ICommandService service = (ICommandService) PlatformUI
                        .getWorkbench().getService(ICommandService.class);
                try {
                    Map<String, String> parms = new HashMap<String, String>();
                    if (substitutions != null) {
                        parms = substitutions;
                    }
                    Command command = service
                            .getCommand(this.menuContribution.xml.command);
                    command.executeWithChecks(new ExecutionEvent(command,
                            parms, null, null));
                } catch (Exception e) {
                    statusHandler.handle(Priority.PROBLEM,
                            "Failed to execute command: "
                                    + this.menuContribution.xml.command, e);
                }
            }

        } catch (VizException e) {
            statusHandler.handle(Priority.PROBLEM, "Error loading bundle : "
                    + this.menuContribution.xml.bundleFile, e);
        }

    }

    private Set<BundleDataItem> loadBundleFromXml() {
        try {
            Bundle b = Bundle.unmarshalBundle(
                    PathManagerFactory.getPathManager().getStaticFile(
                            this.menuContribution.xml.bundleFile),
                    substitutions);

            return BundleUtil.extractMetadata(b);
        } catch (VizException e1) {
            e1.printStackTrace();
            return new HashSet<BundleDataItem>();
        }
    }

    @Override
    public void dispose() {
        super.dispose();
        if (widget != null) {
            widget.dispose();
            widget = null;
        }
    }

    private class BundleRefreshCallback implements IURIRefreshCallback {

        private BinOffset offset;

        /**
         * @param optional2
         * @param offset
         */
        public BundleRefreshCallback(BinOffset offset) {
            this.offset = offset;
        }

        @Override
        public void updateTime(DataTime time) {
            BundleContributionItem.this.updateTime(time, offset);
        }
    }

    protected class PrepareBundleJob implements Runnable {

        @Override
        public void run() {
            BundleContributionItem.this.pdoMapList = loadBundleFromXml();
            for (BundleDataItem d : BundleContributionItem.this.pdoMapList) {
                URICatalog.getInstance().catalogAndQueryDataURI(d.metadata,
                        new BundleRefreshCallback(d.offset));
            }

        }

    }
}
