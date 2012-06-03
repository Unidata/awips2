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
package com.raytheon.uf.viz.collaboration.ui.session;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.ListIterator;
import java.util.Map;

import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.SashForm;
import org.eclipse.swt.custom.StyledText;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Tree;
import org.eclipse.swt.widgets.TreeItem;

import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.localization.exception.LocalizationException;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.viz.collaboration.comm.provider.user.UserId;
import com.raytheon.uf.viz.collaboration.data.CollaborationDataManager;
import com.raytheon.uf.viz.collaboration.ui.session.SearchComposite.SearchText;

/**
 * Browse, view, and search messages in the archive.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 22, 2012            bgonzale     Initial creation
 * 
 * </pre>
 * 
 * @author bgonzale
 * @version 1.0
 */

public class SessionMsgArchiveBrowser extends Composite implements SearchText {

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(SessionMsgArchiveBrowser.class);

    final Color INACTIVE_COLOR = Display.getCurrent().getSystemColor(
            SWT.COLOR_TITLE_INACTIVE_FOREGROUND);

    final SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd.hhmmss");

    final SimpleDateFormat newSdf = new SimpleDateFormat(
            "EEE d MMM yyyy HH:mm:ss z");

    private StyledText logView;

    private Label logName;

    private List<TreeItem> leaves;

    private ListIterator<TreeItem> leafIter;

    private Tree tree;

    private SearchComposite searchComp;

    private String browserName;

    private Object logDir;

    /**
     * @param parent
     * @param style
     */
    public SessionMsgArchiveBrowser(Composite parent, int style) {
        super(parent, style);
        GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        this.setLayout(new GridLayout(1, true));
        this.setLayoutData(gd);
        init();
    }

    /**
     * @param parent
     * @param style
     */
    public SessionMsgArchiveBrowser(Composite parent, int style,
            LocalizationFile logDir) {
        this(parent, style);
        setDir(logDir);
    }

    public void setDir(LocalizationFile logDir) {

        if (this.logDir != null && this.logDir.equals(logDir)) {
            return;
        }

        try {
            if (logDir == null) {
                UserId user = CollaborationDataManager.getInstance()
                        .getCollaborationConnection(true).getUser();
                logDir = SessionMsgArchive.getArchiveDir(user.getHost(),
                        user.getName(), null);
            }
            this.logDir = logDir;
            setBrowserName(logDir);
            populateTree(tree, leaves, logDir);
            leafIter = leaves.listIterator();
        } catch (ParseException e) {
            statusHandler
                    .error("Unable to parse log directory tree to produce collaboration log view",
                            e);
        }
    }

    protected void init() {
        SashForm mainForm = new SashForm(this, SWT.HORIZONTAL);
        mainForm.setLayout(new GridLayout(2, false));

        GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        gd.widthHint = 800;
        gd.heightHint = 600;
        mainForm.setLayoutData(gd);

        Composite childBarComp = new Composite(mainForm, SWT.NONE);
        childBarComp.setLayout(new GridLayout(1, false));
        childBarComp
                .setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));

        tree = new Tree(childBarComp, SWT.BORDER);
        leaves = new ArrayList<TreeItem>();
        leafIter = leaves.listIterator();

        gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        gd.heightHint = 300;
        gd.widthHint = 400;
        tree.setLayoutData(gd);

        Composite logViewPart = new Composite(mainForm, SWT.NONE);
        logViewPart.setLayout(new GridLayout(1, false));
        logViewPart.setLayoutData(new GridData(SWT.LEFT, SWT.TOP, true, true));

        Composite logHeaderPart = new Composite(logViewPart, SWT.NONE);
        logHeaderPart.setLayout(new GridLayout(1, false));
        logHeaderPart.setLayoutData(new GridData(SWT.LEFT, SWT.TOP, false,
                false));

        logName = new Label(logHeaderPart, SWT.READ_ONLY);
        gd = new GridData(SWT.LEFT, SWT.CENTER, false, false);
        gd.widthHint = 180;
        logName.setLayoutData(gd);

        logView = new StyledText(logViewPart, SWT.V_SCROLL | SWT.H_SCROLL
                | SWT.BORDER | SWT.READ_ONLY);
        gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        logView.setLayoutData(gd);
        logView.setEditable(false);

        searchComp = new SearchComposite(logViewPart, SWT.BORDER);
        searchComp.setSearchText(this);
        searchComp.hide(true);

        tree.addKeyListener(searchComp.getSearchKeyListener());
        logView.addKeyListener(searchComp.getSearchKeyListener());

        tree.addSelectionListener(new SelectionListener() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                Object data = event.item.getData();
                if (data != null) {
                    TreeItem tItem = (TreeItem) event.item;
                    setLogView(tItem);
                }
            }

            @Override
            public void widgetDefaultSelected(SelectionEvent e) {
            }
        });
        mainForm.setWeights(new int[] { 30, 70 });
        mainForm.setVisible(true);
    }

    /**
     * Set the current leaf displayed in the log viewer. If search is on, then
     * 
     * @param tItem
     */
    private void setLogView(TreeItem tItem) {
        Object data = tItem.getData();

        if (data != null) {
            tItem.setExpanded(true);
            tItem.getParent().setSelection(tItem);

            try {
                LocalizationFile lFile = (LocalizationFile) tItem.getData();
                File file = lFile.getFile(true);

                if (file != null && file.exists()) {
                    BufferedReader reader = null;
                    try {
                        FileReader fReader = new FileReader(file);

                        reader = new BufferedReader(fReader);
                        char[] cbuf = new char[1024];
                        StringBuilder sb = new StringBuilder();
                        while (reader.read(cbuf) != -1) {
                            sb.append(cbuf);
                        }
                        logName.setText(tItem.getText());
                        searchComp.setText(sb.toString());
                        logView.setText(sb.toString());
                        logView.setStyleRange(null);
                    } finally {
                        if (reader != null) {
                            reader.close();
                        }
                    }
                }
            } catch (LocalizationException e) {
                statusHandler
                        .error("Unable to retrieve collaboration log file from localization",
                                e);
            } catch (FileNotFoundException e) {
                statusHandler
                        .error("Unable to find collaboration log file in localization",
                                e);
            } catch (IOException e) {
                statusHandler
                        .error("Unable to read collaboration log file from localization",
                                e);
            }
        }
    }

    private void populateTree(Tree tree, List<TreeItem> leaves,
            LocalizationFile logDir) throws ParseException {

        tree.clearAll(true);
        leaves.clear();

        IPathManager pm = PathManagerFactory.getPathManager();
        LocalizationContext[] contexts = pm.getLocalSearchHierarchy(logDir
                .getContext().getLocalizationType());
        LocalizationFile[] files = pm.listFiles(contexts, logDir.getName(),
                null, true, true); // Win32

        Map<String, TreeItem> itemMap = new HashMap<String, TreeItem>(
                files.length);
        for (final LocalizationFile lFile : files) {
            String name = lFile.getName().replaceFirst(
                    logDir.getName() + File.separator, "");
            String[] parts = name.split(File.separator);
            String parentPath = "";
            for (int i = 0; i < parts.length; ++i) {
                String part = parts[i];
                TreeItem parentItem = itemMap.get(parentPath);

                parentPath += File.separator + part;
                TreeItem item = itemMap.get(parentPath);
                if (item == null) {
                    if (parentItem == null) {
                        item = new TreeItem(tree, SWT.NONE);
                    } else {
                        item = new TreeItem(parentItem, SWT.NONE);
                    }

                    boolean isLeaf = (i == parts.length - 1);

                    if (isLeaf) {
                        item.setData(lFile);

                        String datePart = part.substring(0,
                                part.lastIndexOf("."));
                        Date d = sdf.parse(datePart);
                        item.setText(newSdf.format(d));
                        leaves.add(item);
                    } else {
                        item.setForeground(INACTIVE_COLOR);
                        item.setText(part);
                    }
                    item.setExpanded(!isLeaf);
                    itemMap.put(parentPath, item);
                }
            }
        }
    }

    @Override
    public void select(int start, int end) {
        logView.setSelection(start, end);
    }

    /**
     * searching forward, reached last item.
     */
    @Override
    public void reachedLast() {
        TreeItem selected = null;
        TreeItem[] arr = tree.getSelection();

        if (arr.length > 0) {
            selected = arr[0];
        } else {
            selected = leaves.get(0);
        }
        if (leafIter.hasNext()) {
            selected = leafIter.next();
        } else {
            leafIter = leaves.listIterator();
            selected = leafIter.next();
        }
        setLogView(selected);
    }

    /**
     * searching backward, reached first item.
     */
    @Override
    public void reachedFirst() {
        TreeItem selected = null;
        TreeItem[] arr = tree.getSelection();

        if (arr.length > 0) {
            selected = arr[0];
        } else {
            selected = leaves.get(0);
        }
        if (leafIter.hasPrevious()) {
            selected = leafIter.previous();
        } else {
            int index = leaves.size();
            leafIter = leaves.listIterator(index);
            selected = leafIter.previous();
        }
        setLogView(selected);
    }

    /**
     * @return the browserName
     */
    public String getBrowserName() {
        return browserName;
    }

    /**
     * @param logDir
     *            the logDir that defines the browserName
     */
    private void setBrowserName(LocalizationFile logDir) {
        String name = logDir.getName();
        String[] parts = name.split(File.separator);

        if (parts.length > 0) {
            String lastPart = parts[parts.length - 1];

            if (lastPart == null || lastPart.isEmpty()) {
                if (parts.length > 1) {
                    lastPart = parts[parts.length - 2];
                }
            }
            this.browserName = "Log: " + lastPart;
        }
    }

    // @Override
    // public void setHighlights(HighlightSelection[] selections) {
    // StyleRange[] ranges = new StyleRange[selections.length];
    // for (int i = 0; i < selections.length; ++i) {
    // HighlightSelection sel = selections[i];
    // ranges[i] = new StyleRange(sel.start, sel.length,
    // logView.getBackground(), HIGHLIGHT_COLOR);
    // }
    // logView.setStyleRanges(ranges);
    // }
    //
    // @Override
    // public void removeHighlights() {
    // StyleRange[] ranges = logView.getStyleRanges();
    // for (StyleRange style : ranges) {
    // if (style.background == HIGHLIGHT_COLOR) {
    // style.background = logView.getBackground();
    // }
    // }
    // logView.setStyleRanges(ranges);
    // }
}
